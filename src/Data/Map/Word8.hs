{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language MagicHash #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Map.Word8
  ( Map
  , lookup
  , null
  , size
  , empty
  , singleton
  , union
  , unionWith
  , insert
  , insertWith
  , foldrWithKeys
  , foldl'
  , traverse_
  , toList
  , fromList
  ) where

import Prelude hiding (lookup, null)

import Control.Monad.ST.Run (runSmallArrayST)
import Data.Bits (testBit,bit,unsafeShiftR,unsafeShiftL,(.&.),(.|.),popCount)
import Data.Primitive (SmallArray)
import Data.WideWord (Word256(Word256))
import Data.Word (Word8)

import qualified Data.Foldable as F
import qualified Data.Primitive as PM

-- | A map whose keys are 8-bit words.
data Map a = Map
  -- Invariant: len(values) = popcnt keys
  {-# UNPACK #-} !Word256
  {-# UNPACK #-} !(SmallArray a)

deriving stock instance Eq a => Eq (Map a)
deriving stock instance Functor Map

instance Show a => Show (Map a) where
  showsPrec p m = showsPrec p (toList m)

instance Semigroup a => Semigroup (Map a) where
  (<>) = unionWith (<>)

instance Semigroup a => Monoid (Map a) where
  mempty = empty

singleton :: Word8 -> a -> Map a
singleton !k v = Map (bit (fromIntegral @Word8 @Int k))
  (runSmallArrayST (PM.newSmallArray 1 v >>= PM.unsafeFreezeSmallArray))

-- | Is the passed map empty?
null :: Map a -> Bool
null m = size m == 0

-- | The number of elements the passed map contains.
size :: Map a -> Int
size (Map keys _) = popCount keys

-- | The empty map.
empty :: Map a
empty = Map 0 mempty

-- | Lookup the value at a key in the map.
lookup :: Word8 -> Map a -> Maybe a
lookup kw (Map keys@(Word256 ksD ksC ksB ksA) vals)
  | k .&. 0b00111111 == 0 = case fromIntegral @Int @Word (unsafeShiftR k 6) of
      0 | testBit ksA 0, (# r #) <- PM.indexSmallArray## vals 0 -> Just r
        | otherwise -> Nothing
      1 | testBit ksB 0, (# r #) <- PM.indexSmallArray## vals (popCount ksA) -> Just r
        | otherwise -> Nothing
      2 | testBit ksC 0, (# r #) <- PM.indexSmallArray## vals (popCount ksA + popCount ksB) -> Just r
        | otherwise -> Nothing
      _ | testBit ksD 0, (# r #) <- PM.indexSmallArray## vals (popCount ksA + popCount ksB + popCount ksC) -> Just r
        | otherwise -> Nothing
  | otherwise = case fromIntegral @Int @Word (unsafeShiftR k 6) of
      0 -> case testBit ksA k of
        False -> Nothing
        True -> case PM.indexSmallArray## vals (popCount (unsafeShiftL ksA (64 - k))) of
          (# r #) -> Just r
      1 -> case testBit ksB (k - 64) of
        False -> Nothing
        True -> 
          let pca = popCount ksA
              pcb = popCount (unsafeShiftL ksB (128 - k))
           in case PM.indexSmallArray## vals (pca + pcb) of
                (# r #) -> Just r
      2 -> case testBit ksC (k - 128) of
        False -> Nothing
        True -> 
          let pca = popCount ksA
              pcb = popCount ksB
              pcc = popCount (unsafeShiftL ksC (192 - k))
           in case PM.indexSmallArray## vals (pca + pcb + pcc) of
                (# r #) -> Just r
      _ -> case testBit ksD (k - 192) of
        False -> Nothing
        True ->
          let pca = popCount ksA
              pcb = popCount ksB
              pcc = popCount ksC
              pcd = popCount (unsafeShiftL ksD (256 - k))
           in case PM.indexSmallArray## vals (pca + pcb + pcc + pcd) of
                (# r #) -> Just r
  where
  k = fromIntegral @Word8 @Int kw

-- | The expression @'union' t1 t2@ takes the left-biased union
-- of @t1@ and @t2@. It prefers @t1@ when duplicate keys are
-- encountered (i.e. @'union' == 'unionWith' const@).
union :: Map a -> Map a -> Map a
union !ma@(Map ksA vsA) !mb@(Map ksB vsB)
  | ksA == 0 = mb
  | ksB == 0 = ma
  | otherwise = Map ks $ runSmallArrayST do
      let sz = popCount ks
      dst <- PM.newSmallArray sz =<< PM.indexSmallArrayM vsA 0
      foldlZipBits256
        ( \(!ix,!ixA,!ixB) a b -> case a of
          True -> do
            PM.writeSmallArray dst ix =<< PM.indexSmallArrayM vsA ixA
            pure (ix + 1, ixA + 1, if b then ixB + 1 else ixB)
          False -> case b of
            True -> do
              PM.writeSmallArray dst ix =<< PM.indexSmallArrayM vsB ixB
              pure (ix + 1, ixA, ixB + 1)
            False -> pure (ix, ixA, ixB)
        ) (0,0,0) ksA ksB
      PM.unsafeFreezeSmallArray dst
      where
      ks = ksA .|. ksB

-- | Union with a combining function.
unionWith :: (a -> a -> a) -> Map a -> Map a -> Map a
unionWith g !ma@(Map ksA vsA) !mb@(Map ksB vsB)
  | ksA == 0 = mb
  | ksB == 0 = ma
  | otherwise = Map ks $ runSmallArrayST do
      let sz = popCount ks
      dst <- PM.newSmallArray sz =<< PM.indexSmallArrayM vsA 0
      foldlZipBits256
        ( \(!ix,!ixA,!ixB) a b -> case a of
          True -> case b of
            True -> do
              a <- PM.indexSmallArrayM vsA ixA
              b <- PM.indexSmallArrayM vsB ixB
              let !c = g a b
              PM.writeSmallArray dst ix c
              pure (ix + 1, ixA + 1, ixB + 1)
            False -> do
              PM.writeSmallArray dst ix =<< PM.indexSmallArrayM vsA ixA
              pure (ix + 1, ixA + 1, ixB)
          False -> case b of
            True -> do
              PM.writeSmallArray dst ix =<< PM.indexSmallArrayM vsB ixB
              pure (ix + 1, ixA, ixB + 1)
            False -> pure (ix, ixA, ixB)
        ) (0,0,0) ksA ksB
      PM.unsafeFreezeSmallArray dst
      where
      ks = ksA .|. ksB

insert :: Word8 -> a -> Map a -> Map a
insert = insertWith const

-- | Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ will insert the pair @(key, value)@ into @mp@
-- if @key@ does not exist in the map.
-- If the key does exist, the function will insert the pair
-- @(key, f new_value old_value)@.
insertWith :: (a -> a -> a) -> Word8 -> a -> Map a -> Map a
insertWith f k v m = unionWith f (singleton k v) m

-- Internal function. This is strict in the accumulator.
foldlZipBits256 :: Monad m
  => (a -> Bool -> Bool -> m a) -> a -> Word256 -> Word256 -> m ()
foldlZipBits256 g !a0 !x !y = go 0 a0
  where
  go !ix !a = case ix of
    256 -> pure ()
    _ -> do
      let xval = testBit x ix
      let yval = testBit y ix
      a' <- g a xval yval
      go (ix + 1) a'

foldrBits256 :: (Word8 -> b -> b) -> b -> Word256 -> b
foldrBits256 g b0 w = go 0
  where
  go ix = case ix of
    256 -> b0
    _ -> case testBit w ix of
      True -> g (fromIntegral @Int @Word8 ix) (go (ix + 1))
      False -> go (ix + 1)

foldrWithKeys :: (Word8 -> a -> b -> b) -> b -> Map a -> b
foldrWithKeys g b0 (Map ks vs) = go 0 0
  where
  go !ix !ixVal = case ix of
    256 -> b0
    _ -> case testBit ks ix of
      True -> g
        (fromIntegral @Int @Word8 ix)
        (PM.indexSmallArray vs ixVal)
        (go (ix + 1) (ixVal + 1))
      False -> go (ix + 1) ixVal

foldl' :: (b -> a -> b) -> b -> Map a -> b
{-# inline foldl' #-}
foldl' f b0 (Map _ vs) = F.foldl' f b0 vs

traverse_ :: Applicative m => (a -> m b) -> Map a -> m ()
{-# inline traverse_ #-}
traverse_ f (Map _ vs) = F.traverse_ f vs

toList :: Map a -> [(Word8,a)]
toList = foldrWithKeys (\k v b -> (k,v) : b) []

fromList :: [(Word8,a)] -> Map a
fromList = F.foldl' (\acc (k,v) -> union acc (singleton k v)) empty
