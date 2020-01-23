{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}

module Data.Map.Word8
  ( Map
  , lookup
  , null
  , size
  , empty
  , singleton
  , union
  , unionWith
  , foldrWithKeys
  , toList
  , fromList
  ) where

import Prelude hiding (lookup, null)

import Control.Monad.ST.Run (runSmallArrayST)
import Data.Bits (testBit,bit,unsafeShiftR,(.&.),(.|.),popCount)
import Data.Primitive (SmallArray)
import Data.WideWord (Word256)
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
lookup kw (Map keys vals) = case testBit keys k of
  False -> Nothing
  True -> case k of
    0 -> Just (PM.indexSmallArray vals 0)
    _ -> 
      let ix = popCount (unsafeShiftR maxBound (256 - k) .&. keys)
       in Just (PM.indexSmallArray vals ix)
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

toList :: Map a -> [(Word8,a)]
toList = foldrWithKeys (\k v b -> (k,v) : b) []

fromList :: [(Word8,a)] -> Map a
fromList = F.foldl' (\acc (k,v) -> union acc (singleton k v)) empty
