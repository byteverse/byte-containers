{-# language BangPatterns #-}
{-# language TypeApplications #-}

module Data.Map.Word8
  ( Map
  , lookup
  , singleton
  ) where

import Prelude hiding (lookup)

import Control.Monad.ST.Run (runSmallArrayST)
import Data.Bits (testBit,bit,unsafeShiftL,(.&.),popCount)
import Data.Primitive (SmallArray)
import Data.WideWord (Word256)
import Data.Word (Word8)

import qualified Data.Primitive as PM

-- | A map whose keys are 8-bit words.
data Map a = Map
  -- Invariant: len(values) = popcnt keys
  {-# UNPACK #-} !Word256
  {-# UNPACK #-} !(SmallArray a)

singleton :: Word8 -> a -> Map a
singleton !k v = Map (bit (fromIntegral @Word8 @Int k))
  (runSmallArrayST (PM.newSmallArray 1 v >>= PM.unsafeFreezeSmallArray))

lookup :: Word8 -> Map a -> Maybe a
lookup kw (Map keys vals) = case testBit keys k of
  False -> Nothing
  True ->
    let ix = popCount (unsafeShiftL maxBound (256 - k) .&. keys)
     in Just (PM.indexSmallArray vals ix)
  where
  k = fromIntegral @Word8 @Int kw
