{-# language BangPatterns #-}
{-# language TypeApplications #-}

module Data.Set.Word8
  ( Set
  , member
  , singleton
  , union
  ) where

import Prelude hiding (lookup)

import Control.Monad.ST.Run (runSmallArrayST)
import Data.Bits (testBit,bit,(.|.))
import Data.Primitive (SmallArray)
import Data.WideWord (Word256)
import Data.Word (Word8)

import qualified Data.Primitive as PM

-- | A map whose keys are 8-bit words.
newtype Set = Set Word256

singleton :: Word8 -> Set
singleton !k = Set (bit (fromIntegral @Word8 @Int k))

member :: Word8 -> Set -> Bool
member k (Set keys) =
  testBit keys (fromIntegral @Word8 @Int k)

union :: Set -> Set -> Set
union (Set x) (Set y) = Set (x .|. y)
