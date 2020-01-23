{-# language BangPatterns #-}
{-# language TypeApplications #-}

module Data.Set.Word8
  ( Set
  , member
  , null
  , size
  , empty
  , singleton
  , union
  ) where

import Prelude hiding (lookup,null)

import Control.Monad.ST.Run (runSmallArrayST)
import Data.Bits (testBit,bit,(.|.),popCount)
import Data.Primitive (SmallArray)
import Data.WideWord (Word256)
import Data.Word (Word8)

import qualified Data.Primitive as PM

-- | A map whose keys are 8-bit words.
newtype Set = Set Word256

-- | Is the passed map empty?
null :: Set -> Bool
null m = size m == 0

-- | The number of elements the passed map contains.
size :: Set -> Int
size (Set keys) = popCount keys

empty :: Set
empty = Set 0

singleton :: Word8 -> Set
singleton !k = Set (bit (fromIntegral @Word8 @Int k))

member :: Word8 -> Set -> Bool
member k (Set keys) =
  testBit keys (fromIntegral @Word8 @Int k)

union :: Set -> Set -> Set
union (Set x) (Set y) = Set (x .|. y)
