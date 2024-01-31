{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Map.Word8 (Map)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum)
import Data.Word (Word8)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, Discard (Discard), property, testProperty, (===))

import qualified Data.List as List
import qualified Data.Map.Word8 as Map
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "byte-containers"
    [ testGroup
        "append"
        [ testProperty "doubleton" $ \ka (va :: Word) kb vb ->
            Map.toList
              (Map.union (Map.singleton ka va) (Map.singleton kb vb))
              === ( case compare ka kb of
                      EQ -> [(ka, va)]
                      LT -> [(ka, va), (kb, vb)]
                      GT -> [(kb, vb), (ka, va)]
                  )
        , testProperty "lookup" lookupProp
        , testProperty "associative" $ \alist blist clist ->
            let a = Map.fromList alist :: Map Word
                b = Map.fromList blist
                c = Map.fromList clist
             in Map.union a (Map.union b c)
                  === Map.union (Map.union a b) c
        , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy (Map [Integer])))
        , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (Map [Integer])))
        , lawsToTest (QCC.commutativeMonoidLaws (Proxy :: Proxy (Map (Sum Integer))))
        , lawsToTest (QCC.functorLaws (Proxy :: Proxy Map))
        ]
    , testGroup
        "insert"
        [ testProperty "replaces" $ \k v v' alist ->
            let a = Map.insert k v (Map.fromList alist) :: Map Word
                a' = Map.insert k v' a
             in if
                  | v /= v' ->
                      Map.lookup k a' === Just v'
                  | otherwise -> property Discard
        ]
    ]

instance (Arbitrary a) => Arbitrary (Map a) where
  arbitrary = Map.fromList <$> TQC.arbitrary

lookupProp :: TQC.Property
lookupProp = TQC.property $ \(xs :: [(Word8, Integer)]) ->
  let ys = Map.fromList xs
   in foldr
        ( \x r -> case List.lookup x xs == Map.lookup x ys of
            True -> r
            False -> TQC.counterexample ("key " ++ show x) False
        )
        (TQC.property True)
        [0 .. 255 :: Word8]

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)
