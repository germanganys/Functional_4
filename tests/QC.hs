{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map as M
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.QuickCheck as QC
    ( Arbitrary(arbitrary),
      Gen,
      forAll,
      testProperty,
      Property )
import Text.SimpleJSON
    ( JSObject,
      JSString,
      toJSString,
      toJSObject,
      Result(Ok),
      JSON,
      decode,
      encode )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Unit tests: json serialize - deserialize"
        [ testProperty "Integer" (test :: T Integer),
          testProperty "Int" (test :: T Int),
          testProperty "Double" (test :: T Double),
          testProperty "Float" (test :: T Float),
          testProperty "String" (test :: T JSString),
          testProperty "Char" (test :: T Char),
          testProperty "[Int]" (test :: T [Int]),
          testProperty "[Bool]" (test :: T [Bool]),
          testProperty "[Integer]" (test :: T [Integer]),
          testProperty "[Int]" (test :: T [Int])
         ]

type T a = a -> Property

test :: forall a. (Show a, Arbitrary a, Eq a, JSON a) => a -> Property
test _ = forAll (arbitrary :: Gen a) $ \a ->
    Ok a == decode (encode a)

instance Arbitrary JSString where
    arbitrary = fmap toJSString arbitrary

instance (Ord e, Arbitrary e) => Arbitrary (JSObject e) where
    arbitrary = do
        ks <- arbitrary
        vs <- arbitrary
        return . toJSObject . M.toList . M.fromList . zip ks $ vs

