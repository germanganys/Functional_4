{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where

import qualified Data.Map as M
import Test.Tasty ( defaultMain, testGroup, TestName, TestTree )
import Test.Tasty.QuickCheck as QC
    ( Arbitrary(arbitrary),
      Gen,
      forAll,
      testProperty,
      Property,
      Testable )
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
        [ t ("Integer", test :: T Integer),
          t ("Int", test :: T Int),
          t ("Double", test :: T Double),
          t ("Float", test :: T Float),
          t ("String", test :: T JSString),
          t ("Char", test :: T Char),
          t ("[Int]", test :: T [Int]),
          t ("[Bool]", test :: T [Bool]),
          t ("[Integer]", test :: T [Integer]),
          t ("[Int]", test :: T [Int])
         ]

t :: forall {a}. Testable a => (TestName, a) -> TestTree
t (nm, te) = testProperty nm te

type T a = a -> Property
type B a = a -> Bool

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

