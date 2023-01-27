{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where

import qualified Control.Exception as C (catch, evaluate)
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int
import qualified Data.IntMap as I
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import Data.Word
import Debug.Trace
import Foreign
import QuickCheckUtils
import System.Environment
import System.IO
import System.IO.Unsafe
import Test.QuickCheck hiding (test)
import Test.QuickCheck.Instances.Time
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Text.JSON
import Text.Printf

------------------------------------------------------------------------
-- low level ones:

main :: IO ()
main = defaultMain tests

tests =
  testGroup
    "Unit tests: Bidirectional Graph"
    [ t $ ("Integer", (test :: T Integer)),
      t $ ("Int", (test :: T Int)),
      t ("Word", (test :: T Word)),
      -- words

      t $ ("Word8", (test :: T Word8)),
      t $ ("Word16", (test :: T Word16)),
      t $ ("Word32", (test :: T Word32)),
      -- integers

      t $ ("Int8", (test :: T Int8)),
      t $ ("Int16", (test :: T Int16)),
      t $ ("Int32", (test :: T Int32)),
      -- rationals

      t $ ("Double", (test :: T Double)),
      t $ ("Float", (test :: T Float)),
      t $ ("String", (test :: T JSString)),
      t $ ("Strict ByteString", (test :: T S.ByteString)),
      t $ ("Lazy ByteString", (test :: T L.ByteString)),
      t $ ("Char", (test :: T Char)),
      t $ ("UTCTime", (test :: T UTCTime)),
      t $ ("[()]", (test :: T [()])),
      t $ ("[Int]", (test :: T [Int])),
      t $ ("[Bool]", (test :: T [Bool])),
      t $ ("[Integer]", (test :: T [Integer])),
      t $ ("[Int]", (test :: T [Int])),
      t $ ("[Word]", (test :: T [Word])),
      t $ ("[S.ByteString]", (test :: T [S.ByteString])),
      t $ ("[L.ByteString]", (test :: T [L.ByteString])),
      t $ ("IntSet", (test :: T IntSet)),
      t $ ("Map String Int", (test :: T (M.Map String Int))),
      t $ ("Map Int String", (test :: T (M.Map Int String)))
    ]

t (nm, te) = testProperty nm te

type T a = a -> Property

type B a = a -> Bool

test :: forall a. (Show a, Arbitrary a, Eq a, JSON a) => a -> Property
test _ = forAll (arbitrary :: Gen a) $ \a ->
  Ok a == decode (encode a)

instance Arbitrary JSString where
  arbitrary = liftM toJSString arbitrary

instance (Ord e, Arbitrary e) => Arbitrary (JSObject e) where
  arbitrary = do
    ks <- arbitrary
    vs <- arbitrary
    return . toJSObject . M.toList . M.fromList . zip ks $ vs
