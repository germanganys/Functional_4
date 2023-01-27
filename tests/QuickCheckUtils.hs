{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- Uses multi-param type classes
--
module QuickCheckUtils where


import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . L.fromChunks . filter (not. B.null) -- maintain the invariant.

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary