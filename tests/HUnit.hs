{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Exit (exitFailure)
import System.IO()
import Control.Monad (when)
import Test.HUnit
    ( runTestTT,
      Assertable(assert),
      Counts(failures, errors),
      Test(..) )
import Text.SimpleJSON ( JSValue, Result(..), JSON, decode )

main :: IO ()
main = do
    cnt <- runTestTT tests
    when (errors cnt > 0 || failures cnt > 0) exitFailure

tests :: Test
tests =
    TestList
        [ shouldFail "unclosed array" "fail2" (undefined :: JSValue),
          shouldFail "object keys must be quoted" "fail3" (undefined :: JSValue),
          shouldFail "extra comma" "fail4" (undefined :: JSValue),
          shouldPass "complex valid input 1" "pass1" (undefined :: JSValue),
          shouldPass "complex valid input 2" "pass2" (undefined :: JSValue),
          shouldPass "complex valid input 3" "pass3" (undefined :: JSValue)
        ]

load :: [Char] -> IO String
load n = readFile ("./tests/unit/" ++ n ++ ".json")

shouldFail :: JSON a => String -> String -> a -> Test
shouldFail comment filename (_ :: a) = TestLabel ("Should fail: " ++ comment) $
    TestCase $ do
        s <- load filename
        assert =<< case decode s :: Result a of
            Ok _ -> do return False
            Error _ -> return True

shouldPass :: JSON a => String -> String -> a -> Test
shouldPass comment filename (_ :: a) = TestLabel ("Should pass: " ++ comment) $
    TestCase $ do
        ss <- load filename
        assert =<< case decode ss :: Result a of
            Ok _ -> return True
            Error _ -> do return False
