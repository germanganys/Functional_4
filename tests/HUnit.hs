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
import Text.SimpleJSON ( JSValue(..), Result(..), JSON, decode )
import Free.Commands
    ( Script, jsonToText, printt, jsonObjFromFile, matchJsonObject )
import Data.Text (pack, unwords)
import Prelude hiding (unwords)
import Free.Interpreter (runScript)


checkJSONPatternScript :: String -> String -> Script Bool
checkJSONPatternScript dataFilename patternFilename = do
   -- reading our input data
   dt <- jsonObjFromFile dataFilename
   ds <- jsonToText (JSObject dt)
   printt $ unwords [pack "input-data: ", ds]

   -- reading our pattern to match
   dpattern <- jsonObjFromFile patternFilename
   dps <- jsonToText (JSObject dpattern)
   printt $ unwords [pack "pattern: ", dps]

   checkedData <- matchJsonObject dpattern dt
   case checkedData of
        Just _ -> return True
        Nothing -> return False; 
            

testFreeMonadScriptStructureTest :: String -> String -> Bool -> Test
testFreeMonadScriptStructureTest dataFilename patternFilename pass = 
    TestCase $ do
        result <- runScript (checkJSONPatternScript dataFilename patternFilename)
        assert =<< do return $ pass == result


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
          shouldPass "complex valid input 3" "pass3" (undefined :: JSValue),
          testFreeMonadScriptStructureTest "./tests/unit/pattern-check-should-pass.json" "./tests/unit/struct-pattern.json" True,
          testFreeMonadScriptStructureTest "./tests/unit/pattern-check-should-fail.json" "./tests/unit/struct-pattern.json" False
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
