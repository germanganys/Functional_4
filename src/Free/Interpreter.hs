
module Free.Interpreter (
    module Free.Commands,
    runCommand, runScript
) where

import Free.Commands
import Control.Monad.Free ( foldFree )
import Data.Text ( pack, unpack )
import Text.SimpleJSON.Types ( JSValue(..), rmField, getField, setField, matchJSStructure )
import Text.Parsec.String (parseFromFile)
import Text.SimpleJSON.String (parseObj, showJSValue)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)


runCommand :: Cmd next -> IO next
runCommand cmd = case cmd of
    Print arg next -> do
        putStrLn $ unpack arg
        pure next
    Echo arg getNext -> do
        pure $ getNext arg
    JSONFromFile fname getNext -> do
        parsed <- parseFromFile parseObj fname
        case parsed of 
            Right val -> do 
                case val of
                    JSObject obj -> do
                        pure $ getNext obj
                    _ -> do
                        hPutStrLn stderr "Error, it is not an object!"
                        exitFailure
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ show err
                exitFailure
    JSONToText jsval getNext -> do
        pure $ getNext (pack (showJSValue jsval ""))
    RemoveJSONField k jsobj getNext -> do
        pure $ getNext (rmField jsobj k)
    GetJSONField k jsobj getNext -> do
        case getField jsobj k of
            Just jsval -> do
                pure $ getNext jsval
            Nothing -> do
                hPutStrLn stderr "Error, no such key in dict!"
                exitFailure
    SetJSONField k jsval jsobj getNext -> do
        pure $ getNext (setField jsobj k jsval)
    MatchJSObject patt jsobj getNext -> do
        pure $ getNext (matchJSStructure patt jsobj)
        

runScript :: Script a -> IO a
runScript = foldFree runCommand