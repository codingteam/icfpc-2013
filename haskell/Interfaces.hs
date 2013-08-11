{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Interfaces where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.BitSet.Word as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Aeson hiding (Error)
import Data.Attoparsec.Number
import Data.Char
import Text.Printf
import Network.HTTP
import Numeric (showHex)

import Trees as E

defaultTimeLeft = 300

instance FromJSON E.Value where
  parseJSON (String t) = do
    return $ E.Value $ read (T.unpack t)
  parseJSON (Number (I i)) =
    return $ E.Value $ fromIntegral i
  parseJSON x = fail $ "Invalid object for Value: " ++ show x

instance ToJSON E.Value where
  toJSON (Value i) = toJSON ("0x" ++ showHex i "")

data Problem = Problem {
    problemId :: T.Text
  , problemSize :: Size
  , problemOperators :: OpSet
  , problemSolved :: Bool
  , problemTimeLeft :: Integer
  } deriving (Eq, Show)

instance FromJSON Problem where
  parseJSON (Object o) =
    Problem
      <$> o .: "id"
      <*> o .: "size"
      <*> o .: "operators"
      <*> o .:? "solved" .!= False
      <*> o .:? "timeLeft" .!= defaultTimeLeft
  parseJSON x = fail $ "Invalid object for Problem: " ++ show x

instance FromJSON OpSet where
  parseJSON (Array v) = do
    xs <- V.mapM parseJSON v
    return $ S.fromList $ V.toList xs
  parseJSON x = fail $ "Invalid object for OpSet: " ++ show x

instance FromJSON AnyOp where
  parseJSON (String t) =
    case t of
      "tfold" -> return ATFold
      "fold" -> return AFold
      "if0" -> return AIf0
      "not" -> return (A1 Not)
      "shl1" -> return (A1 Shl1)
      "shr1" -> return (A1 Shr1)
      "shr4" -> return (A1 Shr4)
      "shr16" -> return (A1 Shr16)
      "and" -> return (A2 And)
      "or" -> return (A2 Or)
      "xor" -> return (A2 Xor)
      "plus" -> return (A2 Plus)
      _ -> fail $ "Unknown op: " ++ T.unpack t
  parseJSON x = fail $ "Invalid object for Op: " ++ show x

readSamples :: IO (M.Map T.Text Problem)
readSamples = do
  json <- L.readFile "../samples/myproblems.json"
  problems <- case eitherDecode json of
                Left err -> fail $ show err
                Right list -> return list
  return $ M.fromList [(problemId problem, problem) | problem <- problems]

data EvalRequest = EvalRequest {
    erId :: Maybe T.Text
  , erProgram :: Maybe Program
  , erArguments :: [E.Value] }
  deriving (Eq, Show)

instance ToJSON EvalRequest where
  toJSON er =
    let idPair = case erId er of
                   Nothing -> []
                   Just i -> ["id" .= i]
        prPair = case erProgram er of
                   Nothing -> []
                   Just p -> ["program" .= p]
    in  object $ idPair ++ prPair ++ ["arguments" .= erArguments er]

instance ToJSON Program where
  toJSON p = String (T.pack $ show p)

data ERStatus = Ok | Error
  deriving (Eq, Show)

data EvalResponse = EvalResponse {
    erStatus :: ERStatus
  , erOutputs :: [E.Value]
  , erMessage :: T.Text }
  deriving (Eq, Show)

instance FromJSON EvalResponse where
  parseJSON (Object o) =
    EvalResponse
      <$> o .: "status"
      <*> o .:? "outputs" .!= []
      <*> o .:? "message" .!= ""
  parseJSON x = fail $ "Invalid object for EvalResponse: " ++ show x

instance FromJSON ERStatus where
  parseJSON (String t) =
    case t of
      "ok" -> return Ok
      "error" -> return Error
      _ -> fail $ "Unknown Response status: " ++ T.unpack t
  parseJSON x = fail $ "Invalid object for Response status: " ++ show x

data Guess = Guess {
    guessId :: T.Text
  , guessProgram :: Program }
  deriving (Eq, Show)

instance ToJSON Guess where
  toJSON g = object ["id" .= guessId g, "program" .= guessProgram g]

data GuessResponse = GuessResponse {
    grStatus :: GRStatus
  , grValues :: [E.Value]
  , grMessage :: T.Text
  , grLightning :: Bool }
  deriving (Eq, Show)

instance FromJSON GuessResponse where
  parseJSON (Object o) =
    GuessResponse
      <$> o .: "status"
      <*> o .:? "values" .!= []
      <*> o .:? "message" .!= ""
      <*> o .:? "lightning" .!= False
  parseJSON x = fail $ "Invalid object for GuessResponse: " ++ show x

data GRStatus = Win | Mismatch | GError
  deriving (Eq, Show)

instance FromJSON GRStatus where
  parseJSON (String t) =
    case t of
      "win" -> return Win
      "mismatch" -> return Mismatch
      "error" -> return GError
      _ -> fail $ "Unknown Response status: " ++ T.unpack t
  parseJSON x = fail $ "Invalid object for Response status: " ++ show x

bsToString :: L.ByteString -> String
bsToString bs = map (chr . fromIntegral) $ L.unpack bs

stringToBs :: String -> L.ByteString
stringToBs str = L.pack $ map (fromIntegral . ord) str

waitTimeout = 4 * 1000 * 1000

wait :: IO ()
wait = do
    putStrLn "Waiting before next request."
    threadDelay waitTimeout

doHttp :: (ToJSON request, FromJSON response) => Bool -> String -> String -> request -> IO response
doHttp shouldRepeat url authToken request = do
  let fullUrl = "http://icfpc2013.cloudapp.net/" ++ url ++ "?auth=" ++ authToken
      postBody = bsToString (encode request)
      postRequest = postRequestWithBody fullUrl "application/json" postBody
  wait
  putStrLn $ "Our request: " ++ postBody
  res <- simpleHTTP postRequest
  case res of
    Left err -> fail $ show err
    Right response -> do
                      code <- getResponseCode res
                      case code of
                        (2,0,0) -> do
                                   responseBody <- getResponseBody res
                                   putStrLn $ "Server response: " ++ responseBody
                                   case eitherDecode (stringToBs responseBody) of
                                    Left err -> fail $ show err
                                    Right result -> return result
                        (4,1,0) -> do
                                   responseBody <- getResponseBody res
                                   fail $ printf "Time exceeded: %s" responseBody
                        (a,b,c) | shouldRepeat -> do
                                   responseBody <- getResponseBody res
                                   putStrLn $ printf "HTTP server returned response code %d%d%d: %s" a b c responseBody
                                   putStrLn "Retrying"
                                   doHttp False url authToken request
                        (a,b,c) -> do
                                   responseBody <- getResponseBody res
                                   fail $ printf "HTTP server returned response code %d%d%d: %s" a b c responseBody

