{-# LANGUAGE OverloadedStrings #-}

module Interfaces where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Aeson hiding (Error)

import Trees as E

defaultTimeLeft = 300

data Problem = Problem {
    problemId :: T.Text
  , problemSize :: Size
  , problemOperators :: S.Set AnyOp
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

instance FromJSON AnyOp where
  parseJSON (String t) =
    case t of
      "tfold" -> return ATFold
      "fold" -> return AFold
      "if0" -> return AIf
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

readSamples :: IO ()
readSamples = do
  json <- L.readFile "../samples/myproblems.json"
  case eitherDecode json of
    Left err -> fail $ show err
    Right list -> forM_ list $ \problem -> do
                    print (problem :: Problem)

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
      <*> o .: "values" .!= []
      <*> o .: "message" .!= ""
      <*> o .: "lightning" .!= False
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
  


