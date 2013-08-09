{-# LANGUAGE OverloadedStrings #-}

module Interfaces where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Aeson

import Trees

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

