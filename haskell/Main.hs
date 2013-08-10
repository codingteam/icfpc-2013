{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import System.Environment

import Interfaces
import Trees as E

ourToken = "0379MPEZKNzwqnYUu1DMm7zn2uyo6oflLxR0vukWvpsH1H"

treesForProblem :: T.Text -> IO [Expression]
treesForProblem pid = do
  pset <- readSamples
  let Just problem = M.lookup pid pset
  es <- evalStateT (generate 1 (problemSize problem - 1) (problemOperators problem)) emptyGState
  return $ filter (hasAll 1 (problemOperators problem)) es

requestEvalTree :: Expression -> [E.Value] -> IO EvalResponse
requestEvalTree expr xvalues = do
  let rq = EvalRequest {
             erId = Nothing,
             erProgram = Just (Program expr),
             erArguments = xvalues
           }
  resp <- doHttp "eval" ourToken rq
  return resp

requestEvalById :: T.Text -> [E.Value] -> IO EvalResponse
requestEvalById pid xvalues = do
  let rq = EvalRequest {
             erId = Just pid,
             erProgram = Nothing,
             erArguments = xvalues
           }
  resp <- doHttp "eval" ourToken rq
  return resp

testShifts :: Expression
testShifts = Op1 Shl1 (Var 1)

main :: IO ()
main = do
  [pid] <- getArgs
  es <- treesForProblem (T.pack pid)
  forM_ es $ \e -> do
    putStrLn $ show e ++ " , size : " ++ show (getSize e)
  putStrLn $ "Total: " ++ show (length es)

