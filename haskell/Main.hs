{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import System.Environment

import Interfaces
import Trees as E

treesForProblem :: T.Text -> IO [Expression]
treesForProblem pid = do
  pset <- readSamples
  let Just problem = M.lookup pid pset
  es <- evalStateT (generate 1 (problemSize problem) (problemOperators problem)) emptyGState
  return $ filter (hasAll 1 (problemOperators problem)) es

main :: IO ()
main = do
  [pid] <- getArgs
  es <- treesForProblem (T.pack pid)
  forM_ es print
  putStrLn $ "Total: " ++ show (length es)

