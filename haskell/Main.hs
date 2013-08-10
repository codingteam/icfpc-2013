{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import System.Environment
import Control.Concurrent (threadDelay)
import System.Random
import Text.Printf

import Interfaces
import Trees as E
import Evaluator (doEval)

ourToken = "0379MPEZKNzwqnYUu1DMm7zn2uyo6oflLxR0vukWvpsH1H"
maxRequests = 75
waitTimeout = 4 * 1000 * 1000

treesForProblem :: T.Text -> (M.Map T.Text Problem) -> IO [Expression]
treesForProblem pid pset = do
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

data SState = SState {
    sTrees :: [Expression]
  , sNPrevTrees :: Int
  , sKnownValues :: [(E.Value, E.Value)]
  , sRequestsLeft :: Int
  }
  deriving (Eq, Show)

type Solver a = StateT SState IO a

generateVals :: Int -> Solver [E.Value]
generateVals n = do
  xs <- lift $ replicateM n $ randomRIO (minBound, maxBound)
  return $ map Value xs

wait :: Solver ()
wait = lift $ do
    putStrLn $ "Waiting before next request."
    threadDelay waitTimeout

solver :: Problem -> Solver Expression
solver problem = do
  trees <- gets sTrees
  lift $ putStrLn $ "Trees left: " ++ show (length trees)
  vals <- generateVals (problemSize problem)
  wait
  lift $ putStrLn $ printf "Ask for /eval for problem %s on values %s" (T.unpack $ problemId problem) (show vals)
  er <- lift $ requestEvalById (problemId problem) vals
  modify $ \st -> st {sRequestsLeft = sRequestsLeft st - 1}
  when (erStatus er /= Ok) $
    fail $ show er
  goodTrees <- filterM (goodTree vals (erOutputs er)) trees
  modify $ \st -> st {sTrees = goodTrees}
  requestsLeft <- gets sRequestsLeft
  prevNTrees <- gets sNPrevTrees
  modify $ \st -> st {sNPrevTrees = length goodTrees}
  if length goodTrees > requestsLeft `div` 2
    then if length goodTrees >= prevNTrees
           then guesser problem
           else solver problem
    else guesser problem

guesser :: Problem -> Solver Expression
guesser problem = do
  tree <- gets (head . sTrees)
  ntrees <- gets (length . sTrees)
  lift $ putStrLn $ "Trees left: " ++ show ntrees
  lift $ putStrLn $ "Guessing: " ++ show (Program tree)
  let guess = Guess {
                guessId = problemId problem,
                guessProgram = Program tree }
  wait
  gr <- lift $ doHttp "guess" ourToken guess
  modify $ \st -> st {sRequestsLeft = sRequestsLeft st - 1}
  case grStatus gr of
    Win -> return tree
    GError -> fail $ show gr
    Mismatch -> do
                let [input, rightResult, ourResult] = grValues gr
                lift $ putStrLn $ printf "Mismatch: for x = %s our result was %s, but right is %s"
                                         (show input) (show ourResult) (show rightResult)
                trees <- gets sTrees
                goodTrees <- filterM (goodTree [input] [rightResult]) trees
                modify $ \st -> st {sTrees = goodTrees}
                guesser problem

checkEval :: Expression -> E.Value -> E.Value -> Solver Bool
checkEval expr x y = do
  result <- lift $ doEval expr x
  return $ result == y

goodTree :: [E.Value] -> [E.Value] -> Expression -> Solver Bool
goodTree [] [] _ = return True
goodTree [x] [y] expr = checkEval expr x y
goodTree (x:xs) (y:ys) expr = do
  ok <- checkEval expr x y
  if (not ok)
    then return False
    else goodTree xs ys expr

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ["solve", pid] -> do
      pset <- readSamples
      let Just problem = M.lookup (T.pack pid) pset
      allTrees <- treesForProblem (T.pack pid) pset
      let state = SState allTrees (length allTrees + 1) [] maxRequests
      result <- evalStateT (solver problem) state
      print result
    ["gen", pid] -> do
      pset <- readSamples
      es <- treesForProblem (T.pack pid) pset
      forM_ es $ \e -> do
        putStrLn $ show e ++ " , size : " ++ show (getSize e)
      putStrLn $ "Total: " ++ show (length es)
    ["small"] -> do
      pset <- readSamples
      let smalls = map snd $ M.toList $ M.filter (\p -> problemSize p < 15) pset
      forM_ smalls (putStrLn . T.unpack . problemId)
    _ -> putStrLn "Synopsis: ./Main gen PROBLEMID\nor: ./Main solve PROBLEMID"

