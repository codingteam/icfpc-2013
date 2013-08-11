{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Exception
import qualified Data.Text as T
import qualified Data.Map as M
import System.Environment
import System.Random
import Text.Printf
import System.Timeout

import Interfaces
import Trees as E
import Evaluator (doEval)

ourToken = "0379MPEZKNzwqnYUu1DMm7zn2uyo6oflLxR0vukWvpsH1H"
maxRequests = 75
genTimeout = 60 * 1000 * 1000

treesForProblemId :: T.Text -> (M.Map T.Text Problem) -> IO [Expression]
treesForProblemId pid pset = do
  let Just problem = M.lookup pid pset
  treesForProblem problem

treesForProblem :: Problem -> IO [Expression]
treesForProblem problem = do
  let ops = (problemOperators problem)
  rr <- timeout genTimeout $
          evalStateT (do
--                       readMemo
                      r <- generate 1 (problemSize problem - 1) ops ops
--                       writeMemo
                      return r) emptyGState
  case rr of
    Nothing -> fail "Trees generation took too long."
    Just es -> do
      return [e | (eOps, e) <- es]
    --   return [e | (eOps, e) <- es, {- trace ("eOps: " ++ show eOps) -} eOps == problemOperators problem]

requestEvalTree :: Expression -> [E.Value] -> IO EvalResponse
requestEvalTree expr xvalues = do
  let rq = EvalRequest {
             erId = Nothing,
             erProgram = Just (Program expr),
             erArguments = xvalues
           }
  resp <- doHttp True "eval" ourToken rq
  return resp

requestEvalById :: T.Text -> [E.Value] -> IO EvalResponse
requestEvalById pid xvalues = do
  let rq = EvalRequest {
             erId = Just pid,
             erProgram = Nothing,
             erArguments = xvalues
           }
  resp <- doHttp True "eval" ourToken rq
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

solver :: Problem -> Solver Expression
solver problem = do
  trees <- gets sTrees
  lift $ putStrLn $ "Trees left: " ++ show (length trees)
  vals <- generateVals (5 * problemSize problem)
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
  gr <- lift $ doHttp True "guess" ourToken guess
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

solveByPid :: String -> IO ()
solveByPid pid = do
      pset <- readSamples
      let Just problem = M.lookup (T.pack pid) pset
      solveProblem problem

solveProblem :: Problem -> IO ()
solveProblem problem = do
      t <- try $ treesForProblem problem
      case t of
        Left e -> print (e :: SomeException)
        Right allTrees -> do
          putStrLn $ "Trees for problem: list thunk is ready."
          let state = SState allTrees (length allTrees + 1) [] maxRequests
          result <- try $ evalStateT (solver problem) state
          case result of
            Left e -> print (e :: SomeException)
            Right r -> print r

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ["solve", pid] -> do
      solveByPid pid
    ["gen", pid] -> do
      pset <- readSamples
      es <- treesForProblemId (T.pack pid) pset
      putStrLn $ "Trees for problem: list thunk is ready."
      forM_ es $ \e -> do
        putStrLn $ show e ++ " , size : " ++ show (getSize e)
      putStrLn $ "Total: " ++ show (length es)
    ["small"] -> do
      pset <- readSamples
      let smalls = map snd $ M.toList $ M.filter (\p -> problemSize p < 15) pset
      forM_ smalls (putStrLn . T.unpack . problemId)
    ["bysize", ns] -> do
      let n = read ns
      pset <- readSamples
      let smalls = map snd $ M.toList $ M.filter (\p -> problemSize p == n) pset
      forM_ (reverse smalls) $ \p -> do
          putStrLn $ "Solving: " ++ (T.unpack $ problemId p)
          solveProblem p
    _ -> putStrLn "Synopsis: ./Main gen PROBLEMID\nor: ./Main solve PROBLEMID"

