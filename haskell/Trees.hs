{-# LANGUAGE TypeSynonymInstances #-}

module Trees where

import Control.Monad
import Control.Monad.State
import Data.List (transpose)
import Text.Printf
import Debug.Trace
-- import System.Random hiding (split)

-- (<$>) :: Functor m => (a -> b) -> m [a] -> m [b]
-- fn <$> mxs = map fn `fmap` mxs
-- 
-- (<*>) :: Monad m => m [a -> b] -> m [a] -> m [b]
-- mfns <*> mxs = do
--   fns <- mfns
--   xs <- mxs
--   return [fn x | fn <- fns, x <- xs]

concatFor :: Monad m => [a] -> (a -> m [b]) -> m [b]
concatFor xs fn = do
  ys <- sequence $ map fn xs
  return (concat ys)

split :: Size -> Size -> [[Size]]
split 1 sz = [[sz]]
split n sz = [[k] ++ ys | k <- [1..sz-1], ys <- split (n-1) (sz-k) ]

type Id = Int

data Program = Program Id Expression
  deriving (Eq)

instance Show Program where
  show (Program var expr) = printf "(lambda (%s) %s)" (show var) (show expr)

data Expression =
    Const Bool
  | Var Id
  | If Expression Expression Expression
  | Fold Expression Expression Id Id Expression
  | Op1 Op1 Expression
  | Op2 Op2 Expression Expression
  deriving (Eq)

instance Show Expression where
  show (Const False) = "0"
  show (Const True)  = "1"
  show (Var i) = "x" ++ show i
  show (If e1 e2 e3) = printf "(if %s %s %s)" (show e1) (show e2) (show e3)
  show (Fold e1 e2 v1 v2 fn) = printf "(fold %s %s (lambda (x%s x%s) %s)" (show e1) (show e2) (show v1) (show v2) (show fn)
  show (Op1 op e) = printf "(%s %s)" (show op) (show e)
  show (Op2 op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)

data Op1 =
    Not
  | Shl1
  | Shr1
  | Shr4
  | Shr16
  deriving (Eq,Enum,Bounded)

instance Show Op1 where
  show Not = "not"
  show Shl1 = "shl1"
  show Shr1 = "shr1"
  show Shr4 = "shr4"
  show Shr16 = "shr16"

data Op2 =
    And
  | Or
  | Xor
  | Plus
  deriving (Eq,Enum,Bounded)

instance Show Op2 where
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show Plus = "plus"

data AnyOp = A1 Op1 | A2 Op2 | AFold | ATFold | AIf
  deriving (Eq, Show)

data GState = GState {
    lastVariable :: Int
  , currentLevel :: Int
  } deriving (Eq, Show)

emptyGState :: GState
emptyGState = GState 1 0

type Generate a = StateT GState IO a

type Size = Int

class Generated a where
  generate :: Size -> [AnyOp] -> Generate [a]

instance Generated Id where
  generate 1 _ = do
    n <- gets lastVariable
    return [1..n]

newVariable :: Generate Id
newVariable = do
    n <- gets lastVariable
    modify $ \st -> st {lastVariable = n+1}
    return (n+1)

instance Generated Op1 where
  generate 1 list = return [op | A1 op <- list]

instance Generated Op2 where
  generate 1 list = return [op | A2 op <- list]

instance Generated Expression where
  generate 1 ops = do
    var <- generate 1 ops
    return $ map Var var ++ [Const False, Const True]

  generate size ops = do
    modify $ \st -> st {currentLevel = currentLevel st + 1}
    ifs <- if AIf `elem` ops
             then do
                  let sizes = split 3 (size-1)
                  lift $ putStrLn $ printf "If size=%d, sizes: %s" size (show sizes)
                  concatFor sizes $ \([sizeCond, sizeE1, sizeE2]) -> do
                    conds <- generate sizeCond ops
                    e1s   <- generate sizeE1 ops
                    e2s   <- generate sizeE2 ops
                    return [If cond e1 e2 | cond <- conds, e1 <- e1s, e2 <- e2s]
             else return []
    let ops_wo_fold = filter (/= AFold) ops
    level <- gets currentLevel
    folds <- if (AFold `elem` ops) && level > 1
               then do
                    let sizes = split 3 (size-2)
                    lift $ putStrLn $ printf "Fold size=%d, sizes: %s" size (show sizes)
                    concatFor sizes $ \([sizeE0, sizeE1, sizeE2]) -> do
                      e0s <- generate sizeE0 ops_wo_fold
                      e1s <- generate sizeE1 ops_wo_fold
                      x <- newVariable
                      y <- newVariable
                      e2s <- generate sizeE2 ops_wo_fold
                      modify $ \st -> st {lastVariable = lastVariable st - 2}
                      return [Fold e0 e1 x y e2 | e0 <- e0s, e1 <- e1s, e2 <- e2s]
               else return []
    tfolds <- if ATFold `elem` ops
               then do
                    let sizes = split 3 (size-2)
                    lift $ putStrLn $ printf "TFold size=%d, sizes: %s" size (show sizes)
                    concatFor sizes $ \([sizeE0, sizeE1, sizeE2]) -> do
                      e0s <- generate sizeE0 ops_wo_fold
                      e1s <- generate sizeE1 ops_wo_fold
                      x <- newVariable
                      e2s <- generate sizeE2 ops_wo_fold
                      modify $ \st -> st {lastVariable = lastVariable st - 1}
                      return [Fold e0 e1 x 0 e2 | e0 <- e0s, e1 <- e1s, e2 <- e2s]
               else return []
    op1s <- case [op | A1 op <- ops] of
              [] -> return []
              o1s -> do
                     es <- generate (size-1) ops
                     return [Op1 op e | op <- o1s, e <- es]
    op2s <- case [op | A2 op <- ops] of
              [] -> return []
              o2s -> do
                     let sizes = split 2 (size-1)
                     lift $ putStrLn $ printf "Op2 size=%d, sizes: %s" size (show sizes)
                     concatFor sizes $ \([sizeE1, sizeE2]) -> do
                       e1s <- generate sizeE1 ops
                       e2s <- generate sizeE2 ops
                       return [Op2 op e1 e2 | op <- o2s, e1 <- e1s, e2 <- e2s]

    modify $ \st -> st {currentLevel = currentLevel st - 1}
    return $ op1s ++ op2s ++ folds ++ tfolds ++ ifs
                  
printTrees :: Size -> IO ()
printTrees size = do
  es <- evalStateT (generate 8 [AFold, AIf, A2 Plus, A2 Or, A1 Not]) emptyGState
  forM_ es $ \e ->
    print (e :: Expression)

