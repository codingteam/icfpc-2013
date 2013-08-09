{-# LANGUAGE TypeSynonymInstances #-}

module Trees where

import Control.Monad
import Control.Monad.State
import Data.List (transpose)
import Data.Word
import Data.Bits
import qualified Data.Set as S
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

type Value = Word64

unfoldWord :: Value -> [Value]
unfoldWord w =
  let x0 = w .&. 0xFF
      x1 = (w `shiftR` 8) .&. 0xFF
      x2 = (w `shiftR` 16) .&. 0xFF
      x3 = (w `shiftR` 24) .&. 0xFF
      x4 = (w `shiftR` 32) .&. 0xFF
      x5 = (w `shiftR` 40) .&. 0xFF
      x6 = (w `shiftR` 48) .&. 0xFF
      x7 = (w `shiftR` 56) .&. 0xFF
  in  [x0, x1, x2, x3, x4, x5, x6, x7]

concatFor :: Monad m => [a] -> (a -> m [b]) -> m [b]
concatFor xs fn = do
  ys <- sequence $ map fn xs
  return (concat ys)

split :: Size -> Size -> [[Size]]
split 1 sz = [[sz]]
split n sz = [k:ys | k <- [1..sz-1], ys <- split (n-1) (sz-k) ]

unionsMap :: (Eq a, Eq b, Ord b) => (a -> S.Set b) -> [a] -> S.Set b
unionsMap fn set = S.unions (map fn set)

type Id = Int

data Program = Program Expression
  deriving (Eq)

instance Show Program where
  show (Program expr) = printf "(lambda (x1) %s)" (show expr)

data Expression =
    Const Value
  | Var Id
  | If Expression Expression Expression
  | Fold Expression Expression Id Id Expression
  | Op1 Op1 Expression
  | Op2 Op2 Expression Expression
  deriving (Eq)

data AnyOp = A1 Op1 | A2 Op2 | AFold | ATFold | AIf
  deriving (Eq, Show, Ord)

instance Show Expression where
  show (Const x) = show x
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
  deriving (Eq,Enum,Bounded,Ord)

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
  deriving (Eq,Enum,Bounded,Ord)

instance Show Op2 where
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show Plus = "plus"

getOps :: Int -> Expression -> S.Set AnyOp
getOps _ (Const _) = S.empty
getOps _ (Var _) = S.empty
getOps l (If e0 e1 e2) = S.singleton AIf `S.union` getOps (l+1) e0 `S.union` getOps (l+1) e1 `S.union` getOps (l+1) e2 
getOps 1 (Fold e0 e1 _ _ e2) = S.singleton ATFold `S.union` getOps 2 e0 `S.union` getOps 2 e1 `S.union` getOps 2 e2
getOps l (Fold e0 e1 _ _ e2) = S.singleton AFold `S.union` getOps (l+1) e0 `S.union` getOps (l+1) e1 `S.union` getOps (l+1) e2
getOps l (Op1 op1 e0) = S.singleton (A1 op1) `S.union` getOps (l+1) e0
getOps l (Op2 op2 e0 e1) = S.singleton (A2 op2) `S.union` getOps (l+1) e0 `S.union` getOps (l+1) e1

hasAll :: Int -> S.Set AnyOp -> Expression -> Bool
hasAll l need e = 
  let ops = getOps l e
  in {- trace ("hasAll: " ++ show ops) -} ops == need

data GState = GState {
    lastVariable :: Int
  } deriving (Eq, Show)

emptyGState :: GState
emptyGState = GState 1

type Generate a = StateT GState IO a

type Size = Int

type Level = Int

class Generated a where
  generate :: Level -> Size -> S.Set AnyOp -> Generate [a]

instance Generated Id where
  generate _ 1 _ = do
    n <- gets lastVariable
    return [1..n]

newVariable :: Generate Id
newVariable = do
    n <- gets lastVariable
    modify $ \st -> st {lastVariable = n+1}
    return (n+1)

instance Generated Op1 where
  generate _ 1 list = return [op | A1 op <- S.toList list]

instance Generated Op2 where
  generate _ 1 list = return [op | A2 op <- S.toList list]

filterFolds :: S.Set AnyOp -> S.Set AnyOp -> S.Set AnyOp
filterFolds was ops = if (AFold `S.member` was) || (ATFold `S.member` was)
                        then S.filter (`notElem` [AFold, ATFold]) ops
                        else ops

instance Generated Expression where
  generate lvl 1 ops = do
    var <- generate (lvl+1) 1 ops
    return $ map Var var ++ [Const 0, Const 1]

  generate level size ops = do
    lift $ putStrLn $ printf "[%d] Generating expression of size %d" level size
    ifs <- if AIf `S.member` ops
             then do
                  let sizes = split 3 (size-1)
                  lift $ putStrLn $ printf "[%d] If size=%d, sizes: %s" level size (show sizes)
                  concatFor sizes $ \([sizeCond, sizeE1, sizeE2]) -> do
                    conds <- generate (level+1) sizeCond ops
                    let condOps = unionsMap (getOps level) conds
                    e1s   <- generate (level+1) sizeE1 $ filterFolds condOps ops
                    let e1ops = unionsMap (getOps level) e1s
                    e2s   <- generate (level+1) sizeE2 $ filterFolds (condOps `S.union` e1ops) ops
                    return [If cond e1 e2 | cond <- conds, e1 <- e1s, e2 <- e2s]
             else return []
    let ops_wo_fold = S.delete AFold ops
    folds <- if (AFold `S.member` ops) && level > 1
               then do
                    let sizes = split 3 (size-2)
                    lift $ putStrLn $ printf "[%d] Fold size=%d, sizes: %s" level size (show sizes)
                    concatFor sizes $ \([sizeE0, sizeE1, sizeE2]) -> do
                      e0s <- generate (level+1) sizeE0 ops_wo_fold
                      let e0ops = unionsMap (getOps level) e0s
                      e1s <- generate (level+1) sizeE1 $ filterFolds e0ops ops_wo_fold
                      let e1ops = unionsMap (getOps level) e1s
                      x <- newVariable
                      y <- newVariable
                      e2s <- generate (level+1) sizeE2 $ filterFolds (e0ops `S.union` e1ops) ops_wo_fold
                      modify $ \st -> st {lastVariable = lastVariable st - 2}
                      return [Fold e0 e1 x y e2 | e0 <- e0s, e1 <- e1s, e2 <- e2s]
               else return []
    tfolds <- if ATFold `S.member` ops
               then do
                    let sizes = split 3 (size-2)
                    lift $ putStrLn $ printf "[%d] TFold size=%d, sizes: %s" level size (show sizes)
                    concatFor sizes $ \([sizeE0, sizeE1, sizeE2]) -> do
                      let e0 = Var 1
                      x <- newVariable
                      y <- newVariable
                      let e1 = Const 0
                      e2s <- generate (level+1) sizeE2 $ ops_wo_fold
                      modify $ \st -> st {lastVariable = lastVariable st - 2}
                      return [Fold e0 e1 x y e2 | e2 <- e2s]
               else return []
    op1s <- case [op | A1 op <- S.toList ops] of
              [] -> return []
              o1s -> do
                     lift $ putStrLn $ printf "[%d] Op1 size=%d, size: %d" level size (size-1)
                     es <- generate (level+1) (size-1) ops
                     return [Op1 op e | op <- o1s, e <- es]
    op2s <- case [op | A2 op <- S.toList ops] of
              [] -> return []
              o2s -> do
                     let sizes = split 2 (size-1)
                     lift $ putStrLn $ printf "[%d] Op2 size=%d, sizes: %s" level size (show sizes)
                     concatFor sizes $ \([sizeE1, sizeE2]) -> do
                       e1s <- generate (level+1) sizeE1 ops
                       let e1ops = unionsMap (getOps level) e1s
                       e2s <- generate (level+1) sizeE2 $ filterFolds e1ops ops
                       return [Op2 op e1 e2 | op <- o2s, e1 <- e1s, e2 <- e2s]

    let allTrees = op1s ++ op2s ++ folds ++ tfolds ++ ifs
    return $ allTrees
                  
printTrees :: Size -> IO ()
printTrees size = do
  let ops = S.fromList [AFold, A2 Plus, A1 Not]
  es <- evalStateT (generate 1 size ops) emptyGState
  forM_ es $ \e -> do
    if hasAll 1 ops e
      then putStrLn $ show e
      else return ()
    

