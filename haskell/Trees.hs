{-# LANGUAGE TypeSynonymInstances #-}

module Trees where

import Control.Monad
import Control.Monad.State
import Data.List (elemIndex, transpose)
import qualified Data.Maybe as Maybe
import Data.Word
import Data.Bits
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Printf
import Debug.Trace
import Numeric (showHex)
-- import System.Random hiding (split)

-- (<$>) :: Functor m => (a -> b) -> m [a] -> m [b]
-- fn <$> mxs = map fn `fmap` mxs
-- 
-- (<*>) :: Monad m => m [a -> b] -> m [a] -> m [b]
-- mfns <*> mxs = do
--   fns <- mfns
--   xs <- mxs
--   return [fn x | fn <- fns, x <- xs]

type Memo = M.Map Size (M.Map (S.Set AnyOp) (M.Map Int [Expression]))

newtype Value = Value Word64
  deriving (Eq)

instance Show Value where
  show (Value 0) = "0"
  show (Value 1) = "1"
  show (Value w) = "0x" ++ showHex w ""

unfoldWord :: Value -> [Value]
unfoldWord (Value w) =
  let x0 = w .&. 0xFF
      x1 = (w `shiftR` 8) .&. 0xFF
      x2 = (w `shiftR` 16) .&. 0xFF
      x3 = (w `shiftR` 24) .&. 0xFF
      x4 = (w `shiftR` 32) .&. 0xFF
      x5 = (w `shiftR` 40) .&. 0xFF
      x6 = (w `shiftR` 48) .&. 0xFF
      x7 = (w `shiftR` 56) .&. 0xFF
  in  map Value [x0, x1, x2, x3, x4, x5, x6, x7]

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
  | If0 Expression Expression Expression
  | Fold Expression Expression Id Id Expression
  | Op1 Op1 Expression
  | Op2 Op2 Expression Expression
  deriving (Eq)

getSize :: Expression -> Int
getSize (Const _) = 1
getSize (Var _) = 1
getSize (If0 e0 e1 e2) = 1 + getSize e0 + getSize e1 + getSize e2
getSize (Fold e1 e2 _ _ e3) = 2 + getSize e1 + getSize e2 + getSize e3
getSize (Op1 _ e) = 1 + getSize e
getSize (Op2 _ e1 e2) = 1 + getSize e1 + getSize e2

data AnyOp = A1 Op1 | A2 Op2 | AFold | ATFold | AIf0
  deriving (Eq, Show, Ord)

instance Show Expression where
  show (Const x) = show x
  show (Var i) = "x" ++ show i
  show (If0 e1 e2 e3) = printf "(if %s %s %s)" (show e1) (show e2) (show e3)
  show (Fold e1 e2 v1 v2 fn) = printf "(fold %s %s (lambda (x%s x%s) %s))" (show e1) (show e2) (show v1) (show v2) (show fn)
  show (Op1 op e) = printf "(%s %s)" (show op) (show e)
  show (Op2 op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)

data Op1 =
    Not
  | Shl1
  | Shr1
  | Shr4
  | Shr16
  deriving (Eq,Enum,Bounded,Ord)

nextOp1' :: Op1 -> Maybe Op1
nextOp1' Not   = Just Shl1
nextOp1' Shl1  = Just Shr1
nextOp1' Shr1  = Just Shr4
nextOp1' Shr4  = Just Shr16
nextOp1' Shr16 = Nothing

nextOp1 :: Op1 -> S.Set AnyOp -> Maybe Op1
nextOp1 o set = case nextOp1' o of
                  Just o' | (A1 o') `S.member` set -> Just o'
                  Just o'                          -> nextOp1 o' set
                  Nothing                          -> Nothing

minOp1 :: S.Set AnyOp -> Maybe Op1
minOp1 set =
  minOp1'$ Just Not
  where minOp1' :: Maybe Op1 -> Maybe Op1
        minOp1' (Just o) = if (A1 o) `S.member` set
                           then Just o
                           else minOp1' (nextOp1' o)
        minOp1' Nothing  = Nothing

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

nextOp2' :: Op2 -> Maybe Op2
nextOp2' And  = Just Or
nextOp2' Or   = Just Xor
nextOp2' Xor  = Just Plus
nextOp2' Plus = Nothing

nextOp2 :: Op2 -> S.Set AnyOp -> Maybe Op2
nextOp2 o set = case nextOp2' o of
                  Just o' | (A2 o') `S.member` set -> Just o'
                  Just o'                          -> nextOp2 o' set
                  Nothing                          -> Nothing

minOp2 :: S.Set AnyOp -> Maybe Op2
minOp2 set =
  minOp2' $ Just And
  where minOp2' (Just o) = if (A2 o) `S.member` set
                           then Just o
                           else minOp2' (nextOp2' o)
        minOp2' Nothing  = Nothing

instance Show Op2 where
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show Plus = "plus"

getOps :: Int -> Expression -> S.Set AnyOp
getOps _ (Const _) = S.empty
getOps _ (Var _) = S.empty
getOps l (If0 e0 e1 e2) = S.singleton AIf0 `S.union` getOps (l+1) e0 `S.union` getOps (l+1) e1 `S.union` getOps (l+1) e2 
getOps l (Fold e0 e1 _ _ e2) = S.singleton op `S.union` getOps 2 e0 `S.union` getOps 2 e1 `S.union` getOps 2 e2
  where
    op | l == 1 && e1 == Const (Value 0) && e0 == Var 1 = ATFold
       | otherwise = AFold
getOps l (Op1 op1 e0) = S.singleton (A1 op1) `S.union` getOps (l+1) e0
getOps l (Op2 op2 e0 e1) = S.singleton (A2 op2) `S.union` getOps (l+1) e0 `S.union` getOps (l+1) e1

hasAll :: Int -> S.Set AnyOp -> Expression -> Bool
hasAll l need e = 
  let ops = getOps l e
  in  {- trace ("hasAll: " ++ show ops) -} ops == need

data GState = GState {
    lastVariable :: Int
  , gMemo :: Memo
  , gFoldAllowedStack :: [Bool]
  } deriving (Eq, Show)

emptyGState :: GState
emptyGState = GState 1 M.empty [True]

type Generate a = StateT GState IO a

type Size = Int

type Level = Int

class Generated a where
  generate :: Level -> Size -> S.Set AnyOp -> Generate [a]

instance Generated Id where
  generate _ 1 _ = do
    n <- gets lastVariable
    return [1..n]

getFoldAllowed :: Generate Bool
getFoldAllowed = gets (head . gFoldAllowedStack)

newFoldContext :: Generate ()
newFoldContext = modify $ \st -> st {gFoldAllowedStack = allow (gFoldAllowedStack st)}
  where
    allow (x:xs) = x: x: xs

forbidFold :: Generate ()
forbidFold = modify $ \st -> st {gFoldAllowedStack = forbid (gFoldAllowedStack st)}
  where
    forbid (_:xs) = False : xs

leaveFoldContext :: Generate ()
leaveFoldContext = modify $ \st -> st {gFoldAllowedStack = tail (gFoldAllowedStack st)}

newVariable :: Generate Id
newVariable = do
    n <- gets lastVariable
    modify $ \st -> st {lastVariable = n+1}
    return (n+1)

memoLookup :: Size -> S.Set AnyOp -> Int -> Memo -> Maybe [Expression]
memoLookup size ops nvars memo = do
  opmap <- M.lookup size memo
  varmap <- M.lookup ops opmap
  M.lookup nvars varmap

memoInsert :: Size -> S.Set AnyOp -> Int -> [Expression] -> Memo -> Memo
memoInsert size ops nvars exprs memo =
    M.insertWith insertOps size (M.singleton ops (M.singleton nvars exprs)) memo
  where 
      insertOps m1 m2 = M.unionWith M.union m1 m2

getMemo :: Size -> S.Set AnyOp -> Int -> Generate (Maybe [Expression])
getMemo size ops nvars = do
  memo <- gets gMemo
  return $ memoLookup size ops nvars memo

putMemo :: Size -> S.Set AnyOp -> Int -> [Expression] -> Generate ()
putMemo size ops nvars exprs = do
  modify $ \st -> st {gMemo = memoInsert size ops nvars exprs (gMemo st)}

instance Generated Op1 where
  generate _ 1 list = return [op | A1 op <- S.toList list]

instance Generated Op2 where
  generate _ 1 list = return [op | A2 op <- S.toList list]

filterFolds _ x = x
-- filterFolds :: S.Set AnyOp -> S.Set AnyOp -> S.Set AnyOp
-- filterFolds was ops = if (AFold `S.member` was) || (ATFold `S.member` was)
--                         then S.filter (`notElem` [AFold, ATFold]) ops
--                         else ops

anyOps = S.fromList [
  A1 Not,
  A1 Shl1,
  A1 Shr1,
  A1 Shr4,
  A1 Shr16,
  A2 And,
  A2 Or,
  A2 Xor,
  A2 Plus]

fromMaybe :: Maybe a -> a
fromMaybe = Maybe.fromMaybe undefined

simpleTree :: S.Set AnyOp -> Size -> Expression
simpleTree set size | ATFold `S.member` set =
  let set'  = S.delete ATFold set
      m     = simpleTree set' 1
      right = simpleTree set' $ size - 4
      x     = 1
      y     = 2
  in Fold m m x y right
simpleTree _ 1 = Const $ Value 0
simpleTree set size | (A1 Not) `S.member` set = Op1 Not $ simpleTree set (size - 1)
simpleTree set size =
  let tree = simpleTree anyOps size
  in fromMaybe $ nextTree tree [1] set

newVar :: [Id] -> Int
newVar (v:vs) = v + 1

nextTree :: Expression -> [Id] -> S.Set AnyOp -> Maybe Expression
nextTree (Const (Value 0)) vs _ = Just $ Const $ Value 1
nextTree (Const (Value 1)) (v:vs) _ = Just $ Var v
nextTree (Var var) vs _ =
  let searcher (x:x2:xss) | x == var = Just x2
      searcher (x:x2:xss) = searcher (x2:xss)
      searcher _                     = Nothing
  in case searcher vs of
    Just v -> Just $ Var v
    Nothing -> Nothing
nextTree (Const (Value 1)) _ _ = Nothing

nextTree (Op1 o a) vs os | nextA /= Nothing 
                           && (A1 o) `S.member` os =
  let a'  = fromMaybe nextA
  in Just $ Op1 o a'
  where nextA = nextTree a vs os
nextTree (Op1 o a) vs os | nextO /= Nothing =
  Just $ Op1 (fromMaybe nextO) $ simpleTree os $ getSize a
  where nextO = nextOp1 o os
nextTree (Op1 o a) vs os | sizeA >= 2
                           && minO2 /= Nothing =
  let left  = simpleTree os 1
      right = simpleTree os $ sizeA - 1
  in Just $ Op2 (fromMaybe minO2) left right
  where sizeA = getSize a
        minO2 = minOp2 os
nextTree (Op1 o a) vs os | sizeA >= 3
                           && AIf0 `S.member` os =
  let m     = simpleTree os 1
      right = simpleTree os $ sizeA - 2
  in Just $ If0 m m right
  where sizeA = getSize a
nextTree (Op1 _ _) _ _ =
  Nothing

nextTree (Op2 o a b) vs os | nextB /= Nothing =
  let b' = fromMaybe nextB
  in Just $ Op2 o a b'
  where nextB = nextTree b vs os
nextTree (Op2 o a b) vs os | nextA /= Nothing =
  let a' = fromMaybe nextA
  in Just $ Op2 o a' $ simpleTree os sizeB
  where nextA = nextTree a vs os
        sizeB = getSize b
nextTree (Op2 o a b) vs os | sizeB > 1 =
  let a' = simpleTree os $ sizeA + 1
      b' = simpleTree os $ sizeB - 1
  in Just $ Op2 o a' b'
  where sizeA = getSize a
        sizeB = getSize b
nextTree (Op2 o a b) vs os | nextO /= Nothing =
  let a' = simpleTree os 1
      b' = simpleTree os $ sizeA + sizeB - 1
  in Just $ Op2 (fromMaybe nextO) a' b'
  where nextO = nextOp2 o os
        sizeA = getSize a
        sizeB = getSize b
nextTree (Op2 o a b) vs os | sizeA > 1
                             && AIf0 `S.member` os =
  let m     = simpleTree os 1
      right = simpleTree os $ sizeA - 1
  in Just $ If0 m m right
  where sizeA = getSize a
nextTree (Op2 _ _ _) _ _ =
  Nothing

nextTree (If0 a b c) vs os | nextC /= Nothing =
  let c' = fromMaybe nextC
  in Just $ If0 a b c'
  where nextC = nextTree c vs os
nextTree (If0 a b c) vs os | nextB /= Nothing =
  let b' = fromMaybe nextB
      c' = simpleTree os sizeC
  in Just $ If0 a b' c'
  where nextB = nextTree b vs os
        sizeC = getSize c
nextTree (If0 a b c) vs os | nextA /= Nothing =
  let a' = fromMaybe nextA
      b' = simpleTree os sizeB
      c' = simpleTree os sizeC
  in Just $ If0 a' b' c'
  where nextA = nextTree a vs os
        sizeB = getSize b
        sizeC = getSize c
nextTree (If0 a b c) vs os | sizeC > 1 =
  let b' = simpleTree os $ sizeB + 1
      c' = simpleTree os $ sizeC - 1
  in Just $ If0 a b' c'
  where sizeB = getSize b
        sizeC = getSize c
nextTree (If0 a b c) vs os | sizeB > 1 =
  let a' = simpleTree os $ sizeA + 1
      b' = simpleTree os $ sizeB - 1
      c' = simpleTree os 1
  in Just $ If0 a' b' c'
  where sizeA = getSize a
        sizeB = getSize b
nextTree (If0 a b c) vs os | sizeA >= 2
                             && AFold `S.member` os =
  let m     = simpleTree os 1
      right = simpleTree os $ sizeA - 1
      x     = newVar vs
      vs'   = x : vs'
      y     = newVar vs'
  in Just $ Fold m m x y right
  where sizeA = getSize a
nextTree (If0 _ _ _) _ _ =
  Nothing

nextTree (Fold a b x y c) vs os | nextC /= Nothing =
  let c' = fromMaybe nextC
  in Just $ Fold a b x y c'
  where vs' = x : y : vs
        nextC = nextTree c vs' os
nextTree (Fold a b x y c) vs os | nextB /= Nothing =
  let b' = fromMaybe nextB
      c' = simpleTree os sizeC
  in Just $ Fold a b' x y c'
  where nextB = nextTree b vs os
        sizeC = getSize c
nextTree (Fold a b x y c) vs os | nextA /= Nothing =
  let a' = fromMaybe nextA
      b' = simpleTree os sizeB
      c' = simpleTree os sizeC
  in Just $ Fold a' b' x y c'
  where nextA = nextTree a vs os
        sizeB = getSize b
        sizeC = getSize c
nextTree (Fold a b x y c) vs os | sizeC > 1 =
  let b' = simpleTree os $ sizeB + 1
      c' = simpleTree os $ sizeC - 1
  in Just $ Fold a b' x y c'
  where sizeB = getSize b
        sizeC = getSize c
nextTree (Fold a b x y c) vs os | sizeB > 1 =
  let a' = simpleTree os $ sizeA + 1
      b' = simpleTree os $ sizeB - 1
      c' = simpleTree os 1
  in Just $ Fold a' b' x y c'
  where sizeA = getSize a
        sizeB = getSize b
nextTree (Fold _ _ _ _ _) _ _ =
  Nothing

generateTrees :: Size -> S.Set AnyOp -> [Expression]
generateTrees size ops =
  let simple = simpleTree ops size
  in simple : loop (simpleTree ops size)
  where hasTFold  = S.member ATFold ops
        ops'      = S.delete ATFold ops
        vars      = if hasTFold then [1, 2] else [1]
        loop tree = case nextTree tree vars ops' of
                      Just newTree -> newTree : loop newTree
                      Nothing      -> []

instance Generated Expression where
  generate lvl 1 ops = do
    var <- generate (lvl+1) 1 ops
    return $ map Var var ++ [Const (Value 0), Const (Value 1)]

  generate level size ops = do
    lift $ putStrLn $ printf "[%d] Generating expression of size %d" level size
    nvars <- gets lastVariable
    mbMemo <- getMemo size ops nvars
    case mbMemo of
      Just exprs -> do
                    lift $ putStrLn $ printf "[%d] Expressions got from memo" level 
                    return exprs
      Nothing -> do
          ifs <- if AIf0 `S.member` ops
                   then do
                        let sizes = split 3 (size-1)
                        lift $ putStrLn $ printf "[%d] If0 size=%d, sizes: %s" level size (show sizes)
                        concatFor sizes $ \([sizeCond, sizeE1, sizeE2]) -> do
                          newFoldContext
                          conds <- generate (level+1) sizeCond ops
                          e1s   <- generate (level+1) sizeE1 ops
                          e2s   <- generate (level+1) sizeE2 ops
                          leaveFoldContext
                          return [If0 cond e1 e2 | cond <- conds, e1 <- e1s, e2 <- e2s]
                   else return []
          let ops_wo_fold = S.delete AFold ops
          foldAllowed <- getFoldAllowed
          folds <- if foldAllowed && (AFold `S.member` ops)
                     then do
                          let sizes = split 3 (size-2)
                          lift $ putStrLn $ printf "[%d] Fold size=%d, sizes: %s" level size (show sizes)
                          concatFor sizes $ \([sizeE0, sizeE1, sizeE2]) -> do
                            newFoldContext
                            e0s <- generate (level+1) sizeE0 ops_wo_fold
                            e1s <- generate (level+1) sizeE1 ops_wo_fold
                            x <- newVariable
                            y <- newVariable
                            e2s <- generate (level+1) sizeE2 ops_wo_fold
                            modify $ \st -> st {lastVariable = lastVariable st - 2}
                            leaveFoldContext
                            return [Fold e0 e1 x y e2 | e0 <- e0s, e1 <- e1s, e2 <- e2s]
                     else return []
          foldAllowed <- getFoldAllowed
          tfolds <- if foldAllowed && (ATFold `S.member` ops) && (level == 1)
                     then do
                          newFoldContext
                          let sizeE2 = size-4
                          lift $ putStrLn $ printf "[%d] TFold size=%d, child size: %d" level size sizeE2
                          let e0 = Var 1
                          x <- newVariable
                          y <- newVariable
                          let e1 = Const (Value 0)
                          e2s <- generate (level+1) sizeE2 $ ops_wo_fold
                          modify $ \st -> st {lastVariable = lastVariable st - 2}
                          leaveFoldContext
                          return [Fold e0 e1 x y e2 | e2 <- e2s]
                     else return []
          op1s <- case [op | A1 op <- S.toList ops] of
                    [] -> return []
                    o1s -> do
                           lift $ putStrLn $ printf "[%d] Op1 size=%d, size: %d" level size (size-1)
                           newFoldContext
                           es <- generate (level+1) (size-1) ops
                           leaveFoldContext
                           return [Op1 op e | op <- o1s, e <- es]
          op2s <- case [op | A2 op <- S.toList ops] of
                    [] -> return []
                    o2s -> do
                           let sizes = split 2 (size-1)
                           lift $ putStrLn $ printf "[%d] Op2 size=%d, sizes: %s" level size (show sizes)
                           concatFor sizes $ \([sizeE1, sizeE2]) -> do
                             newFoldContext
                             e1s <- generate (level+1) sizeE1 ops
                             e2s <- generate (level+1) sizeE2  ops
                             leaveFoldContext
                             return [Op2 op e1 e2 | op <- o2s, e1 <- e1s, e2 <- e2s]

--           lift $ putStrLn $ printf "[%d] For size %d. O1: %d; O2: %d; Fold: %d; TFold: %d; If0: %d" level size
--                                    (length op1s) (length op2s) (length folds) (length tfolds) (length ifs)
          let allTrees = op1s ++ op2s ++ folds ++ tfolds ++ ifs
--           forM_ allTrees $ \e -> do
--             let treeSize = getSize e
--             when (treeSize /= size) $
--               fail $ printf "Invalid generated tree size: %d instead of %d. Tree: %s" treeSize size (show e)
          putMemo size ops nvars allTrees
          return $ allTrees
                  
getTrees :: Size -> [AnyOp] -> IO [Expression]
getTrees size oplist = do
  let ops = S.fromList oplist
  es <- evalStateT (generate 1 size ops) emptyGState
  putStrLn "Trees generated."
  return $ filter (hasAll 1 ops) es

printTrees :: Size -> IO ()
printTrees size = do
  let ops = S.fromList [AFold, A1 Not, A1 Shl1, A1 Shr4, A2 Xor]
  es <- evalStateT (generate 1 size ops) emptyGState
  forM_ es $ \e -> do
    if hasAll 1 ops e
      then putStrLn $ show e
      else return ()
    

