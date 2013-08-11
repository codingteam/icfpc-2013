{-# LANGUAGE TypeSynonymInstances, BangPatterns, DeriveGeneric, FlexibleInstances #-}

module Trees where

import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.List (transpose)
import Data.Word
import Data.Bits
import qualified Data.BitSet.Word as S
import qualified Data.Map as M
import Text.Printf
import Debug.Trace
import Numeric (showHex)
import GHC.Generics (Generic)
import Data.Binary as B
import System.Directory
import System.IO
-- import System.Random hiding (split)

-- (<$>) :: Functor m => (a -> b) -> m [a] -> m [b]
-- fn <$> mxs = map fn `fmap` mxs
-- 
-- (<*>) :: Monad m => m [a -> b] -> m [a] -> m [b]
-- mfns <*> mxs = do
--   fns <- mfns
--   xs <- mxs
--   return [fn x | fn <- fns, x <- xs]

type OpSet = S.BitSet AnyOp

type Memo = M.Map Size (M.Map (OpSet,OpSet) (M.Map Int [(OpSet, Expression)]))

instance Binary OpSet where 
  put ops = B.put $ S.toList ops

  get = do
    lst <- B.get
    return $ S.fromList lst

-- instance Binary Memo where
--   put memo = B.put $ M.assocs memo
-- 
--   get = do
--     lst <- B.get
--     return $ M.fromList lst

newtype Value = Value Word64
  deriving (Eq, Generic)

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

-- unionsMap :: (Eq a, Eq b, Ord b) => (a -> S.BitSet b) -> [a] -> S.BitSet b
-- unionsMap fn set = S.unions (map fn set)

type Id = Int

data Program = Program Expression
  deriving (Eq, Generic)

instance Show Program where
  show (Program expr) = printf "(lambda (x1) %s)" (show expr)

data Expression =
    Const Value
  | Var Id
  | If0 Expression Expression Expression
  | Fold Expression Expression Id Id Expression
  | Op1 Op1 Expression
  | Op2 Op2 Expression Expression
  deriving (Eq, Generic)

instance Binary Value where
instance Binary Op1 where
instance Binary Op2 where
instance Binary Expression where

getSize :: Expression -> Int
getSize (Const _) = 1
getSize (Var _) = 1
getSize (If0 e0 e1 e2) = 1 + getSize e0 + getSize e1 + getSize e2
getSize (Fold e1 e2 _ _ e3) = 2 + getSize e1 + getSize e2 + getSize e3
getSize (Op1 _ e) = 1 + getSize e
getSize (Op2 _ e1 e2) = 1 + getSize e1 + getSize e2

data AnyOp = A1 Op1 | A2 Op2 | AFold | ATFold | AIf0
  deriving (Eq, Show, Ord, Generic)

instance Binary AnyOp where

instance Enum AnyOp where
  fromEnum (A1 op) = fromEnum op
  fromEnum (A2 op) = fromEnum op + 5
  fromEnum AFold = 9
  fromEnum ATFold = 10
  fromEnum AIf0 = 11

  toEnum i
    | i < 5 = A1 (toEnum i)
    | i < 9 = A2 (toEnum (i-5))
    | i == 9  = AFold
    | i == 10 = ATFold
    | i == 11 = AIf0
    | otherwise = error $ "Unexpected AnyOp idx: " ++ show i

instance Show Expression where
  show (Const x) = show x
  show (Var i) = "x" ++ show i
  show (If0 e1 e2 e3) = printf "(if0 %s %s %s)" (show e1) (show e2) (show e3)
  show (Fold e1 e2 v1 v2 fn) = printf "(fold %s %s (lambda (x%s x%s) %s))" (show e1) (show e2) (show v1) (show v2) (show fn)
  show (Op1 op e) = printf "(%s %s)" (show op) (show e)
  show (Op2 op e1 e2) = printf "(%s %s %s)" (show op) (show e1) (show e2)

data Op1 =
    Not
  | Shl1
  | Shr1
  | Shr4
  | Shr16
  deriving (Eq,Enum,Bounded,Ord, Generic)

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
  deriving (Eq,Enum,Bounded,Ord, Generic)

instance Show Op2 where
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show Plus = "plus"

getOps :: Int -> Expression -> OpSet
getOps _ (Const _) = S.empty
getOps _ (Var _) = S.empty
getOps l (If0 e0 e1 e2) = S.singleton AIf0 `S.union` getOps (l+1) e0 `S.union` getOps (l+1) e1 `S.union` getOps (l+1) e2 
getOps l (Fold e0 e1 _ _ e2) = S.singleton op `S.union` getOps 2 e0 `S.union` getOps 2 e1 `S.union` getOps 2 e2
  where
    op | l == 1 && e1 == Const (Value 0) && e0 == Var 1 = ATFold
       | otherwise = AFold
getOps l (Op1 op1 e0) = S.singleton (A1 op1) `S.union` getOps (l+1) e0
getOps l (Op2 op2 e0 e1) = S.singleton (A2 op2) `S.union` getOps (l+1) e0 `S.union` getOps (l+1) e1

hasAll :: Int -> OpSet -> Expression -> Bool
hasAll l need e = 
  let ops = getOps l e
  in  {- trace ("hasAll: " ++ show ops) -} ops == need

data GState = GState {
    lastVariable :: Int
  , gMemo :: Memo
  , gFoldAllowedStack :: [Bool]
  } deriving (Eq, Show, Generic)

emptyGState :: GState
emptyGState = GState 1 M.empty [True]

type Generate a = StateT GState IO a

type Size = Int

type Level = Int

class Generated a where
  generate :: Level -> Size -> OpSet -> OpSet -> Generate [(OpSet, a)]

instance Generated Id where
  generate _ 1 _ _ = do
    n <- gets lastVariable
    return $ zip (repeat S.empty) [1..n]

getFoldAllowed :: Generate Bool
getFoldAllowed = gets (head . gFoldAllowedStack)

newFoldContext :: Generate ()
newFoldContext = do
    modify $ \st -> st {gFoldAllowedStack = allow (gFoldAllowedStack st)}
--     stack <- gets gFoldAllowedStack
--     lift $ putStrLn $ "> New fold context: " ++ show stack
  where
    allow (x:xs) = x: x: xs

forbidFold :: Generate ()
forbidFold = modify $ \st -> st {gFoldAllowedStack = forbid (gFoldAllowedStack st)}
  where
    forbid (_:xs) = False : xs

leaveFoldContext :: Generate ()
leaveFoldContext = do
    modify $ \st -> st {gFoldAllowedStack = tail (gFoldAllowedStack st)}
--     stack <- gets gFoldAllowedStack
--     lift $ putStrLn $ "> Leave fold context: " ++ show stack

newVariable :: Generate Id
newVariable = do
    n <- gets lastVariable
    modify $ \st -> st {lastVariable = n+1}
    return (n+1)

memoLookup :: Size -> (OpSet, OpSet) -> Int -> Memo -> Maybe [(OpSet, Expression)]
memoLookup size ops nvars memo = do
  opmap <- M.lookup size memo
  varmap <- M.lookup ops opmap
  M.lookup nvars varmap

memoInsert :: Size -> (OpSet,OpSet) -> Int -> [(OpSet, Expression)] -> Memo -> Memo
memoInsert size ops nvars exprs memo =
    M.insertWith insertOps size (M.singleton ops (M.singleton nvars exprs)) memo
  where 
      insertOps m1 m2 = M.unionWith M.union m1 m2

getMemo :: Size -> (OpSet,OpSet) -> Int -> Generate (Maybe [(OpSet, Expression)])
getMemo size ops nvars = do
  memo <- gets gMemo
  return $ memoLookup size ops nvars memo

putMemo :: Size -> (OpSet,OpSet) -> Int -> [(OpSet, Expression)] -> Generate ()
putMemo size ops nvars exprs = do
  modify $ \st -> st {gMemo = memoInsert size ops nvars exprs (gMemo st)}

writeMemo :: Generate ()
writeMemo = do
  memo <- gets gMemo
  lift $ encodeFile "memo.dat" $ memo

readMemo :: Generate ()
readMemo = do
  b <- lift $ doesFileExist "memo.dat"
  if not b
    then lift $ putStrLn "No memo.dat"
    else do
         lift $ putStrLn $ "Reading memo from memo.dat"
         r <- lift $ try $ decodeFile "memo.dat"
         case r of
           Left e -> lift $ print (e :: SomeException)
           Right memo -> do
                lift $ putStrLn $ "Memo read from memo.dat"
                modify $ \st -> st {gMemo = memo}

memoSize :: Memo -> Int
memoSize memo = sum $ map M.size $ concatMap M.elems (M.elems memo)

instance Generated Op1 where
  generate _ 1 _ list = return [(S.singleton (A1 op), op) | A1 op <- S.toList list]

instance Generated Op2 where
  generate _ 1 _ list = return [(S.singleton (A2 op), op) | A2 op <- S.toList list]

filterFolds _ x = x
-- filterFolds :: OpSet -> OpSet -> OpSet
-- filterFolds was ops = if (AFold `S.member` was) || (ATFold `S.member` was)
--                         then S.filter (`notElem` [AFold, ATFold]) ops
--                         else ops

instance Generated Expression where
  generate lvl 1 mops ops = do
    if S.null mops
      then do
--           lift $ putStrLn $ printf "[%d] Generating expression of size 1. mops: %s" lvl (show $ S.toList mops)
          x <- generate (lvl+1) 1 S.empty ops
          let var = map snd x
          return $ map (\v -> (S.empty, Var v)) var ++ [(S.empty, Const (Value 0)), (S.empty, Const (Value 1))]
      else return []

  generate level size mops ops = do
--     lift $ putStrLn $ printf "[%d] Generating expression of size %d. mops: %s" level size (show $ S.toList mops)
    nvars <- gets lastVariable
    mbRes <- getMemo size (mops, ops) nvars
    case mbRes of
      Just res -> return res
      Nothing -> do
          ifs <- if AIf0 `S.member` ops && (size >= 4)
                   then do
                        let sizes = split 3 (size-1)
      --                   lift $ putStrLn $ printf "[%d] If0 size: %d, children sizes: %s" level size (show sizes)
                        concatFor sizes $ \([sizeCond, sizeE1, sizeE2]) -> do
                          newFoldContext
                          let mopsCond = S.delete AIf0 mops
                          conds <- generate (level+1) sizeCond mopsCond ops
                          r <- concatFor conds $ \(condOps, cond) -> do
                                let mops1 = mopsCond `S.difference` condOps
                                e1s   <- generate (level+1) sizeE1 mops1 ops
                                concatFor e1s $ \(e1ops, e1) -> do
                                  let mops2 = mopsCond `S.difference` condOps `S.difference` e1ops
                                  e2s   <- generate (level+1) sizeE2 mops2 ops
                                  return [(S.insert AIf0 condOps `S.union` e1ops `S.union` e2ops, If0 cond e1 e2) | (!e2ops, e2) <- e2s, {-trace ("If0 e2ops: " ++ show e2ops)-} mops2 `S.isSubsetOf` e2ops ]
                          leaveFoldContext
                          return r
                   else return []
          let ops_wo_fold = S.delete AFold ops
          foldAllowed <- getFoldAllowed
          folds <- if foldAllowed && (AFold `S.member` ops) && (size >= 5)
                     then do
                          let sizes = split 3 (size-2)
      --                     lift $ putStrLn $ printf "[%d] Fold size: %d, children sizes: %s" level size (show sizes)
                          concatFor sizes $ \([sizeE0, sizeE1, sizeE2]) -> do
                            newFoldContext
                            let mops0 = S.delete AFold mops
                            e0s <- generate (level+1) sizeE0 mops0 ops_wo_fold
                            r <- concatFor e0s $ \(e0ops, e0) -> do
                                  let mops1 = mops0 `S.difference` e0ops
                                  e1s <- generate (level+1) sizeE1 mops1 ops_wo_fold
      --                             lift $ putStrLn $ printf "[%d] Fold sizeE1: %d, mops1: %s; result: %s" level sizeE1 (show $ S.toList mops1) (show e1s)
                                  x <- newVariable
                                  y <- newVariable
                                  r <- concatFor e1s $ \(e1ops, e1) -> do
                                          let mops2 = mops1 `S.difference` e1ops
                                          e2s <- generate (level+1) sizeE2 mops2 ops_wo_fold
      --                                     lift $ putStrLn $ printf "[%d] Fold sizeE2: %d, mops2: %s; result: %s" level sizeE2 (show $ S.toList mops2) (show e2s)
                                          return [(S.insert AFold e0ops `S.union` e1ops `S.union` e2ops, Fold e0 e1 x y e2) | (!e2ops, e2) <- e2s, mops2 `S.isSubsetOf` e2ops ]
                                  modify $ \st -> st {lastVariable = lastVariable st - 2}
                                  return r
                            leaveFoldContext
      --                       lift $ putStrLn $ printf "[%d] Fold (%d, %d, %d). results: %s" level sizeE0 sizeE1 sizeE2 (show r)
                            return r
                     else return []
          foldAllowed <- getFoldAllowed
          tfolds <- if foldAllowed && (ATFold `S.member` ops) && (level == 1) && (size >= 5)
                     then do
                          newFoldContext
                          let sizeE2 = size-4
      --                     lift $ putStrLn $ printf "[%d] TFold size: %d, children size: %s" level size (show sizeE2)
                          let e0 = Var 1
                          x <- newVariable
                          y <- newVariable
                          let e1 = Const (Value 0)
                          let mops2 = S.delete ATFold mops
                          e2s <- generate (level+1) sizeE2 mops2 ops_wo_fold
                          modify $ \st -> st {lastVariable = lastVariable st - 2}
                          leaveFoldContext
                          return [(S.insert ATFold e2ops, Fold e0 e1 x y e2) | (!e2ops, e2) <- e2s, mops2 `S.isSubsetOf` e2ops ]
                     else return []
          op1s <- case [op | A1 op <- S.toList ops] of
                    [] -> return []
                    o1s -> if (size < 2) || (size == 2 && S.size mops > 1)
                             then return []
                             else do
      --                            lift $ putStrLn $ printf "[%d] Op1 size: %d, children size: %d" level size (size-1)
                                 newFoldContext
                                 r <- concatFor o1s $ \op -> do
                                         let mops1 = S.delete (A1 op) mops
                                         es <- generate (level+1) (size-1) mops1 ops
                                         return [(S.insert (A1 op) e1ops, Op1 op e) | (!e1ops, e) <- es,  mops1 `S.isSubsetOf` e1ops ]
                                 leaveFoldContext
                                 return r
          op2s <- case [op | A2 op <- S.toList ops] of
                    [] -> return []
                    o2s -> if (size < 3) || (size == 3 && S.size mops > 2)
                             then return []
                             else do
                                   let sizes = split 2 (size-1)
      --                              lift $ putStrLn $ printf "[%d] Op2 size: %d, children sizes: %s" level size (show sizes)
                                   concatFor sizes $ \([sizeE1, sizeE2]) -> do
                                     newFoldContext
                                     r <- concatFor o2s $ \op -> do
                                           let mops1 = S.delete (A2 op) mops
                                           e1s <- generate (level+1) sizeE1 mops1 ops
                                           concatFor e1s $ \(e1ops, e1) -> do
                                             let mops2 = mops1 `S.difference` e1ops
                                             e2s <- generate (level+1) sizeE2  mops2 ops
                                             return [(S.insert (A2 op) e1ops `S.union` e2ops, Op2 op e1 e2) | (!e2ops, e2) <- e2s,mops2 `S.isSubsetOf` e2ops]
                                     leaveFoldContext
                                     return r

      --           lift $ putStrLn $ printf "[%d] For size %d. O1: %d; O2: %d; Fold: %d; TFold: %d; If0: %d" level size
      --                                    (length op1s) (length op2s) (length folds) (length tfolds) (length ifs)
          let allTrees = op1s ++ op2s ++ folds ++ tfolds ++ ifs
          when (level == 1) $ lift $ do
            putStr "1"
            hFlush stdout
          when (level == 2) $ lift $ do
            putStr "2"
            hFlush stdout
          when (level == 3) $ lift $ do
            putStr "3"
            hFlush stdout
--           putMemo size (mops, ops) nvars allTrees
      --           forM_ allTrees $ \e -> do
      --             let treeSize = getSize e
      --             when (treeSize /= size) $
      --               fail $ printf "Invalid generated tree size: %d instead of %d. Tree: %s" treeSize size (show e)
          return $ allTrees
                  
getTrees :: Size -> [AnyOp] -> IO [Expression]
getTrees size oplist = do
  let ops = S.fromList oplist
  es <- evalStateT (generate 1 size ops ops) emptyGState
  putStrLn "Trees generated."
  return [e | (eOps, e) <- es, {- trace ("eOps: " ++ show eOps) -} eOps == ops]

printTrees :: Size -> IO ()
printTrees size = do
  let ops = S.fromList [AFold, A1 Not, A1 Shl1, A1 Shr4, A2 Xor]
  es <- evalStateT (generate 1 size ops ops) emptyGState
  forM_ es $ \(eOps, e) -> do
    if eOps == ops
      then putStrLn $ show (e :: Expression)
      else return ()
    

