
module Evaluator where

import Control.Monad
import Control.Monad.State
import Data.List (transpose)
import Data.Word
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf
import Debug.Trace

import Trees

data EState = EState {
    currentVariables :: M.Map Id Value
  } deriving (Eq, Show)

emptyEState :: EState
emptyEState = EState M.empty

type Eval a = StateT EState IO a

getVar :: Id -> Eval Value
getVar i = do
  m <- gets currentVariables
  case M.lookup i m of
    Nothing -> fail $ "No such variable: x" ++ show i
    Just y -> return y

setVar :: Id -> Value -> Eval ()
setVar i x = do
  modify $ \st -> st {currentVariables = M.insert i x (currentVariables st)}

eval :: Expression -> Eval Value
eval (Const x) = return x
eval (Var i) = getVar i
eval (If e0 e1 e2) = do
  Value e0val <- eval e0
  if e0val /= 0
    then eval e1
    else eval e2
eval (Fold e0 e1 x y e2) = do
  e0val <- eval e0
  let e0list = unfoldWord e0val
  e1val <- eval e1
  setVar y e1val
  forM_ e0list $ \e0byte -> do
    setVar x e0byte
    res <- eval e2
    setVar y res
  getVar y
eval (Op1 Not x) = do
  Value xval <- eval x
  return $ Value $ complement xval
eval (Op1 Shl1 x) = do
  Value xval <- eval x
  return $ Value $ xval `shiftL` 1
eval (Op1 Shr1 x) = do
  Value xval <- eval x
  return $ Value $ xval `shiftR` 1
eval (Op1 Shr4 x) = do
  Value xval <- eval x
  return $ Value $ xval `shiftR` 4
eval (Op1 Shr16 x) = do
  Value xval <- eval x
  return $ Value $ xval `shiftR` 16
eval (Op2 And x y) = do
  Value xval <- eval x
  Value yval <- eval y
  return $ Value $ xval .&. yval
eval (Op2 Or x y) = do
  Value xval <- eval x
  Value yval <- eval y
  return $ Value $ xval .|. yval
eval (Op2 Xor x y) = do
  Value xval <- eval x
  Value yval <- eval y
  return $ Value $ xval `xor` yval
eval (Op2 Plus x y) = do
  Value xval <- eval x
  Value yval <- eval y
  return $ Value $ xval + yval

testExpr :: Expression
testExpr = Fold (Var 1) (Const (Value 0)) 2 3 (Op2 Or (Var 2) (Var 3))

testEval :: Expression -> Value -> IO ()
testEval expr x = do
  let run = do
            setVar 1 x
            eval expr
  result <- evalStateT run emptyEState
  print result

