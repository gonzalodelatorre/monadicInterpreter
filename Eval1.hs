{-# LANGUAGE GADTs #-}

module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- State


type Env = M.Map Variable Int

-- Empty State
initEnv :: Env
initEnv = M.empty

-- State Monad
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')


instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert


-- Empty state evaluation
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Multiple steps comand evaluation
stepCommStar :: Comm -> State ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Single step command evaluation
stepComm ::  Comm -> State Comm
stepComm (Let x e) = do
  v <- evalExp e
  update x v
  return Skip
stepComm (Seq Skip c1) = return c1
stepComm (Seq c0   c1) = do
  c0' <- stepComm c0
  return (Seq c0' c1)
stepComm (IfThenElse b c0 c1) = do
  vb <- evalExp b
  if vb then return c0 else return c1
stepComm w@(While b c) = do
  vb <- evalExp b
  if vb then return (Seq c w) else return Skip

-- Evaluates an expression
evalExp ::  Exp a -> State a
evalExp (Const c)  = return c
evalExp (Var x)    = lookfor x
evalExp (UMinus e) = (0 -) <$> evalExp e
evalExp (Plus  e1 e2) = evalBOp e1 e2 (+)
evalExp (Minus e1 e2) = evalBOp e1 e2 (-)
evalExp (Times e1 e2) = evalBOp e1 e2 (*)
evalExp (Div e1 e2)   = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case v2 of
    _ -> return (div v1 v2)
evalExp BTrue  = return True
evalExp BFalse = return False
evalExp (Not b)      = not <$> evalExp b
evalExp (Lt e1 e2)   = evalBOp e1 e2 (<)
evalExp (Gt e1 e2)   = evalBOp e1 e2 (>)
evalExp (Eq e1 e2)   = evalBOp e1 e2 (==)
evalExp (NEq e1 e2)  = evalBOp e1 e2 (/=)
evalExp (Or e1 e2)   = evalBOp e1 e2 (||)
evalExp (And e1 e2)  = evalBOp e1 e2 (&&)
evalExp (EAssgn x e) = do
  v <- evalExp e
  update x v
  return v
evalExp (ESeq e1 e2) = evalExp e1 >> evalExp e2


evalBOp ::  Exp a -> Exp a -> (a -> a -> b) -> State b
evalBOp e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (f v1 v2)
  
  

  
  
  
  
  
  
  
