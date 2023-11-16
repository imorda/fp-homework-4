{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad (ap)
import HW4.Types

newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES runA) = ES $ \s -> mapExcept (mapAnnotated f) $ runA s

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success $ a :# s

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES runOuter) = ES $ \s ->
  case runOuter s of
    Error e                         -> Error e
    Success (ES runInner :# outerS) -> runInner outerS

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  (>>=) stateA f = joinExceptState $ mapExceptState f stateA

data EvaluationError = DivideByZero
  deriving Show

evalBinaryExcept :: (Double -> Double -> Double) ->
              (Double -> Double -> Prim Double) ->
              Expr -> Expr ->
              (Double -> Double -> ExceptState EvaluationError [Prim Double] ()) ->
              ExceptState EvaluationError [Prim Double] Double
evalBinaryExcept evalF logF lhsExpr rhsExpr checkExcept = do
  lhsVal <- eval lhsExpr
  rhsVal <- eval rhsExpr
  checkExcept lhsVal rhsVal
  modifyExceptState (logF lhsVal rhsVal :)
  return $ evalF lhsVal rhsVal

evalUnary :: (Double -> Double) ->
              (Double -> Prim Double) ->
              Expr -> ExceptState EvaluationError [Prim Double] Double
evalUnary evalF logF expr = do
  val <- eval expr
  modifyExceptState (logF val :)
  return $ evalF val

dummyState :: ExceptState EvaluationError [Prim Double] ()
dummyState = modifyExceptState id

noCheck :: Double -> Double -> ExceptState EvaluationError [Prim Double] ()
noCheck _ _ = dummyState

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x)                    = return x
eval (Op (Add lhsExpr rhsExpr)) = evalBinaryExcept (+) Add lhsExpr rhsExpr noCheck
eval (Op (Sub lhsExpr rhsExpr)) = evalBinaryExcept (-) Sub lhsExpr rhsExpr noCheck
eval (Op (Mul lhsExpr rhsExpr)) = evalBinaryExcept (*) Mul lhsExpr rhsExpr noCheck
eval (Op (Abs expr))            = evalUnary abs Abs expr
eval (Op (Sgn expr))            = evalUnary signum Sgn expr
eval (Op (Div lhsExpr rhsExpr)) = evalBinaryExcept (/) Div lhsExpr rhsExpr \_ -> \case
  0 -> throwExceptState DivideByZero
  _ -> dummyState
