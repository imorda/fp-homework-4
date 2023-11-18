{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module HW4.T1
  (
  -- * Type
  -- | This module exposes custom types encapsulating the parser information.
    EvaluationError (..),
    ExceptState (..),

  -- * ExceptState monad functionality
  -- | Functions transforming or working with ExceptState
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,

  -- *  Evaluator
  -- | Evaluates an expression represented as 'Expr' type
    eval
  ) where

import Control.Monad (ap)
import HW4.Types

-- | The 'ExceptState' data type encapsulates stateful computations which can fail with an 'e' error,
-- altering an 's' state, and producing a value of type 'a' when successful
newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

-- | 'mapAnnotated' transforms the value within 'Annotated' keeping error as it is.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

-- | 'mapExcept' transforms the successful value within 'Except', keeping error as it is.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

-- | 'wrapExceptState' lifts a value into an 'ExceptState' computation, discarding the old state.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES runA) = ES $ \s -> mapExcept (mapAnnotated f) $ runA s

-- | 'wrapExceptState' lifts a value into an 'ExceptState' computation,
--   turning it into a computation that always successfully returns that value
--   without changing the state.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success $ a :# s

-- | 'joinExceptState' transforms a nested ExceptState computation into a single-layered one.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES runOuter) = ES $ \s ->
  case runOuter s of
    Error e                         -> Error e
    Success (ES runInner :# outerS) -> runInner outerS

-- | 'modifyExceptState' applies a function to the state within an 'ExceptState' computation.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

-- | Throws user-defined 'EvaluationError' error for handling expressions like division by zero.
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  (>>=) stateA f = joinExceptState $ mapExceptState f stateA

-- | Represents possible errors during evaluation
data EvaluationError = DivideByZero
  deriving Show

-- | 'evalBinaryExcept' represent evaluator for binary operations.
-- It further checks for exceptions like division by zero.
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

-- | evalUnary represent evaluator for unary operations.
evalUnary :: (Double -> Double) ->
              (Double -> Prim Double) ->
              Expr -> ExceptState EvaluationError [Prim Double] Double
evalUnary evalF logF expr = do
  val <- eval expr
  modifyExceptState (logF val :)
  return $ evalF val

-- | 'dummyState' does nothing, preserving the state
dummyState :: ExceptState EvaluationError [Prim Double] ()
dummyState = modifyExceptState id

-- | 'noCheck' is a simple operation validator that checks nothing
noCheck :: Double -> Double -> ExceptState EvaluationError [Prim Double] ()
noCheck _ _ = dummyState

-- | 'eval' is the main evaluator which recursively evaluates each Expr to Double.
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
