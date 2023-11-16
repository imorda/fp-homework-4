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

import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState = undefined

wrapExceptState :: a -> ExceptState e s a
wrapExceptState = undefined

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState = undefined

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState = undefined

throwExceptState :: e -> ExceptState e s a
throwExceptState = undefined

instance Functor (ExceptState e s) where
  fmap = undefined

instance Applicative (ExceptState e s) where
  pure = undefined
  (<*>) = undefined

instance Monad (ExceptState e s) where
  (>>=) = undefined

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval = undefined
