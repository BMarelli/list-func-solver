-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Monads where
import AST
import List.FList
import Data.Map.Strict as M hiding (splitAt, delete)
import Control.Monad

class Monad m => MonadState m where
  look4func :: String -> m [Funcs]
  updateFunc :: String -> [Funcs] -> m ()

  look4var :: String  -> m Exp
  updateVar :: String -> Exp -> m ()

class Monad m => MonadError m where
  throw :: Error -> m a

emptyEnvFuncs :: EnvFuncs
emptyEnvFuncs = M.empty

emptyEnvVars :: EnvVars
emptyEnvVars = M.empty

newtype StateError x = StateError {runStateError :: EnvFuncs -> EnvVars -> Either Error (x, EnvFuncs, EnvVars)}

instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError (\f v -> Right (x, f, v))
  m >>= g = StateError (\f v -> runStateError m f v >>= (\(x', f', v') -> runStateError (g x') f' v'))

instance MonadState StateError where
  look4func func = StateError (\f v -> case M.lookup func f of
                                          Just func' -> Right (func', f, v)
                                          Nothing -> Left (UndefinedFunc func))
  updateFunc ss fs = StateError (\f v -> let f' = M.insert ss fs f
                                         in Right ((), f', v))
  look4var var = StateError (\f v -> case M.lookup var v of
                                          Just var' -> Right (var', f, v)
                                          Nothing -> Left (UndefinedVar var))
  updateVar ss var = StateError (\f v -> let v' = M.insert ss var v
                                         in Right ((), f, v'))

instance MonadError StateError where
  throw err = StateError (\_ _ -> Left err)
