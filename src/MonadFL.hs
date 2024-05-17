{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadFL
  ( FL,
    addExp,
    lookUpExp,
    addFunc,
    lookUpFunc,
    lookUpType,
    printFL,
    failPosFL,
    failFL,
    setLastFile,
    catchErrors,
    runFL,
    MonadFL,
    module Control.Monad.Except,
    module Control.Monad.State,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Errors
import Global
import Lang
import Lib
import System.IO

-- | Define MonadLF
class (MonadIO m, MonadState Env m, MonadError Errors.Error m) => MonadFL m

addExp :: MonadFL m => String -> Exp Funcs -> m ()
addExp name e = maybe (modify (\env -> env {envExp = (name, e) : envExp env})) (const (failFL $ "Variable " ++ name ++ " already defined")) =<< lookUpExp name

lookUpExp :: MonadFL m => String -> m (Maybe (Exp Funcs))
lookUpExp name = gets (lookup name . envExp)

addFunc :: MonadFL m => String -> Seq Funcs -> m ()
addFunc name funcs = maybe (modify (\env -> env {envFuncs = (name, funcs) : envFuncs env})) (const (failFL $ "Function " ++ name ++ " already defined")) =<< lookUpFunc name

lookUpFunc :: MonadFL m => String -> m (Maybe (Seq Funcs))
lookUpFunc name = gets (lookup name . envFuncs)

lookUpType :: MonadFL m => Type -> m (Maybe Int)
lookUpType name = gets (lookup name . envTypes)

printFL :: MonadFL m => String -> m ()
printFL = liftIO . putStrLn

failPosFL :: MonadFL m => Pos -> String -> m a
failPosFL pos s = throwError (ErrorPos pos s)

failFL :: MonadFL m => String -> m a
failFL = failPosFL NoPos

setLastFile :: MonadFL m => String -> m ()
setLastFile file = modify (\env -> env {lfile = file})

catchErrors :: MonadFL m => m a -> m (Maybe a)
catchErrors c =
  catchError
    (Just <$> c)
    ( \e ->
        liftIO $
          hPrint stderr e
            >> return Nothing
    )

type FL = StateT Env (ExceptT Errors.Error IO)

instance MonadFL FL

-- | Run a MonadFL computation with the initial enviroment
runFL' :: FL a -> IO (Either Error (a, Env))
runFL' m = runExceptT $ runStateT m initialEnviroment

runFL :: FL a -> IO (Either Error a)
runFL m = fmap fst <$> runFL' m
