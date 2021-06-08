{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
-- TODO:
module ListEval where

import AST
import Data.Map.Strict as M hiding (splitAt, delete, map)
import Data.Maybe
import Control.Monad
import List.FList
import List.TupleFList
import List.ListFList
import Monads

aplicar' :: forall l m . (FList l, MonadState m, MonadError m) => Funcs -> ListElements -> m (l Elements)
aplicar' (Zero or) xs = return $ zero or (List.FList.fromList xs)
aplicar' (Succ or) xs = case succesor or (List.FList.fromList xs) of
                              Left err -> throw err
                              Right l -> return l
aplicar' (Delete or) xs = case delete or (List.FList.fromList xs) of
                              Left err -> throw err
                              Right l -> return l
aplicar' (Rep fs) xs = case rep fs (List.FList.fromList xs) of
                              Left err -> throw err
                              Right l -> return l
aplicar' (Defined ss) xs = do fns <- look4func ss
                              aplicarDefined fns (List.FList.fromList xs)
    where
      aplicarDefined :: forall l m . (FList l, MonadState m, MonadError m) => [Funcs] -> l Elements -> m (l Elements)
      aplicarDefined [] l = return l
      aplicarDefined (f:fs) l = do l' <- aplicar' f (quote l)
                                   aplicarDefined fs l'

aplicar :: (MonadState m, MonadError m) => Funcs -> ListElements -> Type -> m (ListElements, Type)
aplicar f xs DEFAULT = do l <- aplicar' @TList f xs
                          return (quote l, DEFAULT)
aplicar f xs T2 = do l <- aplicar' @[] f xs
                     return (quote l, T2)

changeType :: Exp -> Type -> Exp
changeType (List (xs, _)) t = List (xs, t)
changeType (Var (var, _)) t = Var (var, t)
changeType (Term fns exp) t = Term fns (changeType exp t)

evalExp :: (MonadState m, MonadError m) => Exp -> m TypedList
evalExp (List (xs, t)) = case t of
                              INVALID ss -> throw (InvalidType ss)
                              _ -> return $ Just (xs, t)
evalExp (Var (var, t)) = case t of
                              INVALID ss -> throw (InvalidType ss)
                              _ -> do exp <- look4var var
                                      evalExp (changeType exp t)
evalExp (Term [f] exp) = do l <- evalExp exp
                            l' <- uncurry (aplicar f) (fromJust l)
                            return $ Just l'
evalExp (Term (f:fs) exp) = do l <- evalExp exp
                               (l', t) <- uncurry (aplicar f) (fromJust l)
                               evalExp (Term fs (List (l', t)))
                            
evalFunc :: (MonadState m, MonadError m) => [Funcs] -> m [Funcs]
evalFunc [] = return []
evalFunc ((Defined ss):fns) = do fs <- look4func ss
                                 evalFunc (fs ++ fns)
evalFunc ((Rep fs):fns) = do fns' <- evalFunc fns
                             fs' <- evalFunc fs
                             return $ (Rep fs'):fns'
evalFunc (f:fns) = do fns' <- evalFunc fns
                      return $ f:fns'


evalComms :: (MonadState m, MonadError m) => Comms -> m TypedList
evalComms (Eval exp) = do evalExp exp
evalComms (Def ss fs) = do fns <- evalFunc fs
                           updateFunc ss fns
                           return Nothing
evalComms (Const ss exp) = do l <- evalExp exp
                              updateVar ss (List (fromJust l))
                              return Nothing
evalComms (Infer exp n) = inferExp exp n 0 >> return Nothing


eval' :: Comms -> EnvFuncs -> EnvVars -> Either Error (TypedList, EnvFuncs, EnvVars)
eval' comm = runStateError (evalComms comm)

eval :: [Comms] -> EnvFuncs -> EnvVars -> (Either Error TypedList, EnvFuncs, EnvVars)
eval [x] f v = case eval' x f v of
                  Left err -> (Left err, f, v)
                  Right (res, f', v') -> (Right res, f', v')
eval (x:xs) f v = case eval' x f v of
                      Right (_, f', v') -> eval xs f' v'
                      Left err -> (Left err, f, v)

inferExp :: (MonadState m, MonadError m) => Exp -> Int -> Int -> m Int
inferExp (List (l,_)) n i = let len = length l
                            in if n == i + len then return n
                               else throw (InvalidInfer (i + len) n)
inferExp (Var (ss, _)) n i = do exp <- look4var ss
                                inferExp exp n i
inferExp (Term [] exp) n i = do inferExp exp n i
inferExp (Term ((Zero _):fs) exp) n i = inferExp (Term fs exp) n (i+1)
inferExp (Term ((Succ _):fs) exp) n i = inferExp (Term fs exp) n i
inferExp (Term ((Delete _):fs) exp) n i = inferExp (Term fs exp) n (i-1)
inferExp (Term ((Rep fns):fs) exp) n _ = throw InferRep
inferExp (Term ((Defined ss):fs) exp) n i = do f <- look4func ss
                                               inferExp (Term (f++fs) exp) n i
