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
aplicar' (Succ or) xs = return $ succesor or (List.FList.fromList xs)
aplicar' (Delete or) xs = return $ delete or (List.FList.fromList xs)
aplicar' (Rep fs) xs = return $ rep fs (List.FList.fromList xs)
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
aplicar f xs T1 = do l <- aplicar' @TList f xs
                     return (quote l, T1)
aplicar f xs T2 = do l <- aplicar' @[] f xs
                     return (quote l, T2)

changeType :: Exp -> Type -> Exp
changeType (List (xs, i, _)) t = List (xs, i, t)
changeType (Var (var, i, _)) t = Var (var, i, t)
changeType (Term fns exp) t = Term fns (changeType exp t)

evalExp :: (MonadState m, MonadError m) => Exp -> m TypedList
evalExp (List (xs, i, t)) = case t of
                              INVALID -> throw InvalidType
                              _ -> return $ Just (xs, t)
evalExp (Var (var, i, t)) = case t of
                              INVALID -> throw InvalidType
                              _ -> do exp <- look4var var
                                      evalExp (changeType exp t)
evalExp (Term [f] exp) = do l <- evalExp exp
                            l' <- uncurry (aplicar f) (fromJust l)
                            return $ Just l'
evalExp (Term (f:fs) exp) = do l <- evalExp exp
                               (l', t) <- uncurry (aplicar f) (fromJust l)
                               evalExp (Term fs (List (l', length l' , t)))
-- evalExp (List (Just xs, _)) = return (Just xs)
-- evalExp (Var (ss, _)) = do exp <- look4var ss
--                            evalExp exp
-- evalExp (Term [f] exp) = do l <- evalExp exp
--                             aplicar f l
-- evalExp (Term (f:fs) exp) = do l <- evalExp exp
--                                l' <- aplicar f l
--                                evalExp (Term fs (List (l', length l)))
-- WTF ES ESTO 
-- evalExp (ListT (xs, ss)) = case M.lookup ss mapType of
--                                   Just t -> return (Just (xs, t))
--                                   Nothing -> throw InvalidType 
-- evalExp (Var (ss, t)) = do exp <- look4var ss
--                            evalExp exp
-- evalExp (Term [f] exp) = do (Just (l, t)) <- evalExp exp
--                             exp' <- aplicar f l t
--                             return (Just exp')

-- evalExp (Term (f:fs) exp) = do (Just (l, t)) <- evalExp exp
--                                (l',t') <- aplicar f l t
--                                evalExp (Term fs (List (l', ""))

                            
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
                              updateVar ss (List (fst (fromJust l), length $ fst (fromJust l), snd (fromJust l)))
                              return Nothing


eval' :: Comms -> EnvFuncs -> EnvVars -> Either Error (TypedList, EnvFuncs, EnvVars)
eval' comm = runStateError (evalComms comm)

eval :: [Comms] -> EnvFuncs -> EnvVars -> (Either Error TypedList, EnvFuncs, EnvVars)
eval [x] f v = case eval' x f v of
                  Left err -> (Left err, f, v)
                  Right (res, f', v') -> (Right res, f', v')
eval (x:xs) f v = case eval' x f v of
                      Right (_, f', v') -> eval xs f' v'
                      Left err -> (Left err, f, v)
  -- let (Right (_, f', v')) = eval' x f v
  --                 in eval xs f' v'

-- infer :: (MonadState m, MonadError m) => Exp -> m Int
-- infer (List (l, n, _)) = if length l == n then return n else throw InvalidInfer
-- infer (Var (ss, n, _)) = do exp <- look4var ss
--                             i <- infer exp
--                             if i == n then return n
--                             else throw InvalidInfer
-- infer (Term fns exp) = do n <- infer exp


-- infer :: (MonadState m, MonadError m) => (Exp, Int) -> m Int
-- infer (List l, n) = return n
-- infer (Var ss, n) = do (l, i) <- look4var ss
--                        if n == i then return n
--                        else throw InvalidInfer
-- infer (Term fns exp, n) = undefined
