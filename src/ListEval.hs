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
changeType (List (xs, _)) t = List (xs, t)
changeType (Var (var, _)) t = Var (var, t)
changeType (Term fns exp i) t = Term fns (changeType exp t) i

evalExp :: (MonadState m, MonadError m) => Exp -> m TypedList
evalExp (List (xs, t)) = case t of
                              INVALID -> throw InvalidType
                              _ -> return $ Just (xs, t)
evalExp (Var (var, t)) = case t of
                              INVALID -> throw InvalidType
                              _ -> do exp <- look4var var
                                      evalExp (changeType exp t)
evalExp (Term [f] exp i) = do l <- evalExp exp
                              l' <- uncurry (aplicar f) (fromJust l)
                              return $ Just l'
evalExp (Term (f:fs) exp i) = do l <- evalExp exp
                                 (l', t) <- uncurry (aplicar f) (fromJust l)
                                 evalExp (Term fs (List (l', t)) i)
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
                              updateVar ss (List (fromJust l))
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

infer :: [Comms] -> EnvFuncs -> EnvVars -> (Either Error Int, EnvFuncs, EnvVars)
infer [x] f v = case infer' x f v of
                  Left err -> (Left err, f, v)
                  Right (res, f', v') -> (Right res, f', v')
infer (x:xs) f v = case infer' x f v of
                      Right (_, f', v') -> infer xs f' v'
                      Left err -> (Left err, f, v)

infer' :: Comms -> EnvFuncs -> EnvVars -> Either Error (Int, EnvFuncs, EnvVars)
infer' (Eval exp) f v = runStateError (inferExp exp) f v
infer' (Def _ _) f v = Right (-1, f, v)
infer' (Const _ _) f v = Right (-1, f, v)

inferExp :: (MonadState m, MonadError m) => Exp -> m Int
inferExp (List (l,_)) = return $ length l
inferExp (Var (ss, _)) = do exp <- look4var ss
                            inferExp exp
inferExp (Term [] exp n) = do i <- inferExp exp
                              if i == n then return n
                              else throw InvalidInfer
inferExp (Term ((Zero _):fs) exp n) = inferExp (Term fs exp (n-1)) >> return n
inferExp (Term ((Succ _):fs) exp n) = inferExp (Term fs exp n) >> return n
inferExp (Term ((Delete _):fs) exp n) = inferExp (Term fs exp (n+1)) >> return n
inferExp (Term ((Rep fns):fs) exp n) = return n 
inferExp (Term ((Defined ss):fs) exp n) = do f <- look4func ss
                                             inferExp (Term (f++fs) exp n)

-- infer (Term ((Zero _):fs) exp n) = do i <- infer exp
--                                       i' <- infer (Term fs Exp Int)
--                                       if i + 1 == n then return n
--                                       else throw InvalidInfer
-- infer (Term ((Delete _):fs) exp n) = do i <- infer exp
--                                         if i - 1 == n then return n
--                                         else throw InvalidInfer
