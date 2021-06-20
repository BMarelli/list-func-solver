{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
-- TODO:
module ListEval where
import Debug.Trace
import AST
import Data.Map.Strict as M hiding (splitAt, delete, map)
import Data.Maybe
import qualified Data.Sequence as S
import Control.Monad
import List.FList
import List.TupleFList
import List.ListFList
import List.CListFList
import List.TreeFList
import List.SeqFList
import Control.Monad.IO.Class
import Monads

changeType :: Exp -> Type -> Exp
changeType (List (xs, _)) t = List (xs, t)
changeType (Var (var, _)) t = Var (var, t)
changeType (Term fns exp) t = Term fns (changeType exp t)

-- ========================================= Peor Rendimiento ==========================================================
-- aplicar' :: forall l m . (FList l, MonadState m, MonadError m) => Funcs -> ListElements -> m (l Elements)
-- aplicar' (Zero or) xs = return $ zero or (List.FList.fromList xs)
-- aplicar' (Succ or) xs = case succesor or (List.FList.fromList xs) of
--                               Left err -> throw err
--                               Right l -> return l
-- aplicar' (Delete or) xs = case delete or (List.FList.fromList xs) of
--                               Left err -> throw err
--                               Right l -> return l
-- aplicar' (Rep fs) xs = case rep fs (List.FList.fromList xs) of
--                               Left err -> throw err
--                               Right l -> return l
-- aplicar' (Defined ss) xs = do fns <- look4func ss
--                               aplicarDefined fns (List.FList.fromList xs)
--     where
--       aplicarDefined :: forall l m . (FList l, MonadState m, MonadError m) => [Funcs] -> l Elements -> m (l Elements)
--       aplicarDefined [] l = return l
--       aplicarDefined (f:fs) l = do l' <- aplicar' f (quote l)
--                                    aplicarDefined fs l'

-- aplicar :: (MonadState m, MonadError m) => Funcs -> ListElements -> Type -> m (ListElements, Type)
-- aplicar f xs DEFAULT = do l <- aplicar' @TList f xs
--                           return (quote l, DEFAULT)
-- aplicar f xs T2 = do l <- aplicar' @[] f xs
--                      return (quote l, T2)
-- =====================================================================================================================

-- ======================================== Mejora Rendimiento =========================================================
aplicar' :: forall l m . (FList l, MonadState m, MonadError m) => [Funcs] -> ListElements -> Bool -> m (l Elements)
aplicar' fs l bool | bool      = do l' <- aplicar_ fs (List.FList.fromList l)
                                    traceM (printFL l')
                                    return l'
                   | otherwise = aplicar_ fs (List.FList.fromList l)
    where
      aplicar_ :: forall l m . (FList l, MonadState m, MonadError m) => [Funcs] -> l Elements -> m (l Elements)
      aplicar_ [] l = return l
      aplicar_ ((Zero or):fs) l = do let l' = zero or l
                                    --  traceM ("zero " ++ show or ++ "-" ++ printFL l')
                                     aplicar_ fs l'
      aplicar_ ((Succ or):fs) l = case succesor or l of
                                  Left err -> throw err
                                  Right l' -> aplicar_ fs l'
                                  -- Right l' -> traceM ("succ " ++ show or ++ "-" ++ printFL l') >> aplicar_ fs l'
      aplicar_ ((Delete or):fs) l = case delete or l of
                                  Left err -> throw err
                                  Right l' -> aplicar_ fs l'
                                  -- Right l' -> traceM ("delete " ++ show or ++ "-" ++ printFL l') >> aplicar_ fs l'
      aplicar_ ((Rep f):fs) l = case rep f l of
                                  Left err -> throw err
                                  Right l' -> aplicar_ fs l'
                                  -- Right l' -> traceM ("rep " ++ "-" ++ printFL l') >> aplicar_ fs l'
      aplicar_ ((Defined ss):fs) l = do fns <- look4func ss
                                        aplicar_ (fns ++ fs) l

aplicar :: (MonadState m, MonadError m) => [Funcs] -> ListElements -> Type -> Bool -> m (ListElements, Type)
aplicar fs xs DEFAULT bool = do l <- aplicar' @TList fs xs  bool
                                return (quote l, DEFAULT)
aplicar fs xs T1 bool = do l <- aplicar' @TList fs xs bool
                           return (quote l, T1)
aplicar fs xs T2 bool = do l <- aplicar' @[] fs xs bool
                           return (quote l, T2)
aplicar fs xs T3 bool = do l <- aplicar' @CList fs xs bool
                           return (quote l, T3)
aplicar fs xs T4 bool = do l <- aplicar' @Tree fs xs bool
                           return (quote l, T4)
aplicar fs xs T5 bool = do l <- aplicar' @S.Seq fs xs bool
                           return (quote l, T5)
aplicar _ _ (INVALID ss) bool = throw (InvalidType ss)

evalExp :: (MonadState m, MonadError m) => Exp -> Bool -> m TypedList
evalExp (List (xs, t)) bool = case t of
                              INVALID ss -> throw (InvalidType ss)
                              _ -> return $ Just (xs, t)
evalExp (Var (var, t)) bool = case t of
                              INVALID ss -> throw (InvalidType ss)
                              DEFAULT -> do exp <- look4var var
                                            evalExp exp bool
                              _ -> do exp <- look4var var
                                      evalExp (changeType exp t) bool

evalExp (Term fs exp) bool = do l <- evalExp exp bool
                                fns <- evalFunc fs
                                -- res <- uncurry (aplicar fns) (fromJust l)
                                res <- uncurry (aplicar fns) (fromJust l) bool
                                return $ Just res
-- =====================================================================================================================

evalFunc :: (MonadState m, MonadError m) => [Funcs] -> m [Funcs]
evalFunc [] = return []
evalFunc ((Defined ss):fns) = do fs <- look4func ss
                                 evalFunc (fs ++ fns)
evalFunc ((Rep fs):fns) = do fns' <- evalFunc fns
                             fs' <- evalFunc fs
                             return $ (Rep fs'):fns'
evalFunc (f:fns) = do fns' <- evalFunc fns
                      return $ f:fns'

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

evalComms :: (MonadState m, MonadError m) => Comms -> Bool -> m TypedList
evalComms (Eval exp) bool = do evalExp exp bool
evalComms (Def ss fs) bool = do -- fns <- evalFunc fs
                           updateFunc ss fs
                           return Nothing
evalComms (Const ss exp) bool = do l <- evalExp exp bool
                                   updateVar ss (List (fromJust l))
                                   return Nothing
evalComms (Infer exp n) bool = inferExp exp n 0 >> return Nothing

eval' :: Comms -> EnvFuncs -> EnvVars -> Bool -> Either Error (TypedList, EnvFuncs, EnvVars)
eval' comm f v bool = runStateError (evalComms comm bool) f v

eval :: [Comms] -> EnvFuncs -> EnvVars -> Bool -> (Either Error TypedList, EnvFuncs, EnvVars)
eval [x] f v bool = case eval' x f v bool of
                  Left err -> (Left err, f, v)
                  Right (res, f', v') -> (Right res, f', v')
eval (x:xs) f v bool = case eval' x f v bool of
                      Right (_, f', v') -> eval xs f' v' bool
                      Left err -> (Left err, f, v)
