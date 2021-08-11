{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module ListEval where
-- import Debug.Trace
import AST
import Data.Map.Strict as M hiding (splitAt, delete, map)
import Data.Maybe
import qualified Data.Sequence as S
import Control.Monad
import List.FList
import List.TupleFList
import List.ListFList
import List.CListFList
-- import List.TreeFList
import List.SeqFList
import Control.Monad.IO.Class
import Monads

-- Cambia el tipo de una expresion por otro
changeType :: Exp -> Type -> Exp
changeType (List (xs, _)) t = List (xs, t)
changeType (Var (var, _)) t = Var (var, t)
changeType (Term fns exp) t = Term fns (changeType exp t)

-- ======================================== Mejora Rendimiento =========================================================
-- Aplicamos las funciones a lista de elementos como la instancia de FList que recibimos.
aplicar' :: forall l m . (FList l, MonadState m, MonadError m) => [Funcs] -> ListElements -> m (l Elements)
aplicar' fs l = aplicar_ fs (List.FList.fromList l)
    where
      aplicar_ :: forall l m . (FList l, MonadState m, MonadError m) => [Funcs] -> l Elements -> m (l Elements)
      aplicar_ [] l = return l
      aplicar_ ((Zero or):fs) l = do let l' = zero or l
                                    --  traceM ("zero " ++ show or ++ " -> " ++ printFL l')
                                     aplicar_ fs l'
      aplicar_ ((Succ or):fs) l = case succesor or l of
                                  Left err -> throw err
                                  Right l' -> aplicar_ fs l'
                                  -- Right l' -> traceM ("succ " ++ show or ++ " -> " ++ printFL l') >> aplicar_ fs l'
      aplicar_ ((Delete or):fs) l = case delete or l of
                                  Left err -> throw err
                                  Right l' -> aplicar_ fs l'
                                  -- Right l' -> traceM ("delete " ++ show or ++ " -> " ++ printFL l') >> aplicar_ fs l'
      aplicar_ ((Rep f):fs) l = case rep f l of
                                  Left err -> throw err
                                  Right l' -> aplicar_ fs l'
                                  -- Right l' -> traceM ("rep " ++ " -> " ++ printFL l') >> aplicar_ fs l'
      aplicar_ ((Defined ss):fs) l = do fns <- look4func ss
                                        aplicar_ (fns ++ fs) l

-- Dada una lista de funciones, una lista de elementos y un instancia de FList
-- Aplica las funciones a la lista de elementos utilizando la instancia de FList
aplicar :: (MonadState m, MonadError m) => [Funcs] -> ListElements -> Type -> m (ListElements, Type)
aplicar fs xs DEFAULT = do l <- aplicar' @S.Seq fs xs
                           return (quote l, DEFAULT)
aplicar fs xs T1 = do l <- aplicar' @TList fs xs
                      return (quote l, T1)
aplicar fs xs T2 = do l <- aplicar' @[] fs xs
                      return (quote l, T2)
aplicar fs xs T3 = do l <- aplicar' @CList fs xs
                      return (quote l, T3)
aplicar fs xs T4 = do l <- aplicar' @S.Seq fs xs
                      return (quote l, T4)
-- aplicar fs xs T5 = do l <- aplicar' @S.Seq fs xs
--                       return (quote l, T5)
aplicar _ _ (INVALID ss) = throw (InvalidType ss)

-- Evalua una expresion y devuelve una TypedList como resultado
-- En el caso que se produzca un error, lo devolvemos
evalExp :: (MonadState m, MonadError m) => Exp -> m (Maybe TypedList)
evalExp (List (xs, t)) = case t of
                              INVALID ss -> throw (InvalidType ss)
                              _ -> return $ Just (xs, t)
evalExp (Var (var, t)) = case t of
                              INVALID ss -> throw (InvalidType ss)
                              DEFAULT -> do exp <- look4var var
                                            evalExp exp
                              _ -> do exp <- look4var var
                                      evalExp (changeType exp t)

evalExp (Term fs exp) = do l <- evalExp exp
                           fns <- evalFunc fs
                           -- res <- uncurry (aplicar fns) (fromJust l)
                           res <- uncurry (aplicar fns) (fromJust l)
                           return $ Just res
-- =====================================================================================================================

-- Evalua una lista de funciones y devuelve una lista de funciones canonicas
-- Utilizamos esta funcion para remplazar las funciones definidas
evalFunc :: (MonadState m, MonadError m) => [Funcs] -> m [Funcs]
evalFunc [] = return []
evalFunc ((Zero o1):(Delete o2):fns) | o1 == o2 = return fns
evalFunc ((Succ o1):(Delete o2):fns) | o1 == o2 = return fns
evalFunc ((Defined ss):fns) = do fs <- look4func ss
                                 evalFunc (fs ++ fns)
evalFunc ((Rep fs):fns) = do fns' <- evalFunc fns
                             fs' <- evalFunc fs
                             return $ (Rep fs'):fns'
evalFunc (f:fns) = do fns' <- evalFunc fns
                      return $ f:fns'

-- Infiere la longitud de una expresion 
-- Como no podemos inferir la longitud sobre la funcion Rep, devolvemos un error de inferencia sobre Rep
-- En el caso que se produzca un error sobre la inferencia, devolvemos un error de inferencia
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

-- Evalua los distintos comandos y devuelve una TypedList como resultado
evalComms :: (MonadState m, MonadError m) => Comms -> m (Maybe TypedList)
evalComms (Eval exp) = do evalExp exp
evalComms (Def ss fs) = do fns <- evalFunc fs
                           updateFunc ss fs
                           return Nothing
evalComms (Const ss exp) = do l <- evalExp exp
                              updateVar ss (List (fromJust l))
                              return Nothing
evalComms (Infer exp n) = inferExp exp n 0 >> return Nothing

-- Evalua un comando con los enviroments dados
eval' :: Comms -> EnvFuncs -> EnvVars -> Either Error (Maybe TypedList, EnvFuncs, EnvVars)
eval' comm f v = runStateError (evalComms comm) f v

-- [Comms]: Lista de comandos
-- EnvFuncs: enviroment de funciones
-- EnvVars: enviroment de variables
eval :: [Comms] -> EnvFuncs -> EnvVars -> (Either Error (Maybe TypedList), EnvFuncs, EnvVars)
eval [x] f v = case eval' x f v of
                  Left err -> (Left err, f, v)
                  Right (res, f', v') -> (Right res, f', v')
eval (x:xs) f v = case eval' x f v of
                      Right (_, f', v') -> eval xs f' v'
                      Left err -> (Left err, f, v)
