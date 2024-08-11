module PPrint (pp, ppDecl, ppInfer, sugar, sugarFuncs) where

import Data.List.NonEmpty (NonEmpty (..), fromList, toList, (<|))
import Data.Text (unpack)
import Global
import Lang
import MonadFL
import Prettyprinter
import Prettyprinter.Render.Terminal
import Subst (open)

freshName :: [Name] -> Name -> Name
freshName ns n =
  let candidates = n : map (\i -> n ++ show (i :: Integer)) [1 ..]
   in head (filter (`notElem` ns) candidates)

openAllExp :: [Name] -> Exp Funcs Var -> Exp Funcs Name
openAllExp _ (Const xs) = Const xs
openAllExp _ (V var) =
  case var of
    Bound i -> error ("openAllExp: expression is not locally closed " ++ show i)
    Free n -> V n
    Global n -> V n
openAllExp ns (App fs e t) = App fs (openAllExp ns e) t
openAllExp ns (Print e) = Print (openAllExp ns e)
openAllExp ns (LetIn name u v) =
  let new = freshName ns name
   in LetIn new (openAllExp ns u) (openAllExp (new : ns) (open new v))

sugarFunc :: Funcs -> SFuncs
sugarFunc (Zero o) = SZero o
sugarFunc (Succ o) = SSucc o
sugarFunc (Delete o) = SDelete o
sugarFunc (Rep fs) = SRep (sugarFuncs fs)
sugarFunc (Defined n) = SDefined n
sugarFunc Void = SVoid

collectPower :: Seq Funcs -> Seq SFuncs
collectPower = go SVoid 1
 where
  go p n fs@(f :| [])
    | p == SVoid = sugarFunc f :| []
    | p == sugarFunc f = SPower (n + 1) (p :| []) :| []
    | n == 1 = p <| sugarFuncs fs
    | otherwise = SPower n (p :| []) <| sugarFuncs fs
  go SVoid _ (f :| fs) = go (sugarFunc f) 1 (fromList fs)
  go p n (f :| fs)
    | p == sugarFunc f = go p (n + 1) (fromList fs)
    | n == 1 = p <| go (sugarFunc f) 1 (fromList fs)
    | otherwise = SPower n (p :| []) <| go (sugarFunc f) 1 (fromList fs)

sugarFuncs :: Seq Funcs -> Seq SFuncs
sugarFuncs = collectPower

sugar :: Exp Funcs Name -> Exp SFuncs Name
sugar (Const xs) = Const xs
sugar (V name) = V name
sugar (App fs e t) = App (sugarFuncs fs) (sugar e) t
sugar (Print e) = Print (sugar e)
sugar (LetIn name u v) = LetIn name (sugar u) (sugar v)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- Colors
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = id

funColor :: Doc AnsiStyle -> Doc AnsiStyle
funColor = annotate (colorDull Green)

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (color Blue)

typeColor :: Doc AnsiStyle -> Doc AnsiStyle
typeColor = annotate (color Blue <> italicized)

inferColor :: Doc AnsiStyle -> Doc AnsiStyle
inferColor = annotate (colorDull Blue)

defColor :: Doc AnsiStyle -> Doc AnsiStyle
defColor = annotate (colorDull Magenta <> italicized)

nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = id

name2doc :: String -> Doc AnsiStyle
name2doc n = nameColor (pretty n)

ty2doc :: Type -> Doc AnsiStyle
ty2doc DEFAULT = typeColor (angles (pretty defaultType))
ty2doc (T t) = typeColor (angles (pretty t))

list2doc :: [Element] -> Doc AnsiStyle
list2doc = brackets . hcat . punctuate (pretty ",") . map pretty

funcs2doc :: Seq SFuncs -> Doc AnsiStyle
funcs2doc fs = hcat (punctuate (pretty ".") (map func2doc (toList fs)))

func2doc :: SFuncs -> Doc AnsiStyle
func2doc (SZero L) = funColor (pretty "zero_left")
func2doc (SZero R) = funColor (pretty "zero_right")
func2doc (SSucc L) = funColor (pretty "succ_left")
func2doc (SSucc R) = funColor (pretty "succ_right")
func2doc (SDelete L) = funColor (pretty "delete_left")
func2doc (SDelete R) = funColor (pretty "delete_right")
func2doc (SRep fs) = braces (funcs2doc fs)
func2doc (SDefined n) = funColor (pretty n)
func2doc SVoid = funColor (pretty "void")
func2doc (SPower n fs) = parens (funColor (funcs2doc fs)) <> (pretty "^" <> annotate italicized (pretty n))

exp2doc :: Exp SFuncs Name -> Doc AnsiStyle
exp2doc (Const xs) = constColor (list2doc xs)
exp2doc (V n) = name2doc n
exp2doc (App fs e t) =
  let fs' = funcs2doc fs
      e' = exp2doc e
      t' = ty2doc t
   in fs' <+> e' <+> t'
exp2doc (Print e) = opColor (pretty "print") <+> exp2doc e
exp2doc (LetIn name u v) =
  opColor (pretty "let") <+> name2doc name <+> opColor (pretty "=") <+> exp2doc u <+> opColor (pretty "in") <+> exp2doc v

pp :: (MonadFL m) => LNExp -> m String
pp e = do
  env <- gets envExp
  return (render . exp2doc . sugar . openAllExp (map fst env) $ e)

decl2doc :: SDecl -> Doc AnsiStyle
decl2doc (Decl _ n e) = (defColor (pretty "let") <+> name2doc n <+> pretty "=") <+> exp2doc e
decl2doc (DeclFunc _ n fs) =
  let fs' = funcs2doc fs
   in (defColor (pretty "def") <+> name2doc n <+> pretty "=") <+> fs'

ppDecl :: (MonadFL m) => LNDecl -> m String
ppDecl (Decl p n e) = do
  env <- gets envExp
  return (render . decl2doc $ Decl p n (sugar . openAllExp (map fst env) $ e))
ppDecl (DeclFunc p n fs) = return (render . decl2doc $ DeclFunc p n (sugarFuncs fs))

ppInfer :: Int -> String
ppInfer = render . inferColor . pretty
