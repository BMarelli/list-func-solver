module PPrint (pp, ppDecl, ppInfer, sugar, sugarFuncs) where

import Data.Text (unpack)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.List.NonEmpty (NonEmpty(..), toList, fromList, (<|))
import Lang
import Global

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
      | p == sugarFunc f = SPower (n+1) (p:|[]) :| []
      | n == 1     = p <| sugarFuncs fs
      | otherwise  = SPower n (p:|[]) <| sugarFuncs fs
    go SVoid _ (f:|fs) = go (sugarFunc f) 1 (fromList fs)
    go p n (f :| fs)
      | p == sugarFunc f = go p (n+1) (fromList fs)
      | n == 1           = p <| go (sugarFunc f) 1 (fromList fs)
      | otherwise        = SPower n (p:|[]) <| go (sugarFunc f) 1 (fromList fs) 

sugarFuncs :: Seq Funcs -> Seq SFuncs
sugarFuncs = collectPower

sugar :: Exp Funcs -> Exp SFuncs
sugar (Const xs) = Const xs
sugar (V name) = V name
sugar (App fs e t) = App (sugarFuncs fs) (sugar e) t

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- Colors
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = id

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)

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

funcs2doc :: Seq SFuncs -> Doc AnsiStyle
funcs2doc fs = hcat (punctuate (pretty " . ") (map func2doc (toList fs)))

func2doc :: SFuncs -> Doc AnsiStyle
func2doc (SZero L) = opColor (pretty "zero_left")
func2doc (SZero R) = opColor (pretty "zero_right")
func2doc (SSucc L) = opColor (pretty "succ_left")
func2doc (SSucc R) = opColor (pretty "succ_right")
func2doc (SDelete L) = opColor (pretty "delete_left")
func2doc (SDelete R) = opColor (pretty "delete_right")
func2doc (SRep fs) = braces (funcs2doc fs)
func2doc (SDefined n) = opColor (pretty n)
func2doc SVoid = opColor (pretty "void")
func2doc (SPower n fs) = parens (opColor (funcs2doc fs)) <> (pretty "^" <> annotate italicized (pretty n))

exp2doc :: Exp SFuncs -> Doc AnsiStyle
exp2doc (Const xs) = constColor (pretty xs)
exp2doc (V n) = name2doc n
exp2doc (App fs e t) =
  let fs' = funcs2doc fs
      e' = exp2doc e
      t' = ty2doc t
  in fs' <+> e' <+> t'

pp :: Exp SFuncs -> String
pp = render . exp2doc

decl2doc :: Decl SFuncs -> Doc AnsiStyle
decl2doc (Decl _ n e) = (defColor (pretty "let") <+> name2doc n <+> pretty "=") <+> exp2doc e
decl2doc (DeclFunc _ n fs) =
  let fs' = funcs2doc fs
  in (defColor (pretty "def") <+> name2doc n <+> pretty "=") <+> fs'

ppDecl :: Decl SFuncs -> String
ppDecl = render . decl2doc

ppInfer :: Int -> String
ppInfer = render . inferColor . pretty
