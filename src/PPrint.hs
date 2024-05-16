module PPrint (pp, ppDecl, ppInfer) where

import Data.Text (unpack)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.List.NonEmpty (toList)
import Lang
import Global

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

funcs2doc :: Seq Funcs -> Doc AnsiStyle
funcs2doc fs = hcat (punctuate (pretty " . ") (map func2doc (toList fs)))

func2doc :: Funcs -> Doc AnsiStyle
func2doc (Zero L) = opColor (pretty "zero_left")
func2doc (Zero R) = opColor (pretty "zero_right")
func2doc (Succ L) = opColor (pretty "succ_left")
func2doc (Succ R) = opColor (pretty "succ_right")
func2doc (Delete L) = opColor (pretty "delete_left")
func2doc (Delete R) = opColor (pretty "delete_right")
func2doc (Rep fs) = braces (funcs2doc fs)
func2doc (Defined n) = opColor (pretty n)
func2doc Void = opColor (pretty "void")

exp2doc :: Exp Funcs -> Doc AnsiStyle
exp2doc (Const xs) = constColor (pretty xs)
exp2doc (V n) = name2doc n
exp2doc (App fs e t) =
  let fs' = funcs2doc fs
      e' = exp2doc e
      t' = ty2doc t
  in fs' <+> e' <+> t'

pp :: Exp Funcs -> String
pp = render . exp2doc

decl2doc :: Decl Funcs -> Doc AnsiStyle
decl2doc (Decl _ n e) = (defColor (pretty "let") <+> name2doc n <+> pretty "=") <+> exp2doc e
decl2doc (DeclFunc _ n fs) =
  let fs' = funcs2doc fs
  in (defColor (pretty "def") <+> name2doc n <+> pretty "=") <+> fs'

ppDecl :: Decl Funcs -> String
ppDecl = render . decl2doc

ppInfer :: Int -> String
ppInfer = render . inferColor . pretty