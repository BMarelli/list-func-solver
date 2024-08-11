module Parse where

import Data.Functor
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Lang
import Lib
import Text.Parsec hiding (oneOf, parse, runP)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Prelude hiding (print)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------

-- | Lexer analysis
lexer :: Tok.TokenParser u
lexer =
  Tok.makeTokenParser $
    emptyDef
      { commentLine = "#"
      , reservedNames =
          [ "def"
          , "let"
          , "print"
          , "zero_left"
          , "z_l"
          , "zero_right"
          , "z_r"
          , "succ_left"
          , "s_l"
          , "succ_right"
          , "s_r"
          , "delete_left"
          , "d_l"
          , "delete_right"
          , "d_r"
          ]
      , reservedOpNames = ["^", "."]
      }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

identifier :: P String
identifier = Tok.identifier lexer

parens :: P a -> P a
parens = Tok.parens lexer

bracket :: P a -> P a
bracket = Tok.brackets lexer

braces :: P a -> P a
braces = Tok.braces lexer

angles :: P a -> P a
angles = Tok.angles lexer

comma :: P String
comma = Tok.comma lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

getPos :: P Pos
getPos = do
  pos <- getPosition
  return $ Pos (sourceLine pos) (sourceColumn pos)

oneOf :: [P a] -> P a
oneOf = choice . map try

-----------------------
-- Parsers
-----------------------

typeL :: P Type
typeL = try (angles (T <$> identifier)) <|> return DEFAULT

num :: P Element
num = fromInteger <$> natural

var :: P Name
var = identifier

list :: P [Element]
list = bracket (num `sepBy` comma)

zeroLeft :: P SFuncs
zeroLeft = oneOf [reserved "zero_left", reserved "z_l"] >> return (SZero L)

zeroRight :: P SFuncs
zeroRight = oneOf [reserved "zero_right", reserved "z_r"] >> return (SZero R)

succLeft :: P SFuncs
succLeft = oneOf [reserved "succ_left", reserved "s_l"] >> return (SSucc L)

succRight :: P SFuncs
succRight = oneOf [reserved "succ_right", reserved "s_r"] >> return (SSucc R)

deleteLeft :: P SFuncs
deleteLeft = oneOf [reserved "delete_left", reserved "d_l"] >> return (SDelete L)

deleteRight :: P SFuncs
deleteRight = oneOf [reserved "delete_right", reserved "d_r"] >> return (SDelete R)

defined :: P SFuncs
defined = SDefined <$> identifier

rep :: P SFuncs
rep = braces (SRep <$> funcs)

power :: P SFuncs
power = do
  sfns <- parens funcs
  reservedOp "^"
  n <- fromInteger <$> natural
  return $ SPower n sfns

func :: P SFuncs
func = oneOf [zeroLeft, zeroRight, succLeft, succRight, deleteLeft, deleteRight, defined, rep, power]

funcs :: P (Seq SFuncs)
funcs = do
  sfns <- func `sepBy1` oneOf [reservedOp "."]
  return $ fromList sfns

atom :: P (Exp SFuncs Name)
atom = oneOf [Const <$> list, V <$> var]

app :: P (Exp SFuncs Name)
app = do
  sfns <- funcs
  e <- atom
  App sfns e <$> typeL

print :: P (Exp SFuncs Name)
print = reserved "print" >> Print <$> expr

expr :: P (Exp SFuncs Name)
expr = oneOf [app, atom, print]

declVar :: P SDecl
declVar = do
  i <- getPos
  reserved "let"
  v <- var
  reservedOp "="
  Decl i v <$> expr

declFunc :: P SDecl
declFunc = do
  i <- getPos
  reserved "def"
  v <- var
  reservedOp "="
  DeclFunc i v <$> funcs

decl :: P SDecl
decl = oneOf [declVar, declFunc]

program :: P [SDecl]
program = many decl

declOrExpr :: P (Either SDecl (Exp SFuncs Name))
declOrExpr = oneOf [Left <$> decl, Right <$> expr]

-- | Run parser
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s
