module AST where
import Data.Map.Strict as M

-- Comandos
data Comms = Def String [Funcs] -- Definir una funcion
           | Const String Exp -- Crear una variable
           | Eval Exp -- Evaluar una expresion
           | Infer Exp Int -- Inferir la longitud de la lista resultante de una expresion
           deriving (Show, Eq)

-- Expresion
data Exp = List (ListElements, Type) -- Lista con su tipo
         | Var (String, Type) -- Variable con su tipo
         | Term [Funcs] Exp -- Aplicacion de una lista de funciones a una expresion
         deriving (Show, Eq)

-- Elementos de las listas
type Elements = Int

-- Lista de elementos
type ListElements = [Elements]

-- "Tipos de estructuras" que hacen referencia a las instancias de FList
-- DEFAULT : Tipo default, lo utilizamos cuando no especificamos el tipo
-- INVALID ss : Tipo invalido, lo utilizamos para devolver un error
data Type = DEFAULT | INVALID String | T1 | T2 | T3 | T4
            deriving (Show, Eq)

-- Mapa de los tipos de estructuras
mapType :: M.Map String Type
mapType = M.fromList [("TList", T1), ("List", T2), ("CList", T3), ("Seq", T4)]

-- Lista de elementos con su tipo
type TypedList = (ListElements, Type)

-- Especificamos los extremos de la lista para aplicar las funciones
data Orientation = L | R deriving (Show, Eq)
-- Funciones
data Funcs = Zero Orientation -- Agregamos un 0 en un extremo dado
           | Succ Orientation -- Aplicamos sucesor al elemento en un extremo dado
           | Delete Orientation -- Eliminamos al elemento de un extremo dado
           | Rep [Funcs] -- Repetimos las funciones de la lista
           | Defined String -- Funciones definidas en el enviroment
           deriving (Show, Eq)

-- Enviroment de funciones
type EnvFuncs = M.Map String [Funcs]

-- Enviroment de variables
type EnvVars = M.Map String Exp

-- Errores
data Error = UndefinedFunc String -- Funcion no definida
           | UndefinedVar String -- Variable no definida
           | InvalidAplication -- Aplicacion invalida de una funcion
           | InvalidInfer Int Int -- Inferencia invalida (Valor real) (Valor propuesto)
           | InferRep -- Inferencia invalida de la funcion repetir
           | InferPower
           | InvalidType String -- Tipo de estructura no definida
           deriving (Show, Eq)

-- ==================================== Azucar Sintactico ====================================
-- Comandos
data SComms = SDef String [SFuncs]
            | SConst String SExp
            | SEval SExp
            | SInfer SExp Int
            deriving (Show, Eq)

-- Expresion
data SExp = SList (ListElements, Type)
          | SVar (String, Type)
          | STerm [SFuncs] SExp
          deriving (Show, Eq)

-- Funciones asucaradas
data SFuncs = SZero Orientation
            | SSucc Orientation
            | SDelete Orientation
            | SRep [SFuncs]
            | SDefined String
            | SPower [SFuncs] Int
            deriving (Show, Eq)
-- ===========================================================================================
