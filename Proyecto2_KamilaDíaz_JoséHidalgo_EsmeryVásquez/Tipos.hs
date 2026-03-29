module Tipos where

-- Representa los operadores  permitidos en el interprete.
-- Se usa deriving Show solo para poder mostrar el nombre del operador
-- cuando sea necesario fuera de la impresion de expresiones.
data Operador = Suma | Resta | Mult | Division deriving (Eq, Show)

-- Representa una expresion aritmetica como un arbol.
-- Una expresion puede ser:
--   un numero entero
--   una variable identificada por su nombre
--   la aplicacion de un operador a dos subexpresiones
data Expresion
    = Numero Int
    | Variable String
    | Aplicar Operador Expresion Expresion
    deriving (Eq)

-- Errores posibles durante la evaluacion
data ErrorEval
    = DivisionPorCero
    | VariableNoDefinida String
    deriving (Eq)

-- Entorno de variables
-- Un entorno es la estructura que guarda las variables definidas
-- por el usuario junto con sus valores
type Entorno = [(String, Int)]


{- =========================
   TYPECLASS
   ========================= -}

-- Instancia manual de Show para Expresion.
-- La idea es mostrar la expresion de forma legible,
-- usando parentesis y simbolos matematicos
instance Show Expresion where
    show (Numero n) = show n
    show (Variable nombre) = nombre
    show (Aplicar op e1 e2) = "(" ++ show e1 ++ " " ++ showOperador op ++ " " ++ show e2 ++ ")"

-- Convierte cada operador a su simbolo matematico.
-- Se usa como funcion auxiliar dentro de Show Expresion
showOperador :: Operador -> String
showOperador Suma = "+"
showOperador Resta = "-"
showOperador Mult = "*"
showOperador Division = "/"

-- Instancia Show para los errores.
instance Show ErrorEval where
    show DivisionPorCero = "Error: division por cero."
    show (VariableNoDefinida nombre) =  "Error: la variable \"" ++ nombre ++ "\" no esta definida."
