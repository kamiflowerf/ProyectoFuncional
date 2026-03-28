module Procesamiento where
import Tipos    

{- =========================
   LOGICA 
   ========================= -}

-- Busca el valor de una variable dentro del entorno
-- Si la variable existe, devuelve Right valor
-- Si no existe, devuelve Left con el error correspondiente
buscarVariable :: String -> Entorno -> Either ErrorEval Int
buscarVariable nombre [] = Left (VariableNoDefinida nombre)
buscarVariable nombre ((n, v):resto)
    | nombre == n = Right v
    | otherwise   = buscarVariable nombre resto

-- Aplica un operador a dos enteros
-- Si la operacion puede realizarse, devuelve Right resultado
-- Si ocurre division por cero, devuelve Left con el error
aplicarOperacion :: Operador -> Int -> Int -> Either ErrorEval Int
aplicarOperacion Suma x y = Right (x + y)
aplicarOperacion Resta x y = Right (x - y)
aplicarOperacion Mult x y = Right (x * y)
aplicarOperacion Division _ 0 = Left DivisionPorCero
aplicarOperacion Division x y = Right (x `div` y)

-- Combina dos resultados de evaluacion.
-- Esta idea viene del  Proyecto 1, donde se combinaban resultados Maybe
-- Ahora se hace con Either para no perder la informacion del error
-- Si alguna subexpresion falla, el error se propaga
-- Solo si ambas son validas se aplica la funcion recibida
combinarEval :: Either ErrorEval Int -> Either ErrorEval Int -> (Int -> Int -> Either ErrorEval Int)  -> Either ErrorEval Int
combinarEval (Left err) _ _ = Left err
combinarEval _ (Left err) _ = Left err
combinarEval (Right x) (Right y) f = f x y

-- Evalua una expresion de forma recursiva.
-- Casos:
--   Numero: ya es un valor, se devuelve directamente
--   Variable: se busca en el entorno
--   Aplicar: se evalua cada subexpresion y luego se aplica el operador
evaluar :: Entorno -> Expresion -> Either ErrorEval Int
evaluar _   (Numero n) = Right n
evaluar env (Variable nombre) = buscarVariable nombre env
evaluar env (Aplicar op e1 e2) =
    combinarEval (evaluar env e1)
                 (evaluar env e2)
                 (aplicarOperacion op)

-- Recorre una expresion y recolecta los nombres de las variables
-- que aparecen en ella
-- Si la expresion es un numero, no aporta variables
-- Si es una variable, se devuelve una lista con ese nombre
-- Si es una aplicacion, se recorren ambas subexpresiones
recolectarVariables :: Expresion -> [String]
recolectarVariables (Numero _) = []
recolectarVariables (Variable nombre) = [nombre]
recolectarVariables (Aplicar _ e1 e2) =
    recolectarVariables e1 ++ recolectarVariables e2

-- Calcula la profundidad del arbol de expresiones
profundidad :: Expresion -> Int
profundidad (Numero _) = 0
profundidad (Variable _) = 0
profundidad (Aplicar _ e1 e2) =
    1 + max (profundidad e1) (profundidad e2)

-- Cuenta cuantas variables aparecen en la expresion
-- Esta funcion usa foldr sobre la lista producida por recolectarVariables
-- Aqui no se cuentan variables distintas, sino apariciones
-- Ejemplo: (x + (x * y)) tiene 3 apariciones de variables
cantidadVariables :: Expresion -> Int
cantidadVariables expr = foldr (\_ acc -> acc + 1) 0 (recolectarVariables expr)

--Inserta una variable en el entorno.
--Si la variable ya existia, se reemplaza su valor anterior esto evita 
--tener dos definiciones activas del mismo nombre
insertarVariable :: String -> Int -> Entorno -> Entorno
insertarVariable nombre valor env =
    (nombre, valor) : filter (\(n, _) -> n /= nombre) env

--Convierte un resultado de evaluacion en un mensaje legible
--Right se transforma en resultado exitoso
--Left se muestra usando la instancia Show del error
formatearResultado :: Either ErrorEval Int -> String
formatearResultado (Left err)  = show err
formatearResultado (Right val) = "Resultado: " ++ show val

-- Agrega una etiqueta al mensaje final
agregarEtiqueta :: String -> String
agregarEtiqueta mensaje = "[Evaluacion] " ++ mensaje

-- Primero se formatea el resultado y luego se agrega una etiqueta
procesarResultado :: Either ErrorEval Int -> String
procesarResultado = agregarEtiqueta . formatearResultado