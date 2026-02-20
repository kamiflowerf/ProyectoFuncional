{- Proyecto 1 Programación Funcional: Evaluador de Operaciones -}

-- TIPOS ALGEBRAICOS
-- tipo 'Operador', para separar la estructura de operación
data Operador = Suma | Resta | Mult | Division
-- tipo 'Expresion' que representa el árbol de una expresión aritmética, tipo recursivo
data Expresion = Numero Int | Aplicar Operador Expresion Expresion

--Aplicar la operación
-- Aplica un operador a dos enteros
-- Devuelve Maybe para manejar errores

aplicarOperacion :: Operador -> Int -> Int -> Maybe Int
aplicarOperacion Suma x y = Just (x + y)
aplicarOperacion Resta x y = Just (x - y)
aplicarOperacion Mult x y = Just (x * y)
aplicarOperacion Division _ 0 = Nothing
aplicarOperacion Division x y = Just (x `div` y)

{-  evaluar.
    Toma las expresiones y las evalua llamandose a si misma hasta 
    descomponer la expresion en numeros, caso en donde recurre a la
    llamada orginal devolviendo el/los resultado(s) de aplicarOperacion.

    Toma como argumento una expresion tipo Expression, que puede 
    consistir de varias expresiones tipo Aplicar, y devuelve Maybe Int.
 -}
evaluar :: Expresion -> Maybe Int
evaluar (Numero n) = Just n
evaluar (Aplicar op e1 e2) = do
    v1 <- evaluar e1
    v2 <- evaluar e2
    aplicarOperacion op v1 v2

maybeToIO :: a -> IO a
maybeToIO x = return x

--IO
main ::IO()

main = do
    print "Evaluador de Operaciones"
    print "Introduzca una expresion:"
    exp <- maybeToIO $ evaluar  (Aplicar (Mult) (Numero 2) $ Aplicar (Suma) (Numero 3) (Numero 4))-- Ejemplo de expresión
    print(exp)