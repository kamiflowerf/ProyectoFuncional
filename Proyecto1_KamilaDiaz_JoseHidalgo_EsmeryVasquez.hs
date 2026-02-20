{- Proyecto 1 Programación Funcional: Evaluador de Operaciones -}

-- TIPOS ALGEBRAICOS
-- tipo 'Operador', para separar la estructura de operación
data Operador = Suma | Resta | Mult | Division
-- tipo 'Expresion' que representa el árbol de una expresión aritmética, tipo recursivo
data Expresion = Numero Int | Aplicar Operador Expresion Expresion

-- Aplicar la operación
-- Aplica un operador a dos enteros
-- Devuelve Maybe para manejar errores, Just resultado si la operación es válida y Nothing si ocurre un error

aplicarOperacion :: Operador -> Int -> Int -> Maybe Int
aplicarOperacion Suma x y = Just (x + y)
aplicarOperacion Resta x y = Just (x - y)
aplicarOperacion Mult x y = Just (x * y)
aplicarOperacion Division _ 0 = Nothing
aplicarOperacion Division x y = Just (x `div` y)

--combina dos valores de tipo Maybe Int
--Si alguno de los argumentos es Nothing, devuelve Nothing. Si ambos son Just aplica la función que recibe
--Si hay error en alguna subexpresión, el error se propaga de manera automática.
combinar :: Maybe Int -> Maybe Int -> (Int -> Int -> Maybe Int) -> Maybe Int
combinar Nothing _ _ = Nothing
combinar _ Nothing _ = Nothing
combinar (Just x) (Just y) f = f x y

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
