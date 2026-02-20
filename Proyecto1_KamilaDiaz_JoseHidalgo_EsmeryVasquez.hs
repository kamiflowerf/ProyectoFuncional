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
