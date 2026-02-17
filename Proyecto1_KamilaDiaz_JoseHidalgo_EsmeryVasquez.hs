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