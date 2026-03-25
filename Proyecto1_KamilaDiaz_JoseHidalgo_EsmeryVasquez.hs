{- Proyecto 1 Programacion Funcional: Evaluador de Operaciones -}

{- ============TIPOS ALGEBRAICOS============ -}
-- Representa los operadores aritmeticos permitidos
-- Se usa 'deriving Read' para poder convertir texto a operador automaticamente
data Operador = Suma | Resta | Mult | Division deriving (Read, Show)

-- Representa una expresion aritmetica como un arbol
-- Puede ser un numero o la aplicacion de un operador a dos subexpresiones
data Expresion = Numero Int | Aplicar Operador Expresion Expresion deriving (Read, Show)

{- ================FUNCIONES================ -}

-- Aplica un operador a dos enteros
-- Si la operacion es valida, devuelve Just resultado
-- Si ocurre un error (ej. division por cero), devuelve Nothing
aplicarOperacion :: Operador -> Int -> Int -> Maybe Int
aplicarOperacion Suma x y = Just (x + y)
aplicarOperacion Resta x y = Just (x - y)
aplicarOperacion Mult x y = Just (x * y)
aplicarOperacion Division _ 0 = Nothing -- evita division por cero
aplicarOperacion Division x y = Just (x `div` y)

-- Combina dos valores de tipo Maybe
-- Si alguna subexpresion fallo (Nothing), el error se propaga automaticamente
-- Solo si ambas son validas (Just), se aplica la funcion recibida
combinar :: Maybe Int -> Maybe Int -> (Int -> Int -> Maybe Int) -> Maybe Int
combinar Nothing _ _ = Nothing
combinar _ Nothing _ = Nothing
combinar (Just x) (Just y) f = f x y

-- Evalua una expresion de forma recursiva
-- Si es un numero, devuelve Just n
-- Si es una aplicacion, evalua ambas partes y aplica el operador
-- Si ocurre algun error en el proceso, devuelve Nothing
evaluar :: Expresion -> Maybe Int
evaluar (Numero n) = Just n
evaluar (Aplicar op e1 e2) =
    combinar (evaluar e1)
             (evaluar e2)
             (aplicarOperacion op)


-- Convierte un String en Expresion de forma segura
-- Si el usuario escribe algo mal, devuelve Nothing y el error se maneja despues
leerExpresion :: String -> Maybe Expresion
leerExpresion str =
    case reads str of
        [(exp, "")] -> Just exp
        _           -> Nothing

{- ====================MAIN==================== -}

main :: IO ()
main = do
    putStrLn "=== Evaluador de operaciones aritmeticas ==="
    putStrLn ""
        -- Explicacion clara para el usuario
    putStrLn "Formato requerido:"
    putStrLn "Aplicar Operador Expresion Expresion"
    putStrLn ""
    putStrLn "Ejemplo:"
    putStrLn "Aplicar Mult (Aplicar Suma (Numero 8) (Numero 4)) (Aplicar Resta (Numero 3) (Numero 1))"
    putStrLn ""
    putStrLn "Operadores disponibles: Suma | Resta | Mult | Division"
    putStrLn ""

    putStrLn "Introduzca una expresion:"
    input <- getLine


        -- Primero se intenta convertir la entrada a una expresion valida
    case leerExpresion input of
         -- Si el usuario escribe algo incorrecto, se informa el error
        Nothing -> putStrLn "Error: formato de expresion invalido."
        Just exp ->
             -- Si es correcto, se evalua la expresion
            case evaluar exp of
                -- Error durante la evaluacion (ej. division por cero)
                Nothing -> putStrLn "Error: operacion invalida (ej. division por cero)."
                Just resultado -> do
                    putStrLn "Resultado:"
                    print resultado