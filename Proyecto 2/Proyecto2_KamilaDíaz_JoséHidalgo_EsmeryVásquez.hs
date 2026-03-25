{- Proyecto 2 - Programacion Funcional
   Interprete de expresiones aritmeticas con variables

   Kamila Díaz
   José Hidalgo
   Esmery Vásquez

   Este proyecto extiende el Proyecto 1.
   Antes solo se evaluaban expresiones formadas por numeros y operadores.
   Ahora tambien se permiten variables, se usa Either para manejar errores,
   y se mantiene separada la logica pura de la interaccion con el usuario.
-}

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


{- =========================
   ENTRADA -- SALIDA
   ========================= -}

-- Lee un entero de forma segura desde consola si el usuario escribe algo invalido, 
-- vuelve a pedir la entrada.
leerEntero :: String -> IO Int
leerEntero mensaje = do
    putStrLn mensaje
    entrada <- getLine
    case reads entrada of
        [(n, "")] -> return n
        _ -> do
            putStrLn "Entrada invalida. Debe escribir un numero entero."
            leerEntero mensaje

-- Define o redefine una variable
definirVariableIO :: Entorno -> IO Entorno
definirVariableIO env = do
    putStrLn ""
    putStrLn "=== Definir variable ==="
    putStrLn "Nombre de la variable:"
    nombre <- getLine
    valor <- leerEntero "Valor de la variable:"
    let nuevoEnv = insertarVariable nombre valor env
    putStrLn ("Variable definida: " ++ nombre ++ " = " ++ show valor)
    return nuevoEnv

-- Pide al usuario que seleccione un operador
-- Si la opcion no es valida, se vuelve a pedir
pedirOperador :: IO Operador
pedirOperador = do
    putStrLn "Operadores disponibles:"
    putStrLn "1. Suma"
    putStrLn "2. Resta"
    putStrLn "3. Multiplicacion"
    putStrLn "4. Division"
    op <- getLine
    case op of
        "1" -> return Suma
        "2" -> return Resta
        "3" -> return Mult
        "4" -> return Division
        _   -> do
            putStrLn "Opcion invalida."
            pedirOperador

-- Pide un operando: numero, variable o subexpresion anidada
pedirOperando :: Entorno -> IO Expresion
pedirOperando env = do
    putStrLn "Seleccione el tipo de operando:"
    putStrLn "1. Numero"
    putStrLn "2. Variable"
    putStrLn "3. Otra operacion (anidada)"
    opcion <- getLine

    case opcion of
        "1" -> do
            n <- leerEntero "Ingrese el numero:"
            return (Numero n)

        "2" -> do
            putStrLn "Ingrese el nombre de la variable:"
            nombre <- getLine
            return (Variable nombre)

        "3" -> construirExpresionIO env

        _ -> do
            putStrLn "Opcion invalida."
            pedirOperando env

-- Construye una expresion completa.
-- Aquí, la expresion principal siempre es una operacion, esto hace que la opcion 
-- construir expresion sea mas coherente con la idea de evaluador de operaciones.
construirExpresionIO :: Entorno -> IO Expresion
construirExpresionIO env = do
    putStrLn ""
    putStrLn "=== Construccion de expresion ==="
    putStrLn "Operando izquierdo:"
    e1 <- pedirOperando env

    op <- pedirOperador

    putStrLn "Operando derecho:"
    e2 <- pedirOperando env

    return (Aplicar op e1 e2)

-- Muestra las variables definidas
mostrarEntorno :: Entorno -> IO ()
mostrarEntorno [] = putStrLn "No hay variables definidas."
mostrarEntorno env = mapM_ (\(n, v) -> putStrLn (n ++ " = " ++ show v)) env

-- Construye una expresion, la evalua y muestra informacion adicional
evaluarExpresionIO :: Entorno -> IO ()
evaluarExpresionIO env = do
    expr <- construirExpresionIO env
    let resultado = evaluar env expr

    putStrLn ""
    putStrLn ("Expresion construida: " ++ show expr)
    putStrLn ("Variables usadas: " ++ show (recolectarVariables expr))
    putStrLn ("Cantidad de variables en la expresion: " ++ show (cantidadVariables expr))
    putStrLn ("Profundidad del arbol: " ++ show (profundidad expr))
    putStrLn (procesarResultado resultado)

-- Menu principal del programa.
-- Permite definir variables, construir y evaluar expresiones, ver las variables actuales o salir
-- Aunque usa recursion para continuar el flujo del programa,
-- la logica del dominio sigue estando separada en funciones puras
menuPrincipal :: Entorno -> IO ()
menuPrincipal env = do
    putStrLn ""
    putStrLn "==== INTERPRETE DE EXPRESIONES ===="
    putStrLn "1. Definir variable"
    putStrLn "2. Construir y evaluar expresion"
    putStrLn "3. Ver variables definidas"
    putStrLn "4. Salir"
    opcion <- getLine

    case opcion of
        "1" -> do
            nuevoEnv <- definirVariableIO env
            menuPrincipal nuevoEnv

        "2" -> do
            evaluarExpresionIO env
            menuPrincipal env

        "3" -> do
            putStrLn ""
            mostrarEntorno env
            menuPrincipal env

        "4" -> putStrLn "Saliendo del programa."

        _ -> do
            putStrLn "Opcion invalida."
            menuPrincipal env


{- =========================
   MAIN
   ========================= -}

-- Solo se encarga de mostrar el encabezado inicial
-- y comenzar la interaccion con un entorno vacio.

main :: IO ()
main = do
    putStrLn "=============================================="
    putStrLn " Proyecto 2 - Programacion Funcional"
    putStrLn " Interprete de expresiones aritmeticas"
    putStrLn " con variables y manejo de errores con Either"
    putStrLn "=============================================="
    menuPrincipal []