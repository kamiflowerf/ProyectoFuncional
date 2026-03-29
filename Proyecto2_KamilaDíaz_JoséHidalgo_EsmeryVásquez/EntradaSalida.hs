module EntradaSalida where
import Procesamiento
import Tipos

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
