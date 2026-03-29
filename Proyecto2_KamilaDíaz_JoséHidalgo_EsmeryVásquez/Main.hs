module Main where
import EntradaSalida (menuPrincipal)
{- Proyecto 2 - Programacion Funcional
   Interprete de expresiones aritmeticas con variables

   Kamila Díaz
   José Hidalgo
   Esmery Vásquez

   Este proyecto extiende el Proyecto 1.
   Antes solo se evaluaban expresiones formadas por numeros y operadores.
   Ahora tambien se permiten variables, se usa Either para manejar errores,
   y se mantiene separada la logica pura de la interaccion con el usuario.

   Es recomendable tener los archivos juntos en un folder y correr con
   runhaskell desde el mismo.
-}

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