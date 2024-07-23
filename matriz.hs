-- Sebastian Zeballos Cesar
-- Registro: 222059087


{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MonoLocalBinds #-}
module Main where

import System.IO (hFlush, stdout)
import Data.List (transpose)


type Matriz a = [[Float]]

valido_M :: Matriz a -> Bool
valido_M [] = True
valido_M (x:m) = all (\x' -> length x' == length x) m

iguales :: Matriz a -> Matriz a -> Bool
iguales [][] = True
iguales m1 m2 = length m1 == length m2

cantColum :: Matriz a -> Int
cantColum [] = 0
cantColum (f:m) = length f

cantFila :: Matriz a -> Int
cantFila = length

elemento :: Int -> Int -> Matriz a -> Float
elemento f c m = (m !! f) !! c


suma :: Num Float => Matriz a -> Matriz a -> Matriz a
suma m1 m2 = if iguales m1 m2 then
                zipWith (zipWith (+)) m1 m2
             else
                error "Las Matrices no son Iguales"

resta :: Num Float => Matriz a -> Matriz a -> Matriz a
resta m1 m2 = if iguales m1 m2 then
                zipWith (zipWith (-)) m1 m2
             else
                error "Las Matrices no son Iguales"

multiplicacion :: Num Float => Matriz a -> Matriz a -> Matriz a
multiplicacion m1 m2 = if cantColum m1 == cantFila m2 then
                            [ [sum (zipWith (*) fila columna) | columna <- transpose m2] | fila <- m1 ]
                        else
                            error "La cantidad de columnas de la M1 es distinto a la cantidad de filas de la M2"

submatriz :: Int -> Int -> Matriz a -> Matriz a
submatriz f c m = [[ m !! k !! l | l <- [0..n-1], l /= c] | k <- [0..n-1], k/=f ]
                    where n = length m

determinante :: Num Float => Matriz a -> Float
determinante [[x]] = x
determinante m = sum  [ (-1) ^ i * (m !! 0 !! i) * determinante (submatriz 0 i m)| i <- [0..n-1] ] 
                where n = length m


getCol :: Int -> Matriz Float -> [Float]
getCol _ [] = []
getCol i (f:m) =
    if i < 0 || i >= cantColum (f:m)
        then []
        else (f !! i) : getCol i m



--------------------------FORMULARIO --------------------------------------------------
main :: IO ()
main = do
    putStrLn "Matrices en Haskell"
    interactivoMatriz

interactivoMatriz :: IO ()
interactivoMatriz = do
    putStrLn "\nOpciones:"
    putStrLn "1. Ingresar dos matrices"
    putStrLn "2. Salir"

    opcion <- solicitarOpcion

    case opcion of
        "1" -> do
            putStrLn "Ingrese la primera matriz en el formato [[],[]]:"
            matriz1 <- solicitarMatriz
            putStrLn "\nPrimera matriz ingresada:"
            printMatriz matriz1

            putStrLn "\nIngrese la segunda matriz en el formato [[],[]]:"
            matriz2 <- solicitarMatriz
            putStrLn "\nSegunda matriz ingresada:"
            printMatriz matriz2

            operacion <- operaciones
            ejecutarOperacion operacion matriz1 matriz2

            volver <- reset
            opreset volver matriz1 matriz2

        "2" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida. Inténtelo de nuevo."
            interactivoMatriz

reset :: IO String
reset = do 
    putStrLn "\nDesea continuar:"
    putStrLn "1. Volver a cargar las Matrices"
    putStrLn "2. Elegir otra operacion"
    putStrLn "3. salir"
    putStr "Elija la opcion: "
    hFlush stdout
    getLine

opreset :: String -> Matriz Float -> Matriz Float -> IO ()
opreset "1" _ _ = interactivoMatriz
opreset "2" matriz1 matriz2 = do
    operacion <- operaciones
    ejecutarOperacion operacion matriz1 matriz2
    volver <- reset
    opreset volver matriz1 matriz2
opreset "3" _ _ = putStrLn "Saliendo..."
opreset _ matriz1 matriz2 = do
    putStrLn "Opción no válida. Inténtelo de nuevo."
    volver <- reset
    opreset volver matriz1 matriz2
        

operaciones :: IO String
operaciones = do
    putStrLn "\nOperaciones disponibles entre las matrices:"
    putStrLn "1. Suma"
    putStrLn "2. Resta"
    putStrLn "3. Multiplicación"
    putStrLn "4. Determinante de la primera matriz"
    putStrLn "5. Defensa"
    putStr "Elija la operación: "
    hFlush stdout
    getLine

ejecutarOperacion :: String -> Matriz Float -> Matriz Float -> IO ()
ejecutarOperacion "1" matriz1 matriz2 = do
    putStrLn "Realizando suma de matrices..."
    putStrLn "Resultado:"
    printMatriz (suma matriz1 matriz2)

ejecutarOperacion "2" matriz1 matriz2 = do
    putStrLn "Realizando resta de matrices..."
    putStrLn "Resultado:"
    printMatriz (resta matriz1 matriz2)

ejecutarOperacion "3" matriz1 matriz2 = do
    putStrLn "Realizando multiplicación de matrices..."
    putStrLn "Resultado:"
    printMatriz (multiplicacion matriz1 matriz2)

ejecutarOperacion "4" matriz1 matriz2 = do
    putStrLn "Calculando determinante..."
    select <- selectMatriz
    matrizDet select matriz1 matriz2

ejecutarOperacion "5" matriz1 matriz2 = do
    indice <- solicitarIndiceColumna
    putStrLn "Resultado:"
    printMatriz [getCol (indice-1) matriz1]

ejecutarOperacion _ _ _ = putStrLn "Operación no válida."

solicitarIndiceColumna :: IO Int
solicitarIndiceColumna = do
    putStr "Ingrese el índice de la columna: "
    hFlush stdout
    readLn

selectMatriz :: IO String
selectMatriz = do
    putStrLn "\nElija la Matriz:"
    putStrLn "1. M1"
    putStrLn "2. M2"
    putStr "Elija la operación: "
    hFlush stdout
    getLine

matrizDet :: String -> Matriz Float -> Matriz Float -> IO ()
matrizDet "1" matriz1 matriz2 = do 
    putStrLn $ "Determinante M1: " ++ show (determinante matriz1)
matrizDet "2" matriz1 matriz2 = do 
    putStrLn $ "Determinante M2: " ++ show (determinante matriz2)


solicitarOpcion :: IO String
solicitarOpcion = do
    putStr "Ingrese una opción: "
    hFlush stdout
    getLine

solicitarMatriz :: IO (Matriz Float)
solicitarMatriz = do
    putStr "Ingrese la matriz: "
    hFlush stdout
    linea <- getLine
    case parseMatriz linea of
        Just matriz -> if valido_M matriz
                       then return matriz
                       else do
                           putStrLn "Formato de matriz incorrecto. Inténtelo de nuevo."
                           solicitarMatriz
        Nothing -> do
            putStrLn "Formato de matriz incorrecto. Inténtelo de nuevo."
            solicitarMatriz

parseMatriz :: Read Float => String -> Maybe (Matriz Float)
parseMatriz str = case reads str of
    [(matriz, "")] -> Just matriz
    _              -> Nothing

printMatriz :: Show Float => Matriz Float -> IO ()
printMatriz = mapM_ print 