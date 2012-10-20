--  Module Haskinator
--  Universidad Simón Bolívar
--  Laboratorio de Lenguajes de Programacion (CI-3661)
--  Proyecto I Haskell - Haskinator
--  Desarrollado por: Maria Gracia Hidalgo  03-36048
--                    Alejandro Tarazona    05-38978

module Main (main) where

import Char
import Data.Char
import Oraculo

main :: IO ()

main = menu Nothing

menu :: Maybe Oraculo -> IO ()
menu p =
    do
        putStrLn "\nBienvenido al Haskinator. Seleccione una opción:"
        putStrLn "\nOpciones:"
        putStrLn "0) Salir."
        putStrLn "1) Crear un Oráculo Nuevo."
        putStrLn "2) Predecir."
        putStrLn "3) Persistir."
        putStrLn "4) Cargar."
        putStrLn "5) Consultar Pregunta Crucial."
        putStrLn "6) Consultar Estadísticas.\n"
        opcion <- getLine
        case opcion of
            "0" -> putStrLn "\nBueno, chao\n"
            "1" -> crearNuevoOraculo
            "2" -> predecir p
            "3" -> persistir p
            "4" -> cargar
            "5" -> consultarPreguntaCrucial p
            "6" -> consultarEstadisticas p
            _   -> putStrLn "Opción inválida" >> menu p
        
crearNuevoOraculo :: IO ()
crearNuevoOraculo = putStrLn "\nNuevo Oráculo Creado." >> menu Nothing
     
predecir :: Maybe Oraculo -> IO ()
predecir Nothing = 
    do
        putStrLn "No hay predicción hasta ahora. Introduzca una nueva predicción:"
        respuesta <- getLine
        menu (Just (Prediccion respuesta))
predecir (Just p) = caminaOraculo (Just p) p []

caminaOraculo :: Maybe Oraculo -> Oraculo -> [(String, Bool, Oraculo)] -> IO ()
caminaOraculo oraculo (Pregunta q p n) l =
    do
        putStr "\n¿"
        putStr q
        putStrLn "?"
        respuesta <- getLine
        case map Char.toLower respuesta of
             "si" -> si
             "sí" -> si
             "no" -> no
             _    -> putStrLn "Respuesta inválida." >> menu oraculo
    where si = caminaOraculo oraculo p (l ++ [(q, True, n)])
          no = caminaOraculo oraculo n (l ++ [(q, False, p)])
caminaOraculo oraculo (Prediccion s) l = 
    do
        putStr "\nLa respuesta es "
        putStr s
        putStrLn ".\n"
        putStrLn "¿Es correcta la predicción dada?"
        respuesta <- getLine
        case map Char.toLower respuesta of
             "si" -> si
             "sí" -> si
             "no" -> no
             _    -> putStrLn "Respuesta inválida" >> menu oraculo
    where si = putStrLn "\nGracias por jugar con Haskinator." >> menu oraculo
          no = do
              putStrLn "Introduza la respuesta correcta:"
              rcorrecta <- getLine
              putStrLn "Introduzca una característica que describa y distinga su predicción:"
              pcorrecta <- getLine
              menu (Just (armarArbol (crearPregunta pcorrecta (Prediccion (map Char.toLower rcorrecta)) (Prediccion s)) l))
              
armarArbol :: Oraculo -> [(String, Bool, Oraculo)] -> Oraculo
armarArbol oraculo ((q, b, o):xs)
    | b = crearPregunta q (armarArbol oraculo xs) o
    | otherwise = crearPregunta q o (armarArbol oraculo xs)
armarArbol oraculo [] = oraculo
     
persistir :: Maybe Oraculo -> IO ()
persistir (Nothing) = putStrLn "Oráculo vacío" >> menu Nothing
persistir (Just p) =
    do
        putStrLn "Introduzca el nombre con el que desea guardar el archivo:"
        archivo <- getLine
        writeFile archivo $ show p
        menu (Just p)
                
cargar :: IO ()
cargar = do
        putStrLn "\nIntroduzca el nombre del archivo que desea cargar:"
        archivo <- getLine
        oraculo <- readFile archivo
        menu (Just (read oraculo))
                
consultarPreguntaCrucial :: Maybe Oraculo -> IO ()
consultarPreguntaCrucial (Nothing) = putStrLn "Oráculo vacío." >> menu Nothing
consultarPreguntaCrucial (Just p) = 
    do
        putStrLn "\nIntroduzca la primera predicción:"
        cadena1 <- getLine
        putStrLn "\nIntroduzca la segunda predicción:"
        cadena2 <- getLine
        case (obtenerCadena p (map Char.toLower cadena1) , obtenerCadena p (map Char.toLower cadena2)) of
             (Nothing, _) -> putStrLn "Consulta Invalida." >> menu (Just p)
             (_, Nothing) -> putStrLn "Consulta Invalida." >> menu (Just p)
             (Just c1, Just c2) -> do
                 putStr "\nLa pregunta crucial es: "
                 putStr "¿"
                 putStr (last(elementoComun (map fst c1) (map fst c2)))
                 putStr "?\n"
                 menu (Just p)
             
consultarEstadisticas :: Maybe Oraculo -> IO ()
consultarEstadisticas p =
    do
        case p of
             Nothing -> putStrLn "\nConsulta Inválida. Oráculo Vacío." >> menu Nothing
             Just p -> do
                 putStr "Las estadísticas del oráculo son: "
                 putStrLn (show (obtenerEstadisticas p)) >> menu (Just p)
             
elementoComun :: Eq a => [a] -> [a] -> [a]
elementoComun [] _ = []
elementoComun _ [] = []
elementoComun (x:xs) (y:ys)
    | x == y = [x] ++ elementoComun xs ys
    | otherwise = []