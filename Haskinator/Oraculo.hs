--  Module Oraculo
--  Universidad Simón Bolívar
--  Laboratorio de Lenguajes de Programacion (CI-3661)
--  Proyecto I Haskell - Haskinator
--  Desarrollado por: Maria Gracia Hidalgo  03-36048
--                    Alejandro Tarazona    05-38978


module Oraculo
    (Oraculo (Prediccion, Pregunta), crearPrediccion, crearPregunta, obtenerCadena, obtenerEstadisticas, prediccion, pregunta, positivo, negativo)
where

-- Tipos de Datos

data Oraculo = Prediccion { prediccion :: String }  
             | Pregunta { pregunta :: String, positivo, negativo :: Oraculo }
             deriving (Show, Read)

-- Funciones de Construcción

crearPrediccion :: String -> Oraculo
crearPrediccion = Prediccion

crearPregunta :: String -> Oraculo -> Oraculo -> Oraculo
crearPregunta = Pregunta

-- Funciones de Consulta

pasoCaminos :: String -> Bool -> ([(String, Bool)],String) -> ([(String, Bool)],String)
pasoCaminos q b (p, s) = ([(q,b)] ++ p ,s)

obtenerCaminos :: Oraculo -> [([(String, Bool)],String)]
obtenerCaminos (Prediccion s) = [([],s)]
obtenerCaminos (Pregunta q p n)= map (pasoCaminos q True) (obtenerCaminos p) ++ map (pasoCaminos q False) (obtenerCaminos n)

obtenerCadena :: Oraculo -> String -> Maybe [(String, Bool)]
obtenerCadena p s
    | (elem s (map snd $ obtenerCaminos p) == False) = Nothing
    | otherwise = Just (fst(head (filter (\(_,x) -> x == s) $ obtenerCaminos p)))

obtenerEstadisticas :: Oraculo -> (Int, Int, Double)
obtenerEstadisticas p = (minimum lista, maximum lista, (fromIntegral(sum lista) / fromIntegral(length lista)))
    where lista = map (length.fst) $ obtenerCaminos p