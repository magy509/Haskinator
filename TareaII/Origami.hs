module Origami where

import Data.List

data Origami a = Papel
               | Pico { pico :: a, resto :: Origami a }
               | Valle { valle :: a, resto :: Origami a }
               | Compuesto { compuesto1 :: Origami a, compuesto2 :: Origami a }

plegarOrigami :: (b)
              -> (a -> b -> b)
              -> (a -> b -> b)
              -> (b -> b -> b)
              -> Origami a
              -> b
plegarOrigami transPapel transPico transValle transCompuesto = plegar
    where
        plegar Papel = transPapel
        plegar (Pico x y) = transPico x (plegar y)
        plegar (Valle x y) = transValle x (plegar y)
        plegar (Compuesto x y) = transCompuesto (plegar x) (plegar y)

        
sumarOrigami :: (Num a) => Origami a -> a
sumarOrigami = plegarOrigami transPapel transPico transValle transCompuesto
    where
        transPapel = 0
        transPico = (+)
        transValle = (+)
        transCompuesto = (+)
        
aplanarOrigami :: Origami a -> [a]
aplanarOrigami = plegarOrigami transPapel transPico transValle transCompuesto
    where
        transPapel = []
        transPico = (:)
        transValle = (:)
        transCompuesto = (++)
        
analizarOrigami :: (Ord a) => Origami a -> Maybe(a, a, Bool)
analizarOrigami = plegarOrigami transPapel transPico transValle transCompuesto
    where
        transPapel = Nothing
        transPico = (\elem resto ->
            case resto of
                 Nothing -> Just(elem, elem, True)
                 Just (minimo, maximo, orden) -> Just(min elem minimo, max elem maximo, orden && elem <= minimo))
        transValle = (\elem resto ->
            case resto of
                 Nothing -> Just(elem, elem, True)
                 Just (minimo, maximo, orden) -> Just(min elem minimo, max elem maximo, orden && elem <= minimo))
        transCompuesto = (\resto1 resto2 ->
            case (resto1, resto2) of
                 (Nothing, Nothing) -> Nothing
                 (Nothing, (Just (minimo, maximo, orden))) -> Just (minimo, maximo, orden)
                 (Just (minimo, maximo, orden), Nothing) -> Just (minimo, maximo, orden)
                 (Just (minimo1, maximo1, orden1), Just (minimo2, maximo2, orden2)) -> Just (min minimo1 minimo2, max maximo1 maximo2, orden1 && orden2 && maximo1 <= maximo2))