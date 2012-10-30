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
        transPico = 
        transValle = 
        transCompuesto = 

        
-- analizarOrigami :: (Ord a) => Origami a -> (a, a, Bool)
-- analizarOrigami = plegarOrigami transPapel transPico transValle transCompuesto
--     where
--         transPapel = []
--         transPico = []
--         transValle = []
--         transCompuesto = []