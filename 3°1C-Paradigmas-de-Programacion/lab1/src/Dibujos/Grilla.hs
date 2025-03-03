module Dibujos.Grilla (
    grillaConf
) where

import Dibujo (Dibujo, juntar, apilar,figura)
import FloatingPic(Conf(..),Output)
import Graphics.Gloss (translate, text, scale)

-- El tipo de los enteros agrupados en pares
type Basica = (Int, Int)

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

-- Función que crea una grilla numerada de (n+1)x(n+1)
crearGrillaNumerada :: Int->Dibujo Basica
crearGrillaNumerada n = grilla [[figura (i, j) | j <- [0..n]] | i <- [0..n]]

interpBas :: Output Basica
interpBas (i, j) (x, y) _ _ = translate x y $ scale 0.2 0.2 $ text $ "(" ++ show i ++ ", " ++ show j ++ ")"

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla",
    pic = crearGrillaNumerada 7,
    bas = interpBas
}