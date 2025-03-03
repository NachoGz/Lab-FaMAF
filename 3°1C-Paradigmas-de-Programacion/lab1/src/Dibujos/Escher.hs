module Dibujos.Escher (
    escherConf
) where

import Dibujo(Dibujo, figura, encimar, apilar, juntar, rot45, espejar, cuarteto, rotar, r180, r270)
import FloatingPic(Conf(..),Output, zero, half)
import Graphics.Gloss (line, blank, polygon, pictures)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Supongamos que eligen.
type Escher = Bool

-- Sinonimo de tipos
figEscher :: Dibujo Escher
figEscher = figura True

figEscherF :: Dibujo Escher
figEscherF = figura False

-- El dibujo u.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar (encimar p2 (rotar p2)) (encimar (r180 p2) (r270 p2))
    where
        p2 = espejar (rot45 p)

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = encimar p (encimar p2 p3)
    where
        p2 = espejar (rot45 p)
        p3 = r270 p2

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 1 p = cuarteto figEscherF figEscherF figEscherF (dibujoU p)
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU p)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 1 p = cuarteto figEscherF figEscherF (rotar (dibujoT p)) (dibujoT p)
lado n p = cuarteto (lado (n-1) p) (lado (n-1) p) (rotar (dibujoT p)) (dibujoT p)

-- Por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x = apilar 1 2 (juntar 1 2 p (juntar 1 1 q r))
                            (apilar 1 1 (juntar 1 2 s (juntar 1 1 t u))
                                        (juntar 1 2 v (juntar 1 1 w x)))

-- El dibujo de Escher:
-- escher :: Int -> Escher -> Dibujo Escher
escher :: Int -> Dibujo Escher -> Dibujo Escher
escher n e = noneto 
                    (esquina n e) (lado n e) (r270 (esquina n e))
                    (rotar (lado n e)) (dibujoU e) (r270 (lado n e))
                    (rotar (esquina n e)) (r180 (lado n e)) (r180 (esquina n e))

interpBas :: Output Escher
interpBas False _ _ _ = blank
-- version con poligonos negros (dejé los imports ya puestos en caso de querer probar esta version por mas que en la otra no se usen)
--interpBas True x y z = pictures [polygon $ map (x V.+) [half y, half y V.+ half z, y],
--                                  line $ map (x V.+) [(0,0), z, y, (0,0)]]
interpBas True x y z = line $ map (x V.+) [zero, z, y, zero]

escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = escher 2 figEscher, --version squarelimit2 del articulo de Henderson
    bas = interpBas
}
