module Interp
  ( interp,
    initial,
  )
where

import Dibujo (Dibujo, foldDib)
import FloatingPic
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.

-- Suponemos que la biblioteca provee el tipo Vector y Picture.
-- type Output a = a -> Vector -> Vector -> Vector -> Picture

initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = pictures [p, q]

r45 :: FloatingPic -> FloatingPic
r45 f d w h = f (d V.+ half (w V.+ h)) (half (w V.+ h)) (half (h V.- w))

rot :: FloatingPic -> FloatingPic
rot f d w h = f (d V.+ w) h (V.negate w)

esp :: FloatingPic -> FloatingPic
esp f d w = f (d V.+ w) (V.negate w)

sup :: FloatingPic -> FloatingPic -> FloatingPic
sup f g d w h = ov (f d w h) (g d w h)

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun m n f g d w h = ov (f d w_aux h) (g (d V.+ w_aux) (r_aux V.* w) h) -- (d + (d2 + w_aux) w_aux + (r_aux*w) h1 + h2)
    where
        r_aux = n/(n+m)
        r = m/(m+n)
        w_aux = r V.* w

api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api m n f g d w h = ov (f (d V.+ h_aux) w (r V.* h)) (g d w h_aux) -- (d1 + h_aux + d2 w1 + w2 (r*h1) + h_aux p)
    where
        r_aux = n/(n+m)
        r = m/(m+n)
        h_aux = r_aux V.* h

-- data Formas = Triangulo | Circulo | Cuadrado deriving (Eq, Show)

-- interp deberia ser capaz de usar todas las funciones definidas mas arriba
interp :: Output a -> Output (Dibujo a)
interp f = foldDib f rot esp r45 api jun sup