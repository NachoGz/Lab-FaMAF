module Dibujo ( 
    Dibujo (Figura, Rotar, Espejar, Rot45, Apilar, Juntar, Encimar),
    figura, encimar, apilar, juntar, rot45, rotar, espejar, mapDib, foldDib, 
    (///), (^^^), (.-.), r180, r270, encimar4, cuarteto, ciclar,
) where


-- nuestro lenguaje 
data Dibujo a =  Figura a | Rotar (Dibujo a) | Espejar (Dibujo a) 
    | Rot45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a)
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a) 
    deriving (Eq, Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp n f a
    | n < 0 = error "No se puede componer negativamente"
    | n == 0 = a
    | otherwise = comp (n-1) f (f a)


-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

-- Superpone un dibujo con otro.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = encimar

-- Pone el primer dibujo arriba del segundo, ambos ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = apilar 1 1

-- Pone un dibujo al lado del otro, ambos ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = juntar 1 1

-- rotaciones
-- innecesario
-- r90 :: Dibujo a -> Dibujo a
-- r90 = comp 2 rot45

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 rotar

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 rotar

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 figura1 = comp 4 (^^^ rotar figura1) figura1

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto figura1 figura2 figura3 figura4 = (figura1 /// figura2) .-. (figura3 /// figura4)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar figura1 = cuarteto figura1 (rotar figura1) (r180 figura1) (r270 figura1)

-- map para nuestro lenguaje
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f (Figura a) = f a
mapDib f (Rotar a) = Rotar (mapDib f a)
mapDib f (Espejar a) = Espejar (mapDib f a)
mapDib f (Rot45 a) = Rot45 (mapDib f a)
mapDib f (Apilar x y a b) = Apilar x y (mapDib f a) (mapDib f b)
mapDib f (Juntar x y a b) = Juntar x y (mapDib f a) (mapDib f b)
mapDib f (Encimar a b) = Encimar (mapDib f a) (mapDib f b)

-- comentada por redundante
-- -- Cambiar todas las básicas de acuerdo a la función.
-- change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
-- change f (Figura a) = f a
-- change f (Rotar a) = Rotar (change f a)
-- change f (Espejar a) = Espejar (change f a)
-- change f (Rot45 a) = Rot45 (change f a)
-- change f (Apilar x y a b) = Apilar x y (change f a) (change f b)
-- change f (Juntar x y a b) = Juntar x y (change f a) (change f b)
-- change f (Encimar a b) = Encimar (change f a) (change f b)

-- formas usadas para testear las funciones en ghci
-- data Formas = Triangulo | Circulo | Cuadrado deriving (Eq, Show)


-- Principio de recursión para Dibujos.
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
    (Float -> Float -> b -> b -> b) ->
    (Float -> Float -> b -> b -> b) ->
    (b -> b -> b) ->
    Dibujo a -> b
foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dibujo = case dibujo of
    Figura dib1 -> fFigura dib1
    Rotar dib1 -> fRotar (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1)
    Espejar dib1 -> fEspejar (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1)
    Rot45 dib1 -> fRot45 (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1)
    Apilar x y dib1 dib2 -> fApilar x y (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1) (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib2)
    Juntar x y dib1 dib2 -> fJuntar x y (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1) (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib2)
    Encimar dib1 dib2 -> fEncimar (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib1) (foldDib fFigura fRotar fEspejar fRot45 fApilar fJuntar fEncimar dib2)