-- 1)
    -- a)
esCero :: Int -> Bool
esCero x = x == 0

    -- b)
esPositivo :: Int -> Bool
esPositivo x = x > 0

    -- c)
esVocal :: Char -> Bool
esVocal x = (x == 'a') || (x == 'e') || (x == 'i') || (x == 'o') || (x == 'u')

-- 2)
    -- a)
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = (x == True) && paratodo xs

    -- b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

    -- c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

    -- d)
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

    -- e)
promedio :: [Int] -> Int
promedio lista
  | length lista > 0 = div (sumatoria lista) (length lista)
  | otherwise = error "La lista debe ser no vacía!"

-- 3)
pertenece :: Int -> [Int] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs

-- 4)
    -- a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = (t x) && (paratodo' xs t)

    -- b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t

    -- c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

    -- d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria'(x:xs) t = t x * productoria' xs t

-- 5)

-- paratodo2 :: [Bool] -> Bool
-- paratodo2 xs = paratodo' xs

-- 6)
    -- a)
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even

    -- b)
esMultiplo :: (Int,Int) -> Bool
esMultiplo (a,b) = (mod a b) == 0


-- hayMultiplo :: Int -> [Int] -> Bool
-- hayMultiplo a xs = existe' xs esMultiplo 

    -- c)
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] (^2)

    -- d)
factorial2 :: Int -> Int
factorial2 n = productoria [1..n] 

    -- e)
-- multiplicaPares :: [Int] -> Int
-- multiplicaPares xs = productoria' xs even

-- 8)
    -- a)
duplicaR :: [Int] -> [Int]
duplicaR [] = []
duplicaR (x:xs) = (x*2) : duplicaR xs

    -- b)
duplicaM :: [Int] -> [Int]
duplicaM xs = map (*2) xs

-- 9)
    -- a)
paresR :: [Int] -> [Int]
paresR [] = []
paresR (x:xs)
    | even x = x : paresR xs
    | otherwise = paresR xs

    -- b)
paresF :: [Int] -> [Int]
paresF xs = filter even xs

-- 10)
    -- a)
    -- con recursión
igual :: Eq a => a -> a -> Bool
igual x y = x == y

primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA a [] = []
primIgualesA a (x:xs)
    | (x == a) = a : primIgualesA a xs
    | otherwise = []
    -- con takeWhile
--primIgualesA :: Eq a => a -> [a] -> [a]
 --primIgualesA x xs = takeWhile ( igual x) xs

-- 11)
    -- a)
    -- con recursión
--primIguales :: Eq a => [a] -> [a]
--primIguales [] = []
--primIguales (x:(y:xs))
 --   | x == y = x : primIguales (y:xs)
 --   | otherwise = (x:[])

    -- con takeWhile
primIguales :: Eq a => [a] -> [a]
primIguales (x:xs) = takeWhile ( == x ) (x:xs)


-- 12)
cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t = (t x) `op` (cuantGen op z xs t)

paratodoGen :: [a] -> (a -> Bool) -> Bool
paratodoGen xs t = cuantGen (&&) True xs t

existeGen :: [a] -> (a -> Bool) -> Bool
existeGen xs t = cuantGen (||) False xs t

sumatoriaGen :: [Int] -> Int
sumatoriaGen xs = cuantGen (+) 0 xs id

productoriaGen :: [Int] -> Int
productoriaGen xs = cuantGen (*) 1 xs id

{-
-- 13)
    a)
f :: (a, b) -> ...
f (x, y) = ...
Esta bien tipado y el patron cubre todos los casos de definicion

    b)
f :: [(a, b)] -> ...
(*)f (a, b) = ...
Esta mal tipado porque la funcion recibe como argumento una lista de tuplas y en este caso se esta usando como parametro una tupla sola.

    c)
f :: [(a,b)] ->  ...
f (x:xs) = ...
Esta bien tipado pero el patron no cubre todos los casos porque no logra acceder a cada elemento de la tupla.

    d)
f :: [(a,b)] -> ...
f ((x, y) : ((a, b) : xs)) = ...
Esta bien tipado y el patron cubre todos los casos de definicion.

    e)
f :: [(Int, a)] -> ...
f [(0, a)] = ...
Esta bien tipado y el patron no cubre todos los casos de definicion porque al usar el 0 como parametro queda restringido el 0 como el unico Int que se puede usar.

    f)
f :: [(Int, a)] -> ..
f ((x, 1) : xs) = ...
Esta bien tipado y el patron no cubre todos los casos de definicion porque al usar el 1 como parametro queda restringido el 1 como el unico "a" que se puede usar.

    g)
f :: (Int -> Int) -> Int -> ...
f a b = ...
Esta bien tipado y el patron cubre todos los casos de definición.

    h)
f :: (Int -> Int) -> Int -> ...
f a 3 = ...
Esta bien tipado y el patron no cubre todos los casos de definicion porque al usar el 3 como parametro queda restringido el 3 como el unico Int que se puede usar.

    i)
f :: (Int -> Int) -> Int -> ...
f 0 1 2 = ...
Esta mal tipado porque el primer argumento requiere una funcion Int -> Int y pasa un 0 como parametro.
-}



{-
-- 14)

    a)
f :: (a, b) -> b
f (a,b) = b
alternativa
f (a, b) = id b

    b)
f :: (a, b) -> c
f (a, b) = c
alternativa
f (a, b)
    | a > b = (b, a)
    | oterwise = (a, b)

    c)  
f :: (a -> b) -> a -> b
f g a = b
alternativa
f g a = g a 

--para g :: (a -> b)

    d)
f :: (a -> b) -> [a] -> [b]
f g [a] = [b] 
--para g :: (a -> b)
--alternativa
f :: (a -> b) -> [a] -> [b]
map g a = b

    e)
f :: (a -> b) -> (b -> c) -> a -> c
f g t a = c 
--alternativa

para g :: (a -> b) y t :: (b -> c)


-}
