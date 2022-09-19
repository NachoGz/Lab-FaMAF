
-- 1 )
  -- a )
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq, Show)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

  -- b )
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Show, Bounded)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

-- 3)
  -- a)
minimoElemento :: Ord a => [a] -> a
minimoElemento [] = error "Solo para listas no vacias"
minimoElemento (x:[]) = x
minimoElemento (x:xs) = min (x) (minimoElemento xs) 

  -- b)
minimoElemento' :: (Bounded a, Ord a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = x `min` minimoElemento xs

  -- c)
-- minimoElemento' ([Fa, La, Sol, Re, Fa] :: [NotaBasica])

 -- 4)
  -- a)
type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Show)
data Area = Administrativa | Enseñanza | Economica | Postgrado deriving (Eq, Show)

data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Eq, Show)

  -- b)
{-
El constructor Docente es de tipo Docente :: Cargo -> Persona.
-}

  -- c)
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (x:xs) c 
  | (x == Docente c) = 1 + cuantos_doc xs c
  | otherwise = cuantos_doc xs c

  -- d)
cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' xs c = length (filter (== Docente c) xs)

-- 5)
  -- a)
data Alteracion = Bemol | Sostenido | Natural deriving (Eq)
data NotaMusical = Nota NotaBasica Alteracion deriving (Eq)

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12
 
  -- b)
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota b a)
  | a == Bemol = (sonido b) - 1
  | a == Sostenido = (sonido b) + 1
  | a == Natural = (sonido b) 


nota1 = Nota Si Bemol -- deberia ser 11
nota2 = Nota Do Sostenido -- deberia ser 2
nota3 = Nota Sol Natural --deberia ser 8
nota4 = Nota Mi Sostenido -- debeseria ser 6
nota5 = Nota Fa Natural -- deberia ser 6

mostrarNota :: NotaMusical -> NotaBasica
mostrarNota (Nota b a) = b

  -- d)
instance Ord NotaMusical
  where
       n1 <= n2 = sonidoCromatico n1 <= sonidoCromatico n2

-- 6)
  -- a)
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento xs = Just (xs!!0)

-- 7)
data Cola = VaciaC | Encolada Persona Cola deriving (Show, Eq)

  -- a)

mostrarC :: Cola -> [Persona]
mostrarC VaciaC = []
mostrarC (Encolada p c) = p : mostrarC c

atender :: Cola -> Cola
atender (Encolada p VaciaC) = VaciaC
atender (Encolada p c) = Encolada p (atender c)

encolar :: Persona -> Cola -> Cola
encolar p VaciaC = (Encolada p VaciaC)
encolar p c = (Encolada p c)

-- busca :: Cola -> Cargo -> Maybe Persona
-- busca VaciaC cargo = Nothing
-- busca (Encolada p c) cargo 
--   | (p == cargo) = Just(p)
--   | otherwise = busca c cargo

cola = Docente Titular `Encolada` (Decane `Encolada` (NoDocente Administrativa `Encolada` VaciaC))
cola2 = Docente Titular `Encolada` (Docente Adjunto `Encolada` (Docente Asociado `Encolada` VaciaC))
-- mostrarCargo :: [Persona] -> [Cargo]
-- mostrarCargo [] = []
-- mostrarCargo (x:xs)
--   | x == (Docente c)  = c : mostrarCargo xs
--   | otherwise = mostrarCargo xs

-- cargo = Titular
-- p = Docente cargo

  -- b)
{-
La cola se parece a una lista
-}

-- 8)
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)

  -- a)
type Guia_Telefonica = ListaAsoc String String
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String


  -- b)
    -- 1)
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b (l)) = 1 + la_long l

    -- 2)
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat (Nodo a b (Vacia)) l2 = Nodo a b (l2)
la_concat (Nodo a b (l1)) l2 = la_concat l1 l2

la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar (lista) a b = Nodo a b (lista)

la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo a b (lista)) = (a,b) : la_pares lista

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia clave = Nothing
la_busca (Nodo a b lista) clave
  | a == clave = Just(b)
  | otherwise = la_busca lista clave

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar clave Vacia = Vacia
la_borrar clave (Nodo a b lista)
  | clave == a = lista
  | otherwise = la_borrar clave lista
