-- module Main (main) where
import qualified System.Exit as Exit
import Pred 
import Test.HUnit
import Dibujo


data Formas = Triangulo | Circulo | Cuadrado deriving (Eq, Show)

-- Define some predicates for testing
isTriangle :: Pred Formas
isTriangle Triangulo = True
isTriangle _ = False

isCircle :: Pred Formas
isCircle Circulo = True
isCircle _ = False

-- Define some Dibujo for testing
dibujoTriangulo :: Dibujo Formas
dibujoTriangulo = Figura Triangulo

-- testeo de cambiar
testCambiar :: Test
testCambiar = TestCase (assertEqual "for: cambiar isTriangle" (cambiar isTriangle (Rotar . Figura) dibujoTriangulo) (Rotar (Figura Triangulo)))

-- testeo de anyDib
testAnyDib :: Test
testAnyDib = TestCase (assertBool "for: anyDib isTriangle" (anyDib isTriangle (Juntar 1 1(Figura Triangulo) (Figura Circulo))))

-- testeo de allDib
testAllDib :: Test
testAllDib = TestCase (assertBool "for: allDib isTriangle" (allDib isTriangle dibujoTriangulo))

-- testeo de andP 
testAndP :: Test
testAndP = TestCase (assertBool "for: andP isTriangle isCircle" (not (andP isTriangle isCircle Triangulo)))

-- testeo de orP
testOrP :: Test
testOrP = TestCase (assertBool "for: orP isTriangle isCircle" (orP isTriangle isCircle Triangulo))

-- Define the test list
tests :: Test
tests = TestList [TestLabel "testCambiar" testCambiar, TestLabel "testAnyDib" testAnyDib, TestLabel "testAllDib" testAllDib,
  TestLabel "testAndP" testAndP, TestLabel "testOrP" testOrP]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess