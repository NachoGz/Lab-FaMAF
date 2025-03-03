module Main (main) where
import qualified System.Exit as Exit-- import Control.Monad (when)
import Pred(Pred, cambiar, anyDib, allDib, orP, andP)
import Test.HUnit
-- import Test.HUnit (Test (TestLabel, TestList, TestCase), Test (TestCase), assertEqual, assertBool, runTestTT, errors, failures, counts, Counts (Counts))
import Dibujo (Dibujo (Figura, Rotar))


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

dibujoCirculo :: Dibujo Formas
dibujoCirculo = Figura Circulo

-- Define some tests
testCambiar :: Test
testCambiar = TestCase (assertEqual "for: cambiar isTriangle" (cambiar isTriangle (Rotar . Figura) dibujoTriangulo) (Rotar (Figura Triangulo)))

testAnyDib :: Test
testAnyDib = TestCase (assertBool "for: anyDib isTriangle" (anyDib isTriangle dibujoTriangulo))

testAllDib :: Test
testAllDib = TestCase (assertBool "for: allDib isTriangle" (allDib isTriangle dibujoTriangulo))

testAndP :: Test
testAndP = TestCase (assertBool "for: andP isTriangle isCircle" (not (andP isTriangle isCircle Triangulo)))

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