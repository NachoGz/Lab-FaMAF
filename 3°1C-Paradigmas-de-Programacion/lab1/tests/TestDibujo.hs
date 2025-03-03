import Test.HUnit
import Dibujo
import qualified System.Exit as Exit

data Formas = Triangulo | Circulo | Cuadrado | Rectangulo deriving (Eq, Show)


-- Test for figura function
testFigura :: Test
testFigura = TestCase $ assertEqual "figura test" (figura Triangulo) (Figura Triangulo)

-- Test for encimar function
testEncimar :: Test
testEncimar = TestCase $ assertEqual "encimar test" (encimar (Figura Triangulo) (Figura Circulo)) (Encimar
                                                                                                    (Figura Triangulo)
                                                                                                    (Figura Circulo))

-- Test for apilar function
testApilar :: Test
testApilar = TestCase $ assertEqual "apilar test" (apilar 1 2 (Figura Triangulo) (Figura Circulo)) (Apilar 1 2
                                                                                                        (Figura Triangulo)
                                                                                                        (Figura Circulo))

-- Test for juntar function
testJuntar :: Test
testJuntar = TestCase $ assertEqual "juntar test" (juntar 1 2 (Figura Triangulo) (Figura Circulo)) (Juntar 1 2
                                                                                                        (Figura Triangulo)
                                                                                                        (Figura Circulo))

-- Test for rot45 function
testRot45 :: Test
testRot45 = TestCase $ assertEqual "rot45 test" (rot45 (Figura Triangulo)) (Rot45 (Figura Triangulo))

-- Test for rotar function
testRotar :: Test
testRotar = TestCase $ assertEqual "rotar test" (rotar (Figura Triangulo)) (Rotar (Figura Triangulo))

-- Test for espejar function
testEspejar :: Test
testEspejar = TestCase $ assertEqual "espejar test" (espejar (Figura Triangulo)) (Espejar (Figura Triangulo))

-- Test for (^^^) function
testEncimarOp :: Test
testEncimarOp = TestCase $ assertEqual "encimar operator test" (Figura Triangulo ^^^ Figura Circulo) (Encimar
                                                                                                        (Figura Triangulo)
                                                                                                        (Figura Circulo))

-- Test for (.-.) function
testApilarOp :: Test
testApilarOp = TestCase $ assertEqual "apilar operator test" (Figura Triangulo .-. Figura Circulo) (Apilar 1 1
                                                                                                        (Figura Triangulo)
                                                                                                        (Figura Circulo))

-- Test for (///) function
testJuntarOp :: Test
testJuntarOp = TestCase $ assertEqual "juntar operator test" (Figura Triangulo /// Figura Circulo) (Juntar 1 1
                                                                                                        (Figura Triangulo)
                                                                                                        (Figura Circulo))

-- Test for r180 function
testR180 :: Test
testR180 = TestCase $ assertEqual "r180 test" (r180 (Figura Triangulo)) (Rotar (Rotar (Figura Triangulo)))

-- Test for r270 function
testR270 :: Test
testR270 = TestCase $ assertEqual "r270 test" (r270 (Figura Triangulo)) (Rotar (Rotar (Rotar (Figura Triangulo))))

-- Test for encimar4 function
testEncimar4 :: Test
testEncimar4 = TestCase $ assertEqual "encimar4 test" (encimar4 (Figura Triangulo)) (Encimar (Encimar (Encimar (Encimar (Figura Triangulo)
                                                                                    (Rotar (Figura Triangulo))) (Rotar (Figura Triangulo)))
                                                                                    (Rotar (Figura Triangulo))) (Rotar (Figura Triangulo)))

-- Test for cuarteto function
testCuarteto :: Test
testCuarteto = TestCase $ assertEqual "cuarteto test" (cuarteto (Figura Triangulo) (Figura Circulo) (Figura Cuadrado) (Figura Rectangulo))
                                                                ((Figura Triangulo /// Figura Circulo) .-. (Figura Cuadrado /// Figura Rectangulo))

-- Test for ciclar function
testCiclar :: Test
testCiclar = TestCase $ assertEqual "ciclar test" (ciclar (Figura Triangulo)) (Apilar 1.0 1.0 
                                                                                (Juntar 1.0 1.0 (Figura Triangulo) (Rotar (Figura Triangulo))) 
                                                                                (Juntar 1.0 1.0 
                                                                                    (Rotar (Rotar (Figura Triangulo))) 
                                                                                    (Rotar (Rotar (Rotar (Figura Triangulo))))
                                                                                ))

-- Test for mapDib function
testMapDib :: Test
testMapDib = TestCase $ assertEqual "mapDib test" (mapDib figura (Figura Triangulo)) (Figura Triangulo)



-- Test for change function
-- testChange :: Test
-- testChange = TestCase $ assertEqual "change test" (change trianguloToCirculo (Apilar 1 1 (Figura Triangulo) (Rotar (Figura Cuadrado)))) (Apilar 1 1 (Figura Circulo) (Rotar (Figura Cuadrado)))

-- Test for foldDib function
testFoldDib :: Test
testFoldDib = TestCase $ assertEqual "foldDib test" (foldDib figura rotar espejar rot45 apilar juntar encimar (Figura Triangulo)) 
                                                                                                                (Figura Triangulo)


-- Test suite
tests :: Test
tests = TestList [testFigura, testEncimar, testApilar, testJuntar, testRot45, testRotar, testEspejar, testEncimarOp,
    testApilarOp, testJuntarOp, testR180, testR270, testEncimar4, testCuarteto, testCiclar,
    testMapDib, testFoldDib]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess