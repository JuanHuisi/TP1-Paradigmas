module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

-- obs : no agrego test del 1 porque el fold se catchea en los tests del 2 al 100%
testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,                                                    -- testea vacio - vacio
      texto "a" <+> texto "b" ~?= texto "ab",                                       -- testea (texto vacio) - (texto vacio)
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),  -- testea (texto linea) - (texto vacio)
      vacio <+> texto "a" ~?= texto "a",                                            -- testea vacio - (texto vacio)
      vacio <+> linea ~?= linea,                                                    -- testea vacio (linea vacio)
      linea <+> vacio ~?= linea,                                                    -- testea (linea vacio) vacio   
      -- puse estos 2 q "hacen lo mismo" pero el fold debería catchearlo de formas distintas
      (linea <+> texto "b") <+> linea ~?= linea <+> (texto "b" <+> linea)           -- ult test de linea
    ]
    -- un par para probar en ghci (deberían ser True (?))
    -- texto "a" <+> linea == Texto "a" (Linea 0 Vacio)        -- testea (texto linea) - (linea vacio
    -- linea <+> linea == Linea 0 (Linea 0 Vacio)              -- testea (linea vacio) - (linea vacio)
    -- debería ser lo suficientemente exhaustivo para considerar todos los casos sumandole los tests del ej8 y ej9.

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,           -- test vacio
      indentar 2 (texto "a") ~?= texto "a", -- test sin lineas 
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")), -- test linea se suma bien
      indentar 0 (linea <+> texto "a") ~?= linea <+> texto "a" -- test trivial
      
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",

      -- agrego tests (los q están en el enunciado)
      mostrar (texto "abc") ~?= "abc",
      mostrar (texto "a" <+> texto "b") ~?= "ab",
      mostrar (indentar 2 (texto "abc" <+> linea <+> texto "def")) ~?= "abc\n  def",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c")) ~?= "a\n  b\n  c"
    ]


pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

-- no agrego tests del ej5 porque los catchea los tests del ej9

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c"
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}"
    ]
