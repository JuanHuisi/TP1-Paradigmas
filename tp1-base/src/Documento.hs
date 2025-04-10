module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

-- foldDoc :: ... PENDIENTE: Ejercicio 1 ...
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc fVacio fTexto fLinea doc = case doc of
                            Vacio -> fVacio
                            Texto s d -> fTexto s (rec d)
                            Linea i d -> fLinea i (rec d)
                    
            where rec = foldDoc fVacio fTexto fLinea
                                

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) d1 d2 = foldDoc d2
                      (\s rec  -> case rec of 
                                      Vacio -> Texto s rec 
                                      Texto s2 doc -> Texto (s ++ s2) doc
                                      Linea n doc -> Texto s rec 
                      ) 
                      (\n rec -> if rec == Vacio then Linea n d2 else  Linea n rec)
                      d1 
 
{-
Observación - Primero nosotros consideramos que la idea del ejercicio era lo que pedía el enunciado: 
"No modifica la primera linea del documento, sino las que siguen"

Por lo tanto, nuestra solución legacy es la que está explicada bajo el nombre de indentar' (con un bloque de texto que es la idea del algoritmo explicada).

Más adelante se envió un mail donde se mandaron algunos casos de tests que se esperaba que el TP resuelva, en donde, en particular - indentar con una linea al inicio
efectuaba la suma (lo cual el enunciado pide que no)

Por lo tanto decidimos dejar la solución con nombre de función indentar que sería la "trivial" que resuelve lo pedido en ese mail, y dejamos la solución legacy
bajo el nombre de indentar', que handlea ese caso particular que consideramos. 
-}

indentar :: Int -> Doc -> Doc 
indentar i  = foldDoc (Vacio) (\s res -> Texto s res) (\n res -> Linea (n + i) res)

{- Primero se consideró usar recursión primitiva con recr para tener guardada la estructura
original del documento. Pero al pensar un poco una resolución nos dimos cuenta que no sería necesario
tener lo original.-}

recrDoc :: b -> (String -> Doc -> b -> b) -> (Int -> Doc -> b -> b) -> Doc -> b 
recrDoc fVacio fTexto fLinea doc = case doc of 
                                  Vacio -> fVacio 
                                  Texto s d -> fTexto s d (rec d)
                                  Linea i d -> fLinea i d (rec d)
                                  where rec = recrDoc fVacio fTexto fLinea

{- 
La idea que consideramos para resolver esto es un approach usado en problemas recursivos, que se aplica 
en backtracking, divide and conquer, o cualquier tipo de problema con algoritmos recursivos.
La idea es usar a indentar como wrapper de una función auxiliar que mantiene una estructura diferente. 
En este caso, la recursión mantiene un valor Booleano adicional que arranca en True pero tras hacer 1 llamado recursivo pasa a False
y no varía más, así considerando el caso borde de la primera linea 
 
Obs (i) -> Si tengo documento que arranca en linea,
x ej: indentar 2 (Linea 1 (texto "a")), debería devolver Linea 1 (texto "a")

indentar' :: Int -> Doc -> Doc 
indentar' i doc = indentarAux i doc True

indentarAux :: Int -> Doc -> Bool -> Doc
indentarAux i doc = foldDoc (const Vacio) 
                        (\s resd -> \primeraLinea -> Texto s (resd False))
                        (\n resd -> \primeraLinea -> if primeraLinea then (Linea n (resd False))
                                                                 else (Linea (n + i) (resd False))) doc 

Nota -> Nosotros entendimos que la idea era que haga lo que está en la Obs (i), pero después en un mail mandaron que se tenía que indentar todo.
Por lo tanto, dejamos la solución que hicimos al principio que pasa los tests pedidos y handlea la primer linea con suma en lugar de ignorarla. 

Se puede ejecutar indentar' en winGHCi y correrlo con estos casos de test para ver como sería lo que pensamos nosotros: 

  para probar en ghci:
  
  indentar 3 (linea <+> texto "a" <+> linea <+> texto "b") == linea <+> texto "a" <+> Linea 3 (texto "b")
  (este no debería funcionar sin indentar')

  indentar 4 (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c") == texto "a" <+> Linea 4 (texto "b" <+> Linea 4 (texto "c")) check suma lineas
  (este debería funcionar en ambos indentar e indentar', pues arranca en texto)

  indentar' 2 (Linea 1 (Linea 3 (texto "a"))) == Linea 1 (Linea 5 (texto "a")) para check de sumas correctas
  (indentar normal devolvería Linea 3 (Linea 5 (texto "a"))

Obs (ii) -> La diferencia entre indentar e indentar' (que fue lo que pensamos que se pedía) difiere en el caso que el Documento a indentar inicie en una linea.
  
-}
mostrar :: Doc -> String
mostrar = foldDoc "" (\s rec -> s ++ rec) (\n rec -> "\n" ++ take n (repeat ' ') ++ rec) 

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
