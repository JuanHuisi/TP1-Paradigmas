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
 
-- esto no resuelve bien, está agregando en todas incluso la primera
indentar' :: Int -> Doc -> Doc 
indentar' i  = foldDoc (Vacio) (\s res -> Texto s res) (\n res -> Linea (n + i) res)

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
La idea es usar a la función indentar como wrapper de una función auxiliar que mantiene una estructura de datos diferente. 
En este caso, la recursión mantiene (devuelve) una tupla (Doc, Bool) tal que el valor booleano arranca en True pero tras hacer 1 llamado recursivo pasa a False
y no varía más, así considerando el caso borde de la primera iteración.

Obs -> Si tengo documento que arranca en linea,
x ej: indentar 2 (Linea 1 (texto "a")), debería devolver Linea 1 (texto "a")
-}

indentar :: Int -> Doc -> Doc 
indentar i doc = fst (indentarAux i doc True)

indentarAux :: Int -> Doc -> Bool -> (Doc, Bool)
indentarAux i doc esPl = foldDoc (Vacio, False) 
                        (\s (resd, primeraLinea) -> (Texto s resd, False))
                        (\n (resd, primeraLinea) -> if primeraLinea then (Linea n resd, False)
                                                                   else (Linea (n + i) resd, False)) doc 

mostrar :: Doc -> String
mostrar = foldDoc "" (\s rec -> s ++ rec) (\n rec -> "\n" ++ take n (repeat ' ') ++ rec) 

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
