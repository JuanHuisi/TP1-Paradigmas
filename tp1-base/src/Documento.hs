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

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc fVacio fTexto fLinea doc =
  case doc of
    Vacio -> fVacio
    Texto s d -> fTexto s (rec d)
    Linea i d -> fLinea i (rec d)
  where rec = foldDoc fVacio fTexto fLinea
                                

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

-- Como precondición se asume que d1 y d2 respetan el invariante
(<+>) :: Doc -> Doc -> Doc
(<+>) d1 d2 = foldDoc d2
  -- Si esta funcion se ejecuta es porque foldDoc atraveso un doc de tipo texto.
  (\s rec -> case rec of 
    Vacio -> Texto s rec 
    -- Este es el único caso que podria dar como resultado un Doc de tipo texto con
    -- una subestructura Doc tambien de tipo texto, que sucede solo cuando la
    -- anteúltima subestructura de d1 es de tipo texto y la primera tambien lo es.
    -- En dicho caso, en vez de concatenar d2 al final de d1, lo cual violaría el 
    -- invariante, se concatenan las strings en un mismo Doc de tipo texto, y se 
    -- concatena la cola de d2, preservandose el invariante. (⟺ d1 y d2 lo respetaban)
    Texto s2 doc -> Texto (s ++ s2) doc 
    Linea n doc -> Texto s rec 
  )
  -- Si esta funcion se ejecuta es porque foldDoc atravesó un doc de tipo linea,
  -- y es el único caso que podria dar como resultado un Doc de tipo linea con
  -- n < 0, pero como no se modifica el valor de n, d1 y d2 preservan el
  -- invariante, ⟹ aquí tambien se preserva.
  (\n rec -> if rec == Vacio then Linea n d2 else Linea n rec)
  d1

-- Mas allá de la justificación, nuestros pensamientos sobre el ejercicio 3 se
-- encuentran en el archivo readme.txt

-- Como precondición se asume que i > 0 y que el Doc pasado cumple el invariante,
-- luego, primera función no modifica el Doc de tipo texto, así que se preserva
-- el invariante, y la segunda función solo le suma un numero positivo al Doc de
-- tipo linea, por lo que también se preserva el invariante.
indentar :: Int -> Doc -> Doc 
indentar i = foldDoc (Vacio) (\s res -> Texto s res) (\n res -> Linea (n + i) res)

mostrar :: Doc -> String
mostrar = foldDoc "" (\s rec -> s ++ rec) (\n rec -> "\n" ++ take n (repeat ' ') ++ rec) 

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
