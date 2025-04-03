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
(<+>) Vacio d = d
(<+>) d Vacio = d
(<+>) (Texto s1 Vacio) (Texto s2 d2)  = if null s2 then Texto s1 d2 else Texto (s1 ++ s2) d2
(<+>) (Texto s1 d1) d2 = (Texto s1 ((<+>) d1 d2))
(<+>) (Linea n Vacio) d2 = (Linea n d2)
(<+>) (Linea n d1) d2 = (Linea n ((<+>) d1 d2))


{- (<+>) :: Doc -> Doc -> Doc
(<+>) d1 d2 = foldDoc (\d1 d2 -> if d1 == Vacio then d2 else d1) 
                () () d1 d2
 -}
indentar :: Int -> Doc -> Doc
indentar i = error "PENDIENTE: Ejercicio 3"

mostrar :: Doc -> String
mostrar = error "PENDIENTE: Ejercicio 4"

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
