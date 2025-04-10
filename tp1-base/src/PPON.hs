module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico (ObjetoPP a) = False 
pponAtomico _ = True 

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP xs) = foldr(\(x, y) rec -> pponAtomico y && rec) True xs

intercalar :: Doc -> [Doc] -> Doc
intercalar sep = foldr(\s rec -> if(rec == vacio) then s else s <+> sep <+> rec) vacio 

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\s rec -> texto s <+> rec) (\n rec -> texto " " <+> rec)

-- Esta funcion utiliza el esquema de recursion primitiva
pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s) -- Caso base
pponADoc (IntPP n) = texto (show n) -- Caso base
-- Caso recursivo, donde se necesita de la forma original de (ObjetoPP l) para saber
-- si este es un ppon simple o no y formatear el Doc acorde. Por esto mismo el 
-- esquema no es simplemente recursivo, y tampoco es global, puesto que no se necesita
-- de cada paso intermedio de forma explicita.
pponADoc (ObjetoPP l) =
  if(pponObjetoSimple (ObjetoPP l)) 
  then texto "{ " <+> intercalar (texto ", ") rec <+> texto " }"  
  else entreLlaves $ rec
  where rec = map(\(x, y) -> texto (show x) <+> texto ": " <+> pponADoc y) l
