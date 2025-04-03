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
intercalar sep = foldr(\s rec -> if(rec == vacio) then s else s <+> sep <+> rec) vacio {-primer caso: rec = vacio-}

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
aplanar = error "PENDIENTE: Ejercicio 8"

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
