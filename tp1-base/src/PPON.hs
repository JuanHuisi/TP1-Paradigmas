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
aplanar = foldDoc vacio (\s rec -> texto s <+> rec) (\n rec -> texto " " <+> rec)

pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s)
pponADoc (IntPP n) = texto (show n)
pponADoc (ObjetoPP l) = if(pponObjetoSimple (ObjetoPP l)) 
                        then texto "{ " <+> intercalar (texto ", ") rec <+> texto " }"  
                        else entreLlaves $ rec
  where rec = map(\(x, y) -> texto (show x) <+> texto ": " <+> pponADoc y) l

{- data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

  f x : rec
 -}
