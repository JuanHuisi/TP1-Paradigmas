Primero nosotros consideramos que la idea del ejercicio era lo que pedía el enunciado: 
"No modifica la primera linea del documento, sino las que siguen"

Si tengo documento que arranca en linea, por ejemplo,
Luego -> indentar 2 (Linea 1 (texto "a")) debería devolver Linea 1 (Texto "a" Vacio)

Por lo tanto, para nuestra primera solucion consideramos para resolver un approach usado en
problemas recursivos, que se aplica en backtracking, divide and conquer, o cualquier tipo de problema
con algoritmos recursivos, que consiste en usar a indentar como wrapper de una función auxiliar que mantiene
una estructura diferente. En este caso, la recursión mantiene un valor Booleano adicional que arranca en
True pero tras hacer 1 llamado recursivo pasa a False y no varía más, así considerando el caso borde de
la primera linea.

indentar :: Int -> Doc -> Doc 
indentar i doc = indentarAux i doc True

indentarAux :: Int -> Doc -> Bool -> Doc
indentarAux i doc = foldDoc (const Vacio) 
                        (\s resd -> \primeraLinea -> Texto s (resd False))
                        (\n resd -> \primeraLinea -> if primeraLinea then (Linea n (resd False))
                                                                 else (Linea (n + i) (resd False))) doc

Más adelante se envió un mail donde se mandaron algunos casos de tests que se esperaba que el TP
resuelva, en donde, en particular, indentar con una linea al inicio efectuaba la suma lo cual el
enunciado pide que no suceda, o al menos eso entendiamos.

indentar 2 (Linea 4 Vacio) devuelve Linea 6 Vacio, cuando nosotros entendiamos que debia
devolver Linea 4 Vacio

indentar 2 (Texto "a" (Linea 4 (Texto "b" (Linea 1 (Texto "c" Vacio))))) devuelve
Texto "a" (Linea 6 (Texto "b" (Linea 3 (Texto "c" Vacio)))), cuando nosotros entendiamos que debia
devolver Texto "a" (Linea 4 (Texto "b" (Linea 3 (Texto "c" Vacio))))

Al final, decidimos dejar la version incluida en Documento.hs, la cual si indenta la primer linea. Va
en contra del enunciado, pero pasa los casos de test que nos dieron, los del mail, y hace que las
funciones que la utilizan pasen sus propios casos de test.

=======================================================================================================

Los siguientes tests pueden ejecutarse directamente en GHCi utilizando la función indentar adjuntada en este archivo.
indentar 3 (linea <+> texto "a" <+> linea <+> texto "b") == linea <+> texto "a" <+> Linea 3 (texto "b")

indentar 4 (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c") == texto "a" <+> Linea 4 (texto "b" <+> Linea 4 (texto "c"))

indentar 2 (Linea 1 (Linea 3 (texto "a"))) == Linea 1 (Linea 5 (texto "a")) 
