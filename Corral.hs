module Corral
  ( 
    generarCorral_,
    generarAdy_
  )
where

import Posiciones
import Utils

--Para generar los ninnos y el corral voy a crearlo de forma cuadrada alrededor de una posicion ramdom inicial
generarCorral_ :: (Int, Int) -> Int -> [(Int, Int)]
generarCorral_ (n, m) cant = let pos = posicionPosibleGen (n, m) in generarAdy_ (n, m) [pos] [pos] (cant -1)

--(n,m) valores maximos de fila y columna
--[(x,y)] una posicion inicial
-- lis: lista de las posiciones en el corral
-- faltan: numero de posiciones que faltan
generarAdy_ :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
generarAdy_ (_, _) inp [] _ = inp --Esto no debe pasar porque no pueden haber mas corrales que casillas
generarAdy_ (_, _) inp _ 0 = inp --Esto no debe pasar porque caeria en el otherwise
generarAdy_ (n, m) inp (f : r) cant
  | l == 0 = generarAdy_ (n, m) inp r cant
  | l < cant = generarAdy_ (n, m) (inp ++ li) (r ++ li) (cant - l)
  | otherwise = inp ++ take cant li
  where
    li = borrarComunes (validPosGenAll f (n, m) 4) inp
    l = length li
