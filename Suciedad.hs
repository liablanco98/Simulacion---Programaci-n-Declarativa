module Suciedad
  ( generarSuciedad_
    
  )
where

import Posiciones
import Utils

--Para generar la suciedad las posiciones no pueden estar ocupadas por ningun otro elemento
-- (n,m) inp pos cant
-- (maximo de filas,maximo de columnas) posiciones ocupadas total de casillas con suciedad
generarSuciedad_ :: (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
generarSuciedad_ (n, m) inp = posicionPosibleGenL (n, m) inp []

--agregarSuciedad :: Suciedad -> [(Int, Int)] -> Suciedad
--agregarSuciedad suc@Suciedad {cantSuciedad = cant, posSuciedad = pos} nuevas =
--  let nc = cant + length nuevas
--      npos = pos ++ nuevas
--   in Suciedad {cantSuciedad = nc, posSuciedad = npos}
--
--eliminarSuciedad :: Suciedad -> [(Int, Int)] -> Suciedad
--eliminarSuciedad suc@Suciedad {cantSuciedad = cant, posSuciedad = pos} viejas =
--  let nc = cant - length viejas
--      npos = borrarComunes pos viejas
--   in Suciedad {cantSuciedad = nc, posSuciedad = npos}