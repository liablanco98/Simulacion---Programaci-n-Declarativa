module Obstaculo
  ( genO_,
    posMovO,
    posMovPosO,
    posFinO
  )
where

import Posiciones
import Utils

--Para generar los las posiciones no pueden estar ocupadas por ningun otro elemento
-- (n,m) inp pos cant
-- (maximo de filas,maximo de columnas) posiciones ocupadas total de casillas con Obstaculo
genO_ :: (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
genO_ (n, m) inp = posicionPosibleGenL (n, m) inp []

-- Dada la psocicion de un obstaculo,una direccion y la lista de posiciones ocupadas,y la lista de obstaculos
--saber si puede moverse
posMovO :: (Int, Int) -> Direccion -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Bool -> Bool
posMovO (_, _) _ (_, _) _ _ True = True
posMovO (n, m) dir (x, y) otherObs inp b
  | not (posicionPosible (x, y) dir (n, m)) = False
  | pos `elem` inp = False
  | pos `notElem` otherObs = True
  | otherwise = posMovO (n, m) dir pos otherObs inp False
  where
    pos = moverseDireccion (x, y) dir

-- Una lista de obstaculos y movimientos que resultarian
posMovPosO :: [(Direccion, (Int, Int))] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
posMovPosO [] _ _ _ = []
posMovPosO (x : xs) obstaculos ninnos visitados =
  let dir = fst x
      pos = snd x
      res = posMovPosO_ pos dir obstaculos ninnos visitados
   in if null res
        then (pos, pos) : posMovPosO xs obstaculos ninnos (visitados ++ [pos])
        else
          let vis2 = map snd res
              nV = vis2 ++ visitados ++[pos]
           in res ++ posMovPosO xs obstaculos ninnos nV

-- Un obstaculo puede moverse hacia una posicion que:
-- Una poscicion donde no va a quedar ningun otro obstaculo:visitadas
-- Una poscicion donde no va a quedar ningun ninno:ninno
-- Una poscicion donde haya obstaculo solo si el puede ser movido:out
posMovPosO_ :: (Int, Int) -> Direccion -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
posMovPosO_ pos dir obstaculos ninnos visitados
  | e `elem` ninnos || e `elem` visitados = []
  | e `elem` obstaculos =
    let res = posMovPosO_ e dir obstaculos ninnos visitados
     in if null res then [] else (pos, e) : res
  | otherwise = [(pos, e)]
  where
    e = moverseDireccion pos dir

--SE le pasan los ninnos que se mueven hacia un obst y en que direccion, los ninnos que hay y los obstaculos
-- SE retorna que posiciones finales ocuparian los obstaculos
posFinO:: [(Direccion, (Int, Int))]-> [(Int,Int)]->[(Int,Int)] -> [(Int, Int)]
posFinO obsAMover ninnos obstaculos =
  let ninnosNoObst=borrarComunes ninnos obstaculos
      aMover=posMovPosO obsAMover obstaculos ninnosNoObst []
      noMover=borrarComunes obstaculos (map fst aMover)
  in map snd aMover ++ noMover    