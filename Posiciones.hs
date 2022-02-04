module Posiciones
  ( Direccion (..),
    posicionPosible,
    posicionPosible2,
    posicionPosibleGen,
    posicionPosibleGenL,
    moverseDireccion,
    moverseDireccion2,
    validPosGenAll,
    validPosGenAll2,
    genCuadrilla,
    relativaDir,
    posicionMCercana
  )
where

import System.IO.Unsafe
import System.Random
import Utils

data Direccion = Norte | Sur | Este | Oeste deriving (Show)

data Direccion2 = NEste | SEste | NOeste | SOeste

-- A partir de una coordenada (x,y), una direccion, la cantidad de filas y de columnas
-- retorna si es posible moverse en esa direccion
posicionPosible :: (Int, Int) -> Direccion -> (Int, Int) -> Bool
posicionPosible (x, y) dir (n, m) = case dir of
  Norte -> x > 0
  Sur -> x < n -1
  Este -> y < m -1
  Oeste -> y > 0

-- Para las demas posiciones asociadas a la cuadrilla
posicionPosible2 :: (Int, Int) -> Direccion2 -> (Int, Int) -> Bool
posicionPosible2 (x, y) dir (n, m) = case dir of
  NEste -> x > 0 && y < m -1
  SEste -> x < n -1 && y < m -1
  NOeste -> x > 0 && y > 0
  SOeste -> x < n -1 && y > 0

-- Genera posicion random dentro de un intervalo de filas y columnas (n,m)
posicionPosibleGen :: (Int, Int) -> (Int, Int)
posicionPosibleGen (n, m) = (unsafePerformIO (getStdRandom (randomR (0, n -1))), unsafePerformIO (getStdRandom (randomR (0, m -1))))

--  Genera cant de posiciones random distintas que no esten en inp
--(n,m) inp posGeneradas(inicialmente pasa []) cant
posicionPosibleGenL :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
posicionPosibleGenL (_, _) _ gen 0 = gen
posicionPosibleGenL (n, m) inp gen cant
  | l `notElem` gen && l `notElem` inp = posicionPosibleGenL (n, m) inp (gen ++ [l]) (cant -1)
  | otherwise = posicionPosibleGenL (n, m) inp gen cant
  where
    l = posicionPosibleGen (n, m)

-- A partir de una coordenada (x,y) y una direccion retorna las coordenadas resultantes de
-- Moverse en esa direccion
moverseDireccion :: (Int, Int) -> Direccion -> (Int, Int)
moverseDireccion (x, y) dir = case dir of
  Norte -> (x -1, y)
  Sur -> (x + 1, y)
  Este -> (x, y + 1)
  Oeste -> (x, y -1)

-- A partir de una coordenada (x,y) y una direccion retorna las coordenadas resultantes de
-- Moverse en esa direccion2
moverseDireccion2 :: (Int, Int) -> Direccion2 -> (Int, Int)
moverseDireccion2 (x, y) dir = case dir of
  NEste -> (x -1, y + 1)
  SEste -> (x + 1, y + 1)
  NOeste -> (x -1, y - 1)
  SOeste -> (x + 1, y -1)

--- Generar todas las direcciones asociadas a una posicion (x,y)
validPosGenAll :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
validPosGenAll (x, y) (n, m) 0 = [moverseDireccion (x, y) Norte | posicionPosible (x, y) Norte (n, m)]
validPosGenAll (x, y) (n, m) 1 = if posicionPosible (x, y) Sur (n, m) then validPosGenAll (x, y) (n, m) 0 ++ [moverseDireccion (x, y) Sur] else validPosGenAll (x, y) (n, m) 0
validPosGenAll (x, y) (n, m) 2 = if posicionPosible (x, y) Este (n, m) then validPosGenAll (x, y) (n, m) 1 ++ [moverseDireccion (x, y) Este] else validPosGenAll (x, y) (n, m) 1
validPosGenAll (x, y) (n, m) 3 = if posicionPosible (x, y) Oeste (n, m) then validPosGenAll (x, y) (n, m) 2 ++ [moverseDireccion (x, y) Oeste] else validPosGenAll (x, y) (n, m) 2
validPosGenAll (x, y) (n, m) cant = validPosGenAll (x, y) (n, m) 3

--- Generar todas las direcciones2 asociadas a una posicion (x,y)
validPosGenAll2 :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
validPosGenAll2 (x, y) (n, m) 0 = [moverseDireccion2 (x, y) NEste | posicionPosible2 (x, y) NEste (n, m)]
validPosGenAll2 (x, y) (n, m) 1 = if posicionPosible2 (x, y) SEste (n, m) then validPosGenAll2 (x, y) (n, m) 0 ++ [moverseDireccion2 (x, y) SEste] else validPosGenAll2 (x, y) (n, m) 0
validPosGenAll2 (x, y) (n, m) 2 = if posicionPosible2 (x, y) NOeste (n, m) then validPosGenAll2 (x, y) (n, m) 1 ++ [moverseDireccion2 (x, y) NOeste] else validPosGenAll2 (x, y) (n, m) 1
validPosGenAll2 (x, y) (n, m) 3 = if posicionPosible2 (x, y) SOeste (n, m) then validPosGenAll2 (x, y) (n, m) 2 ++ [moverseDireccion2 (x, y) SOeste] else validPosGenAll2 (x, y) (n, m) 2
validPosGenAll2 (x, y) (n, m) cant = validPosGenAll2 (x, y) (n, m) 3

-- A partir de una posicion genera todas las posiciones de su cuadrilla que son validas
genCuadrilla :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
genCuadrilla (x, y) (n, m) = validPosGenAll (x, y) (n, m) 3 ++ validPosGenAll2 (x, y) (n, m) 3

--A partir de una posicion inicial y otra final 
-- Q direccion tiene la final con respecto a la inicial
--0 ->Norte. 1->Sur 2->Este 3->Oeste
relativaDir :: (Int, Int) -> (Int, Int) -> Direccion
relativaDir (x, y) (nx, ny)
  | dx == -1 && dy == 0 = Norte
  | dx == 1 && dy == 0 = Sur
  | dx == 0 && dy == 1 = Este
  | otherwise = Oeste
  where
    dx = nx - x
    dy = ny - y

--Buco por cada adyacente y que posiciones posibles a mover cual es la mas cercana
posicionMCercana:: [(Int,Int)]->[(Int,Int)]->[(Int,Int)]->(Int,(Int,Int))->(Int,Int)
posicionMCercana [] _ _ (val,pos)=pos
posicionMCercana (x:xs) ady [] (val,pos)=posicionMCercana xs ady ady (val,pos) 
posicionMCercana (x:xs) ady (y:ys) (val,pos)=
  let dist=abs (fst x - fst y) + abs (snd x - snd y)
  in if dist<val then posicionMCercana (x:xs) ady ys (dist,y) else posicionMCercana (x:xs) ady ys (val,pos)
