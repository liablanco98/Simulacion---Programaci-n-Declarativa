module Robot
  ( Robot (..),
    asignaFalse,
    generarRobot,
    generarRobot_
  )
where

import Posiciones
import Utils

data Robot = Robot
  { 
    conNinno::[Bool],
    posRobot :: [(Int, Int)]
  }
  deriving (Show)


--Asigna false a todas las posiciones de los roboces
asignaFalse:: Int->[Bool]
asignaFalse 0=[]
asignaFalse n=False: asignaFalse (n-1)

-- Actualizar el Listado de Robots a partir de la lista de posiciones
generarRobot :: [(Int, Int)] -> Robot
generarRobot nuevos = Robot {conNinno=asignaFalse (length nuevos) , posRobot = nuevos}

--Para generar los las posiciones no pueden estar ocupadas por ningun otro elemento
-- (n,m) inp pos cant
-- (maximo de filas,maximo de columnas) posiciones ocupadas total de casillas con suciedad
generarRobot_ :: (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
generarRobot_ (n, m) inp = posicionPosibleGenL (n, m) inp []

