module Ninno
  ( 
    genN_,
    moverN,
    moverN__,
    moverNNO__,
    moverNO__,
    genS,
    movidosHaciaObst,
    posFinN,
    actNI,
    movidos
  )
where

import Obstaculo
import Posiciones
import System.IO.Unsafe
import System.Random
import Utils

---- Generar los Ninnos a partir de una lista de posiciones no ensuciando (corral y cargados) y
---- de posiciones de entradas a ocupar por ninnos se crea un NINNO nuevo
--genN :: [(Int, Int)] -> [(Int, Int)] ->[(Int, Int)]
--genN noEnsuciando nuevas = borrarComunes nuevas noEnsuciando

--  Es un submetoddo de genN
--  Genera posciiones random ( ya que no importa si tienen corral)
genN_ :: (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
genN_ (_, _) out 0 = out
genN_ (n, m) out cant
  | pos `notElem` out = genN_ (n, m) (out ++ [pos]) (cant -1)
  | otherwise = genN_ (n, m) out cant
  where
    pos = posicionPosibleGen (n, m)

--Los ninnos que puden moverse son los que no estan en el corral
-- Teniendo en cuenta las posiciones ocupadas por el ambiente,
-- calculo por cada ninno si va a moverse y en caso de que lo decida hacia que posicion lo hace
moverN :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
moverN posEnsuciando (n, m) noObs obs =
  let posiblePos = moverN__ 1 (n, m) posEnsuciando (posEnsuciando ++ noObs) obs
   in moverN_ [] posiblePos

-- Es un submetodo del anterior donde a partir de todas las posibles posiciones a Moverse
-- Elige una posicion que no haya sido previamente seleccionada por otro ninno
moverN_ :: [(Int, Int)] -> [((Int, Int), (Bool, [(Int, Int)]))] -> [((Int, Int), (Int, Int))]
moverN_ _ [] = []
moverN_ temp (x : xs) =
  let posInicial = fst x
      segunParte = snd x
      puedeMoverse = fst segunParte
      posMoverse = snd segunParte
   in if not puedeMoverse
        then (posInicial, posInicial) : moverN_ temp xs
        else
          let noComunes = borrarComunes posMoverse temp
              l = length noComunes
           in if l == 0
                then (posInicial, posInicial) : moverN_ temp xs
                else
                  let rd = unsafePerformIO (getStdRandom (randomR (0, l -1)))
                      pos = elemEnPos rd noComunes
                   in (posInicial, pos) : moverN_ (temp ++ [pos]) xs

--Es un metodo que calcula para cada ninno si desea moverse y en caso q asi lo quiera, hacia que posiciones le
-- es posible hacerlo
moverN__ :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Bool, [(Int, Int)]))]
moverN__ _ (_, _) [] _ _ = []
moverN__ rdmInp (n, m) (x : xs) noObs obs =
  let rd = unsafePerformIO (getStdRandom (randomR (0, rdmInp)))
   in if rd == 0
        then (x, (False, [])) : moverN__ rdmInp (n, m) xs noObs obs --Si el ninno decide no moverse
        else
          let conObsT = moverNNO__ (n, m) x noObs
              s = snd conObsT
              ps = fst s
              ss = snd s
           in if not ps
                then (x, (False, [])) : moverN__ rdmInp (n, m) xs noObs obs --Si todas ady ocupadas
                else
                  let conObst = comunes ss obs --Busco cuales adyacente son ocupadas por Obstaculos
                      ln = length conObst
                   in if ln == 0
                        then conObsT : moverN__ rdmInp (n, m) xs noObs obs --Si no ady obst entonces se puede mover a todas las pos conObsT
                        else
                          let nposPos = moverNO__ (n, m) x ss obs noObs
                              nBool = not (null nposPos)
                              nTupla = (nBool, nposPos)
                              nA = (x, nTupla)
                           in nA : moverN__ rdmInp (n, m) xs noObs obs

-- Retorna las pos q un ninno puede moverse sin tener en cuenta los obstaculos
-- (n,m) (lista de ninnos) (posiciones Ocupadas sin obstaculos) -> (posAnterior,(si puede moverse, que posiciones))
moverNNO__ :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> ((Int, Int), (Bool, [(Int, Int)]))
moverNNO__ (n, m) (x, y) inp =
  let p = validPosGenAll (x, y) (n, m) 4
      posNoOcupadas = borrarComunes p inp
      bol = not (null posNoOcupadas)
   in ((x, y), (bol, posNoOcupadas))

-- Retorna las pos q un ninno puede moverse teniendo en cuenta los obstaculos
-- Se le pasan: los limites (n,m)- la posicion desde la que se desea mover-las posiciones posibles a moverse a ver cuales de ellas son validas
-- la lista de obstaculos-la lista de posiciones ocupadas- la salida que se va construyendo
moverNO__ :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
moverNO__ (n, m) _ [] _ _ = []
moverNO__ (n, m) pos (x : xs) obstaculos inp =
  if x `notElem` obstaculos
    then x : moverNO__ (n, m) pos xs obstaculos inp
    else
      let dir = relativaDir pos x
          b = posMovO (n, m) dir x obstaculos inp False
       in if b
            then x : moverNO__ (n, m) pos xs obstaculos inp
            else moverNO__ (n, m) pos xs obstaculos inp

-- El cambio de ambiente va produciendo nuevas suciedades
-- (n,m) (ninnosQmueven) (posiciones no vacias) (posiciones que se iran ensuciando)
genS :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
genS (_, _) [] _ = []
genS (n, m) (x : xs) inp =
  let cua = genCuadrilla x (n, m)
      ninnosEnMismaCua = comunes cua xs
      len = length ninnosEnMismaCua
      inp2 = borrarComunes cua inp
   in if len >= 2
        then
          let f = borrarComunes xs ninnosEnMismaCua
              c = unsafePerformIO (getStdRandom (randomR (0, 6)))
              nl = genS_ inp2 c
           in nl ++ genS (n, m) f inp
        else
          if len == 1
            then
              let f = borrarComunes xs ninnosEnMismaCua
                  c = unsafePerformIO (getStdRandom (randomR (0, 3)))
                  nl = genS_ inp2 c
               in nl ++ genS (n, m) f inp
            else
              let c = unsafePerformIO (getStdRandom (randomR (0, 1)))
                  nl = genS_ inp2 c
               in nl ++ genS (n, m) xs inp

genS_ :: [(Int, Int)] -> Int -> [(Int, Int)]
genS_ [] _ = []
genS_ _ 0 = []
genS_ inp cant =
  let rdm = unsafePerformIO (getStdRandom (randomR (0, length inp -1)))
      pos = elemEnPos rdm inp
   in pos : genS_ (borrarElemEnPos rdm inp) (cant -1)

--- Movidos me da cuales se mueven
movidos:: [((Int, Int), (Int, Int))] ->[(Int,Int )]
movidos []=[]
movidos (x:xs)= if uncurry (/=) x then fst x:movidos xs else movidos xs

-- Obtengo cuales de los que cambian, ocupan  una posicion de obstaculo
movidosHaciaObst :: [((Int, Int), (Int, Int))] ->[(Int,Int )]-> [(Direccion, (Int, Int))]
movidosHaciaObst _ []=[]
movidosHaciaObst [] _= []
movidosHaciaObst (x : xs) obstaculos =
  let fs = fst x
      sn = snd x
      eq = fs==sn
      obstPos=sn `elem` obstaculos
   in if not eq && obstPos --Solo me interesn los que se mueven y hacia una posicion que estaba ocupada por un obst
        then
          let dir = relativaDir fs sn
              tup = (dir, sn)
           in tup : movidosHaciaObst xs obstaculos
        else movidosHaciaObst xs obstaculos

--Cambiar las posiciones finales de las que presentan obstaculos que no pueden mover que son aquellas
-- que ovupan posiciones donde los obstaculos iniciales y finales son los mismos
actNI :: [((Int, Int), (Int, Int))] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
actNI [] _ = []
actNI (x : xs) incorrectas
  | snd x `elem` incorrectas = (fst x,fst x) : actNI xs incorrectas
  | otherwise = x : actNI xs incorrectas

-- Determina cuales posiciones quedarian ocupadas por ninnos en este cambio de ambiente
-- Y por lo tanto que no pueden ser ocupadas por suciedad
-- 3- Crear pos finales de los ninnos luego del cambio de turno
posFinN :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
posFinN = map snd