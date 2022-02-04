module Ambiente
  ( Ambiente (..),
  )
where

import Corral
import Ninno
import Obstaculo
import Posiciones
import Robot
import Suciedad
import Utils
import Text.Read.Lex (Number)

data Ambiente = Ambiente
  { lim :: (Int, Int),
    robot :: Robot,
    ninno :: [(Int,Int)],
    corral :: [(Int,Int)],
    corralSinNinno::[(Int,Int)],
    obstaculo :: [(Int,Int)],
    suciedad :: [(Int,Int)]
  }
  deriving (Show)

--Solo considero ambientes donde 2*n_0+r_0+s_0<n*m
ambienteValido :: (Int, Int) -> Int -> Int -> Int ->Int -> Bool
ambienteValido (n, m) n_0 r_0 s_0 o_0 = n * m > 2 * n_0 + r_0 + s_0+ o_0

-- Iniciar Ambiente
-- Un Ambiente se crea con:
-- (n,m) : cantidad de filas y columnas
-- n_0: cantidad de ninnos=cantidad de corrales
-- r-0: cantidad de robots
-- s_0: cantidad de casillas sucias
-- o_0: cantidad de casillas con obstaculos
crearA :: (Int, Int) -> Int -> Int -> Int -> Int -> Ambiente
crearA (n, m) n_0 r_0 s_0 o_0 =
  let corral = generarCorral_ (n, m) n_0
      posNinno = genN_ (n, m) [] n_0
      ninnosNoCorral = borrarComunes posNinno corral
      corralSinNinnos=borrarComunes corral posNinno
      noOcupadas = ninnosNoCorral ++ corral
      posRobot = generarRobot_ (n, m) noOcupadas r_0
      suciedad = generarSuciedad_ (n, m) (noOcupadas ++ posRobot) s_0
      obstaculo = genO_ (n, m) (noOcupadas ++ posRobot ++ suciedad) o_0
   in Ambiente
        { lim = (n, m),
          corral = corral,
          ninno = ninnosNoCorral,
          corralSinNinno=corralSinNinnos,
          robot = generarRobot posRobot,
          suciedad = suciedad,
          obstaculo = obstaculo
        }

-- La simulacion del cambio de turno
-- 1- Seleccionar por cada ninno la pos por la que se va a mover (si o se mueve es la misma poscicion)
-- 2- Estas posiciones no tienen que ser del todo validas ya que pueden existir conflictos en cuanto
--    a mover los obstaculos y tal por ello hay q ver como quedarian las posciciones por obstaculos
-- 3- Crear Ninnos que se mueven=aquellos donde posfinal=!pos inicial
-- 4- Crear pos finales de los ninnos luego del cambio de turno
-- 5- Crear las posiciones ocupadas =pos ninnos finales, pos robots, pos sucias, pos corrales, pos obstaculos
-- 6- Genera una cantidad de posiciones a entuciar a partir del criterio de las cuadrillas
cambioN :: Ambiente -> Ambiente
cambioN a@Ambiente {lim = lim, ninno = ninno, corral = corral, suciedad = suciedad,corralSinNinno=corralSinNinno, robot = robot@Robot {posRobot = posRobot}, obstaculo=obstaculo} =
  let n1 = moverN ninno lim (corral ++ suciedad ++ posRobot) obstaculo --Por ninno se genera pos final
      n1C = movidosHaciaObst n1 obstaculo --De los anteriores los que cambian de posicion para una ocupada por un obstaculo
      n1PF = posFinN n1 --Todas las posibles psiciones Finales de los ninnos (si no se mueven es la misma en que estan)
      o1PF = posFinO n1C n1PF obstaculo --Como quedarian los obstaculos de ser movidos en n1C
      n1I = comunes n1PF o1PF --Si existen posiciones que se iban a mover hacia obst y estos no se pueden mover
      n2PF = actNI n1 n1I --quitar los posibles mov de las invalidas
      n2C = movidos n2PF --de las validas cuales se mueven
      noVacias = map snd n2PF ++ o1PF ++ corral ++ posRobot ++ suciedad --Todas las posiciones donde no se puede generar Basura
      s1 = genS lim n2C noVacias
   in Ambiente {lim = lim, ninno = map snd n2PF, corral = corral,corralSinNinno=corralSinNinno,
    suciedad = suciedad ++ s1, robot = robot, obstaculo =o1PF}

--Siempre que no este cargando un ninno el robot:
-- Limpia si se encuentra en una posicion sucia
-- Busca si algun adyacente hacia el que se puede mover tiene un ninno o suciedad y en caso de que si se mueve hacia esa direccion
-- Sino busca el ninno fuera del corral mas cercano en caso de que exista, sino busca la suciedad mas cercana
movSinNinno::(Int,Int)->[(Int,Int)]->Ambiente->(([(Int,Int)],Ambiente),(Bool,(Int,Int)))
movSinNinno pos tomadas ambiente@Ambiente{lim=lim,robot=robot@Robot{posRobot=posRobot},ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
  --Si estoy sin ninno y en una posicion sucia este turno me toca limpiar
  |pos `elem` suciedad= let nSuciedad=borrarElem pos suciedad
                            a=Ambiente{lim=lim,robot=robot,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=nSuciedad,corralSinNinno=corralSinNinno}
                        in ((tomadas,a),(False,pos))
  --Sino me tengo que mover hacia algun adyacente por lo que si no es posible permanezco en el lugar
  |null adyPos=((tomadas,ambiente),(False,pos))
  --Si tengo adyacente con ninno me muevo hacia esa posicion y al momento cargo al ninno
  |not (null adyNinno)= let posN=head adyNinno
                            tomadasT=tomadas++[posN]
                            nNinno=borrarElem posN ninno
                            a=Ambiente{lim=lim,robot=robot,ninno =nNinno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
                        in ((tomadasT,a),(True,posN))
  --Si algun ady con suciedad moverme hacia esa pos para luego limpiarla
  |not (null adySuciedad)=let posN=head adySuciedad
                              tomadasT=tomadas++[posN]
                          in ((tomadasT,ambiente),(False,posN))
  --Si no tengo ningun adyacente con ninno ni con suciedad 
  --SI existen ninnos fuera de corral me muevo en su direccion
  |not (null ninno) = let posN=posicionMCercana ninno adyPos adyPos (10000,(-1,-1))-- Nunca sera -1,-1 poruq al menos existe un adyacente y un ninno
                          tomadasT=tomadas++[posN]
                      in ((tomadasT,ambiente),(False,posN))
  --Sino al menos existe una posicion sucia porque para entrar este metodo verifico que exista al menos un ninno fuera del corral
  -- o una suciedad
  |otherwise=let posN=posicionMCercana suciedad adyPos adyPos (10000,(-1,-1))-- Nunca sera -1,-1 poruq al menos existe un adyacente y un sucio
                 tomadasT=tomadas++[posN]
             in ((tomadasT,ambiente),(False,posN))

  where adyAll=validPosGenAll pos lim 3
        corralCNinno=borrarComunes corral corralSinNinno
        noPosibles=tomadas++posRobot++obstaculo++corralCNinno
        --Los adyacentes posibles son los que no hayan sido previamente seleccionados
        -- Ni ocupados por obstaculos, ni otros roboces ni ninnos ya ocupando posiciones en corral
        adyPos=borrarComunes adyAll noPosibles
        adyNinno=comunes adyPos ninno
        adySuciedad=comunes adyPos suciedad

--Esta estrategia contempla que si esta cargando un ninno:
-- Esta en un corral vacio entonces deja al ninno
-- De lo contrario se mueva en busca de un corral vacio
movConNinno1::Bool->(Int,Int)->[(Int,Int)]->Ambiente->(Bool,(([(Int,Int)],Ambiente),(Bool,(Int,Int))))
movConNinno1 primerMov pos tomadas ambiente@Ambiente{lim=lim,robot=robot@Robot{posRobot=posRobot},ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
    --Si estoy en un corral dejarlo
    |pos `elem` corralSinNinno= let cSN=borrarElem pos corralSinNinno
                                    a=Ambiente{lim=lim,robot=robot,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=cSN}
                                in (primerMov,((tomadas,a),(False,pos)))
    --si no puedo moverme permanecer
    |null adyPos=(primerMov,((tomadas,ambiente),(True,pos)))
    --Si algun ady con corral vacio moverme hacia esa pos
    |not (null adyCorralVacio)= let posN=head adyCorralVacio
                                    tomadasT=tomadas++[posN]
                                in (primerMov,((tomadasT,ambiente),(True,posN)))
    --Sino al menos existe una posicion vacia de corral ya que esta es igual a la cantidad de ninnos
    |otherwise=let posN=posicionMCercana corralSinNinno adyPos adyPos (10000,(-1,-1))--El primer paso
               in if primerMov then
                    let segundoPaso=movConNinno1 False posN (tomadas++[pos]) ambiente -- El segundo paso
                        posNN=snd (snd (snd segundoPaso))--La nueva poscicion es la ocupada y el bool siempre que se haya podido mover dos veces es true
                    in (True ,((tomadas++[posNN],ambiente),(True,posNN)))
                  else (True ,((tomadas++[posN],ambiente),(True,posN)))

    where adyAll=validPosGenAll pos lim 3
          corralCNinno=borrarComunes corral corralSinNinno
          noPosibles=tomadas++posRobot++obstaculo++corralCNinno++ninno
          --Los adyacentes posibles son los que no hayan sido previamente seleccionados
          -- Ni ocupados por obstaculos, ni otros roboces ni ninnos ya sea ocupando posiciones en corral o no
          adyPos=borrarComunes adyAll noPosibles
          adyCorralVacio=comunes adyPos corralSinNinno

--Esta estrategia contempla que si esta cargando un ninno:
-- Esta en un corral vacio entonces deja al ninno
-- Esta en una posicion sucia ese turno lo toma para limpiar
-- Alguna adyacente con corral vacio se mueve en esa direccion
-- Alguna adyacente con suciedad se mueve en esa direccion
-- Se mueve en direccion del corral
movConNinno2::Bool->(Int,Int)->[(Int,Int)]->Ambiente->(Bool,(([(Int,Int)],Ambiente),(Bool,(Int,Int))))
movConNinno2 primerMov pos tomadas ambiente@Ambiente{lim=lim,robot=robot@Robot{posRobot=posRobot},ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
    --Si estoy en un corral dejarlo
    |pos `elem` corralSinNinno= let cSN=borrarElem pos corralSinNinno
                                    a=Ambiente{lim=lim,robot=robot,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=cSN}
                                in (primerMov,((tomadas,a),(False,pos)))
    --Si esta en una posicion sucia limpiarla
    |pos `elem` suciedad= let nSuciedad=borrarElem pos suciedad
                              a=Ambiente{lim=lim,robot=robot,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=nSuciedad,corralSinNinno=corralSinNinno}
                          in (primerMov,((tomadas,a),(True,pos)))
    --si no puedo moverme permanecer
    |null adyPos=(primerMov,((tomadas,ambiente),(True,pos)))
    --Si algun ady con corral vacio moverme hacia esa pos
    |not (null adyCorralVacio)= let posN=head adyCorralVacio
                                    tomadasT=tomadas++[posN]
                                in (primerMov,((tomadasT,ambiente),(True,posN)))
    --Si algun ady con suciedad moverme hacia esa pos y soltar al ninno
    |not (null adySuciedad)=let posN=head adySuciedad
                                tomadasT=tomadas++[posN]
                            in (primerMov,((tomadasT,ambiente),(True,posN)))
   --Sino al menos existe una posicion vacia de corral ya que esta es igual a la cantidad de ninnos
    |otherwise=let posN=posicionMCercana corralSinNinno adyPos adyPos (10000,(-1,-1))--El primer paso
               in if primerMov then
                    let segundoPaso=movConNinno2 False posN (tomadas++[pos]) ambiente -- El segundo paso
                        posNN=snd (snd (snd segundoPaso))--La nueva poscicion es la ocupada y el bool siempre que se haya podido mover dos veces es true
                    in (True ,((tomadas++[posNN],ambiente),(True,posNN)))
                  else (True ,((tomadas++[posN],ambiente),(True,posN)))
    where adyAll=validPosGenAll pos lim 3
          corralCNinno=borrarComunes corral corralSinNinno
          noPosibles=tomadas++posRobot++obstaculo++corralCNinno++ninno
          --Los adyacentes posibles son los que no hayan sido previamente seleccionados
          -- Ni ocupados por obstaculos, ni otros roboces ni ninnos ya sea ocupando posiciones en corral o no
          adyPos=borrarComunes adyAll noPosibles
          adyCorralVacio=comunes adyPos corralSinNinno
          adySuciedad=comunes adyPos suciedad

--Se van generando por cada robot los movimientos con la estrategia 1
movRoboces1:: [Bool]->[(Int,Int)]->[(Int,Int)]->[Bool]->[(Int,Int)]->Ambiente->Ambiente
movRoboces1 _ [] _ boolV posV a@Ambiente{lim=lim,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}=
  Ambiente{lim=lim,robot=Robot{posRobot=posV,conNinno=boolV},ninno=ninno,corral=corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
movRoboces1 [] _ _ boolV posV a@Ambiente{lim=lim,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}=
  Ambiente{lim=lim,robot=Robot{posRobot=posV,conNinno=boolV},ninno=ninno,corral=corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
movRoboces1 (b:bs) (pos:posS) tomadas boolSalida posSalida a=
  if b then let tt=movConNinno1 True pos tomadas a
                t=snd tt
                nTomadas=fst (fst t)
                nAmbiente=snd (fst t)
                nb= fst (snd t)
                np=snd (snd t)
                nBSalida=boolSalida++[nb]
                nPSalida=posSalida++[np]
            in movRoboces1 bs posS nTomadas nBSalida nPSalida nAmbiente
  else let t=movSinNinno pos tomadas a
           nTomadas=fst (fst t)
           nAmbiente=snd (fst t)
           nb= fst (snd t)
           np=snd (snd t)
           nBSalida=boolSalida++[nb]
           nPSalida=posSalida++[np]
       in movRoboces1 bs posS nTomadas nBSalida nPSalida nAmbiente

--Se van generando por cada robot los movimientos con la estrategia 2          
movRoboces2:: [Bool]->[(Int,Int)]->[(Int,Int)]->[Bool]->[(Int,Int)]->Ambiente->Ambiente
movRoboces2 [] _ _ boolV posV a@Ambiente{lim=lim,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}=
  Ambiente{lim=lim,robot=Robot{posRobot=posV,conNinno =boolV},ninno=ninno,corral=corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
movRoboces2 _ [] _ boolV posV a@Ambiente{lim=lim,ninno =ninno,corral =corral,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}=
  Ambiente{lim=lim,robot=Robot{posRobot=posV,conNinno =boolV},corral=corral,ninno=ninno,obstaculo=obstaculo,suciedad=suciedad,corralSinNinno=corralSinNinno}
movRoboces2 (b:bs) (pos:posS) tomadas boolSalida posSalida a=
  if b then let tt=movConNinno2 True pos tomadas a
                t=snd tt
                nTomadas=fst (fst t)
                nAmbiente=snd (fst t)
                nb= fst (snd t)
                np=snd (snd t)
                nBSalida=boolSalida++[nb]
                nPSalida=posSalida++[np]
            in movRoboces1 bs posS nTomadas nBSalida nPSalida nAmbiente
  else let t=movSinNinno pos tomadas a
           nTomadas=fst (fst t)
           nAmbiente=snd (fst t)
           nb= fst (snd t)
           np=snd (snd t)
           nBSalida=boolSalida++[nb]
           nPSalida=posSalida++[np]
       in movRoboces1 bs posS nTomadas nBSalida nPSalida nAmbiente

--Genera los cambios de ambiente por parte de los roboces teniendo en cuenta la primera estrategia
cambioRE1::Ambiente->Ambiente
cambioRE1 ambiente@Ambiente{robot=robot@Robot{posRobot=posRobot,conNinno=conNinno},ninno=ninno,suciedad=suciedad}=
  if null ninno && null suciedad then ambiente else movRoboces1 conNinno posRobot [] [] [] ambiente

--Genera los cambios de ambiente por parte de los roboces teniendo en cuenta la segunda estrategia
cambioRE2::Ambiente->Ambiente
cambioRE2 ambiente@Ambiente{robot=robot@Robot{posRobot=posRobot,conNinno=conNinno},ninno=ninno,suciedad=suciedad}=
  if null ninno && null suciedad then ambiente else movRoboces2 conNinno posRobot [] [] [] ambiente

--Genera la simulacion:
--tipo es un int que es 1 cuando se quiere aplicar la estrategia 1 y analogo para la 2
--tiempo de la simulacion
--tiempo del cambio de ambiente
-- (n,m) : cantidad de filas y columnas
-- n_0: cantidad de ninnos=cantidad de corrales
-- r-0: cantidad de robots
-- s_0: cantidad de casillas sucias
-- o_0: cantidad de casillas con obstaculos
simulacion:: Int-> Int->Int ->(Int,Int)->Int->Int->Int->Int->String 
simulacion tipo t_g t_cambio (n,m) n_0 r_0 s_0 o_0
  | tipo>2||tipo<1 =
  "No es valido el tipo introducido"
  | not (ambienteValido (n, m) n_0 r_0 s_0 o_0) =
  "No son validos los valores introducidos para crear el ambiente "
  | otherwise =
  let ambienteInicial=crearA (n, m) n_0 r_0 s_0 o_0
      casillasLimpias=(n*m)-s_0
      porcLimpieza=(casillasLimpias*100) `myDivid` (n*m)
      lis=simulacion_ tipo (t_g-1) (t_cambio-1) t_cambio ambienteInicial [porcLimpieza]
      sumL=mySum lis
      promedio=sumL / fromIntegral t_g
  in "Cantidad de Iteraciones::"++show t_g++"\n"++
     "Tiempo de Cambio de Ambiente::"++show t_cambio++"\n"++
     "Porcientos de limpieza obtenidos::"++show lis++"\n"++
     "Promedio de Porcientos::"++show promedio

simulacion_:: Int->Int->Int->Int->Ambiente->[Float]->[Float]
simulacion_ _ 0 _ _ _ out=out--Se acabo la simulacion
simulacion_ tipo t_restante 0 t_cambio ambiente out=
  let amb=cambioN ambiente
  in simulacion_ tipo t_restante t_cambio t_cambio amb out
simulacion_ tipo t_restante t_cambioR t_cambio ambiente@Ambiente{lim=lim} out
  |tipo==1 = let amb=cambioRE1 ambiente
                 suc=suciedad amb
                 s_0=length suc
                 n_m=uncurry (*) lim
                 casillasLimpias=n_m-s_0
                 porcLimpieza=(casillasLimpias*100) `myDivid` n_m
             in simulacion_ tipo (t_restante-1) (t_cambioR-1) t_cambio amb (out++[porcLimpieza])
  |otherwise = let amb=cambioRE2 ambiente
                   suc=suciedad amb
                   s_0=length suc
                   n_m=uncurry (*) lim
                   casillasLimpias=n_m-s_0
                   porcLimpieza=(casillasLimpias*100) `myDivid` n_m
               in simulacion_ tipo (t_restante-1) (t_cambioR-1) t_cambio amb (out++[porcLimpieza])

  --Si se quisiera ver como se va comportando el ambiente emplear estos codigos

simulacionConAmbiente:: Int-> Int->Int ->(Int,Int)->Int->Int->Int->Int->String 
simulacionConAmbiente tipo t_g t_cambio (n,m) n_0 r_0 s_0 o_0
  | tipo>2||tipo<1 =
  "No es valido el tipo introducido"
  | not (ambienteValido (n, m) n_0 r_0 s_0 o_0) =
  "No son validos los valores introducidos para crear el ambiente "
  | otherwise =
    let ambienteInicial=crearA (n, m) n_0 r_0 s_0 o_0
        casillasLimpias=(n*m)-s_0
        porcLimpieza=(casillasLimpias*100) `myDivid` (n*m)
        lis=simulacionConAmbiente_ tipo (t_g-1) (t_cambio-1) t_cambio ambienteInicial [(porcLimpieza,ambienteInicial)]
        porcientos=map fst lis
        ambs=map snd lis
        sumL=mySum porcientos
        promedio=sumL / fromIntegral t_g
    in "Cantidad de Iteraciones::"++show t_g++"\n"++
       "Tiempo de Cambio de Ambiente::"++show t_cambio++"\n"++
       "Porcientos de limpieza obtenidos::"++show porcientos++"\n"++
       "Ambientes Generados::"++show ambs++"\n"++
       "Promedio de Porcientos::"++show promedio

simulacionConAmbiente_:: Int->Int->Int->Int->Ambiente->[(Float,Ambiente)]->[(Float,Ambiente)]
simulacionConAmbiente_ _ 0 _ _ _ out=out--Se acabo la simulacion
simulacionConAmbiente_ tipo t_restante 0 t_cambio ambiente out=
  let amb=cambioN ambiente
  in simulacionConAmbiente_ tipo t_restante t_cambio t_cambio amb out
simulacionConAmbiente_ tipo t_restante t_cambioR t_cambio ambiente@Ambiente{lim=lim} out
  |tipo==1 = let amb=cambioRE1 ambiente
                 suc=suciedad amb
                 s_0=length suc
                 n_m=uncurry (*) lim
                 casillasLimpias=n_m-s_0
                 porcLimpieza=(casillasLimpias*100) `myDivid` n_m
             in simulacionConAmbiente_ tipo (t_restante-1) (t_cambioR-1) t_cambio amb (out++[(porcLimpieza,amb)])
  |otherwise = let amb=cambioRE2 ambiente
                   suc=suciedad amb
                   s_0=length suc
                   n_m=uncurry (*) lim
                   casillasLimpias=n_m-s_0
                   porcLimpieza=(casillasLimpias*100) `myDivid` n_m
               in simulacionConAmbiente_ tipo (t_restante-1) (t_cambioR-1) t_cambio amb (out++[(porcLimpieza,amb)])
