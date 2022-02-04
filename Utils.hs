module Utils
    (
        comunes,
        borrarComunes,
        elemEnPos,
        borrarElemEnPos,
        borrarElem,
        myDivid,
        mySum
    )

where

-- Da los elementos comunes entre dos listas
comunes :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
comunes _[]=[]
comunes [] _ = []
comunes (x:xs) lis |x `elem` lis= comunes xs lis ++ [x]
                   |otherwise = comunes xs lis

--Elimina los elementos comunes de la primera lista con respecto a la segunda
borrarComunes:: [(Int,Int)]->[(Int,Int)]->[(Int,Int)]
borrarComunes [] _=[]
borrarComunes inp []=inp
borrarComunes (x:xs) lis|x `elem` lis=borrarComunes xs lis
                        |otherwise =borrarComunes xs lis ++ [x]

-- Da el elemento que se encuentra en un determinado indice de la lista
elemEnPos:: Int->[(Int,Int)]->(Int,Int)
elemEnPos = elemEnPos_ 0

elemEnPos_:: Int->Int->[(Int,Int)]->(Int,Int)
elemEnPos_ _ _ []=(-1,-1)
elemEnPos_ curr ind (x:xs)|curr==ind =x
                          |otherwise=elemEnPos_ (curr+1) ind xs

-- Borra el elemento que se encuentre en una determinada posicion de la lista
borrarElemEnPos::Int->[(Int,Int)]->[(Int,Int)]
borrarElemEnPos pos list=borrarElemEnPos_ 0 pos list []

borrarElemEnPos_::Int->Int->[(Int,Int)]->[(Int,Int)]->[(Int,Int)]
borrarElemEnPos_ _ _ [] out=out
borrarElemEnPos_ curr ind (x:xs) out |curr==ind= borrarElemEnPos_ (curr+1) ind xs out
                                     |otherwise =borrarElemEnPos_ (curr+1) ind xs (out++[x])


borrarElem::(Int,Int)->[(Int,Int)]->[(Int,Int)]
borrarElem _ []=[]
borrarElem elem (x:xs)|elem==x=xs
                      |otherwise =x:borrarElem elem xs

myDivid:: Int->Int->Float
myDivid a b=fromIntegral a/fromIntegral b

mySum:: [Float ]->Float
mySum = sum