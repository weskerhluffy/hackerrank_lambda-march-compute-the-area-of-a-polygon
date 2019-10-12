module Main where

import Control.Monad

-- XXX: https://www.wikihow.com/Calculate-the-Area-of-a-Polygon

areaPoligono::[(Int,Int)]->Float
-- XXX: https://stackoverflow.com/questions/3275193/whats-the-right-way-to-divide-two-int-values-to-obtain-a-float
areaPoligono puntos = (fromIntegral ((suma1 x puntos True 0) - (suma1 y puntos False 0)))/2.0
  where x = (fst.last) puntos
        y = (snd.last) puntos

suma1::Int->[(Int,Int)]->Bool->Int->Int
suma1 _ [] _ acum = acum
suma1 prev puntos@(p1:restoPuntos) inicialX acum = suma1 x restoPuntos inicialX ((y*prev)+acum)
  where (xFn,yFn) = if inicialX then (fst,snd) else (snd,fst)
        x = xFn p1
        y = yFn p1

main :: IO ()
main = do
  n <- getLine
  puntosStr <- replicateM (read n) getLine
--  let puntos = map ((map (\x -> read x::Int)).words) puntosStr
  let puntos = map ((\[x,y] -> (read x::Int,read y::Int)).words) puntosStr
  print puntos
  print (abs.areaPoligono $ puntos)
