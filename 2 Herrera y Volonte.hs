import Text.Show.Functions
import Data.List

--Cliente Parte 1
--data Cliente = Cliente {nombre::String, resistencia::Float, amigos::[Amigos]} deriving (Show) 
--Cliente Parte 2
data Cliente = Cliente {nombre::String, resistencia::Float, amigos::[Amigos], tragos::[Trago]} deriving (Show)
type Amigos = Cliente
data Trago = Trago {nombreTrago::String, variante::Float} deriving (Show)

 --Parte 1
 --1
juan = Cliente "juan" 100 [] []
--2
-- rodri y marcos Parte 1
rodri = Cliente "rodri" 150 [] []
--marcos = Cliente "marcos" 40 [rodri]
--3
jarraLoca = Trago "jarraLoca" 10
grogXD = Trago "grogXD" 0
klusenerHuevo = Trago "klusenerHuevo" 5
klusenerFrutilla = Trago "klusenerFrutilla" 8
klusenerChocolate = Trago "klusenerChocolate" 9

--4
rescatarse tiempo cliente
 | (>3) tiempo = cliente{resistencia = resistencia cliente + 200}
 | otherwise = cliente{resistencia = resistencia cliente + 100}

--Parte 2
--5
marcos = Cliente "marcos" 40 [rodri, manu] [grogXD, grogXD, klusenerHuevo, klusenerFrutilla]
manu = Cliente "manu" 40 [rodri] [grogXD, grogXD, klusenerHuevo, klusenerFrutilla, jarraLoca]
--6
tomar trago cliente
 | nombreTrago trago == "jarraLoca" = cliente{resistencia = resistencia cliente - variante trago, tragos = tragos cliente ++ [trago]}
 | nombreTrago trago == "grogXD" = cliente{resistencia = 0, tragos = tragos cliente ++ [trago]}
 | nombreTrago trago == "klusenerHuevo" = cliente{resistencia = resistencia cliente - variante trago, tragos = tragos cliente ++ [trago]}
 | nombreTrago trago == "klusenerFrutilla" = cliente{resistencia = resistencia cliente - variante trago, tragos = tragos cliente ++ [trago]}
 | nombreTrago trago == "klusenerChocolate" = cliente{resistencia = resistencia cliente - variante trago, tragos = tragos cliente ++ [trago]}
 | otherwise = cliente{tragos = tragos cliente ++ [trago]}

--Parte 3
--7
multipleFuncion = tomar jarraLoca . rescatarse 1 . tomar grogXD

--8
cantidadQueTomo = length.tragos 
tomoMasQueSusAmigos cliente = cantidadQueTomo cliente > maximum (map cantidadQueTomo (amigos cliente))

--Inferencia de tipos
--9
f :: Num t => (b -> Bool) -> a -> [a] -> (Int -> (t, Bool) -> b) -> Int -> Int
f a b c d e 
 | (a . d e) (1, True) = 0
 | otherwise           = length (b:c) + e

-- Restricciones

-- Modelado se utiliza en cada data.
-- Orden superior es la función map que se usa para restarle 10
-- a los amigos de alguien que se toma la jarra loca.
-- Aplicación parcial se utiliza en (>3) horas = Cliente resistencia + 200
-- (>3) es una función que espera 1 solo parámetro en vez de 2.

