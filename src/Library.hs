{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat
import Data.List (sort, map, sortBy, filter, head, and)


{-Data-}
data Auto = Auto {
    color::Color,
    distanciaRecorrida::Distancia,
    velocidad::Velocidad,
    powerup::PowerUp
}deriving(Eq,Ord,Show)


data Carrera = Carrera Posiciones deriving(Eq,Ord,Show)

{-Datos de prueba-}
mario = Auto {color="rojo", distanciaRecorrida = 20, velocidad = 50, powerup=""}
luigi = Auto {color="verde", distanciaRecorrida = 12, velocidad = 50, powerup=""}

starChampionship= Carrera [luigi,mario]


{-types-}
type Color = String
type PowerUp = String
type Distancia = Number
type Velocidad = Number
type Posiciones = [Auto]
type Puesto = Number

{-Funciones Auxiliares-}
posicionamiento :: Posiciones -> Posiciones
posicionamiento = foldl ordenarAuto []

ordenarAuto :: Posiciones -> Auto -> Posiciones
ordenarAuto [] auto = [auto]
ordenarAuto (primero:resto) auto
    | distanciaRecorrida auto > distanciaRecorrida primero = auto:primero:resto
    | otherwise                          = primero:ordenarAuto resto auto


diferenciaAbsolutaDistancia::Distancia -> Distancia -> Distancia
diferenciaAbsolutaDistancia puntoUno puntoDos = (abs . (-) puntoDos) puntoUno

esPrimero::Auto -> Carrera -> Bool
esPrimero autoUno=  ((==autoUno) . (!!1) . posicionamiento)

{-Funciones Principales-}
estaCerca:: Auto -> Auto -> Bool
estaCerca autoUno autoDos = ((<10) . diferenciaAbsolutaDistancia (distanciaRecorrida autoUno) . distanciaRecorrida) autoDos && (/= autoUno) autoDos

vaTranquilo::Auto -> Carrera ->Bool
vaTranquilo autoUno = (not.estaCerca autoUno . (!!2).(&&).esprimero autoUno) posicionamiento carrera

puesto:: Auto -> Carrera -> Puesto
puesto autoUno carrera = ((+1) . elemIndex autoUno . posicionamiento) carrera


-- {-Data-}
-- data Chocolate = Chocolate {
--     nombre::String,
--     gramaje::Gramos,
--     cacao:: Porcentaje,
--     azucar::Porcentaje,
--     ingredientes::[Calorias]
-- }deriving (Eq, Ord, Show)

-- data Persona = Persona {
--     alias::String,
--     limiteDeCalorias::Calorias
-- }deriving (Eq, Ord, Show)


-- {-types-}
-- type Calorias = Number
-- type Precio = Number
-- type Porcentaje = Number
-- type Gramos = Number
-- type CajaDeChocolates = [Chocolate]
-- type Unidades = Number
-- type Grados = Number
-- type Proceso = Chocolate -> Chocolate
-- type Receta = [Proceso]


-- {-Datos de Prueba-}
-- chocolateBase = Chocolate{
--     nombre = "Chocolate",
--     gramaje=10,
--     cacao= 50,
--     azucar = 0,
--     ingredientes = [400]
-- }

-- vauquita = Chocolate {
--     nombre="Vauquita",
--     gramaje=30,
--     cacao = 50,
--     azucar = 0,
--     ingredientes = [400,220]
-- }


-- federico = Persona{
--     alias = "Federico",
--     limiteDeCalorias = 800
-- }






-- {-Funciones secundarias-}
-- mencionadas en el enunciado
-- esAmargo :: Chocolate -> Bool
-- esAmargo = (>60) . cacao

-- aptoParaDiabeticos:: Chocolate -> Bool
-- aptoParaDiabeticos = (== 0) . azucar

-- precioPremium:: Chocolate -> Precio
-- precioPremium chocolate | aptoParaDiabeticos chocolate = 8
--  | otherwise = 5


-- Auxiliares
-- cantidadDeIngredientes :: Chocolate -> Number
-- cantidadDeIngredientes = length . ingredientes

-- excedeCalorias:: Calorias -> Bool
-- excedeCalorias = (> 200)




-- agregarIngrediente :: Calorias -> Proceso
-- agregarIngrediente calorias chocolate= chocolate {ingredientes = ingredientes chocolate ++ [calorias]}


-- chocolateTentacion :: Proceso
-- chocolateTentacion chocolate = chocolate {nombre=((++ " tentacion" ). nombre) chocolate }

-- aceptaCalorias:: Calorias ->Persona->Bool
-- aceptaCalorias calorias persona = (limiteDeCalorias persona - calorias) > 0

-- comerChocolate:: Chocolate ->Persona -> Persona
-- comerChocolate chocolate persona = persona {limiteDeCalorias = limiteDeCalorias persona - totalCalorias chocolate }


-- {-Funciones principales-}
-- Punto 1
-- precioChocolate:: Chocolate -> Precio
-- precioChocolate chocolate | esAmargo chocolate = (* precioPremium chocolate) . gramaje $ chocolate
--  |cantidadDeIngredientes chocolate > 4 = (* 8) . cantidadDeIngredientes $ chocolate
--  | otherwise = (*1.5) . gramaje $ chocolate

-- Punto 2
-- esBombonAsesino:: Chocolate -> Bool
-- esBombonAsesino chocolate = any excedeCalorias (ingredientes chocolate)

-- totalCalorias:: Chocolate -> Calorias
-- totalCalorias chocolate = sum (ingredientes chocolate)

-- aptoParaNinos:: CajaDeChocolates -> CajaDeChocolates
-- aptoParaNinos = take 3 . filter (not . esBombonAsesino)

-- Punto 3
-- frutalizado::Unidades ->Proceso
-- frutalizado unidades = agregarIngrediente (unidades * 2)

-- dulceDeLeche:: Proceso
-- dulceDeLeche = chocolateTentacion . agregarIngrediente 220

-- celiaCrucera::Porcentaje -> Proceso
-- celiaCrucera nivelAzucar chocolate  = chocolate {azucar= ((+ nivelAzucar).azucar) chocolate }


-- embriagadora :: Grados -> Proceso
-- embriagadora grados = agregarIngrediente (min 30 grados)

-- Punto 4

-- receta = (embriagadora 32 . dulceDeLeche . frutalizado 10) chocolateBase
-- chocolateBase es de tipo Data Chocolate

-- Punto 5
-- preparacionDeChocolate:: Receta -> Chocolate -> Chocolate
-- preparacionDeChocolate receta chocolateBase = foldr ($) chocolateBase (reverse receta)


-- Punto 6
-- hastaAcaLlegue:: CajaDeChocolates -> Persona -> CajaDeChocolates
-- hastaAcaLlegue (chocolate:chocolates) persona | aceptaCalorias (totalCalorias chocolate) persona = chocolate : (hastaAcaLlegue chocolates . comerChocolate chocolate) persona
--  | otherwise = []


