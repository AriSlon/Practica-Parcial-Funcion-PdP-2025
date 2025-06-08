module Minigolfito where
import PdePreludat
import Data.Bool (Bool)

type Puntos = Number
type Palo =  Habilidad -> Tiro

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)


-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b



modeladorPalos :: Number -> Number -> Number -> Tiro
modeladorPalos  vel prec al = UnTiro{
        velocidad = vel,
        precision = prec,
        altura = al
}

putter :: Palo
putter habilidad = modeladorPalos 10  (precisionJugador habilidad*2) 0

madera :: Palo
madera habilidad = modeladorPalos 100  (precisionJugador habilidad/2) 5

hierros:: Number -> Palo
hierros n habilidad = modeladorPalos (fuerzaJugador habilidad * n) (precisionJugador habilidad / n) (max (n-3) 0)

palos :: [Palo]
palos = [putter,madera] ++ map hierros [1..10]


golpe :: Jugador -> Palo ->  Tiro
golpe jugador palo = palo (habilidad jugador)


tiroDetenido :: Tiro
tiroDetenido = UnTiro {velocidad = 0, precision = 0, altura = 0}

data Obstaculo = UnObstaculo {
    puedeSuperar :: (Tiro -> Bool),
    efectoPostSuperar :: (Tiro -> Tiro)
} 
tunelConRampita:: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita  modificacionTunelConRampita

superaTunelConRampita:: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && altura tiro == 0

modificacionTunelConRampita:: Tiro -> Tiro
modificacionTunelConRampita tiro = tiro {velocidad = 2* velocidad tiro, precision = 100, altura = 0}


laguna:: Number -> Obstaculo
laguna largo = UnObstaculo superaLaguna  (modificacionLaguna largo)

superaLaguna:: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 90 && between 1 5 (altura tiro)

modificacionLaguna:: Number -> Tiro -> Tiro
modificacionLaguna largo tiro = tiro {altura = altura tiro /largo}


hoyo:: Obstaculo
hoyo = UnObstaculo superaHoyo modificacionHoyo

superaHoyo:: Tiro -> Bool
superaHoyo tiro = precision tiro > 95 && between 5 20 (velocidad tiro) && altura tiro == 0

modificacionHoyo:: Tiro -> Tiro
modificacionHoyo tiro = tiroDetenido


palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObstaculo obstaculo jugador) palos

superaObstaculo :: Obstaculo -> Jugador -> Palo  ->  Bool
superaObstaculo obstaculo jugador palo  = puedeSuperar  obstaculo ( golpe jugador palo)

superarConsecutivo :: [Obstaculo] -> Tiro -> Number
superarConsecutivo (head:tail) tiro
    | puedeSuperar head tiro = 1 + superarConsecutivo tail (efectoPostSuperar head tiro) 
    | otherwise = 0

listaObs:: [Obstaculo]
listaObs = [tunelConRampita,tunelConRampita, hoyo]

tiroPrueba :: Tiro
tiroPrueba = UnTiro {velocidad = 10, precision = 95, altura = 0}

{-
Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.
-}




paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador lista = maximoSegun (superarConsecutivo lista.golpe jugador ) palos

{-
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b
-}
{-
Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.-}
