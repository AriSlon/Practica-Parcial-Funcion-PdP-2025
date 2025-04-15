module TP1 where
import PdePreludat
import Test.Hspec (xcontext)

esMes :: Number -> Bool
esMes x = x >= 1 && x <= 12

mesAnterior :: Number -> Number
mesAnterior 1 = 12
mesAnterior x | esMes(x) = x-1 --Utiliza esMes para comprobar que estemos dentro del dominio del problema

mesSiguiente :: Number -> Number
mesSiguiente 12 = 1
mesSiguiente x | esMes(x) = x+1 --Utiliza esMes para comprobar que estemos dentro del dominio del problema.

estacion :: Number -> String
estacion 1 = "verano"
estacion 4 = "otonio"
estacion 7 = "invierno"
estacion 10 = "primavera"
estacion x
     | mod x 3 == 0 = (estacion (mesAnterior x)) ++ "/" ++ (estacion (mesSiguiente x))
     | otherwise = (estacion (mesAnterior x))  --En caso de ingresar un numero por fuera del dominio, va a saltar el error cuando se llame a mesAnterior, por lo que no hace falta preguntar por si esMes(x) es True.