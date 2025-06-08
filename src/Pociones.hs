module Pociones where
import PdePreludat
import Test.Hspec (xcontext)

type Efecto = Persona -> Persona

data Persona = Persona {
  nombrePersona :: String,
  suerte :: Number,
  inteligencia :: Number,
  fuerza :: Number
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos :: [String]
nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)

niveles:: Persona -> [Number]
niveles persona = [suerte persona, inteligencia persona, fuerza persona]

sumaDeNiveles:: Persona -> Number
sumaDeNiveles persona = sum (niveles persona)

diferenciaDeNiveles:: Persona -> Number
diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

maximoNivel :: Persona -> Number
maximoNivel persona = maximum (niveles persona)

minimoNivel :: Persona -> Number
minimoNivel persona = minimum (niveles persona)

nivelesMayoresA :: Number -> Persona -> Number
nivelesMayoresA n  = length . filter (>n) . niveles

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion pocion = foldl1 (++) (map efectos (ingredientes pocion))

{-
Dada una lista de pociones, consultar:
Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.
La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre figura en la lista de ingredientes prohibidos.
Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.-}

pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore  = map nombrePocion . filter((>= 4).length . efectosDePocion) 

cantidadPocionesProhibidas :: [Pocion] -> Number
cantidadPocionesProhibidas  = length . filter(any ingredienteEnLista . ingredientes)

ingredienteEnLista :: Ingrediente -> Bool
ingredienteEnLista = flip elem nombresDeIngredientesProhibidos. nombreIngrediente

sonTodasDulces ::[Pocion] -> Bool
sonTodasDulces  = all tieneAzucar 

tieneAzucar :: Pocion -> Bool
tieneAzucar  = (elem "azúcar"). (map nombreIngrediente . ingredientes)

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion persona = foldl aplicarEfecto persona (efectosDePocion pocion) 

aplicarEfecto :: Persona -> Efecto  -> Persona
aplicarEfecto persona efecto = efecto persona

esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe pocion1 pocion2 personaInicial = personaInicial == (tomarPocion pocion2 . tomarPocion pocion1) personaInicial

{-
Definir la función personaMasAfectada que recibe una poción, una función cuantificadora (es decir, una función que dada una persona retorna un número) y una lista de personas, y 
devuelve a la persona de la lista que hace máxima el valor del cuantificador. Mostrar un ejemplo de uso utilizando los cuantificadores definidos en el punto 1.-}

personaMasAfectada:: Pocion -> (Persona -> Number) -> [Persona] -> Persona
personaMasAfectada pocion funcion lista = undefined

{-
maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)
-}