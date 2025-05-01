module TP2 where
import PdePreludat

data Video = Video {
  titulo :: String,
  minutos :: Number,
  hashtags :: [String]
} deriving (Show, Eq)

data Playlist = Playlist {
  descripcion :: String,
  videos :: [Video]
} deriving (Show, Eq)

tieneHashtag :: String -> Video -> Bool
tieneHashtag = undefined

minutosTotalesConHashtag :: String -> Playlist -> Number
minutosTotalesConHashtag = undefined

estanRelacionados :: Video -> Video -> Bool
estanRelacionados = undefined

recomendable :: Video -> Playlist -> Bool
recomendable = undefined
