module TP2 where
import PdePreludat

type Hashtag = String --Expresividad
type Minutos = Number --Expresividad

data Video = Video {
  titulo :: String,
  minutos :: Minutos,
  hashtags :: [Hashtag]
} deriving (Show, Eq)


data Playlist = Playlist {
  descripcion :: String,
  videos :: [Video]
} deriving (Show, Eq)


tieneHashtag :: Hashtag -> Video -> Bool
tieneHashtag hashtag video = elem hashtag (hashtags video)

getMinutos:: [Video] -> [Minutos]
getMinutos  =  map minutos --Point-free ([Video])

getVideosFiltrados:: Hashtag -> [Video] -> [Video]
getVideosFiltrados hashtag  = filter (tieneHashtag hashtag) --Point-free ([Video])

minutosTotalesConHashtag :: Hashtag -> Playlist -> Minutos
minutosTotalesConHashtag hashtag  = sum . getMinutos . getVideosFiltrados hashtag . videos --getMinutos y getVideosFiltrados no son necesarias, pero considero que favorecen la expresividad.

estanRelacionados :: Video -> Video -> Bool
estanRelacionados video1 video2 = any (`tieneHashtag` video1) (hashtags video2) --tieneHashtag esta entre comillas invertidas con el objetivo de fijar el segundo parametro. Ahora, "`tieneHashtag` video2" es una funcion que recibe un Hashtag y devuelve un Bool.

recomendable :: Video -> Playlist -> Bool
recomendable video = (>= 2) . length . filter (estanRelacionados video) . videos --En este caso es necesaria la notacion point-free para poder componer todas las funciones. Si explicito el parametro "playlist", "videos playlist" ya no seria una funcion, sino un valor concreto (lista de videos).
