module TP2 where
import PdePreludat

type Hashtag = String 

luisito :: Video
luisito = Video{
  titulo = "mexico",
  minutos = 90,
  hashtags = ["hola", "chau"]
} 


data Video = Video {
  titulo :: String,
  minutos :: Number,
  hashtags :: [String]
} deriving (Show, Eq)



data Playlist = Playlist {
  descripcion :: String,
  videos :: [Video]
} deriving (Show, Eq)


tieneHashtag :: Hashtag -> Video -> Bool
tieneHashtag hashtag video = elem hashtag (hashtags video)

getMinutos:: [Video] -> [Number]
getMinutos  =  map minutos --Ap Parcial ([Video])

getVideosFiltrados:: Hashtag -> [Video] -> [Video]
getVideosFiltrados hashtag  = filter (tieneHashtag hashtag) --Ap Parcial ([Video])


minutosTotalesConHashtag :: Hashtag -> Playlist -> Number
minutosTotalesConHashtag hashtag  = sum . getMinutos . getVideosFiltrados hashtag . videos 


estanRelacionados :: Video -> Video -> Bool
estanRelacionados video1 video2 = any (`tieneHashtag` video2)(hashtags video1)

recomendable :: Video -> Playlist -> Bool
recomendable video playlist = (>= 2) (length (filter (estanRelacionados video) (videos playlist)))
