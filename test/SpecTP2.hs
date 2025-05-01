module SpecTP2 where
import PdePreludat

import TP2
import Test.Hspec

spec = do
  describe "Hashtags" $ do
    describe "Tiene hashtags" $ do
      it "Un video tiene un hashtag si forma parte de sus hashtags" $ do
        tieneHashtag "#declaratividad"
          (Video "Clase 1" 20 ["#intro", "#declaratividad"]) `shouldBe` True
      it "Un video no tiene un hashtag si no hay ninguno como ese entre sus hashtags" $ do
        tieneHashtag "#PdePandemia"
          (Video "Clase 1" 20 ["#intro", "#declaratividad"]) `shouldBe` False
      it "Un video sin hashtags no tiene uno en particular" $ do
        tieneHashtag "#PdePandemia"
          (Video "Clase 1" 20 []) `shouldBe` False
    describe "Minutos totales con hashtag" $ do
      it "Una playlist sin videos para el hashtag indicado retorna 0 minutos" $ do
        minutosTotalesConHashtag "#PdePandemia" (Playlist "Funcional" [Video "Clase 2" 60 ["#intro", "#funcional"]]) `shouldBe` 0
      it "Una playlist con varios videos para el hashtag indicado retorna la suma de minutos de esos videos" $ do
        minutosTotalesConHashtag "#funcional" (Playlist "PdeP" [
          Video "Clase 3" 45 ["#funcional", "#data", "#tipos"],
          Video "Clase 1" 20 ["#intro", "#declaratividad"],
          Video "Clase 2" 60 ["#intro", "#funcional"]]) `shouldBe` 105
  describe "Videos recomendados" $ do
    describe "Estan relacionados" $ do
      it "Dos videos sin hashtags no están relacionados" $ do
        estanRelacionados (Video "Clase 1" 20 [])
          (Video "Clase 1" 20 []) `shouldBe` False
      it "Dos videos con hashtags todos distintos no están relacionados" $ do
        estanRelacionados (Video "Clase 1" 20 ["#intro", "#declaratividad"])
          (Video "Clase 3" 45 ["#funcional", "#data", "#tipos"]) `shouldBe` False
      it "Dos videos con algún hashtag en común están relacionados" $ do
        estanRelacionados (Video "Clase 2" 60 ["#intro", "#funcional"])
          (Video "Clase 3" 45 ["#funcional", "#data", "#tipos"]) `shouldBe` True
      it "Dos videos con varios hashtags en común están relacionados" $ do
        estanRelacionados (Video "Clase N" 60 ["#tipos", "#lazy", "#funcional"]) (Video "Clase 3" 45 ["#funcional", "#data", "#tipos"]) `shouldBe` True
    describe "Recomendable" $ do
      it "Un video no es recomendable para una playlist sin videos" $ do
        recomendable (Video "Clase 3" 45 ["#funcional", "#data", "#tipos"])
          (Playlist "nada" []) `shouldBe` False
      it "Un video no es recomendable para una playlist si sólo se relaciona con uno de la playlist" $ do
        recomendable (Video "Clase X" 45 ["#data"])
          (Playlist "PdeP" [
            Video "Clase 1" 20 ["#intro", "#declaratividad"],
            Video "Clase 2" 60 ["#intro", "#funcional"],
            Video "Clase 3" 45 ["#funcional", "#data", "#tipos"]
          ]) `shouldBe` False
      it "Un video es recomendable para una playlist si se relaciona con al menos dos de la playlist" $ do
        recomendable (Video "Clase X" 45 ["#lambda", "#funcional"])
          (Playlist "PdeP" [
            Video "Clase 1" 20 ["#intro", "#declaratividad"],
            Video "Clase 2" 60 ["#intro", "#funcional"],
            Video "Clase 3" 45 ["#funcional", "#data", "#tipos"]
          ]) `shouldBe` True