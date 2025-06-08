module SpecMinigolfito where
import PdePreludat

import Minigolfito
import Test.Hspec

spec = do
  describe "La respuesta" $ do
    it "... a la vida, el universo y todo lo demas ..." $ do
      laRespuesta `shouldBe` 42

