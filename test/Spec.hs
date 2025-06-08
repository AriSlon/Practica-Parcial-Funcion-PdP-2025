import Test.Hspec
import PdePreludat
import qualified SpecMinigolfito
import qualified SpecPociones
import qualified SpecTP2


main :: IO ()
main = hspec $ do
  describe "Minigolfito"  SpecMinigolfito.spec
  describe "Pociones"  SpecPociones.spec
  describe "TP2"  SpecTP2.spec