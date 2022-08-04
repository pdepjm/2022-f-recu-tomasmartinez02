module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "Test Punto 3" $ do
    it "Sofia come menu con los valores esperados" $ do
      comeMenu menu1 sofia `shouldBe` UnaPersona {calorias = 5120, nutrientes = ["Hierro","Calcio","Vitamina A","Vitamina C","Vitamina E","Vitamina K","Fibras","Zinc"]}
  
  describe "Test Punto 4" $ do
    it "Evento1 es alta fiesta" $ do
      altaFiesta evento1 `shouldBe` True
    it "Evento2 no es alta fiesta" $ do
      altaFiesta evento2 `shouldBe` False  
    it "Evento3 es alta fiesta" $ do
      altaFiesta evento3 `shouldBe` True   

  describe "Test Punto 5" $ do
    it "Santi se invita a evento3" $ do
      length (invitados (valeLaPena santi evento3)) > length (invitados evento3) `shouldBe` True
    it "Santi no se invita a evento2" $ do
      length (invitados (valeLaPena santi evento2)) == length (invitados evento2) `shouldBe` True
    it "Santi se invita a evento1" $ do
      length (invitados (valeLaPena santi evento1)) > length (invitados evento1) `shouldBe` True



