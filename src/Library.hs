module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- PUNTO 1

data Persona = UnaPersona {
    calorias :: Number,
    nutrientes :: [String]
}

sofia = UnaPersona 0 []

incorporaNutriente :: String -> Persona -> Persona
incorporaNutriente nutriente persona | elem nutriente (nutrientes persona) = persona
                                     | otherwise = persona {nutrientes = (nutriente:nutrientes persona)}

-- PUNTO 2 

type Nutriente = Persona -> Persona

tomate :: Nutriente
tomate = incorporaNutriente "Vitamina A".incorporaNutriente "Vitamina C"

zanahoria :: Nutriente 
zanahoria = tomate.incorporaNutriente "Vitamina E".incorporaNutriente "Vitamina K" 

carne :: Number -> Nutriente
carne calorias = incorporaNutriente "Hierro".incorporaNutriente "Calcio".aportaCalorias ((div calorias 10) * 240) 

aportaCalorias :: Number -> Nutriente
aportaCalorias caloriash persona = persona {calorias = (calorias persona) + caloriash}

pan :: Nutriente -> Nutriente
pan nutriente = nutriente.incorporaNutriente "Zinc"

blanco :: Nutriente
blanco = aportaCalorias 265



























































