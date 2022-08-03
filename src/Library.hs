module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- PUNTO 1

data Persona = UnaPersona {
    calorias :: Number,
    nutrientes :: [String]
}deriving(Show,Eq)

sofia = UnaPersona 0 []

incorporaNutriente :: String -> Persona -> Persona
incorporaNutriente nutriente persona | elem nutriente (nutrientes persona) = persona
                                     | otherwise = persona {nutrientes = (nutriente:nutrientes persona)}

-- PUNTO 2 

type Comida = Persona -> Persona

tomate :: Comida
tomate = incorporaNutriente "Vitamina A".incorporaNutriente "Vitamina C"

zanahoria :: Comida 
zanahoria = tomate.incorporaNutriente "Vitamina E".incorporaNutriente "Vitamina K" 

carne :: Number -> Comida
carne gramos = incorporaNutriente "Hierro".incorporaNutriente "Calcio".aportaCalorias ((div gramos 10) * 240) 

aportaCalorias :: Number -> Comida
aportaCalorias caloriash persona = persona {calorias = (calorias persona) + caloriash}

pan :: Comida -> Comida
pan tipo = tipo.incorporaNutriente "Zinc"

blanco :: Comida
blanco = aportaCalorias 265

integral :: Comida
integral = aportaCalorias 200.incorporaNutriente "Fibras"

dePapa :: Comida 
dePapa persona | estaPipona persona = aportaCalorias 100 persona
               | otherwise = aportaCalorias 500 persona

estaPipona :: Persona -> Bool
estaPipona persona = calorias persona > 2000

hamburguesaCheta :: Comida
hamburguesaCheta = pan dePapa.carne 180.tomate.pan dePapa

-- PUNTO 3

type Menu = [Comida]

comeMenu :: Menu -> Persona -> Persona
comeMenu menu persona = foldl come persona menu

come :: Persona -> Comida -> Persona
come persona comida = comida persona

menu1 :: Menu
menu1 = [pan integral, zanahoria, hamburguesaCheta]

-- PUNTO 4

data Evento = UnEvento {
    nombre :: String,
    menu :: Menu,
    invitados :: Invitados
}

type Invitados = [Persona]

satisfecho :: Persona -> Bool
--satisfecho persona = estaPipona persona || length(nutrientes persona) > 5
satisfecho persona = estaPipona persona || ((>5).length.nutrientes) persona

altaFiesta :: Evento -> Bool
altaFiesta evento = all satisfecho (map (comeMenu (menu evento)) (invitados evento))


-- PUNTO 5

type Eventos = [Evento]

valeLaPena :: Persona -> Evento -> Evento
valeLaPena persona evento | altaFiesta evento = evento {invitados = (persona:invitados evento)}
                        | otherwise = evento

seInvita :: Persona -> Eventos -> Eventos
seInvita persona eventos = map (valeLaPena persona) eventos  

-- PUNTO 6
-- No se podria determinar si un evento infinito es alta fiesta ya que utilizamos la funcion all, que busca analizar si todos los 
-- invitados estan satisfechos. Es por esto que infinitamente se quedara analizando si todos los invitados estan satisfechos y 
-- nunca podra determinar si es alta fiesta o no.

















































