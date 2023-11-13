

--1) Votos en Blanco [1 punto] , (LLEGUE A 0.63)
--
--problema votosEnBlanco (formulas: seq⟨String x String⟩,votos:seq< Z >, cantTotalVotos: Z) : Z {
--  requiere: {formulasValidas(formulas)}
--  requiere: {|formulas| = |votos|}
--  requiere: {Todos los elementos de votos son mayores o iguales a 0}
--  requiere: {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
--  asegura: {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron}
--}
--}

--1) Votos en Blanco [1 punto]


sumatoriaDeVotosValidos :: [Int] -> Int
sumatoriaDeVotosValidos [] = 0
sumatoriaDeVotosValidos (x:xs) = x + sumatoriaDeVotosValidos xs


votosEnBlanco :: [(String,String)] -> [Int]-> Int ->Int
--vector de votos siempre sera menor o igual que CantidadTotal Votos 
--poner guion bajo "_" si no lo voy a usar 
votosEnBlanco _ (y:ys) z =  z - sumatoriaDeVotosValidos (y:ys)

--2) Formulas Válidas [3 puntos]

--problema formulasValidas (formulas: seq⟨String x String⟩) : Bool {
--  requiere: {True}
--  asegura: {(res = true) <=> formulas no contiene nombres repetidos, es decir que 
--cada candidato está en una única fórmula (no se puede ser candidato a presidente y vicepresidente ni en la misma fórmula ni en fórmulas distintas)}
--}

---------------------------------------------------------------
-- Ejercicio 2
estaEnLaTuplaString :: String -> [(String, String)] -> Bool
estaEnLaTuplaString _ [] = False
estaEnLaTuplaString s ((x,y):xs)| s == x = True 
                                | s == y = True
                                |otherwise = estaEnLaTuplaString s xs

formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = True
formulasValidas ((x,y):fs) = x /= y && not (estaEnLaTuplaString (x) fs) && not (estaEnLaTuplaString (y) fs) && (formulasValidas fs)


----------pruebas
pruebaVacia = [] --True 
pruebaUnElemento = [("NALDO","REGI")] --True 
pruebaExistePresidenteYVicepresidenteEnLaMismaTupla = [("AR","AR"),("SE","ER"),("IR","SO"),("OR","UR")] --espero un FALSE
pruebaExisteMasoIgualDe2PresidenteyVicepresidenteIguales = [("AR","OR"),("ER","IR"),("SA","SE"),("SI","SO"),("SU","RU"),("AR","OR")] --espero un FALSE
pruebaExistePresidenteComoVicepresidenteYViseversa = [("AR","OR"),("SA","SE"),("SI","SO"),("OR","AR")] --espero un FALSE 
pruebaExisten2PresidentesIgualesConViceDistintos = [("AR","OR"),("SA","SE"),("AR","ZE"),("SI","SO"),("IR","UR")] --espero un FALSE
pruebaExisten2VicepresidentesIgualesConPresiDistintos = [("RE","OR"),("SA","SE"),("RI","OR"),("SU","TA"),("IR","TE")] --espero un FALSE

pruebaExisteUnPresidenteYUnVicepresidenteIguales =  [("AR","OR"),("SA","SE"),("CA","AR"),("SI","SO"),("IR","UR")] --espero un FALSE
----------------------------------------------------------------------------------------------



{--
3) Porcentaje de Votos [3 puntos]

problema porcentajeDeVotos (presidente: String, formulas: seq⟨String x String⟩,votos:seq< Z >) : R {
  requiere: {La primera componente de algún elemento de formulas es presidente}
  requiere: {formulasValidas(formulas)}
  requiere: {|formulas| = |votos|}
  requiere: {Todos los elementos de votos son mayores o iguales a 0}
  requiere: {Hay al menos un elemento de votos que es mayor que estricto que 0}
  asegura: {res es el porcentaje de votos que obtuvo la fórmula encabezada por presidente sobre el total de votos afirmativos}
}
Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos números de tipo Int:

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b) 
[1,2,3,4,5]
15 ---100 
1 ----x
--}
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b) 

sumatoriaDeVotosValidos2 :: [Int] -> Int
sumatoriaDeVotosValidos2 [] = 0 
sumatoriaDeVotosValidos2 (x:xs) = x + sumatoriaDeVotosValidos2 (xs)

puntajePresidente :: String -> [(String,String)] -> [Int] -> Int
puntajePresidente presidente ((x,y):xs) (n:ns)  | presidente == x = n 
                                                |otherwise = puntajePresidente presidente xs ns


porcentajeDeVotos :: String -> [(String,String)] -> [Int] ->Float 
porcentajeDeVotos presidente formulas votos = division  ((puntajePresidente presidente formulas votos)*100) (sumatoriaDeVotosValidos2 votos)



{--

4) Próximo Presidente [3 puntos]

problema proximoPresidente (formulas: seq⟨String x String⟩, votos:seq< Z >) : String {
  requiere: {formulasValidas(formulas)}
  requiere: {|formulas| = |votos|}
  requiere: {Todos los elementos de votos son mayores o iguales a 0}
  requiere: {Hay al menos un elemento de votos mayores estricto a 0}
  requiere: {|formulas| > 0}
  asegura: {res es el candidato a presidente de formulas más votado de acuerdo a los votos contabilizados en votos}
}
--}
esMayor :: [Int] -> Bool
esMayor [] = True
esMayor [x] = True 
esMayor (x:y:xs)    | x >= y = esMayor(x:xs)
                    |otherwise = False                    
--[1,2,3,4,5] --(1)
--[5,4,3,2,1] --(2)
--[1,2,6,3,2] -- (3)
--[1,2,6,3,8]  --(4)

buscarMaximo :: [Int] -> Int 
buscarMaximo [x] = x 
buscarMaximo (x:xs) | esMayor (x:xs) = x 
                    |otherwise = buscarMaximo (xs)

proximoPresidente :: [(String,String)] -> [Int] -> String
proximoPresidente ((x,y):xs) (n:ns) | n == buscarMaximo (n:ns) = x
                                    |otherwise = proximoPresidente xs ns

pruebaCreciente :: [Int]
pruebaCreciente = [1,2,3,4,5]

pruebaDecreciente = [5,4,3,2,1]
pruebaCreceyDecre = [1,2,6,3,2]
pruebaCreceDecreCre=[1,2,6,3,8]
pruebaDecreCre = [9,8,7,12,13,14]
pruebaDecreCreDecre = [9,8,7,12,13,14,6,4]

presidentes = [("RA","LA"),("RE","LE"),("RI","LI"),("RO","LO"),("RU","LU")]


---ejercicio 4 
-- Ejercicio 4
proximoPresidente :: [(String, String)] -> [Int] -> String
proximoPresidente [(presi,_)] _ = presi
proximoPresidente (f1:f2:fs) (v1:v2:vs) | v1 >= v2 = proximoPresidente (f1:fs) (v1:vs)
                                        | otherwise = proximoPresidente (f2:fs) (v2:vs)
