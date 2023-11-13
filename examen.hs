import GHC.CmmToAsm.AArch64.Instr (x0)
{--La elección periódica de los gobernantes es la base de los Estados Modernos. Este ,,
sistema, denominado "democracia" (término proveniente de la antigua Grecia), tiene diferentes variaciones, que 
incluyen diferentes formas de elección del/a máximo/a mandatario/a. Por ejemplo, en algunos países se eligen representantes en un colegio electoral (EEUU). En otros se vota a los/as miembros del #parlamento (España). En nuestro país elegimos de forma directa la fórmula presidencial (Presidente/a y Vicepresidente/a) cada 4 años.

A continuación presentamos una serie de ejercicios que tienen como objetivo implementar funciones para sistema de escrutinio de una elección presidencial. Leer las descripciones y 
especificaciones e implementar las funciones requeridas en Haskell, utilizado sóĺamente las herramientas vistas en clase.

Las fórmulas presidenciales serán representadas por tuplas (String x String), donde la primera componente será el nombre del candidato a presidente, y la segunda componente será el nombre del 
candidato a vicepresidente.

En los problemas en los cuales se reciban como parámetro secuencias de fórmulas y votos, cada posición de la lista votos representará la cantidad de votos obtenidos por la fórmula del parámetro 
formulas en esa misma posición. Por ejemplo, si la lista de fórmulas es [("Juan Pérez","Susana García"), ("María Montero","Pablo Moreno")] y la lista de votos fuera [34, 56], eso indicaría que la 
fórmula encabezada por María Montero obtuvo 56 votos, y la lista encabezada por Juan Pérez obtuvo 34 votos.  --}

{--la tupla sera [ PRESIDENTE , VICEPRESIDENTEN] --}
{--

1) Votos en Blanco [1 punto] , (LLEGUE A 0.63)

problema votosEnBlanco (formulas: seq⟨String x String⟩,votos:seq< Z >, cantTotalVotos: Z) : Z {
  requiere: {formulasValidas(formulas)}
  requiere: {|formulas| = |votos|}
  requiere: {Todos los elementos de votos son mayores o iguales a 0}
  requiere: {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
  asegura: {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron}
}
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

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x [y] | x == y = True
                |otherwise = False
pertenece  i (x:xs) | i == x = True
                    | otherwise = pertenece i xs

-- Ejercicio 2
formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = True
formulasValidas (f:fs) = fst f /= snd f && not (estaEnFormulas (fst f) fs) && not (estaEnFormulas (snd f) fs) && (formulasValidas fs)

---------------------------------------------------------------
estaEnFormulas :: String -> [(String, String)] -> Bool
estaEnFormulas _ [] = False
estaEnFormulas c ((x,y):fs) | c == (x) = True
                        | c == (y) = True
                        | otherwise = estaEnFormulas c fs

formulasValidas2 :: [(String, String)] -> Bool
formulasValidas2 [] = True
formulasValidas2 ((x,y):fs) = x /= y && not (estaEnFormulas (x) fs) && not (estaEnFormulas (y) fs) && (formulasValidas fs)


formulasPrueba1 = [("AR","AR"),("OR","ER")]
formulasPrueba2 = [("AR","OR"),("AR","ER")]

-- = c == (fst f) || c == (snd f) || estaEnFormulas c fs
