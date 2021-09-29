-- Javier Mulero Martín
-- Sesión de laboratiorio 2, 26 de octubre de 2020


-- Ej 1
-- Las funciones son correctas para enteros positivos
-- a)
cuadrados::Integral a => a -> [a]
cuadrados 0 = [0]
cuadrados n = cuadrados (n-1) ++ [n^2]
-- b)
pares_cuadrados::Integral a => a -> [(a,a)]
pares_cuadrados 0 = [(0,0)]
pares_cuadrados n = (n,n^2):(pares_cuadrados (n-1))
-- c)
sumatorio:: (Eq a, Floating a) => a -> a
sumatorio 1 = 1*abs(cos 1)
sumatorio n = n*abs(cos n)+sumatorio (n-1)
-- d) Considero <= n
suma::Integral a => a -> a
suma n
  | n == 0 = 0
  | mod n 3 == 0 = n+suma(n-1)
  | mod n 5 == 0 = n+suma(n-1)
  | otherwise = suma(n-1)


  -- Ej 2
cuadrados'::Integral a => a -> [a]
cuadrados' n = map (^2) [0..n]

pares_cuadrados'::Integral a => a -> [(a,a)]
pares_cuadrados' n = zip [n,n-1..0] (map (^2) [n,n-1..0])

sumatorio':: (Enum a, Floating a) => a -> a
sumatorio' n = sum (zipWith (*) [1..n] (map (abs.cos) [1..n]))

-- Ej 3
-- a)
iguales::(Eq a, Enum a) => (a -> a) -> (a -> a) -> a -> a -> Bool
iguales f g n m = map f [n..m] == map g [n..m]
-- b)
menorA:: Enum a => a -> a -> (a -> Bool) -> a
menorA n m p = let lista = (filter p [n..m]) in
              if null (lista)
              then error "No hay elementos cumpliendo esa propiedad"
              else head (lista)
-- c)
mayor::(Num a, Enum a) => a -> (a -> Bool) -> a
mayor n p = let lista = (filter p [0..n]) in
              if null (lista)
              then error "No hay elementos cumpliendo esa propiedad"
              else last (lista)
-- d)
ex:: Enum a => a -> a -> (a -> Bool) -> Bool
ex n m p = any p [n..m]

-- Ej 4
-- a)
filter2::[a] -> (a -> Bool) -> (a -> Bool) -> ([a],[a])
filter2 xs p q = (filter p xs, filter q xs)
-- b)
filters::[a] -> [(a -> Bool)] -> [[a]]
filters xs [] = []
filters xs (p:ps) = [filter p xs] ++ filters xs ps
