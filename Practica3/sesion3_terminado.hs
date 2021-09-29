-- Javier Mulero Martínew
-- Sesión 3 de laboratorio
-- Lunes 16 de Noviembre de 2020

-- Ejercicio 1
last' :: [a] -> a
last' [] = error("Lista vacia")
last' (x:xs) = foldl (\_ b -> b) x xs

reverse'::[a]->[a]
reverse' xs = foldl (\a b -> [b]++a) [] xs

all'::(a->Bool)->[a]->Bool
all' p xs = foldl (\a b -> a && p b) True xs

minimum'::Ord a => [a] -> a
minimum' [] = error("Lista vacia")
minimum' (x:xs) = foldl (\a b -> min a b) x xs

map'::(a->b)->[a]->[b]
--map' f xs = foldl (\a b -> a++[f b]) [] xs
map' f xs = foldr (\a b -> [f a] ++ b) [] xs

filter'::(a -> Bool)->[a]->[a]
filter' p xs = foldr (\a b ->  (if p a then [a] else []) ++ b) [] xs

takeWhile'::(a -> Bool)->[a]->[a]
takeWhile' p xs = foldr (\a b-> (if p a then [a]++b else [])) [] xs

-- Ejercicio 2

foldr1'::(a->a->a)->[a]->a
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

foldl1'::(a->a->a)->[a]->a
foldl1' f [x] = x
foldl1' f (x:y:xs) = foldl1' f ((f x y):xs)


-- Ejercicio 3

-- a) [1,-1,2,-2,..]
lista::Integral a => [a]
lista = concat [[n,-n] | n<-[1..]]
--[if odd x then -(1 + div x 2) else 1 + div x 2 | x<-[0..]]
-- b) [(0,0), (0,1), (1,0)...]
pares_naturales::Integral a => [(a,a)]
pares_naturales = [(m-n,n) | m<-[0..], n<-[m,m-1..0]]

-- Ejercicio 4

sufijos::[a]->[[a]]
sufijos xs = [drop n xs | n<-[0..length xs]]

prefijos::[a]->[[a]]
prefijos xs = [take n xs | n<-[0..length xs]]

sublistas::[a]->[[a]]
sublistas xs =  [[]] ++ [take n ys| ys<-sufijos xs, n<-[1..length ys]]
-- Otra forma para que quede como en el orden de la hoja (aplicando init n veces)
--sublistas xs = [[]] ++ [take n ys | n<-[1..length xs], ys <- (foldr (.) id (replicate n init) $ sufijos xs)]
-- Otra forma con funcion auxiliar
--sublistas xs = sublistas_aux xs (length xs)
--sublistas_aux xs 0 = [[]]
--sublistas_aux xs n = sublistas_aux xs (n-1) ++ [take n ys | ys<-take (length (sufijos xs) - n) $ sufijos xs]

remove::Eq a => a->[a]->[a]
remove _ [] = []
remove x xs = let par = (break (==x) xs) in (fst par) ++ (drop 1 $ snd par)

permutaciones::Eq a=> [a]->[[a]]
permutaciones [] = [[]]
permutaciones xs = [x:cola | x<-xs, cola<-permutaciones (remove x xs)]

sumandos::(Num a, Enum a, Ord a)=> a->[[a]]
sumandos 0 = [[]]
sumandos n = [x:y:xs | x <- [1..n], (y:xs) <- sumandos (n-x), y>=x] ++ [[n]]
