-- Sesión 4bis de laboratorio
-- 8 de Diciembre de 2020
-- Javier Mulero

-- Ejercicio 1

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Ord, Show)
type Punto = (Int, Int)

mover:: Punto -> Direccion -> Punto
mover (x,y) Arriba = (x,y+1)
mover (x,y) Abajo = (x,y-1)
mover (x,y) Izquierda = (x-1,y)
mover (x,y) Derecha = (x+1,y)

destino:: Punto -> [Direccion] -> Punto
--destino p [] = p -- recursivo
--destino p (x:xs) = destino (mover p x) xs -- recursivo
destino origen acciones = foldr (\x p -> mover p x) origen acciones

trayectoria:: Punto -> [Direccion] -> [Punto]
-- trayectoria punto [] = [punto] -- recursivo
-- trayectoria punto (x:xs) = punto : :trayectoria (mover punto x) xs -- recursivo
trayectoria punto movs = foldl (\ptos acc -> ptos ++ [(mover (last ptos) acc)]) [punto] movs

inferior:: Int -> [Direccion] -> [Direccion] -> Bool
inferior n movs movs' = maxAltura (trayectoria (0,0) movs) < maxAltura (trayectoria (0,0) movs')
   where maxAltura [] = 0
         maxAltura xs = maximum [snd x | x<- xs]
-- Creo que no he entendido muy bien el ejercicio porque no sé cuando usar lo del tablero nxn

-- enRecinto n (a,b) = 0 <= a && a <= n && 0 <= b && b <= n -- aqui probaba otras cosas pero no entiedno

--[if (all (enReciento n) $ trayectoria (i,j) movs) -- aqui probaba otras cosas pero no entiedno
--   then trayectoria (i,j) movs
--   else [] | i<-[0..n], j<-[0..n]


-- Ejercicio 2
data Arbol a =  Nodo a [Arbol a] --deriving (Show)

listaHojas:: Arbol a -> [a]
listaHojas (Nodo a []) = [a]
listaHojas (Nodo a xs) = foldr (\arb list -> listaHojas arb ++ list) [] xs

listaNodos:: Arbol a -> [a]
listaNodos (Nodo a []) = [a]
listaNodos (Nodo a xs) = [a] ++ foldr (\arb list -> listaNodos arb ++ list) [] xs

repMax:: Ord a => Arbol a -> Arbol a
repMax (Nodo a []) = Nodo a []
repMax t = repMax' (maximum $ listaNodos t) t

repMax':: a -> Arbol a -> Arbol a -- coloca m en todos los nodos
repMax' m (Nodo a []) = Nodo m []
repMax' m (Nodo a xs) = Nodo m (foldr (\arb list -> repMax' m arb:list) [] xs)

instance Eq a => Eq (Arbol a ) where
  (==) (Nodo a xs) (Nodo b ys) = a == b && xs == ys

instance Ord a => Ord (Arbol a) where
  compare (Nodo a xs) (Nodo b ys) = if (compare a b) == EQ then compare xs ys else compare a b

instance Show a => Show (Arbol a) where
  show t = mostrarArbol 0 t ++ "--- (el número de '>' indica el nivel del árbol)"

-- OBSERVACIÓN: La última frase (lo de "el número de ...) la he puesto porque me ponía
--              un salto de linea al final que no me gustaba como quedaba, entonces para
--              ocuparlo pues he escrito una leyenda informativa, pero es prescindible

mostrarArbol:: Show a => Int -> Arbol a -> String
mostrarArbol prof (Nodo a []) = replicate prof '>' ++ show a ++ "\n"
mostrarArbol prof (Nodo a xs) = replicate prof '>' ++ show a ++ "\n" ++
       (foldr (\arb str -> mostrarArbol (prof+1) arb ++ str) "" xs)


-- Para probar (Nodo 3 [Nodo 4 [Nodo 1 [],Nodo 2[]],Nodo 5 [Nodo 6 [], Nodo 7 [Nodo 8 [Nodo 81 [], Nodo 82 [], Nodo 83 []]]], Nodo 9 [Nodo 91 []]])
