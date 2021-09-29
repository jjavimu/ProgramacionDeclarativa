
-- Práctica Final 2020/2021
-- Javier Mulero Martín

data Rel a = R [(a,a)] deriving (Read)

instance Show a => Show (Rel a) where
   show (R c) =  "{" ++ foldr (\x xs ->  if xs == [] then show x else show x ++ ", "++ xs) "" c ++ "}"

esRelacion::Eq a => Rel a -> Bool
esRelacion (R []) = True
esRelacion (R (x:xs)) = if elem x xs then False else esRelacion (R xs)

-- Definimos algunas relaciones para hacer pruebas
r1:: Integral a => Rel a -- Relacion de equivalencia
r1 = R [(1,1), (2,2), (3,3), (4,4), (5,5), (1,2), (2,1), (2,4), (4,2), (1,4), (4,1)]

r2:: Integral a => Rel a -- Esta es refl, pero no simetrica ni transitiva
r2 = R [(1,1), (2,2), (3,3), (1,2), (2,1), (2,3)]

r3:: Rel Char -- Por probar con otros tipos
r3 = R [('D','8'), ('E','B'), ('C','B'), ('E','C'), ('8','D')]

r4:: Integral a => Rel a -- Es igual que r5, no es ni ref, ni sim ni, transitiva
r4 = R [(1,1),(1,3),(2,1),(2,2),(3,1)]

r5:: Integral a => Rel a -- Es igual que r4
r5 = R [(3,1),(2,2),(2,1),(1,1),(1,3)]

r6:: Integral a => Rel [a]
r6 = R [([1, 2], []), ([2,-2], [3, 3, 1]), ([1, 3], [0]), ([4], [4])]

contenido:: Eq a => Rel a -> Rel a -> Bool
contenido (R xs) (R ys) = and [elem x ys| x<-xs]

instance Eq a => Eq (Rel a) where
  (==) c d = (contenido c d) && (contenido d c)

eliminaRepetidos:: Eq a=> [a]-> [a]
eliminaRepetidos xs = foldl (\acc x ->if elem x acc then acc else acc++[x]) [] xs

-- Vamos a suponer que metemos siempre relaciones, se puede comprobar
-- que algo es una relación con la funcion esRelacion

dominio::Eq a => Rel a -> [a]
dominio (R xs) = eliminaRepetidos (map fst xs)

soporte::Eq a => Rel a -> [a]
soporte (R xs) = eliminaRepetidos (concatMap (\(x,y) -> [x,y]) xs )

esReflexiva::Eq a =>  Rel a -> Bool
esReflexiva r@(R xs) = and [elem (x,x) xs | x <- soporte r]

esTransitiva::Eq a =>  Rel a -> Bool
esTransitiva (R xs) = and [elem (x,z) xs | (x,y)<- xs, (w,z)<-xs, w==y]

esSimetrica::Eq a =>  Rel a -> Bool
esSimetrica (R xs) = and [elem (y,x) xs | (x,y)<-xs]

relEquivalencia:: Eq a => Rel a -> Bool
relEquivalencia r = esReflexiva r && esTransitiva r && esSimetrica r

conjCociente:: Eq a =>  Rel a -> [[a]]
conjCociente r
       | relEquivalencia r  = unionClasesEq r (dominio r)
       | otherwise = error("La relación no es de equivalencia.")

unionClasesEq:: Eq a => Rel a -> [a] -> [[a]]
unionClasesEq _ [] = []
unionClasesEq r@(R rel) (x:xs) = claseEq x : unionClasesEq r (filter (\y -> not $ elem (y,x) rel) xs)
      where claseEq x = [y | y<-(x:xs), elem (y,x) rel]

generaDiv::Integral a => a -> a -> Rel a
generaDiv n m = R [(x,y) | y<-[1..m],x<-[n..y], mod y x == 0]

generaEQ:: Ord a => [a] -> Rel a
generaEQ xs = R [(x,y) | x<-xs, y<-xs, x>=y]

union:: Eq a => Rel a -> Rel a -> Rel a
--union r1@(R xs) r2@(R ys) = R (eliminaRepetidos (xs ++ ys))
union r1@(R xs) r2@(R ys) = R (xs ++ [y | y<-ys, not $ elem y xs])

cierreR:: Eq a => Rel a -> Rel a
--cierreR r@(R xs) = R (eliminaRepetidos $ xs ++ [(x,x) | x<- soporte r])
cierreR r@(R xs) = R (xs ++ [(x,x) | x<- soporte r, not $ elem (x,x) xs])

cierreS:: Eq a => Rel a -> Rel a
---cierreS r@(R xs) = R (eliminaRepetidos $ xs ++ [(y,x) |(x,y)<-xs])
cierreS r@(R xs) = R (xs ++ [(y,x) |(x,y)<-xs, not $ elem (y,x) xs])

cierreT:: Eq a => Rel a -> Rel a
cierreT r@(R xs) = cierreT' (iterate ((union r).composicion r) r)

cierreT':: Eq a => [Rel a] -> Rel a
cierreT' [] = R []
cierreT' (x1:x2:xs) = if x1 == x2 then x1 else cierreT' (x2:xs)

cierreRST:: Eq a => Rel a -> Rel a
cierreRST r@(R xs) = (cierreT.cierreS.cierreR) r

composicion:: Eq a => Rel a -> Rel a -> Rel a
composicion r1@(R xs) r2@(R ys) = R (eliminaRepetidos [(x,z) | (x,y)<-ys, (w,z)<-xs, w==y])

-- La parte de IO la voy a hacer solo para relaciones donde el conjunto A es
-- un conjunto de Int, ya que solo puedo hacerlo para un tipo concreto.
-- Si quisiese añadir para relaciones de Char por ejemplo, solo tendria que modificar:
-- 1) Los tipos de IO donde pone Int
-- 2) El tipo para que infiera bien la funcion read
-- 3) Ajustar el espaciado de dibujaMatriz para que saque
-- bien la tabla (está ajustado a enteros).

getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

introRel:: IO (Rel Int)
introRel = do putStr "¿Cuántos pares vas a introducir? "
              n <- getInt
              introRel' n

introRel':: Int -> IO (Rel Int)
introRel' 0 = return (R [])
introRel' n = do putStr "Introduce un par de la relación (Int, Int): "
                 linea <- getLine
                 linea2 <- introRel' (n-1)
                 return $ (union (R ((read linea::(Int, Int)):[])) linea2)

creaMatriz:: Eq a => Rel a -> [(a, [Int])]
creaMatriz (R xs) = [(f,[(if elem (f,c) xs then 1 else 0) |  c<-soporte (R xs) ]) |f<-soporte (R xs)]

muestraRel::IO ()
muestraRel = do r <- introRel
                dibujaMatriz (creaMatriz r) 0

-- dibujaMatriz: Le llega un vector de pares (a,[Int]) donde a indica
-- a qué elemento le corresponde esa fila, seguido de la fila, que es un vector
-- de 0 y 1 que 1 indica si el elemento a esta relacionado con cada columna de esa fila.
-- Va pintando fila a fila, empezando por la 0
dibujaMatriz:: Show a =>[(a,[Int])] -> Int -> IO ()
dibujaMatriz m n
  | n == 0 =
    do
    -- Primero conseguimos los elementos para hacer la tabla, y los colocamos
    -- en la parte superior. Después ponemos una linea "------"
    putStrLn ("    " ++ pintaNombreCol (foldr (\x acc -> [fst x] ++ acc ) [] m))
    putStrLn ("   " ++ (take ((length m)*2+1) (repeat '-')))
    -- Ahora colocamos la primera fila, enseñando el elemento al que corresponde
    -- y sus relacionados
    (putStr.show) (fst (m!!n))
    putStrLn $ (" | " ++ pintaDatos (m!!n) ++ "|")
    dibujaMatriz m (n+1)
  | n < length m =
    do
    -- Colocamos la siguiente fila
    (putStr.show) (fst (m!!n))
    putStrLn $ (" | " ++ pintaDatos (m!!n) ++ "|")
    dibujaMatriz m (n+1)
  -- Terminamos cerrando la tabla
  | otherwise = putStrLn ("   " ++ (take ((length m)*2+1) (repeat '-')))

-- Esta función está hecha para el tipo Int. Si se prueba para, por ejemplo,
-- el tipo Char, el espaciado no está ajustado y la tabla se mostrará desplazada.
-- Se puede modificar para ajustarla, pero como haskell está fuertemente tipado,
-- es complicado trabajar con un tipo que no sabes cuál es.

pintaDatos:: Show a =>(a,[Int]) -> [Char]
pintaDatos (_,[]) = ""
pintaDatos (x,(v:vs)) = (show v) ++ " " ++ pintaDatos (x,vs)

pintaNombreCol:: Show a =>[a] -> [Char]
pintaNombreCol [] = ""
pintaNombreCol (v:vs) = (show v) ++ " " ++ pintaNombreCol vs


-------- FIN


-------- AHORA VIENEN RUEBAS QUE NO VALEN PARA NADA --------------
------------- LO GUARDO PARA NO PERDERLO!!!!! --------------------
-------------- ABAJO ESTABA HACIENDO PRUEBAS ---------------------

creaMatriz':: (Show a,Eq a) => Rel a -> [(String, [Int])]
creaMatriz' (R xs) = [(show f,[(if elem (f,c) xs then 1 else 0) |  c<-soporte (R xs) ]) |f<-soporte (R xs)]

espMax [] = 0
espMax (x:xs) = max (length (fst x)) (espMax xs)

dibujaMatriz':: [(String,[Int])] -> Int -> IO ()
dibujaMatriz' m n
  | n == 0 =
    do
    -- Primero conseguimos los elementos para hacer la tabla, y los colocamos
    -- en la parte superior. Después ponemos una linea "------"
    espaciado <- return $ (espMax m)
    print (espaciado)
    --separador <- return $ (replicate (espaciado+2) ' ' ) ++ (replicate ((length m)*2+1) '-')
    columnas <- return ((foldr (\x acc -> [fst x] ++ acc ) [] m))
    columnasEsp <- return (espaciar' ((length m)*espaciado) (foldr (\x acc -> x ++ " " ++ acc ) "" columnas))
    putStrLn columnasEsp
    --putStrLn separador
    -- Ahora colocamos la primera fila, enseñando el elemento al que corresponde
    -- y sus relacionados

    putStr $ ("" ++ (espaciar' (length columnasEsp)  (pintaDatos' (m!!n))) ++ "")
    putStrLn (fst (m!!n))
    dibujaMatriz' m (n+1)
  | n < length m =
    do
    -- Colocamos la siguiente fila
    espaciado <- return $ (espMax m)
    columnas <- return ((foldr (\x acc -> [fst x] ++ acc ) [] m))
    columnasEsp <- return (espaciar' ((length m)*espaciado) (foldr (\x acc -> x ++ " " ++ acc ) "" columnas))

    putStr $ ("" ++ (espaciar' (length columnasEsp) (pintaDatos' (m!!n))) ++ "")
    putStrLn (fst (m!!n))
    dibujaMatriz' m (n+1)
  -- Terminamos cerrando la tabla
  | otherwise = return ()
    --do
    --espaciado <- return $ espMax m
    --putStrLn ((replicate (espaciado+2) ' ' ) ++ (replicate ((length m)*2+1) '-'))

-- Esta función está hecha para el tipo Int. Si se prueba para, por ejemplo,
-- el tipo Char, el espaciado no está ajustado y la tabla se mostrará desplazada.
-- Se puede modificar para ajustarla, pero como haskell está fuertemente tipado,
-- es complicado trabajar con un tipo que no sabes cuál es.

pintaDatos':: Show a =>(a,[Int]) -> [Char]
pintaDatos' (_,[]) = ""
pintaDatos' (x,(v:vs)) = (show v) ++ " " ++ pintaDatos' (x,vs)

pintaNombreCol':: Show a =>[a] -> [Char]
pintaNombreCol' [] = ""
pintaNombreCol' (v:vs) = (show v) ++ " " ++ pintaNombreCol' vs

palabras :: String -> Int
palabras str = (length.words) str

espaciar':: Int -> String -> String
espaciar' n linea = if (palabras linea == 1 || length linea > n) then linea
  else concat $ zipWith (\p1 p2 -> p1 ++ p2)
  (foldr (\p lp-> if lp == [] then p:lp else (p ++ replicate tam_espacios ' '):lp) [] lista_palabras)
  (replicate sobrantes " " ++ replicate (num_espacios - sobrantes + 1) "")
  where lista_palabras = words linea
        longitud_palabras = map length lista_palabras
        caracteres_ocupados = sum longitud_palabras
        num_espacios = length lista_palabras - 1
        tam_espacios = div (n - caracteres_ocupados) num_espacios
        sobrantes = mod (n - caracteres_ocupados) num_espacios


















-------


pares x
  | x/10 >= 10 = if even (mod x 10) then (mod x 10):pares (x/10) else pares (x/10)
  | otherwise = if even x then [x] else []
