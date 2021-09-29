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

generaGE:: Ord a => [a] -> Rel a
generaGE xs = R [(x,y) | x<-xs, y<-xs, x>=y]

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

-- Tras varios días probando, he optado por poner también esta forma de mostrar
-- las tablas. No he cambiado la entrada de que sea el tipo (Int, Int), simplemente
-- he dado más funcionalidad a la función dibujaMatriz'. Ahora dibujaMatriz' puede
-- dibujar la tabla de cualquier tipo de relación. Para llegar a este punto he
-- ido probando y "jugando" (que más bien me he peleado fuertemente) con los
-- Strings, para finalmente poder usar, por ejemplo:
-- dibujaMatriz' (creaMatriz' r1) 0, y que muestre su Matriz
-- dibujaMatriz' (creaMatriz' r6) 0, y también muestra su matriz.

-- La matriz de la relacion ahora viene dada por:
-- [(String, [Int])] donde String es el "nombre" de la fila (su elemento correspondiente)
-- y la lista [Int] es la fila formada por 0s y 1s.
creaMatriz':: (Show a,Eq a) => Rel a -> [(String, [Int])]
creaMatriz' (R xs) = [(show f,[(if elem (f,c) xs then 1 else 0) |  c<-soporte (R xs) ]) |f<-soporte (R xs)]

-- Esta funcion nos da la longitud de
espMax::[(String,[Int])] -> Int
espMax [] = 0
espMax (x:xs) = max (length (fst x)) (espMax xs)
-- espMax xs = maximum (map (\(a, ys)-> length a) xs) -- Otra forma con orden sup
-- hay que mantener ajute patrones con [] para no hacer maximum de []


dibujaMatriz':: [(String,[Int])] -> Int -> IO ()
dibujaMatriz' m n
  | n == 0 =
    do
    espaciado <- return $ (espMax m) -- Calculo el nombre del elemento que mas ocupa
    columnas <- return ((foldr (\x acc -> [fst x] ++ acc ) [] m)) -- Calculo los nombres (en String) de las columas
    -- Formateo para que las columnas se ajusten a la longitud del nombre mas largo
    -- El fold se usa para añadir un " " entre nombres para poder aplicar espaciar'
    columnasEsp <- return (espaciar' ((length m)*espaciado)
                          (foldr (\x acc -> if acc=="" then x else x ++ " " ++ acc ) "" columnas))
    -- Imprimo con 3 espacios, que es lo que ocupa " | "
    putStrLn ("   " ++ columnasEsp)
    -- Creo la linea de la tabla que ocupa tanto como el espaciado de las columnas
    separador <- return $ (replicate (length columnasEsp) '-' )
    putStrLn ("   " ++ separador)
    -- Pinto los datos
    putStr $ (" | " ++ (espaciar' (length columnasEsp)  (pintaDatos' (m!!n))) ++ "| ")
    -- Escribo el nombre de la fila
    putStrLn (fst (m!!n))
    -- Seguimos con la siguiente fila
    dibujaMatriz' m (n+1)
  | n < length m =
    do
    espaciado <- return $ (espMax m)
    columnas <- return ((foldr (\x acc -> [fst x] ++ acc ) [] m))
    -- Igual que antes
    columnasEsp <- return (espaciar' ((length m)*espaciado)
                          (foldr (\x acc -> if acc=="" then x else x ++ " " ++ acc ) "" columnas))
    -- Igual que antes sin separar con lineas
    putStr $ (" | " ++ (espaciar' (length columnasEsp) (pintaDatos' (m!!n))) ++ "| ")
    putStrLn (fst (m!!n))
    dibujaMatriz' m (n+1)
  | otherwise =
    do
    espaciado <- return $ (espMax m)
    columnas <- return ((foldr (\x acc -> [fst x] ++ acc ) [] m))
    -- Igual que antes, pero solo cierro la tabla con lineas
    columnasEsp <- return (espaciar' ((length m)*espaciado)
                          (foldr (\x acc -> if acc=="" then x else x ++ " " ++ acc ) "" columnas))
    separador <- return $ (replicate (length columnasEsp) '-' )
    putStrLn ("   " ++ separador)

-- Paso a String las filas, 1 si están relacionados 0 si no
pintaDatos':: Show a =>(a,[Int]) -> [Char]
pintaDatos' (_,[]) = ""
pintaDatos' (x,(v:vs)) = (if v == 1 then (show 1) else show 0 )++ " " ++ pintaDatos' (x,vs)

-- Esto para conseguir el String de la primera fila (el nombre de las columnas)
pintaNombreCol':: Show a =>[a] -> [Char]
pintaNombreCol' [] = ""
pintaNombreCol' (v:vs) = (show v) ++ " " ++ pintaNombreCol' vs

-- Calcula cuantas palabras (separadas por espacio) hay en un String
palabras :: String -> Int
palabras str = (length.words) str

-- Dado un entero y un String, formatea el String para que ocupe el entero,
-- dejando con un buen formato de separacion las palabras
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







creaMatrizSencilla:: Eq a => Rel a -> [([Bool])]
creaMatrizSencilla (R xs) = [[(if elem (f,c) xs then True else False) |  c<-soporte (R xs) ] |f<-soporte (R xs)]

printMatriz m fila xs
  | fila == length m  = do putStrLn ("   "++ replicate (2*(length xs)) '-' ++ "-")
                           return ()
  | fila == 0 = do putStrLn ("    " ++ foldr (\x acc -> show x ++ " " ++ acc) "" xs)
                   putStrLn ("   "++ replicate (2*(length xs)) '-' ++ "-")
                   putStr (show (xs!!fila) ++ " | ")
                   miPrint (m!!fila)
                   printMatriz m (fila+1) xs
  | otherwise =  do putStr (show (xs!!fila) ++ " | ")
                    miPrint (m!!fila)
                    printMatriz m (fila+1) xs

miPrint [] = putStrLn "|"
miPrint (x:xs) = do
   putStr (if x then "x " else "o ")
   miPrint xs


dibujaMatriz2 [] sop pos = return ()
dibujaMatriz2 (fila:fs) sop pos = do putStr ( show (sop!!pos) ++ " | ")
                                     miPrint fila
                                     dibujaMatriz2 fs sop (pos+1)
