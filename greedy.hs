type Arco = (Integer, Integer, Integer)

maximo_matching :: [Arco] -> Integer
maximo_matching arcos = maximizar_puntaje [] arcos_ordenados
    where
        merge :: [Arco] -> [Arco] -> [Arco]
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys) = if peso_x > peso_y
            then x:(merge xs (y:ys))
            else y:(merge (x:xs) ys)
                where
                    (_, _, peso_x) = x
                    (_, _, peso_y) = y

        split :: [Arco] -> ([Arco], [Arco])
        split [] = ([], [])
        split [x] = ([x], [])
        split (x:y:xs) = (x:a, y:b)
            where
                (a, b) = split xs

        mergesort :: [Arco] -> [Arco]
        mergesort [] = []
        mergesort [x] = [x]
        mergesort lista = merge (mergesort a) (mergesort b)
            where
                (a, b) = split lista
        
        arcos_ordenados = mergesort arcos

        esta :: Integer -> [Integer] -> Bool
        esta elemento [] = False
        esta elemento (x:xs) = if x == elemento 
            then True 
            else esta elemento xs

        -- Saco un arco y verifico si una de sus puntas no esta incluida ya
        -- Si no esta ninguna de las puntas, las incluyo y cuento el peso del arco
        maximizar_puntaje :: [Integer] -> [Arco] -> Integer
        maximizar_puntaje bolsa [] = 0
        maximizar_puntaje bolsa (arco:arcos) = if esta a bolsa || esta b bolsa
            then maximizar_puntaje bolsa arcos
            else peso + (maximizar_puntaje bolsa_actualizada arcos)
                where
                    (a, b, peso) = arco
                    bolsa_actualizada = a:b:bolsa

-- Matching maximo : 85
grafo = [(1, 2, 2),(1, 4, 17),(1, 5, 5),(2, 4, 7),(2, 3, 30),(3, 4, 1),(3, 6, 10),(4, 5, 16),(5, 6, 31),(4,6,50)]