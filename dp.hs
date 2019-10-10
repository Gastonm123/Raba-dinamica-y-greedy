maximo_subpalindromo :: [Char] -> Int
maximo_subpalindromo string = foldr max 0 resultados
    where
        longitud = (length string) - 1

        substrings = [(y,x) | y <- [0..longitud], x <- [(y+1)..longitud]]

        -- funcion para insertar un valor calculado en la matriz
        insertar :: Int -> Int -> Int -> [[Int]] -> [[Int]]
        insertar x y value matriz = [if i == x then insertar_col column else column | (i, column) <- zip [0..] matriz]
            where
                insertar_col column = [if j == y then value else element | (j, element) <- zip [0..] column]

        -- funcion para castear bool a int
        int :: Bool -> Int
        int True = 1
        int False = 0

        -- funcion auxiliar de palindromo
        pali :: Int -> Int -> [[Int]] -> ([[Int]], Int)
        pali x y dp = if value == 1
            then (dp1, int (string!!x == string!!y))
            else (dp1, 0)
            where
                (dp1, value) = palindromo (x+1) (y-1) dp

        -- Funcion q toma un x e y que marcan inicio y fin de un substring
        -- y dice si es palindromo (devolviendo 1 para True y 0 para False)
        -- Ademas esta funcion utiliza la matriz de memorizacion llamada dp
        -- Si en la matriz se cambio el valor por defecto (-1) es q ya fue calculada
        -- la funcion para los parametros pasados, entonces se devuelve el valor de dp
        palindromo :: Int -> Int -> [[Int]] -> ([[Int]], Int)
        palindromo x y dp = if dp!!x!!y /= -1
            then (dp, dp!!x!!y)
            else if x == y
                then (insertar x y 1 dp, 1)
                else (insertar x y value dp1, value)
                    where
                        (dp1, value) = if x == (y-1)
                            then (dp, int (string!!x == string!!y))
                            else pali x y dp

        -- funcion que calcula el largo del substring si es palindromo, sino 0
        largoSubstring :: (Int, Int) -> [[Int]] -> ([[Int]], Int)
        largoSubstring (x, y) dp = if value == 1
            then (dp1, y-x+1)
            else (dp1, 0)
            where
                (dp1, value) = palindromo x y dp

        -- funcion parecida a map pero q pasa la dp en cada llamada
        calcularResultados dp [] = []
        calcularResultados dp (x:xs) = value:(calcularResultados dp1 xs)
            where
                (dp1, value) = largoSubstring x dp

        dp = [[-1 | i <- [0..longitud]] | j <- [0..longitud]]

        resultados = calcularResultados dp substrings