maximo_subpalindromo :: [Char]-> Int
maximo_subpalindromo string = foldr max 0 resultados
    where
        longitud = (length string) - 1

        substrings = [(y,x) | y <- [0..longitud], x <- [(y+1)..longitud]]
        
        insertar :: Int -> Int -> Int -> [[Int]] -> [[Int]]
        insertar x y value matriz = [if i == x then insertar_row column else column | (i, column) <- zip [0..] matriz]
            where
                insertar_row column = [if j == y then value else element | (j, element) <- zip [0..] column]
        
        int True = 1
        int False = 0

        pali :: (Int, Int) -> [[Int]] -> ([[Int]], Int)       
        pali x y dp = if value == 1
            then (dp1, int (string!!x == string!!y))
            else (dp1, 0)
            where 
                (dp1, value) = palindromo (x+1) (y-1) dp
        
        palindromo :: (Int, Int) -> [[Int]] -> ([[Int]], Int)       
        palindromo x y dp = if dp!!x!!y /= -1
            then dp!!x!!y
            else if x == y
                then (insertar x y 1 dp1, 1)
                else (insertar x y value dp1, value)
                    where
                        (dp1, value) = if x == (y-1) 
                            then (dp, int (string!!x == string!!y)) 
                            else pali x y dp
        
        largoSubstring :: (Int, Int) -> [[Int]] -> ([[Int]], Int)
        largoSubstring (x, y) dp = if value == 1
            then (dp1, y-x)
            else (dp1, 0)
            where
                (dp1, value) = palindromo x y dp

        calcularResultados dp [] = []
        calcularResultados dp (x:xs) = value:(calcularResultados dp1 xs)
            where
                (dp1, value) = largoSubstring x dp

        dp = [[-1 | i <- [0..longitud]] | j <- [0..longitud]]

        resultados = calcularResultados dp substrings