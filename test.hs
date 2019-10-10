insertar x y value matriz = [if i == x then insertar_row column else column | (i, column) <- zip [0..] matriz]
    where
        insertar_row :: [Int] -> [Int]
        insertar_row column = [if j == y then value else element | (j, element) <- zip [0..] column]
