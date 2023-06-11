Autores: Daniel Baeta e Thiago Cabral
Matrícula: 201965122C e 201965220AC

> module Logic where

> import Utils

> import Data.Char (ord,isDigit)
> import Data.List

> data Cell = Cell {
>   row :: Int,
>   col :: Char,
>   hasMine :: Bool,
>   isOpen :: Bool,
>   isMarkedAsMine :: Bool,
>   minesAround :: Int
> } deriving (Eq, Show)
> 
> type Field = [[Cell]]

> -- Cria o campo (celula vazia -> randomiza minas -> conta as minas adjacentes)
> createField :: Int -> Int -> IO Field
> createField size numMines = do
>   let ajustedSize = max 2 (min size 26)
>       positions = [(row, col) | row <- [1..ajustedSize], col <- take ajustedSize ['A'..]]
>   let emptyCells = map createCell positions
>   randomizedIndexes <- randomizeMineIndexes (ajustedSize*ajustedSize) numMines
>   let cells = updateHasMinesList emptyCells randomizedIndexes
>       field = chunksOf ajustedSize cells
>       updatedField = countNeighborsWithMine field
>   return updatedField

> -- Cria a celula com suas coordenadas e com as condições inicias 
> createCell :: (Int, Char) -> Cell
> createCell (row, col) = Cell { row = row, col = col, hasMine = False, isOpen = False, isMarkedAsMine = False,  minesAround = 0}

> -- Verifica a condição de derrota, se a celula aberta é uma bomba
> checkIfGameOver :: Field -> Bool
> checkIfGameOver field = any cellIsGameOver (concat field)
>   where
>     cellIsGameOver :: Cell -> Bool
>     cellIsGameOver cell = isOpen cell && hasMine cell

> -- Verifica a condição de vitoria, se todas as bombas estão marcadas e as outras celulas abertas
> checkIfGameWon :: Field -> Bool
> checkIfGameWon field = all cellIsGameWon (concat field)
>   where
>     cellIsGameWon :: Cell -> Bool
>     cellIsGameWon cell
>       | isMarkedAsMine cell && hasMine cell = True
>       | isOpen cell && not (hasMine cell) = True
>       | otherwise = False

> -- Realiza a ação do usuário
> makeMove :: Field -> String -> Int -> Char -> Field
> makeMove field action targetRow targetCol = updatedField
>   where
>     updatedField = map (map updateCell) field
>     updateCell cell
>       | rowMatches && colMatches = case action of
>           "*" -> if not (isMarkedAsMine cell)
>                   then cell { isOpen = True }
>                   else cell
>           "+" -> if countMarkedMines field < numMines
>                   then cell { isMarkedAsMine = True }
>                   else cell
>           "-" -> cell { isMarkedAsMine = False }
>           _   -> cell
>       | otherwise = cell
>       where
>         rowMatches = row cell == targetRow
>         colMatches = col cell == targetCol
>         numMines = countMines field

> -- Recebe a String de entrada e divide nas possibilidades de jogada
> parseMove :: String -> Maybe (String, Char, Int)
> parseMove input =
>   case input of
>     ('+':move) -> parseActionMove "+" move
>     ('-':move) -> parseActionMove "-" move
>     move -> parseActionMove "*" move
>   where
>     parseActionMove action move = do
>       let parsedCol = head move
>       parsedRow <- parseRow (tail move)
>       Just (action, parsedCol, parsedRow)
>       where
>           parseRow col
>               | all isDigit col = Just (read col)
>               | otherwise = Nothing

> -- Atualiza as celulas que possuem mina
> updateHasMinesList :: [Cell] -> [Int] -> [Cell]
> updateHasMinesList xs indices = zipWith updateMineCell xs (map (`elem` indices) [0..])

> updateMineCell :: Cell -> Bool -> Cell
> updateMineCell cell True = cell { hasMine = True}
> updateMineCell cell False = cell


> -- Conta o número de vizinhos com minas para cada célula
> countNeighborsWithMine :: Field -> Field
> countNeighborsWithMine field =
>   let updateCellNeighbors cell =
>         let neighbors = getNeighbors field cell
>             neighborsWithMine = filter hasMine neighbors
>         in cell { minesAround = length neighborsWithMine }
>   in map (map updateCellNeighbors) field


> -- Obtém as células vizinhas na horizontal e vertical (excluindo as diagonais)
> getNeighbors :: Field -> Cell -> [Cell]
> getNeighbors field cell =
>   let (cellRow, cellCol) = (row cell, col cell)
>       neighbors = filter (\c -> abs (row c - cellRow) + abs (ord (col c) - ord cellCol) == 1)  (concat field)
>   in filter (/= cell) neighbors

> -- Conta quantas minas existem no campo
> countMines :: Field -> Int
> countMines field = sum [1 | cell <- concat field, hasMine cell]

> -- Conta quantas celulas já foram marcadas como minas
> countMarkedMines :: Field -> Int
> countMarkedMines field = length [cell | cell <- concat field, isMarkedAsMine cell]

> -- Monta a vizualização do campo
> visualizeField :: Field -> String
> visualizeField field = intercalate "\n" $ map visualizeRow field

> visualizeRow :: [Cell] -> String
> visualizeRow rowCells = concatMap visualizeCell rowCells ++ "\n"
> 
> visualizeCell :: Cell -> String
> visualizeCell cell
>   | isOpen cell = show (minesAround cell) ++ " "
>   | isMarkedAsMine cell = "B "
>   | otherwise = "* "
