> module Main where

> import Logic

> import Control.Monad (replicateM)
> import System.Environment

> -- Começa o jogo com os parametros de entrada
> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         [sizeStr, minesStr] -> do
>             let size = read sizeStr :: Int
>                 mines = read minesStr :: Int
>             field <- createField size mines
>             playGame False False field
>         _ -> putStrLn "Usage: cabal run LPHaskell -- <size> <mines>"

> -- Verifica as condições de termino
> playGame :: Bool -> Bool -> Field -> IO ()
> playGame isGameOver isGameWon field = do
>   if isGameOver 
>       then doGameOverActions field
>       else if isGameWon
>           then do
>               putStrLn (visualizeField (reverse field))
>               putStrLn "Parabens! Voce venceu!"
>           else playGame2 isGameOver isGameWon field

> -- Pega a jogada, atualiza o campo e realiza o loop (playGame -> playGame2 -> playGame)
> playGame2 :: Bool -> Bool -> Field -> IO ()
> playGame2 isGameOver isGameWon field = do
>   putStrLn (visualizeField (reverse field))
>   putStrLn "Digite sua jogada: "
>   input <- getLine
>   let move = parseMove input
>   case move of
>      Just (action, col, row) -> do
>          let updatedField = makeMove field action row col
>              isGameOver = checkIfGameOver updatedField
>              isGameWon = checkIfGameWon updatedField
>          playGame isGameOver isGameWon updatedField
>      Nothing -> do
>          playGame isGameOver isGameWon field 

> -- Abre todas as celulas e mensagem de game over
> doGameOverActions :: Field -> IO ()
> doGameOverActions field = do
>   let updatedField = map (map updateCell) field
>   putStrLn (visualizeField (reverse updatedField))
>   putStrLn "Game Over!"
>   where
>     updateCell :: Cell -> Cell
>     updateCell cell
>       | hasMine cell = cell { isOpen = False, isMarkedAsMine = True }
>       | otherwise = cell { isOpen = True, isMarkedAsMine = False }
