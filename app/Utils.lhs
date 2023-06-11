> module Utils where

> import System.Random

> -- Randomiza os indices que terão minas no jogo
> randomizeMineIndexes :: Int -> Int -> IO [Int]
> randomizeMineIndexes n givenNumMines = do
>   gen <- newStdGen
>   let half = n `div` 2
>       numMines = max 1 (min givenNumMines half)
>       randomizedIndices = takeRandomIndices gen n numMines
>   return randomizedIndices
> 
> takeRandomIndices :: RandomGen g => g -> Int -> Int -> [Int]
> takeRandomIndices gen n k = take k $ removeDuplicates $ randomIndices gen n
> 
> randomIndices :: RandomGen g => g -> Int -> [Int]
> randomIndices gen n = take n $ randomRs (0, n-1) gen
> 
> removeDuplicates :: Eq a => [a] -> [a]
> removeDuplicates [] = []
> removeDuplicates (x:xs)
>   | x `elem` xs = removeDuplicates xs
>   | otherwise = x : removeDuplicates xs

> -- Pega uma lista e a transforma em uma lista de listas do tamanho desejado
> chunksOf :: Int -> [a] -> [[a]]
> chunksOf _ [] = []
> chunksOf n xs = take n xs : chunksOf n (drop n xs)

