

printBoard :: [[Char]] -> IO ()
printBoard board = do
    putStrLn "  1 2 3 4 5 6 7 8"
    printRows board 1

printRows :: [[Char]] -> Int -> IO ()
printRows [] _ = return ()
printRows (row:rows) n = do
    putStr (show n ++ " ")
    printRow row
    putStrLn ""
    printRows rows (n+1)

printRow :: [Char] -> IO ()
printRow [] = return ()
printRow (x:xs) = do
    putChar x
    putChar ' '
    printRow xs

transform :: [[Char]] -> String -> [[Char]]
transform board notation = transform' board notation 0 0

transform' :: [[Char]] -> String -> Int -> Int -> [[Char]]
transform' board [] _ _ = board
transform' board (x:xs) row col
    | x >= '1' && x <= '8' = transform' board xs row (col + read [x])
    | x == '/' = transform' board xs (row + 1) 0
    | otherwise = transform' (replace board row col x) xs row (col + 1)

replace :: [[Char]] -> Int -> Int -> Char -> [[Char]]
replace board row col x = 
    let (before, current:after) = splitAt row board
        (left, _:right) = splitAt col current
    in before ++ [left ++ [x] ++ right] ++ after

main :: IO ()
main = do
    let notation = "rnbqkbn1/pppp1pp1/4p2r/7p/3P2P1/8/PPPKPP1P/RNBQ1BNR"
    let emptyBoard = replicate 8 (replicate 8 ' ')
    let board = transform emptyBoard notation
    printBoard board