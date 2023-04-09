

import Data.List
printBoard :: [[Char]] -> IO ()
printBoard board = do
    putStrLn "  0 1 2 3 4 5 6 7"
    printRows board 0

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


findKing :: [[Char]] -> (Int, Int)
findKing board = 
    let king = head $ filter (\x -> board !! (fst x) !! (snd x) == 'K') [(i,j) | i <- [0..7], j <- [0..7]]
    in case king of
        (a,b) -> (a,b)



isLower :: Char -> Bool
isLower c = c `elem` ['a'..'z']

-- pawnAttacks :: [[Char]] -> (Int, Int) -> [(Int, Int)]
-- pawnAttacks board (x,y)
--     | color == 'w' = filter (\(i,j) -> i == x-1 && abs(j-y) == 1) [(x-1,y-1), (x-1,y+1)]
--     | otherwise = filter (\(i,j) -> i == x+1 && abs(j-y) == 1) [(x+1,y-1), (x+1,y+1)]
--     where color = if isLower (board !! x !! y) then 'b' else 'w'

pawnAttacks :: [[Char]] -> (Int, Int) -> [(Int, Int)]
pawnAttacks board (x,y) = filter (\(i,j) -> i == x-1 && abs(j-y) == 1) [(x-1,y-1), (x-1,y+1)]


bishopAttacks board (x,y) = filter isValidPosition [(i,j) | i <- [0..7], j <- [0..7], abs(i-x) == abs(j-y), i /= x, j /= y]
    where isValidPosition (i, j) = board !! i !! j == ' ' || isLower (board !! i !! j) /= isLower (board !! x !! y)


-- encontra as posicoes dos elementos com valor 'b' na matriz
bishopPositions :: [[Char]] -> [(Int, Int)]
bishopPositions board = 
    let indices = findIndices (== 'b') (concat board)
        positions = map (\idx -> (idx `div` 8, idx `mod` 8)) indices
    in positions

-- chama bishopAttacks para cada posicao encontrada
allBishopAttacks :: [[Char]] -> [[(Int, Int)]]
allBishopAttacks board = map (\pos -> bishopAttacks board pos) (bishopPositions board)

-- getAttackPositions board (x,y)
--     | piece == 'b' = bishopAttacks board (x,y)
--     | otherwise = []
--     where piece = board !! x !! y

getAttackPositions board (x,y) = do
    let bishop = bishopAttacks board (x,y)
    any (\(i, j) -> board !! i !! j == 'b')  bishop
-- isWhiteKingInCheck :: [[Char]] -> Bool
-- isWhiteKingInCheck board = 
--     let (row, col) = findKing board
--         opponentPieces = ['p', 'n', 'h', 'b', 'q', 'k'] -- peças do jogador adversário
--         attacksKing = any (\(i,j) -> board !! i !! j `elem` opponentPieces) (getAttackPositions board (row,col))
--     in attacksKing -- retorna True se o rei branco estiver sob ataque

-- isWhiteKingInCheck :: [[Char]] -> Bool
-- isWhiteKingInCheck board = 
--     let (row, col) = findKing board
--         opponentPieces = ['p', 'n', 'h', 'b', 'q', 'k'] -- peças do jogador adversário
--         attacksKing = any (\(i,j) -> board !! i !! j `elem` opponentPieces) (getAttackPositions board (row,col))
--     in attacksKing -- retorna True se o rei branco estiver sob ataque


main :: IO ()
main = do
    --let notation = "rnbqkbn1/pppp1pp1/4p2r/7p/3P2P1/8/PPPKPP1P/RNBQ1BNR"
    --let notation = "rnbqkbnr/ppp1p1pp/8/5p2/3pP3/4K3/PPPP1PPP/RNBQ1BNR" -- Pawn check test 
    let notation = "rn2k1nr/ppp3pp/4b3/P2p1pq1/1P6/3P4/2P2bPP/RN1QKBNR" -- Bishop check test
    --let notation = "rnbqkbnr/ppp1p1pp/8/5p2/3pP3/4K3/PPPP1PPP/RNBQ1BNR" --bug
    let emptyBoard = replicate 8 (replicate 8 ' ')
    let board = transform emptyBoard notation
    printBoard board
    let value = findKing board
    print value 
    --print(isWhiteKingInCheck board)
    print(getAttackPositions board value)
    print(bishopAttacks board (6,5))
    --print(any (\(i,j) -> board !! i !! j `elem` ['p', 'n', 'h', 'b', 'q', 'k']) (getAttackPositions board value))
    --print (value `elem` getAttackPositions board value)
    --print (value `elem` allBishopAttacks board)
    --print(pawnAttacks board value)
