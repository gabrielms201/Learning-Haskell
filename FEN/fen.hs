
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
    let king = head $ filter (\x -> board !! fst x !! snd x == 'K') [(i,j) | i <- [0..7], j <- [0..7]]
    in case king of
        (a,b) -> (a,b)



isLower :: Char -> Bool
isLower c = c `elem` ['a'..'z']


pawnAttacks :: [[Char]] -> (Int, Int) -> [(Int, Int)]
pawnAttacks board (x,y) = filter (\(i,j) -> i == x-1 && abs(j-y) == 1) [(x-1,y-1), (x-1,y+1)]


bishopAttacks :: [[Char]] -> (Int, Int) -> [(Int, Int)]
bishopAttacks board (x,y) = filter isValidPosition [(i,j) | i <- [0..7], j <- [0..7], abs(i-x) == abs(j-y), i /= x, j /= y]
    where isValidPosition (i, j) = board !! i !! j == ' ' || isLower (board !! i !! j) /= isLower (board !! x !! y)

--TODO
knightAttacks :: [[Char]] -> (Int, Int) -> [(Int, Int)]
knightAttacks board (x,y) = filter isValidPosition [(i,j) | i <- [0..7], j <- [0..7], abs(i-x) + abs(j-y) == 3, abs(i-x) /= 0, abs(j-y) /= 0, i /= x, j /= y]
    where isValidPosition (i, j) = board !! i !! j == ' ' || isLower (board !! i !! j) /= isLower (board !! x !! y)


rookAttacks :: [[Char]] -> (Int, Int) -> [(Int, Int)]
rookAttacks board (x,y) = filter isValidPosition [(i,j) | i <- [0..7], j <- [0..7], (i == x || j == y) && (i /= x || j /= y)]
    where isValidPosition (i, j) = board !! i !! j == ' ' || isLower (board !! i !! j) /= isLower (board !! x !! y)


queenAttacks :: [[Char]] -> (Int, Int) -> [(Int, Int)]
queenAttacks board (x,y) = bishopAttacks board (x,y) ++ rookAttacks board (x,y)


isKingInCheck :: [[Char]] -> (Int, Int) -> Bool
isKingInCheck board (x,y) = do
    -- Armazena cada ataque possivel de cada inimigo
    let possibleBishopAttacks = bishopAttacks board (x,y)
    let possiblePawnAttacks = pawnAttacks board (x,y)
    let possibleKnightAttacks = knightAttacks board (x,y)
    let possibleRookAttacks = rookAttacks board (x,y)
    let possibleQueenAttacks = queenAttacks board (x,y)
    let enemyPieces = ['b', 'p', 'n', 'r', 'q', 'k']
    -- Verifica para cada peca inimiga se eh possivel atacar o rei Branco
    -- Um rei nunca pode checar o outro rei
    any (\(i, j) -> board !! i !! j == 'b') possibleBishopAttacks || 
        any (\(i, j) -> board !! i !! j == 'p') possiblePawnAttacks ||
        any (\(i, j) -> board !! i !! j == 'n') possibleKnightAttacks ||
        any (\(i, j) -> board !! i !! j == 'q') possibleQueenAttacks ||
        any (\(i, j) -> board !! i !! j == 'r') possibleRookAttacks


main :: IO ()
main = do
    -- Captura a notacao
    -- Notacoes de teste.
    --let notation = "rnbqkbnr/ppp1p1pp/8/5p2/3pP3/4K3/PPPP1PPP/RNBQ1BNR"   -- Pawn check test  | OK
    --let notation = "rn2k1nr/ppp3pp/4b3/P2p1pq1/1P6/3P4/2P2bPP/RN1QKBNR"   -- Bishop check test | OK
    --let notation = "rnb1kbnr/pppp1ppp/4p3/8/7q/5P1N/PPPPP1PP/RNBQKB1R"    -- Queen check test | OK
    --let notation =  "2b1k1nr/2p2ppp/2p1p3/3q4/5PP1/2PPP3/7P/r3KBNR"       -- Rook check test | OK
    --let notation = "r1bqkb1r/p1pppppp/1p6/8/1P4n1/P1PP1n2/R3PPPP/1NBQKBNR"  -- Knight check test | OK
    --let notation = "rn1qkbnr/bpp1p1pp/8/5p2/4P3/8/PPPPKPPP/RNBQ1BNR" -- NotCheck | OK
    --let notation = "rnb1kb1r/p1p1pp2/3p3p/1p1n2p1/QPqP1P1P/5NK1/P3P1P1/RNB2B1R" -- NotCheck | OK
    print "Digite a notacao FEN: (exemplo: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR): "
    notation <- getLine
    -- Cria o tabuleiro vazio
    let emptyBoard = replicate 8 (replicate 8 ' ')
    -- Converte a notacao em tabuleiro
    let board = transform emptyBoard notation
    -- Imprime o estado do tabuleiro
    printBoard board


    -- Valida a posicao do rei
    let kingPosition = findKing board
    print "King Position"
    print kingPosition
    -- Imprime as posicoes que cada peca deve estar para poder atacar o rei
    print "Precisamos ter as seguintes pecas nas seguintes posicoes para ter check: "
    print "Bishop Attacks"
    print(bishopAttacks board kingPosition)
    print "Pawn Attacks"
    print(pawnAttacks board kingPosition)
    print "Knight Attacks"
    print(knightAttacks board kingPosition)
    print "Rook Attacks"
    print(rookAttacks board kingPosition)
    print "Queen Attacks"
    print(queenAttacks board kingPosition)

    -- Verifica se o rei esta em cheque
    putStr "\n\n---> IS WHITE KING IN CHECK?\n"
    print(isKingInCheck board kingPosition)
