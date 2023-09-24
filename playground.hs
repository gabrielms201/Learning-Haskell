-- intere [] ls = []
-- 
-- intere (x:xs) ls
--     | intera x ls = x : intere xs ls
--     | otherwise = intere xs ls
-- 
-- intera e [] = False
-- intera e (a:x)
--     | e == a = True
--     | otherwise = intera e x

data T = F Integer | N Integer T T
    deriving (Show, Eq)


main :: IO()
main = do 
    let t1 = N 7 (N 5 (F 3)(F 4)) (F 9)
    print t1