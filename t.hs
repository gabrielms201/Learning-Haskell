import Data.IntMap.Merge.Lazy (lmapWhenMissing)
-- data T = N Integer T T | F Integer
--     deriving Show
-- 
-- t2 = N 7 (N 5 (F 3)(F 4)) (F 9)
-- 
-- 
-- 
-- leaf e (F f) 
--     | f == e = True
--     | otherwise = False
-- leaf e (N n te td) = leaf e te || leaf e td



-- l1 = [3,3,3,1,1,2,3]
-- 
-- 
-- del [a] = [a]
-- del [] = []
-- del (x:xs:ys)
--     | x == xs = del (xs:ys)
--     | otherwise = x : del (xs:ys)



l2 = [9,3,2,10,4,3]

quick [] = []
quick ls = quick lme ++ [pivo] ++ quick lma
    where lme = (\(x,y,z) -> x) tripla
          pivo = (\(x,y,z) -> y ) tripla
          lma = (\(x,y,z)-> z) tripla
          tripla = par ls

par (x:xs) = partit xs x [] []


partit [] p lme lma = (lme, p, lma)
partit (x:xs) p lme lma
    | x <= p = partit xs p (x:lme) lma
    | x > p = partit xs p lme (x:lma)
