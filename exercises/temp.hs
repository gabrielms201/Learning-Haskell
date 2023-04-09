
inter  _ [] = []
inter  [] _ = []
inter (a:x) ls
    | isElem a ls = a : inter x ls
    | otherwise = inter x ls


isElem e [] = False
isElem e (a:x) 
    | e == a = True
    | otherwise = isElem e x