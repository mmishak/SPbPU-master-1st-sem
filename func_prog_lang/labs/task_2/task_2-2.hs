myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x [] = x
myFoldl f x (head:list) = myFoldl f (f x head) list

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f x []     = x 
myFoldr f x (head:list) = f head (myFoldr f x list) 

myMap :: (a -> b) -> [a] -> [b]
myMap f a = myFoldr (\h list -> (f h) : list) [] a

myFlatMap :: (a -> [b]) -> [a] -> [b] 
myFlatMap f a = myFoldr (\h list -> (f h) ++ list) [] a 

myConcat :: [a] -> [a] -> [a]
myConcat a b = myFoldl (\list h -> list ++ [h]) a b 

myFilter :: (a -> Bool) -> [a] -> [a]  
myFilter f a = myFoldl (\list h -> list ++ (if f h then [h] else [])) [] a

myMaxBy :: (a -> Integer) -> [a] -> a 
myMaxBy f a = myFoldl (\x y -> if f x > f y then x else y) (myElementAt 0 a) a

myMinBy :: (a -> Integer) -> [a] -> a 
myMinBy f a = myFoldl (\x y -> if f x < f y then x else y) (myElementAt 0 a) a

myReverse :: [a] -> [a]
myReverse a = myFoldl (\list h -> h : list) [] a  

myElementAt :: Integer -> [a] -> a
myElementAt index (head:list) = snd $ myFoldl (\(j, x) y -> if j < 0 then (j-1, x) else (j-1, y)) (index-1, head) list

myIndexOf :: String -> [String] -> Integer
myIndexOf str array = if snd p then fst p else (-1) where
          p = myFoldl 
                (\x element -> if snd x == False 
                                     then if str == element 
                                                then (fst x, True) 
                                                else (fst x + 1, False)
                                     else x
                ) 
                (0, False) 
                array
 