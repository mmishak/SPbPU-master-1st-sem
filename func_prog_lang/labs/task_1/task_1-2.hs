--
-- Наибольший общий делитель
--
nod :: Integer -> Integer -> Integer
nod a b = if a == b then a
					else if a > b then if a `mod` b == 0 then b 
														 else nod (a - b) b
								  else nod b a

--
-- Возведение целого числа в целую степень
--
toBinaryList :: Integer -> [Integer]
toBinaryList 0 = [0]
toBinaryList n | n `mod` 2 == 1 = toBinaryList (n `div` 2) ++ [1]
        	   | n `mod` 2 == 0 = toBinaryList (n `div` 2) ++ [0]

power :: Integer -> Integer -> Integer
power value pow = foldl (\x y -> if y == 1 then x^2*value else x^2) 1 (toBinaryList pow) 

--
-- Является ли дата корректной
-- 
isTrueDate :: Int -> Int -> Int -> Bool
isTrueDate day month year | day < 1 || month < 1 || year < 1 = False
						  | month > 12 = False
						  | month == 2 = if year `mod` 4 == 0 then day <= 29 else day <= 28
                          | otherwise = day <= daysInMonth!!(month - 1)
                          where daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
