integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * culc where
                               h = (b - a) / 1000
                               culc = (f a + f b) / 2 + funcSum 0 999
                               funcSum acc n | n <= 0 = acc
                                             | otherwise = funcSum (acc + f (a + h * n)) (n - 1)
