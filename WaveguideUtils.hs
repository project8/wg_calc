module WaveguideUtils where

-- Returns the floor of the square root of an integer.
iSqrt :: Integral a => a -> a
iSqrt = floor . sqrt . fromIntegral

-- Pythagorean functions. 

-- Given two legs, calculate the hypotenuse.
hyp :: Integer -> Integer -> Integer
hyp a b = iSqrt (a^2 + b^2)

-- Given a hypotenuse and one leg, calculate the other leg.
leg :: Integer -> Integer -> Integer
leg r a = iSqrt (r^2 - a^2)
