module WaveguideUtils where

-- Returns the floor of the square root of an integer.
iSqrt :: Integral a => a -> a
iSqrt = floor . sqrt . fromIntegral
