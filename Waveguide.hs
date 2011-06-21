-- Waveguide.hs
-- file which contains code for the Waveguide data type.

-- a data type which encodes the aspect that the waveguide actually takes
data Aspect = Circular | Rectangular
            deriving Show

-- the waveguide data type.  it is an aspect and either one or two numbers.
data Waveguide = Waveguide {shape :: Aspect, 
                            broadDim,shortDim :: Int}
               deriving Show