-- Bore radius
boreRadius :: Int
boreRadius = 1175

-- These functions return the dimensions of the amplifier.  NOT FREE.
ampWidth :: Int
ampWidth = 1800

ampLength :: Int
ampLength = 1100

ampDepth :: Int
ampDepth = 500

-- The radius of the cast bends as made by Penn engineering.
castBendRadius :: Int
castBendRadius = 170

-- The radius of WR42 flanges
wr42FlangeRadius :: Int
wr42FlangeRadius = 438

-- Returns the position of the corner with the depth of the plate, the
-- top of the amplifier, and the bending radius of the copper spring.
rCorner :: Int -> Int -> Int -> Int
rCorner plateDepth ampSurface springRadius = floor . sqrt $ x^2 + y^2
  where
    x = fromIntegral (plateDepth + ampSurface + springRadius + ampDepth)
    y = (fromIntegral ampWidth/2.0)
    
-- Returns the distance from the G10 bore for a given set of dimensions
