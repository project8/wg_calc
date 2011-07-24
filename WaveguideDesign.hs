-- Utility functions such as integer square root, etc etc
import qualified WaveguideUtils as WU
import qualified WaveguideParameters as WP
import qualified MagnetParameters as MP

-- A parameterization of the magnet bore.
data MagnetBore = MagnetBore {boreRadius :: Integer} deriving Show

-- The detector can be characterized by a handful of physical parameters.
data Detector = Detector {ampWidth :: Integer,
                          ampLength :: Integer,
                          ampDepth :: Integer,
                          castBendCenterline :: Integer}              
              deriving Show

-- Just a convenient 2-d point representation
data Position = Position {x :: Integer, y :: Integer} deriving (Show, Eq)

-- Want to add two positions?
(+++) :: Position -> Position -> Position
(+++) (Position {x=x1,y=y1}) (Position {x=x2,y=y2}) = addedPos
  where
    addedPos = (Position {x=x1+x2,y=y1+y2})
    
-- Converts from 2-d position to radius
posRadius :: Position -> Integer
posRadius Position {x = ex, y = ey} = WU.iSqrt (ex^2 + ey^2)

-- Calculates the "height" of the detector, from the bottom of the 
-- amplifier to the top of the active region.
detectorHeight :: Detector -> Integer
detectorHeight d = h
  where
    h = (ampDepth d) + (castBendCenterline d) + (WP.wr42FlangeHalfHeight)
    
-- A detector is "compatible" with a bore if when the amplifier corners
-- are touching the inner bore, the active region is more than 50 mils
-- from the bore wall.
compatible :: Detector -> MagnetBore -> Bool
compatible d (MagnetBore {boreRadius = r}) 
  | ((2*r) - y_base) > (detectorHeight d + 100) = True
  | otherwise           = False                           
    where
      y_base = WU.iSqrt (r^2 - (quot (ampWidth d) 4)^2)
      
-- Tell us the answer!            
main :: IO ()
main = do
  putStrLn . show $ willItWork
  where
    willItWork = compatible detector bore
    detector = Detector {ampWidth = WP.kh3Width, 
                         ampLength = WP.kh3Length, 
                         ampDepth = WP.kh3Depth, 
                         castBendCenterline = 0}
    bore = MagnetBore {boreRadius = MP.stdBoreRadius}