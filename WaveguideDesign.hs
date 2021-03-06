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

-- Calculates the "height" of the detector, from the bottom of the 
-- amplifier to the top of the active region.
detectorHeight :: Detector -> Integer
detectorHeight d = h
  where
    h = (ampDepth d) + (castBendCenterline d) + (WP.wr42FlangeHalfHeight)
    
-- Calculates the position of the base of the amplifier when the corners    
-- of the amp are touching the G10 bore.
ampCornerPoint :: Detector -> MagnetBore -> Integer    
ampCornerPoint d b = WU.leg (boreRadius b) (quot (ampWidth d) 2)
    
-- A detector is "compatible" with a bore if when the amplifier corners
-- are touching the inner bore, the active region is more than tol mils
-- from the bore wall.
compatible :: Detector -> MagnetBore -> Integer -> Bool
compatible d b tol
  | headroom d b + tol < 0 = True
  | otherwise              = False                           
      
-- Calculates the available headroom for a detector in a bore, which is
-- the distance from the G10 bore to the top of the active region.
headroom :: Detector -> MagnetBore -> Integer     
headroom d b
  | fit > 0 = 0
  | otherwise = fit
    where
      fit = (detectorHeight d) - (ampCornerPoint d b) - (boreRadius b)
      
-- Calculates the position of the trap for a set of given dimensions      
detectorTrapPosition :: Detector -> MagnetBore -> Integer
detectorTrapPosition d b
  | clear < 0 = (boreRadius b) + clear - (WP.wr42FlangeHalfHeight)
  | otherwise = error "detector is too big!"
    where
      clear = headroom d b
                
-- Tell us the answer!            
main :: IO ()
main = do
  putStrLn . show $ detectorTrapPosition detector bore
  putStrLn . show $ willItWork
  where
    willItWork = compatible detector bore 500
    detector = Detector {ampWidth = WP.kh3Width, 
                         ampLength = WP.kh3Length, 
                         ampDepth = WP.kh3Depth, 
                         castBendCenterline = 440}
    bore = MagnetBore {boreRadius = MP.stdBoreRadius}