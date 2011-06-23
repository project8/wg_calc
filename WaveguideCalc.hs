import Waveguide

-- converts an integer number of mils to
-- meters
mil_to_m :: Int -> Float
mil_to_m x = 0.0000254 * (fromIntegral x)

-- calculates the wavenumber of a given frequency
r_wavenumber :: Float -> Float
r_wavenumber f = (2 * pi * f) / 299792458.0

-- given rectangular waveguide dimensions, this calculates the
-- cutoff wavenumber for a given mode
r_cutoff_n :: Int -> Int -> Int -> Int -> Float
r_cutoff_n broadDim shortDim broadIdx shortIdx =
  (*) pi $ sqrt $ sqFirstTerm + sqSecondTerm 
  where
    sqFirstTerm = (fromIntegral broadIdx) / (mil_to_m broadDim)
    sqSecondTerm = (fromIntegral shortIdx) / (mil_to_m shortDim)
    
-- given circular waveguide radius in mils, this calculates the cutoff 
-- wavenumber for a given n,m
c_cutoff_n :: Int -> Int -> Int -> Float
c_cutoff_n circRadius idxN idxM = (c_cutoff_n' idxN idxM) / circRadiusM
  where
    circRadiusM = mil_to_m circRadius
    c_cutoff_n' 0 1 = 3.832
    c_cutoff_n' 0 2 = 7.016
    c_cutoff_n' 0 3 = 10.174
    c_cutoff_n' 1 1 = 1.841
    c_cutoff_n' 1 2 = 5.331
    c_cutoff_n' 1 3 = 8.536
    c_cutoff_n' 2 1 = 3.054
    c_cutoff_n' 2 2 = 6.706
    c_cutoff_n' 2 3 = 9.970

-- given rectangular waveguide dimensions, returns the propagation
-- constant for a given mode
r_beta :: Int -> Int -> Int -> Int -> Float -> Float
r_beta broadDim shortDim broadIdx shortIdx waveNumber =
  sqrt $ sqWaveNumber - sqCutoffNumber
  where
    sqWaveNumber = waveNumber * waveNumber
    sqCutoffNumber = r_cutoff_n broadDim shortDim broadIdx shortIdx
    
-- given rectangular waveguide dimensions, returns the propagation
-- cutoff frequency for a given mode
r_cutoff_f :: Int -> Int -> Int -> Int -> Float
r_cutoff_f broadDim shortDim broadIdx shortIdx =
  0.5 * c * sqrt ( sqFirstTerm + sqSecondTerm )
  where
    c = 299792458.0
    firstTerm = (fromIntegral broadIdx) / (mil_to_m broadDim)
    secondTerm = (fromIntegral shortIdx) / (mil_to_m shortDim)
    sqFirstTerm = firstTerm * firstTerm
    sqSecondTerm = secondTerm * secondTerm
    
-- print the TE10 mode cutoff for WR42 waveguide
main = do
  putStrLn "The guide full/quarter wavelengths for 26GHz TE10 (WR42):"
  putStrLn $ (show $ 100 * guide_wl) ++ 
    "cm/" ++ (show $ 1000 * 0.25 * guide_wl) ++ "mm"
  putStrLn "\n99% TE11 attenuation length in 3/32 gas inlet at 26GHz"
  putStrLn $ (show minLength) ++ " mils"
  where
    guide_wl = (/) (2 * pi) $ r_beta 470 120 1 0 $ r_wavenumber 26.0e9
    minLength = fst $ head $ dropWhile (\(_,y) -> y > 1e-2) attPairs
    attPairs = [(x,exp(-1.0 * (mil_to_m x) * 557.1909)) 
               | x <- [400,410..1000]]
