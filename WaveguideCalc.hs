-- converts an integer number of mils to
-- meters
mil_to_m :: Int -> Float
mil_to_m x = 0.0000254 * (fromIntegral x)

-- given rectangular waveguide dimensions, this calculates the
-- cutoff wavenumber for a given mode
r_cutoff_n :: Int -> Int -> Int -> Int -> Float
r_cutoff_n broadDim shortDim broadIdx shortIdx =
  (*) pi $ sqrt $ sqFirstTerm + sqSecondTerm 
  where
    sqFirstTerm = (fromIntegral broadIdx) / (mil_to_m broadDim)
    sqSecondTerm = (fromIntegral shortIdx) / (mil_to_m shortDim)

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
  putStrLn $ show $ r_cutoff_f 420 170 1 0
