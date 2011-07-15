A series of measurements have been made on the new waveguide prototype
pieces.  In particular, we have been designing and re-designing the
inlet sections for the waveguide active region in an attempt to optimize
the insertion loss (IL) of the inlets.

The following data was taken on 7/14/2011.  The format is in a list
of (frequency, power_bare, power_blank, power_inlet).  There are functions
to pull off these components.

> import Data.List
>
> type Measurement = (Int,Float,Float,Float)
> points :: [Measurement] -- These are the actual data points.
> points = [(25000,-36.3,-36.4,-36.7),
>           (25100,-36.7,-37.5,-37.8),
>           (25200,-38.5,-39.8,-40.0),
>           (25300,-39.2,-40.3,-40.6),
>           (25400,-39.2,-39.8,-40.1),
>           (25500,-39.1,-39.2,-39.5),
>           (25600,-38.7,-38.5,-38.8),
>           (25700,-38.4,-38.7,-39.1),
>           (25800,-38.7,-39.7,-40.1),
>           (25900,-38.8,-40.1,-40.6),
>           (26000,-39.1,-39.9,-40.1),
>           (26100,-38.7,-39.1,-39.1),
>           (26200,-38.7,-38.6,-38.6),
>           (26300,-39.2,-39.1,-39.2),
>           (26400,-39.4,-39.6,-39.7),
>           (26500,-38.8,-39.8,-39.6)]

The utility functions below pull off specific components of each 
measurement.

> frequency :: Measurement -> Int -- Grab the frequency of a measurement
> frequency m@(f,_,_,_) = f
>
> bare_power :: Measurement -> Float -- Grab the bare power
> bare_power m@(_,p,_,_) = p
>
> blank_power :: Measurement -> Float -- Grab the blanked power
> blank_power m@(_,_,p,_) = p
>
> inlet_power :: Measurement -> Float -- Grab the power with inlet
> inlet_power m@(_,_,_,p) = p

What we care about is the frequency dependent IL of the gas inlet wrt to
the blank.  This function will calculate the difference P2-P1, where P2
is the power measured with the blank section inserted, and P1 is the power
measured with the inlet section inserted instead.

> il_wrt_blank :: [Measurement] -> [Float]
> il_wrt_blank m = zipWith (-) blank_measurements inlet_measurements
>                    where
>                      blank_measurements = [blank_power x | x <- m] 
>                      inlet_measurements = [inlet_power x | x <- m]

There is fairly significant error on this measurement (order ~ .1dB).  
However, the variation of IL(f) is pretty slow, so we will average over
the frequency band 25000MHz - 26500MHz.  The following function just
calculates the average of a given list.

> average :: (Real a, Fractional b) => [a] -> b
> average l = realToFrac (sum l) / genericLength l

This is the main function which will calculate everything we need
and print it out.

> main = do
>  putStrLn "avg. inlet IL over band 25 - 26.5GHz in dB:"
>  putStrLn $ show . average . il_wrt_blank $ points
