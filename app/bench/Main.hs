module Main where
import Raytracer
import Criterion.Main
import System.Random.MWC

main :: IO ()
main = let scene = testScene 32 50 1080 (16/9)
           f = renderScene 2 50 1080 (16/9)
    in defaultMain [ bench "raytracer" $ nfAppIO f scene]

-- performance of (testScene 32 50 (generateRect 1080 (16/9)))
-- before strictness and parallel strategy
-- time                 36.11 s    (31.77 s .. 41.45 s)
--                      0.997 R²   (0.991 R² .. 1.000 R²)
-- mean                 35.56 s    (34.78 s .. 36.05 s)
-- std dev              755.1 ms   (183.1 ms .. 992.8 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- performance of (testScene 32 50 (generateRect 1080 (16/9)))
-- after strictness and parallel strategy
-- time                 4.098 s    (3.895 s .. 4.368 s)
--                      1.000 R²   (0.998 R² .. 1.000 R²)
-- mean                 3.708 s    (3.464 s .. 3.848 s)
-- std dev              238.4 ms   (72.29 ms .. 323.7 ms)
-- variance introduced by outliers: 19% (moderately inflated)