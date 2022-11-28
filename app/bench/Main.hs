module Main where
import Raytracer
import Criterion.Main

main = defaultMain [ bench "raytracer" $ whnfIO (testScene 32 50 (generateRect 1080 (16/9))) ]

-- performance of (testScene 32 50 (generateRect 1080 (16/9)))
-- with iterate and lazy HitRecord
-- time                 36.11 s    (31.77 s .. 41.45 s)
--                      0.997 R²   (0.991 R² .. 1.000 R²)
-- mean                 35.56 s    (34.78 s .. 36.05 s)
-- std dev              755.1 ms   (183.1 ms .. 992.8 ms)
-- variance introduced by outliers: 19% (moderately inflated)

-- performance of (testScene 32 50 (generateRect 1080 (16/9)))
-- with iterate' and strict HitRecord
-- time                 34.18 s    (30.40 s .. 37.14 s)
--                      0.999 R²   (0.995 R² .. 1.000 R²)
-- mean                 35.60 s    (34.57 s .. 36.46 s)
-- std dev              1.033 s    (681.7 ms .. 1.227 s)
-- variance introduced by outliers: 19% (moderately inflated)