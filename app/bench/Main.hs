module Main where
import Raytracer (testScene)
import Criterion.Main

main = defaultMain [ bench "raytracer" $ nfIO (testScene 8 50 1080) ]