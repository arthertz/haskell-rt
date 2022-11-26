module Main where
import Raytracer
import Criterion.Main

main = defaultMain [ bench "raytracer" $ nfIO (testScene 8 50 (generateRect 720 (16/9))) ]