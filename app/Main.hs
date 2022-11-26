module Main where
import Raytracer
import Codec.PPM.Binary ( writePPM )

main :: IO()
main = do
    let numSamples = 2
    let imageWidth = 720
    let aspectRatio = 16/9
    let im = generateRect imageWidth aspectRatio
    let imageHeight = height im
    let maxDepth = 50

    putStrLn "Starting to generate image"
    ppm <- testScene numSamples maxDepth im
    putStrLn "Done generating image"
    writePPM "./testScene.ppm" (imageWidth, imageHeight) ppm
    writePPM "./testImage.ppm" (imageWidth, imageHeight) $ generateTestImage im
    putStrLn "Done"