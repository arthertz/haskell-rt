module Main where
import Raytracer
import Codec.PPM.Binary ( writePPM )

main :: IO()
main = do
    let numSamples = 32
        imageWidth = 720
        aspectRatio = 3/2
        im = generateRect imageWidth aspectRatio
        imageHeight = height im
        maxDepth = 50

    putStrLn "Starting to generate image"
    ppm <- testScene numSamples maxDepth imageWidth aspectRatio
    putStrLn "Done generating image"
    putStrLn "Writing image to file"
    writePPM "./testScene.ppm" (imageWidth, imageHeight) ppm
    -- writePPM "./testImage.ppm" (imageWidth, imageHeight) $ generateTestImage im
    putStrLn "Image written to file"