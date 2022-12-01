module Main where
import Raytracer
import Codec.PPM.Binary ( writePPM )

main :: IO()
main = do
    let numSamples = 100
        imageWidth = 1200
        aspectRatio = 3/2
        im = generateRect imageWidth aspectRatio
        imageHeight = height im
        maxDepth = 50

    putStrLn "Starting to generate image"
    let scene = randomScene --testScene numSamples maxDepth imageWidth aspectRatio
    ppm <- renderScene numSamples maxDepth imageWidth aspectRatio scene 
    putStrLn "Done generating image"
    putStrLn "Writing image to file"
    writePPM "./testScene.ppm" (imageWidth, imageHeight) ppm
    -- writePPM "./testImage.ppm" (imageWidth, imageHeight) $ generateTestImage im
    putStrLn "Image written to file"