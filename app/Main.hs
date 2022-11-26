module Main where
import Raytracer
import Codec.PPM.Binary ( writePPM )

main :: IO()
main = do
    let numSamples = 16
    let imageHeight = 1080
    let imageWidth = 1920
    let maxDepth = 50

    putStrLn "Starting to generate image"
    ppm <- testScene numSamples maxDepth imageHeight
    putStrLn "Done generating image"
    writePPM "./testScene.ppm" (imageWidth, imageHeight) ppm
    writePPM "./testImage.ppm" (imageWidth, imageHeight) (generateTestImage Rect{width=1080, height = 1920})
    putStrLn "Done"