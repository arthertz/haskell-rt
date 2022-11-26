module Main where
import Codec.PPM.Binary ( writePPM )
import GHC.Word (Word8)
import Linear.Metric
import Linear.V3
import Linear.Vector
import System.Random.Stateful
import System.Random.MWC as MWC
import qualified Linear as V3
import Control.Monad
import System.ProgressBar

-- NECESSARY
-- DONE refactor rayColor to use HitRecord
-- DONE add random number utilities
-- DONE handle normals
-- DONE diffuse materials
-- DONE add true lambertian reflection
-- DONE implement hittable lists
-- DONE material typeclass
-- DONE reflective metal material
-- TODO dielectric materials (naive approach)
-- TODO corrections to dielectrics

-- NOT NECESSARY
-- DONE add mwc-random for sampling
-- DONE antialiasing
-- DONE limit child rays
-- DONE add gamma correction
-- DONE fix shadow acne
-- TODO improve memory efficiency with profiling
-- TODO refactor things into typeclasses

filePath :: String
filePath = "./test1.ppm"

-- image constants
aspectRatio :: Double
aspectRatio = 16.0/9.0
imageWidth :: Integer
imageWidth = 1080
imageHeight :: Integer
imageHeight = round (fromIntegral imageWidth / aspectRatio)

-- camera
viewportHeight :: Double
viewportHeight = 2.0
viewportWidth :: Double
viewportWidth = aspectRatio * viewportHeight
focalLength :: Double
focalLength = 1.0
origin :: V3 Double
origin = V3 0 0 0
horizontal :: V3 Double
horizontal = V3 viewportWidth 0 0
vertical :: V3 Double
vertical   = V3 0 viewportHeight 0
lowerLeftCorner :: V3 Double
lowerLeftCorner = origin - horizontal/2 - vertical/2 - V3 0 0 focalLength

data Ray = Ray { o   :: V3 Double,
                 dir :: V3 Double}

data HitRecord = HitRecord {
    point :: V3 Double,
    normal :: V3 Double,
    hitMaterial :: Material,
    root :: Double,
    frontFace :: Bool
}

-- newtype to avoid orphan instance
data RandomV3 = RandomV3 Double Double Double
instance UniformRange RandomV3
    where
        uniformRM (a, b) g = RandomV3
            <$> uniformRM (a1, b1) g
            <*> uniformRM (a2, b2) g
            <*> uniformRM (a3, b3) g
            where RandomV3 a1 a2 a3 = a
                  RandomV3 b1 b2 b3 = b

wrapRV3 :: V3 Double -> RandomV3
wrapRV3 (V3 x y z) = RandomV3 x y z

unwrapRV3 :: RandomV3 -> V3 Double
unwrapRV3 (RandomV3 x y z) = V3 x y z

reflect :: V3 Double -> V3 Double -> V3 Double
reflect v n = v - 2 * dot v n *^ n

randomInUnitSphereM :: StatefulGen g m => g -> m (V3 Double)
randomInUnitSphereM g =  let p = uniformRM (RandomV3 (-1) (-1) (-1), RandomV3 1 1 1) g
                             v3 = unwrapRV3 <$> p
    in v3 >>= (\ v -> if quadrance v < 1 then pure v else randomInUnitSphereM g)

randomInUnitSphere :: RandomGen g => g -> (V3 Double, g)
randomInUnitSphere g = runStateGen g randomInUnitSphereM

setFaceNormal :: HitRecord -> Ray -> V3 Double -> HitRecord
setFaceNormal HitRecord{point=p, root=rt, hitMaterial=hmt} ry outwardNormal = do
    let newFrontFace = dot (dir ry) outwardNormal < 0
        newNormal = if newFrontFace then outwardNormal else -outwardNormal
    HitRecord{point=p, normal=newNormal, root=rt, frontFace=newFrontFace, hitMaterial=hmt}

data Hittable = Sphere {
    center :: V3 Double,
    radius :: Double,
    material :: Material
} | HittableList [Hittable]

data Bounds = Bounds {
    tmin :: Double,
    tmax :: Double
}

data Material = Lambertian{albedo :: V3 Double}
                 | Metal{albedo :: V3 Double, f :: Double}

class Scatterable a where
    scatter :: StatefulGen g m => g -> a -> Ray -> HitRecord -> m (Either HitInvalid (Ray, V3 Double))

instance Scatterable Material where
    scatter g (Lambertian albedo) _ record = do
        scatterDirection <- (normal record +) . normalize <$> randomInUnitSphereM g
        let isDegenerate = V3.nearZero scatterDirection
        if isDegenerate then
            return (Right (Ray{o= point record, dir=normal record},   albedo))
        else
            return (Right (Ray{o=point record, dir=scatterDirection}, albedo))
    scatter g (Metal albedo f) rayIn record = do
        let reflected = reflect (normalize (dir rayIn)) (normal record)
        let fuzz = if f < 1 then f else 1
        scattered <- (\ r -> Ray{o=point record, dir=reflected + fuzz *^ r}) <$> randomInUnitSphereM g
        if dot reflected (normal record) > 0 then
            return (Right (scattered, albedo))
        else
            return (Left (HitInvalid "No reflection"))


getCameraRay :: (Double, Double) -> Ray
getCameraRay (u, v) = Ray{o=origin, dir=lowerLeftCorner + u*^horizontal + v*^vertical - origin}

rayAt :: Ray -> Double -> V3 Double
rayAt r t = let Ray {o = ro, dir = rdir} = r in ro + (t*^rdir)

rayColorM :: StatefulGen g m => g -> Hittable -> Ray -> Int -> m (V3 Double)
rayColorM g world r depth = if depth <= 0 then pure (V3 0 0 0)
        else case hit world r Bounds{tmin=0.001, tmax=1/0} of
            Right hr ->
                do
                    scatterResult <- scatter g (hitMaterial hr) r hr
                    case scatterResult of
                        Left _ -> pure (V3 0 0 0)
                        Right (scattered, attenuation) ->
                             (attenuation*) <$> rayColorM g world scattered (depth-1)
            Left _ ->
                let Ray {o = _, dir = d} = r
                    V3 _ y _ = normalize d
                    t = 0.5*(y+1.0)
                in pure (blendValue (V3 1.0 1.0 1.0) (V3 0.5 0.7 1.0) t)

processSamples :: Int -> [V3 Double] -> V3 Word8
processSamples n ijs = fmap (floor . (255 * ) . clamp 0 0.999 . sqrt . (/ fromIntegral n))  (foldr (+) V3.zero ijs)

generateTestSceneM :: StatefulGen g m => g -> Hittable -> Int -> Int -> ProgressBar () -> m [(Word8, Word8, Word8)]
generateTestSceneM g world numSamples maxDepth progressBar =
    fmap  (\ x -> do
            let _ = incProgress progressBar 1
            (v3toTuple . processSamples numSamples) x) <$> generateRandomRaysM g world numSamples maxDepth

genRandomRayM :: StatefulGen g m => g -> Hittable -> Int -> (Integer, Integer)-> m (V3 Double)
genRandomRayM g world maxDepth (i, j) = do
    u <- (/ fromIntegral (imageWidth - 1)) . (fromIntegral i +) <$> uniformDouble01M g
    v <- (/ fromIntegral (imageHeight - 1)) . (fromIntegral j +) <$> uniformDouble01M g
    let uv = (u, v)
    let cr = getCameraRay uv
    rayColorM g world cr maxDepth

generateRandomRaysM :: StatefulGen g m => g -> Hittable -> Int -> Int -> m [[V3 Double]]
generateRandomRaysM g world numSamples maxDepth =
    mapM (replicateM numSamples . genRandomRayM g world maxDepth) [(i, j) | j <- map (imageHeight -) [1 .. imageHeight], i <- [1 .. imageWidth]]

-- bounds = (tmin, tmax)
hit :: Hittable -> Ray -> Bounds -> Either HitInvalid HitRecord
hit Sphere{center=ctr, radius=r, material=mat} ray bounds =
    let Ray {o = ro, dir = rdir} = ray
        oc = ro - ctr
        a = quadrance rdir
        -- we use half of b to save a few multiplications during intersection testing
        -- this works because only care if the discriminant is greater than 0 and can factor out a 4
        half_b = dot oc rdir
        c = quadrance oc - r^2
        discriminant = half_b*half_b - a*c
        sqrt_discriminant = sqrt discriminant
        in if discriminant < 0 then
            Left (HitInvalid "Discriminant less than 0")
        else let roots = ((-half_b - sqrt_discriminant)/a, (-half_b + sqrt_discriminant)/a)
                 validRoot = nearestValidRoot roots bounds
              in case validRoot of
                -- if there isn't a valid root, propagate the error up
                Left e -> Left e
                -- otherwise, return the correct output
                Right root -> let
                                p = rayAt ray root
                                outwardNormal = (1/r) *^ (p - ctr)
                                hr = setFaceNormal HitRecord{root = root, point = p, hitMaterial=mat} ray outwardNormal
                                in Right hr
hit (HittableList ls) ray bounds = snd $ foldr
    (\ x (closestSoFar, oldResult)
        -> let result = hit x ray Bounds{tmin=tmin bounds, tmax=closestSoFar}
        in case oldResult of
            Left _ -> case result of
                Left _ -> (closestSoFar, oldResult)
                Right hitRecord -> (root hitRecord, result)
            Right oldHitRecord -> case result of
                Left _ -> (closestSoFar, oldResult)
                Right newHitRecord ->
                    if root newHitRecord < root oldHitRecord then (root newHitRecord, result)
                    else (closestSoFar, oldResult) ) (tmax bounds, Left (HitInvalid "No valid target")) ls

newtype HitInvalid = HitInvalid String

nearestValidRoot :: (Double, Double) -> Bounds -> Either HitInvalid Double
nearestValidRoot (nearRoot, farRoot) Bounds{tmin=tmin, tmax=tmax}
  | nearRoot >= tmin && nearRoot <= tmax = Right nearRoot
  | farRoot >= tmin &&  farRoot <= tmax = Right farRoot
  | otherwise = Left (HitInvalid "Neither root is within bounds")


v3toTuple :: V3 c -> (c, c, c)
v3toTuple (V3 x y z) = (x, y, z)

-- Print a PPM to the console from IO
printPPM :: (Show a1, Show a2, Show a3) => [(a1, a2, a3)] -> IO ()
printPPM = putStrLn . unlines . map showPixels

--Print a tuple representing a pixel to the console
showPixels :: (Show a1, Show a2, Show a3) => (a1, a2, a3) -> String
showPixels (r, g, b) =     " r:"
                        ++ show r
                        ++ " g: "
                        ++ show g
                        ++ " b: "
                        ++ show b

flerp :: Num a => a -> a -> a -> a
flerp x y t = (1-t)*x + t*y

blendValue :: (Additive f, Num a) => f a -> f a -> a -> f a
blendValue start end t = lerp t end start

-- Generate a pixel of the test image based on its image coordinates
genTestPixel :: Integer -> Integer -> (Word8, Word8, Word8)
genTestPixel i j = let r = floor (255.999* fromIntegral i/ fromIntegral(imageWidth-1))
                       g = floor (255.999*fromIntegral j/ fromIntegral(imageHeight-1))
                       b = floor (0.25*255.999)
                    in (r, g, b)

-- Generate a test image as a list of pixel values
generateTestImage :: [(Word8, Word8, Word8)]
generateTestImage = [genTestPixel i j | j <- map (imageHeight -) [1 .. imageHeight], i <- [1 .. imageWidth]]

colorToPixel :: Functor f => f Double-> f Word8
colorToPixel c = floor.(255.999*) <$> c

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

main :: IO ()
main = do
    -- initialize the pseudorandom number generator
    g <- MWC.create
    let materialCenter = Lambertian (V3 0.7 0.3 0.3)
    let materialGround = Lambertian (V3 0.8 0.8 0.0)
    let materialLeft = Metal (V3 0.8 0.8 0.8) 0.3
    let materialRight = Metal (V3 0.8 0.6 0.2) 1.0
    let world = HittableList [Sphere (V3 0.0 (-100.5) (-1)) 100 materialCenter,
           Sphere (V3 0.0 0.0 (-1.0)) 0.5 materialGround,
           Sphere (V3 (-1.0) 0.0 (-1.0)) 0.5 materialLeft, Sphere (V3 1.0 0.0 (-1.0)) 0.5 materialRight]

    let samplesPerPixel = 32
    let maxDepth = 50000

    pb <- newProgressBar defStyle 10 (Progress 0 20 ())
    
    testScene <- generateTestSceneM g world samplesPerPixel maxDepth pb

    putStrLn "Starting to generate image"
    writePPM "./testScene.ppm" (imageWidth, imageHeight) testScene
    putStrLn "Done generating image"
    writePPM "./testImage.ppm" (imageWidth, imageHeight) generateTestImage

    -- print ppm to console
    -- printPPM generateTestScene

    putStrLn "Done"