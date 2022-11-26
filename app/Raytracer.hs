module Raytracer where
import GHC.Word (Word8)
import GHC.List (iterate')
import Linear.Metric
import Linear.V3
import Linear.Vector
import System.Random.Stateful
import System.Random.MWC as MWC
import qualified Linear as V3
import Control.Applicative ( Applicative(liftA2) )
import Control.Monad

-- NECESSARY
-- DONE refactor rayColor to use HitRecord
-- DONE add random number utilities
-- DONE handle normals
-- DONE diffuse materials
-- DONE add true lambertian reflection
-- DONE implement hittable lists
-- DONE material typeclass
-- DONE reflective metal material
-- DONE dielectric materials (naive approach)
-- DONE corrections to dielectrics
-- TODO translate camera transform
-- TODO rotate camera transform
-- TODO camera field of view
-- TODO camera depth of field


-- NOT NECESSARY
-- DONE add mwc-random for sampling
-- DONE antialiasing
-- DONE limit child rays
-- DONE add gamma correction
-- DONE fix shadow acne
-- DONE improve memory efficiency with profiling
-- DONE refactor monadic code 
-- DONE random test scene

-- camera
viewportHeight :: Double
viewportHeight = 2.0

focalLength :: Double
focalLength = 1.0
origin :: V3 Double
origin = V3 0 0 0
aspectRatio :: Double
aspectRatio = 16.0/9.0
viewportWidth :: Double
viewportWidth = aspectRatio * viewportHeight
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

randomColor :: StatefulGen g m => g -> m (V3 Double)
randomColor g = unwrapRV3 <$> uniformRM (RandomV3 0 0 0, RandomV3 1 1 1) g

reflect :: V3 Double -> V3 Double -> V3 Double
reflect v n = v - 2 * dot v n *^ n

randomInUnitSphereM :: StatefulGen g m => g -> m (V3 Double)
randomInUnitSphereM g = let cubeInterval = uniformRM (-1, 1) g
                            v3 = liftM3 V3 cubeInterval cubeInterval cubeInterval
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

data Rect = Rect {
    height :: Integer,
    width :: Integer
}
generateRect :: Integer -> Double -> Rect
generateRect imageWidth aspectRatio = Rect {width=imageWidth, height=round(fromIntegral imageWidth / aspectRatio)}

data Material = Lambertian{albedo :: V3 Double}
                | Metal{albedo :: V3 Double, f :: Double}
                | JustRefractive{refractionIndex :: Double}
                | Dielectric{refractionIndex :: Double}

class Scatterable a where
    scatter :: StatefulGen g m => g -> a -> Ray -> HitRecord -> m (Either HitInvalid (Ray, V3 Double))


refract :: V3 Double -> V3 Double -> Double -> V3 Double
refract uv n etaRatio =
    let cosTheta = min (dot (-uv) n) 1.0
        perp = etaRatio *^ (uv + cosTheta *^ n)
        parallelTo = (n^*).(0-).sqrt.abs.(1.0-).quadrance
    in perp + parallelTo perp


instance Scatterable Material where
    scatter g (Lambertian albedo) _ record = do
        scatterDirection <- (normal record +) . normalize <$> randomInUnitSphereM g
        let isDegenerate = V3.nearZero scatterDirection
        return $ Right (Ray{o= point record, dir=if isDegenerate then normal record else scatterDirection},   albedo)
    scatter g (Metal albedo f) rayIn record = do
        unitSphere <- randomInUnitSphereM g
        let reflected = reflect (normalize (dir rayIn)) (normal record)
            fuzz = if f < 1 then f else 1
            scattered = Ray{o=point record, dir=reflected + fuzz *^ unitSphere}
        if dot reflected (normal record) > 0 then
            return $ Right (scattered, albedo)
        else
            return (Left (HitInvalid "No reflection"))
    scatter _ (JustRefractive ir) rayIn record = let
            attenuation = V3 1.0 1.0 1.0
            refractionRatio = if frontFace record then 1.0/ir else ir
            refracted = refract (normalize $ dir rayIn) (normal record) refractionRatio
            scattered = Ray{o=point record, dir=refracted}
        in pure $ Right (scattered, attenuation)
    scatter g (Dielectric ir) rayIn record = do
        let attenuation = V3 1.0 1.0 1.0
            refractionRatio = if frontFace record then 1.0/ir else ir
            cosTheta = min (dot (((0-).normalize.dir) rayIn) (normal record) ) 1.0
            sinTheta = sqrt.(1.0-) $ cosTheta*cosTheta
            cantRefract = refractionRatio * sinTheta > 1.0
        randomDouble <- uniformDouble01M g
        let direction = if cantRefract || reflectance cosTheta refractionRatio > randomDouble
            then reflect ((normalize.dir) rayIn) (normal record)
            else refract ((normalize.dir) rayIn) (normal record) refractionRatio
            scattered = Ray{o=point record, dir=direction}
        return $ Right (scattered, attenuation)

reflectance :: Fractional a => a -> a -> a
reflectance cos rr = let
    -- Schlick's approximation
    r0 = (1-rr)/(1+rr)
    r1 = r0*r0
    in r1 + (1-r1)*(1.0-cos)^5

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

processSamples :: Int -> V3 Double -> V3 Word8
processSamples n = fmap (floor . (255 * ) . clamp 0 0.999 . sqrt . (/ fromIntegral n))

genRandomRayM :: StatefulGen g m => g -> Hittable -> Rect -> Int -> (Integer, Integer)-> m (V3 Double)
genRandomRayM g world im maxDepth (i, j) = do
    u <- (/ fromIntegral (width im - 1)) . (fromIntegral i +) <$> uniformDouble01M g
    v <- (/ fromIntegral (height im - 1)) . (fromIntegral j +) <$> uniformDouble01M g
    let cr = getCameraRay (u, v)
    rayColorM g world cr maxDepth

generateRandomRaysM :: StatefulGen g m => g -> Hittable -> Rect -> Int -> Int -> m [(Word8, Word8, Word8)]
generateRandomRaysM g world im numSamples maxDepth = do
    averageSamples <- mapM (\ ij -> iterate' (liftA2 (+) (genRandomRayM g world im maxDepth ij)) (pure $ V3 0 0 0) !! numSamples)
        [(i, j) | j <- map (height im -) [1 .. height im], i <- [1 .. width im]]
    let processedSamples = map (v3toTuple . processSamples numSamples) averageSamples
    return $! processedSamples


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
genTestPixel :: Rect -> Integer -> Integer -> (Word8, Word8, Word8)
genTestPixel im i j =
    let r = floor (255.999* fromIntegral i/ fromIntegral(width im -1))
        g = floor (255.999*fromIntegral j/ fromIntegral(height im -1))
        b = floor (0.25*255.999)
    in (r, g, b)

-- Generate a test image as a list of pixel values
generateTestImage :: Rect -> [(Word8, Word8, Word8)]
generateTestImage im = [genTestPixel im i j | j <- map (height im -) [1 .. height im], i <- [1 .. width im]]

colorToPixel :: Functor f => f Double-> f Word8
colorToPixel c = floor.(255.999*) <$> c

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx


randomSphere :: StatefulGen g m => g -> (Double, Double) -> (Integer, Integer) -> m Hittable
randomSphere g (xdiff, ydiff) (a, b) = do
    chooseMat <- uniformDouble01M g

    c1 <- randomColor g
    c2 <- randomColor g
    fuzz <- uniformDouble01M g
    let center = V3 (fromIntegral a + 0.9*xdiff) 0.2 (fromIntegral b +0.9*ydiff)
    let result  | chooseMat < 0.8 = Sphere center 0.2 (Lambertian (c1 * c2))
                | chooseMat < 0.95 = Sphere center 0.2 (Metal (0.5 ^+^ (0.5 *^ c1)) (0.5 * fuzz))
                | otherwise = Sphere center 0.2 (Dielectric 1.5)
    return result

randomScene :: StatefulGen g m => g -> m Hittable
randomScene g = do
    xdiff <- uniformDouble01M g
    ydiff <- uniformDouble01M g
    let diff = (xdiff, ydiff)
        indices = [(a,b) | a <- [-11..11] , b <- [-11..11]]
        abToCenter (a,b) = V3 (fromIntegral a + 0.9*xdiff) 0.2 (fromIntegral b + 0.9*ydiff)
        pred ab = abToCenter ab - norm (V3 4 0.2 0) > 0.9
        world = [Sphere (V3 0 (-1000) 0) 1000 (Lambertian 0.5),
                 Sphere (V3 0 1 0) 1.0 (Dielectric 1.5),
                 Sphere (V3 (-4) 1 0) 1.0 (Lambertian $ V3 0.4 0.2 0.1),
                 Sphere (V3 4 1 0) 1.0 (Metal (V3 0.7 0.6 0.5) 0.0)]
        goodIndices = filter pred indices
    HittableList.(world ++) <$> mapM (randomSphere g diff) goodIndices


testScene :: Int -> Int -> Rect -> IO [(Word8, Word8, Word8)]
testScene samplesPerPixel maxDepth im = do
    -- initialize the pseudorandom number generator
    g <- MWC.create

    -- camera
    let viewportHeight = 2.0
        viewportWidth = aspectRatio * viewportHeight
        focalLength = 1.0
        origin = V3 0 0 0
        horizontal = V3 viewportWidth 0 0
        vertical   = V3 0 viewportHeight 0
        lowerLeftCorner = origin - horizontal/2 - vertical/2 - V3 0 0 focalLength

    --let materialCenter = Lambertian (V3 0.7 0.3 0.3)
    --let materialLeft = Metal (V3 0.8 0.8 0.8) 0.3
        materialGround = Lambertian (V3 0.8 0.8 0.0)
        materialCenter = Lambertian (V3 0.1 0.2 0.5)
        materialLeft = Dielectric{refractionIndex=1.5}
        materialRight = Metal (V3 0.8 0.6 0.2) 0.0
        world = HittableList [Sphere (V3 0.0 (-100.5) (-1)) 100 materialGround,
           Sphere (V3 0.0 0.0 (-1.0)) 0.5 materialCenter,
           Sphere (V3 (-1.0) 0.0 (-1.0)) 0.5 materialLeft,
           Sphere (V3 (-1.0) 0.0 (-1.0)) (-0.4) materialLeft,
           Sphere (V3 1.0 0.0 (-1.0)) 0.5 materialRight]
    -- world <- randomScene g

    generateRandomRaysM g world im samplesPerPixel maxDepth