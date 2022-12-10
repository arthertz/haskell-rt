module Raytracer where
import GHC.Word (Word8)
import Linear.Metric
    ( normalize, Metric(norm, signorm, dot, quadrance) )
import Linear.V3
import Linear.Vector ( Additive((^+^), lerp), (*^), (^*), (^/) )
import Linear (nearZero)
import System.Random.Stateful
    ( RandomGen,
      runStateGen,
      uniformDouble01M,
      StatefulGen)
import System.Random.MWC
    ( UniformRange(..), asGenIO, create, GenIO )
import Control.Applicative ( Applicative(liftA2) )
import Control.Monad ( liftM3 )
import System.ProgressBar
    ( defStyle,
      incProgress,
      newProgressBar,
      Progress(Progress),
      ProgressBar )
import Control.DeepSeq ( force )
import Control.Parallel.Strategies ( using, parListChunk, rdeepseq)
import Control.Parallel (pseq)

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
-- DONE translate camera transform
-- DONE rotate camera transform
-- DONE camera field of view
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
-- DONE progress bar
-- DONE project benchmark
-- TODO acceleration structure
-- TODO textures for diffuse materials
-- TODO lights
-- DONE parallelism
-- TODO some kind of denoiser on the output

-- camera
data Camera = Camera {
viewportHeight :: Double,
origin :: V3 Double,
aspectRatio :: Double,
viewportWidth :: Double,
horizontal :: V3 Double,
vertical :: V3 Double,
lowerLeftCorner :: V3 Double,
vfov :: Double,
w :: V3 Double,
u :: V3 Double,
v :: V3 Double}


makeCamera :: V3 Double -> V3 Double -> V3 Double -> Double -> Double -> Camera
makeCamera lookFrom lookAt vup verticalFieldOfView aspectRatio =
    let theta = verticalFieldOfView * pi/180
        h = tan (theta/2)
        vh = 2.0 * h
        vw = aspectRatio*vh
        !w = normalize $! lookFrom - lookAt
        !u = normalize $! cross vup w
        v = cross w u
        origin = lookFrom
        hor = vw *^ u
        vert = vh *^ v
        llc = origin - (hor ^/ 2) - (vert ^/ 2) - w

        in Camera {
            origin=origin,
            aspectRatio=aspectRatio,
            viewportHeight = vh,
            viewportWidth = vw,
            horizontal = hor,
            vertical = vert,
            lowerLeftCorner = llc,
            vfov = verticalFieldOfView,
            w = w, u=u, v=v}

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

data Ray = Ray { o   :: V3 Double,
                 dir :: V3 Double}

data HitRecord = HitRecord {
    point :: {-# UNPACK #-}  !(V3 Double),
    normal :: {-# UNPACK #-} !(V3 Double),
    hitMaterial :: {-# UNPACK #-}  !Material,
    root :: {-# UNPACK #-} !Double,
    frontFace :: {-# UNPACK #-} !Bool
}

data Material = Lambertian{albedo :: V3 Double}
                | Metal{albedo :: V3 Double, f :: Double}
                | JustRefractive{refractionIndex :: Double}
                | Dielectric{refractionIndex :: Double}

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

refract :: V3 Double -> V3 Double -> Double -> V3 Double
refract uv n etaRatio =
    let cosTheta = min (dot (-uv) n) 1.0
        perp = etaRatio *^ (uv + cosTheta *^ n)
        parallelTo = (n^*).(0-).sqrt.abs.(1.0-).quadrance
    in perp + parallelTo perp

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

generateRect :: Integer -> Double -> Rect
generateRect imageWidth aspectRatio = Rect {width=imageWidth, height=round(fromIntegral imageWidth / aspectRatio)}

class Scatterable a where
    scatter :: StatefulGen g m => g -> a -> Ray -> HitRecord -> m (Either HitInvalid (Ray, V3 Double))

instance Scatterable Material where
    scatter g (Lambertian albedo) _ record = do
        scatterDirection <- (normal record +) . signorm <$> randomInUnitSphereM g
        let isDegenerate = nearZero scatterDirection
        return $ Right (Ray{o= point record, dir=if isDegenerate then normal record else scatterDirection},   albedo)

    scatter g (Metal albedo f) rayIn record = do
        unitSphere <- randomInUnitSphereM g
        let !reflected = reflect (signorm (dir rayIn)) (normal record)
            fuzz = if f < 1 then f else 1
            scattered = Ray{o=point record, dir=reflected + fuzz *^ unitSphere}
        return (if dot reflected (normal record) > 0
                then Right (scattered, albedo)
                else Left (HitInvalid "No reflection"))

    scatter _ (JustRefractive ir) rayIn record = let
            attenuation = V3 1.0 1.0 1.0
            refractionRatio = if frontFace record then 1.0/ir else ir
            !refracted = refract (signorm $ dir rayIn) (normal record) refractionRatio
            scattered = Ray{o=point record, dir=refracted}
        in pure $ Right (scattered, attenuation)

    scatter g (Dielectric ir) rayIn record = do
        let attenuation = V3 1.0 1.0 1.0
            refractionRatio = if frontFace record then 1.0/ir else ir
            !cosTheta = min (dot (((0-).signorm.dir) rayIn) (normal record) ) 1.0
            !sinTheta = sqrt.(1.0-) $ cosTheta*cosTheta
            cantRefract = refractionRatio * sinTheta > 1.0
        randomDouble <- uniformDouble01M g
        let direction = if cantRefract || reflectance cosTheta refractionRatio > randomDouble
            then (reflect $! (signorm.dir) rayIn) $! normal record
            else ((refract  $! (signorm.dir) rayIn) $! normal record) refractionRatio
            scattered = Ray{o=point record, dir=direction}
        return $ Right (scattered, attenuation)

reflectance :: Fractional a => a -> a -> a
reflectance cos rr = let
    -- Schlick's approximation
    r0 = (1-rr)/(1+rr)
    r1 = r0*r0
    in r1 + (1-r1)*(1.0-cos)^5

getCameraRay :: Camera -> (Double, Double) -> Ray
getCameraRay Camera{origin=org, lowerLeftCorner=llc, horizontal=hor, vertical=vert} (u, v) =
    Ray{o=org, dir=llc + u*^hor + v*^vert - org}

rayAt :: Ray -> Double -> V3 Double
rayAt r t = let Ray {o = ro, dir = rdir} = r in force (ro + (t*^rdir))

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
                    V3 _ !y _ = signorm d
                    t = 0.5*(y+1.0)
                in pure (blendValue (V3 1.0 1.0 1.0) (V3 0.5 0.7 1.0) t)

processSamples :: Int -> V3 Double -> V3 Word8
processSamples n = fmap (floor . (255 * ) . clamp 0 0.999 . sqrt . (/ fromIntegral n))

genRandomRayM :: StatefulGen g m => Camera -> Hittable -> Rect -> Int -> (Integer, Integer)-> g -> m (V3 Double)
genRandomRayM cam world im maxDepth (i, j) g = do
    u <- (/ fromIntegral (width im - 1)) . (fromIntegral i +) <$> uniformDouble01M g
    v <- (/ fromIntegral (height im - 1)) . (fromIntegral j +) <$> uniformDouble01M g
    let cr = getCameraRay cam (u, v)
    force rayColorM g world cr maxDepth

genRandomRay  :: RandomGen g => Camera -> Hittable -> Rect -> Int -> (Integer, Integer) ->  g -> (V3 Double, g)
genRandomRay cam world im maxDepth (i,j) g = runStateGen g $ genRandomRayM cam world im maxDepth (i,j)

generateRandomRays :: GenIO -> Camera -> Hittable -> Rect -> Int -> Int -> ProgressBar () -> IO [(Word8, Word8, Word8)]
generateRandomRays g cam world im numSamples maxDepth pb = do
    let getRandomRay ij = asGenIO (genRandomRayM cam world im maxDepth ij) g
        iterator = liftA2 (+) . getRandomRay
        samplePixel ij = (do -- increment the progress bar for one pixel
                            _ <- incProgress pb 1
                            iterate (iterator ij) (pure (V3 0 0 0)) !! numSamples)
        evalPixel ij = fmap (v3toTuple.processSamples numSamples) (samplePixel ij)
    averageSamples <- sequence [evalPixel (i, j) | j <- map (height im -) [1 .. height im], i <- [1 .. width im]]
    return (averageSamples `using` parListChunk 64 rdeepseq)

hit :: Hittable -> Ray -> Bounds -> Either HitInvalid HitRecord
hit Sphere{center=ctr, radius=r, material=mat} ray Bounds{tmin=tmin, tmax=tmax} =
    let Ray {o = ro, dir = rdir} = ray
        !oc = ro - ctr
        !a = quadrance rdir
        -- we use half of b to save a few multiplications during intersection testing
        -- this works because only care if the discriminant is greater than 0 and can factor out a 4
        !half_b = dot oc rdir
        !c = quadrance oc - r^2
        !discriminant = half_b*half_b - a*c
        in if discriminant < 0 then
            Left (HitInvalid "Discriminant less than 0")
        else let
                 !nearRoot = (-half_b - sqrt discriminant)/a
                 farRoot  = (-half_b + sqrt discriminant)/a
                 !validRoot | nearRoot >= tmin && nearRoot <= tmax = Right nearRoot
                            | farRoot >= tmin &&  farRoot <= tmax = Right farRoot
                            | otherwise = Left (HitInvalid "Neither root is within bounds")
              in case validRoot of
                -- if there isn't a valid root, propagate the error up
                Left e -> Left e
                -- otherwise, return the correct output
                Right root -> let
                                p = rayAt ray root
                                outwardNormal = (1/r) *^ (p - ctr)
                                hr = setFaceNormal HitRecord{root = root, point = p, normal=V3 0 0 0, frontFace = False, hitMaterial=mat} ray outwardNormal
                                in Right hr
hit (HittableList ls) ray bounds = snd $ foldr
    (\ x (closestSoFar, oldResult)
        -> let !result = hit x ray Bounds{tmin=tmin bounds, tmax=closestSoFar}
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


fastSqrt :: Double -> Double
fastSqrt x = let n = sqrt (3/8)
                 y = n*x^3
            in x * (15/8 - y*(sqrt(25/6) - y))

fastNorm :: V3 Double -> V3 Double
fastNorm x = let n = fastSqrt (quadrance x) in n *^ x


randomSphere :: StatefulGen g m => g -> (Double, Double) -> (Integer, Integer) -> m Hittable
randomSphere g (xdiff, ydiff) (a, b) = do
    chooseMat <- uniformDouble01M g
    c1 <- randomColor g
    c2 <- randomColor g
    fuzz <- uniformDouble01M g
    let center = V3 (fromIntegral a + 0.9*xdiff) 0.2 (fromIntegral b +0.9*ydiff)
        result  | chooseMat < 0.8 = Sphere center 0.2 (Lambertian (c1 * c2))
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

wideAngleWorld :: Hittable
wideAngleWorld =
    let r = cos (pi/4)
        left = Lambertian $ V3 0 0 1
        right = Lambertian $ V3 1 0 0
    in HittableList [
        Sphere (V3 (-r) 0 (-1)) r left,
        Sphere (V3 r 0 (-1)) r right]

renderScene :: Int -> Int -> Integer -> Double -> (GenIO -> IO Hittable) -> IO [(Word8, Word8, Word8)]
renderScene samplesPerPixel maxDepth imageWidth aspectRatio genWorld = do
    -- initialize the pseudorandom number generator
    g <- create

    -- camera
    let imageHeight = round (fromIntegral imageWidth / aspectRatio) :: Integer
        origin = V3 13 2 3
        lookAt = V3 0 0 0
        up = V3 0 1 0
        fov = 20
        cam = makeCamera origin lookAt up fov aspectRatio

    scene <- genWorld g
    pb <- newProgressBar defStyle 10 (Progress 0 (fromIntegral (imageWidth * imageHeight)) ())
    generateRandomRays g cam scene Rect{ width=imageWidth, height=imageHeight} samplesPerPixel maxDepth pb

testScene :: Int -> Int -> Integer -> Double -> GenIO -> IO Hittable
testScene samplesPerPixel maxDepth imageWidth aspectRatio _ =do
    let materialCenter = Lambertian (V3 0.7 0.3 0.3)
        materialBehind = Metal (V3 0.8 0.8 0.8) 0.3
        materialGround = Lambertian (V3 0.8 0.8 0.0)
        materialLeft = Dielectric{refractionIndex=1.5}
        materialRight = Metal (V3 0.8 0.6 0.2) 0.0
    return $ HittableList [Sphere (V3 0.0 (-100.5) (-1)) 100 materialGround,
           Sphere (V3 0.0 0.0 (-1.0)) 0.5 materialCenter,
           Sphere (V3 (-1.0) 0.0 (-1.0)) 0.5 materialLeft,
           Sphere (V3 (-1.0) 0.0 (-1.0)) (-0.4) materialLeft,
           Sphere (V3 1.0 0.0 (-1.0)) 0.5 materialRight,
           Sphere (V3 1.0 0.0 (-1.0)) 0.5 materialRight]
        