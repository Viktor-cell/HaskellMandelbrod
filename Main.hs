{-# LANGUAGE CApiFFI #-}

import Foreign.C.Types
import Foreign.C.String
import Data.Int
import Foreign.Ptr
import Foreign.Storable
import Text.Printf (printf)

type UInt8 = CUChar

foreign import capi "raylibWrapper.h DrawPixel_" drawPixel :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import capi "raylibWrapper.h DrawRectangle_" drawRectangle :: Int -> Int -> Int -> Int -> UInt8 -> UInt8 -> UInt8 -> UInt8 -> IO ()

foreign import capi "raylib.h InitWindow" initWindow :: Int -> Int -> CString -> IO ()
foreign import capi "raylib.h BeginDrawing" beginDrawing ::  IO ()
foreign import capi "raylib.h EndDrawing" endDrawing ::  IO ()
foreign import capi "raylib.h CloseWindow" closeWindow ::  IO ()
foreign import capi "raylib.h WindowShouldClose" windowShouldClose :: IO Bool

foreign import capi "raylib.h SetTargetFPS" setTargetFPS :: Int -> IO ()
foreign import capi "raylib.h GetFrameTime" getFrameTime :: IO Float
foreign import capi "raylib.h GetFPS" getFPS :: IO Int

data Complex = Complex
    { real :: Double
    , imag :: Double
    } deriving Show

windowWidth :: Int
windowWidth = 1000

windowHeight :: Int
windowHeight = 1000

maxDepth :: Int
maxDepth = 75

run :: IO ()
run = do
    withCString ("haskell") (\name -> initWindow windowWidth windowHeight name)

    setTargetFPS 60

    while (not <$> windowShouldClose) $ do
        beginDrawing
        generate 0 0 windowWidth windowHeight (-2) 1 (-1.5) (1.5)
        endDrawing

    closeWindow

generate :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> Double -> IO ()

generate x y w h sx ex sy ey = do
    if (x == (w - 1)) && (y == (h - 1))
    then return ()
    else do
        (when, does) <- return $ belongsToSet (complexFromPos x y windowWidth windowHeight sx ex sy ey) (Complex 0 0) maxDepth
        p <- return $ scaleEscape when maxDepth
        if does
        then drawPixel x y   0   0   0 255
        else drawPixel x y   p   p   p 255

        generate nx ny w h sx ex sy ey where
            nx = if x == (w - 1) then 0     else x + 1
            ny = if x == (w - 1) then y + 1 else y

scaleEscape :: Int -> Int -> Int
scaleEscape e d = round (fromIntegral e / fromIntegral d * 255)

complexFromPos :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> Double -> Complex
-- complexFromPos x y w h sx ex sy ey = Complex (x / w * ((ex - sx) + sx)) (y / h * ((ey - sy) + sy))
complexFromPos x y w h sx ex sy ey = Complex (real) (imag) where
    real = sx + (fromIntegral x) / (fromIntegral w) * (ex - sx)
    imag = sy + (fromIntegral y) / (fromIntegral h) * (ey - sy)

belongsToSet :: Complex -> Complex -> Int -> (Int, Bool)
belongsToSet c z depth | magUnsqrt z > 16  = (depth, False)
                       | depth == 0 = (0, True)
                       | otherwise         = belongsToSet (c) (cpow2 z `cplus` c) (depth - 1)


magUnsqrt :: Complex -> Double
magUnsqrt x = (real x) * (real x) + (imag x) * (imag x)

cpow2 :: Complex -> Complex
cpow2 c = Complex (a * a - b * b) (2 * a * b) where
          a = real c
          b = imag c

cplus :: Complex -> Complex -> Complex
cplus c1 c2 = Complex ( (real c1) + (real c2) ) ( (imag c1) + (imag c2) )


while :: Monad m => m Bool -> m () -> m ()
while cond body = do
    res <- cond
    if (res /= True) then
        return ()
    else do
        body
        while cond body

main :: IO ()
main = run
