module Main where

import Data.Bifunctor ( bimap )
import Control.Monad ( join )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

data State = State Float (Float,Float)

main :: IO ()
main = play (InWindow "mach sim" (join (,) $ round size) (0,0)) (makeColorI 0 0 0 0) fps (State 0 (0,0)) render catch step

render :: State -> Picture
render (State δ pos) = Pictures [tx,ty,tr,curve]
   where
   (x,y) = join bimap (succ . round . (/ (size * 2 / steps)) . (+ size)) pos
   (x',y') = join bimap fromIntegral (x,y)
   tx = translate 0 -size $ color white $ scale 0.1 0.1 $ text $ show x
   ty = translate -size 0 $ color white $ scale 0.1 0.1 $ text $ show y
   tr = translate -size -size $ color white $ scale 0.1 0.1 $ text $ show (div y $ gcd x y) <> ":" <> show (div x $ gcd x y)
   curve = color (makeColor 0 1 0 intensity) $ lineLoop $ zip xs ys
   xs = (* zoom) . sin . (* x') . (- δ * sc) <$> [0,res..2*pi]
   ys = (* zoom) . cos . (* y') . (+ δ * sc) <$> [0,res..2*pi]
   sc = 1 / fromIntegral (lcm x y)  -- speed coefficient (slow down more complex shapes)

catch :: Event -> State -> State
catch (EventMotion pos) (State δ _) = State δ pos
catch _ s = s

step :: Float -> State -> State
step _ (State δ pos) = State (δ + speed) pos

-- size of quadrant in pixels
size :: Float
size = 100

-- line resolution (lower is better)
res :: Float
res = 1/90

-- curve size
zoom :: Float
zoom = 0.7 * size

-- x/y cycles
steps :: Float
steps = 9

fps :: Int
fps = 30

-- rotation per frame in radians (lower is slower)
speed :: Float
speed = 1/100

-- line brightness
intensity :: Float
intensity = 0.2

