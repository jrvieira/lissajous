module Main where

import Data.Bifunctor ( bimap )
import Control.Monad ( join )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Conf

data State = State { time :: Float , crt :: [Dot] , input :: (Word,Word) }

type Dot = (Float,Float)

main :: IO ()
main = play (InWindow "lissajous" (join (,) $ round size * 2) (0,0)) (makeColorI 0 0 0 0) fps (State 0 [] (1,1)) render catch step

render :: State -> Picture
render (State _ ds (x,y)) = Pictures (bg:tx:ty:tr:curve)
   where
   bg = translate -size -size $ color (makeColor 0 1 0 0.2) $ polygon [(0,0),(0,size * 2),(size * 2,size * 2),(size * 2,0)]
   tx = translate 0 -size $ color (makeColor 1 1 1 0.2) $ scale 0.06 0.06 $ text $ show x
   ty = translate -size 0 $ color (makeColor 1 1 1 0.2) $ scale 0.06 0.06 $ text $ show y
   tr = translate -size -size $ color (makeColor 1 1 1 0.2) $ scale 0.06 0.06 $ text $ show (div y $ gcd x y) <> ":" <> show (div x $ gcd x y)
   curve = zipWith dot ds [1,1-1/(points)..0]

   ease v = 2 ** (10 * v - 10)

   dot :: (Float,Float) -> Float -> Picture
   dot pos i = Pictures $ uncurry translate pos <$>
      [ color (makeColor 1 1 1 i) $ circleSolid 1.2
      , color (makeColor 1 1 1 (ease $ ease i)) $ circleSolid 3
      ]

catch :: Event -> State -> State
catch (EventMotion xy) st@(State δ _ p)
   | p == p' = st
   | otherwise = State δ [] p'
   where
   p' = join bimap (max 1 . round . (/ (size * 2 / steps)) . (+ size)) xy
catch _ s = s

step :: Float -> State -> State
step _ (State δ ds (x,y)) = State (δ + speed) ds' (x,y)
   where
   ds' = join bimap (* zoom) (x',y') : ds
   x' = sin $ fromIntegral x * δ + δ/60
   y' = cos $ fromIntegral y * δ - δ/60
   sc = 1 / fromIntegral (max 1 $ lcm x y)  -- speed coefficient (slow down more complex shapes)

points :: Float
points = 2 * pi / res
