module Main where

import Data.Bifunctor ( bimap )
import Control.Monad ( join )

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

w, h :: Float
w = 200
h = 200

main :: IO ()
main = play (InWindow "mach sim" (round w,round h) (0, 0)) (makeColorI 0 0 0 0) 30 state render catch step

data State = State Float (Float,Float)

state :: State
state = State 0 (0.0,0.0)

render :: State -> Picture
render (State t pos) = Pictures [tx,ty,curve]
   where
   (x,y) = join bimap (fromInteger . round . (/ 10)) pos
   tx = translate 0 -100 $ color white $ scale 0.1 0.1 $ text $ show (round x)
   ty = translate -100 0 $ color white $ scale 0.1 0.1 $ text $ show (round y)
   curve = color green $ line $ zip xs ys
   xs = map ((* 60) . sin . (* x)) (take 1000 $ (+ (t / max x y)) <$> [0,0.01..])
   ys = map ((* 60) . cos . (* y)) (take 1000 $ (- (t / max x y)) <$> [0,0.01..])

catch :: Event -> State -> State
catch (EventMotion pos) (State t _) = State t pos
catch _ s = s

step :: Float -> State -> State
step _ (State t pos) = State (t + 0.01) pos
