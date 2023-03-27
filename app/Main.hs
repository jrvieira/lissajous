module Main where

import Data.Fixed ( mod' )
import Graphics.Gloss.Interface.IO.Game

data Wave = Sine | Square | Triangle | Saw

fun :: Wave -> Float -> Float
fun Sine = sin
fun Square = \v' -> let v = abs $ mod' v' pi in if v < pi / 2 then 1 else -1
fun Triangle = \v' -> let v = abs $ mod' v' (2*pi) in if v < pi then 2 * (v / pi - 1) + 1 else -2 * (v / pi - 1) + 1
fun Saw = \v' -> let v = abs $ mod' v' (2*pi) in v / pi - 1

data State = State
   { δ :: Float  -- radian time
   , φ :: (Word,Word)  -- input frequencies
   , σ :: Float  -- radian phase shift per frame
   , ρ :: Word  -- resolution
   , α :: Float  -- amplitude
   , β :: Float  -- brightness
   , γ :: Float  -- rational grid
   , ω :: Wave
   , κ :: Bool  -- sc
   }

state :: State
state = State
   { δ = 0
   , φ = (1,1)
   , σ = 0
   , ρ = 16
   , α = 7/10
   , β = 0.2
   , γ = 9
   , ω = Sine
   , κ = False
   }

main :: IO ()
main = playIO (InWindow "mach sim" (round $ size * 4,round $ size * 2) (0,0)) (makeColorI 0 0 0 0) fps state renderIO catchIO stepIO

renderIO :: State -> IO Picture
renderIO s = pure $ render s

catchIO :: Event -> State -> IO State
catchIO e s = pure $ catch e s

stepIO :: Float -> State -> IO State
stepIO i s = pure $ step i s

render :: State -> Picture
render st = Pictures [tx,ty,tr,curve,sx,sy]
   where
   (x,y :: Word) = φ st

   tx = translate  -size      -size $ color white $ scale 0.07 0.07 $ text $ show x
   ty = translate (-size * 2)  0    $ color white $ scale 0.07 0.07 $ text $ show y
   tr = translate (-size * 2) -size $ color white $ scale 0.07 0.07 $ text $ show (div y $ gcd x y) <> ":" <> show (div x $ gcd x y)

   curve = translate -size 0 $ color (makeColor 0 1 0 (β st)) $ lineLoop $ zip xs ys
   xs = (* (α st * size)) . fun (ω st) . (* fromIntegral x) . (- δ st * sc) <$> init [0,2*pi / fromIntegral res..2*pi]
   ys = (* (α st * size)) . fun (ω st) . (* fromIntegral y) . (+ δ st * sc) <$> init [0,2*pi / fromIntegral res..2*pi]
   --    ^scale                           ^periodicity         ^rotation

   res = ρ st ^ (2 :: Word) -- resolution coefficient
   sc | κ st = 1 / fromIntegral (lcm x y)
      | otherwise = 1  -- speed coefficient (slow down more complex shapes)

   sx = translate 0 0 $ color (makeColor 1 0.5 0 0.4) $ line $ zip [0,2 * size / fromIntegral res..] xs
   sy = translate 0 0 $ color (makeColor 0 0.5 1 0.4) $ line $ zip [0,2 * size / fromIntegral res..] ys

catch :: Event -> State -> State
-- catch (EventMotion c) st = st { ξ = c }
catch (EventKey k Down _ _) st
   | Char '0' <- k = state { φ = φ st , ω = ω st , σ = σ st }
   | Char 'x' <- k = st { φ = let (x,y) = φ st in (min maxBound $ succ x,y) }
   | Char 'X' <- k = st { φ = let (x,y) = φ st in (max 1 $ pred x,y) }
   | Char 'y' <- k = st { φ = let (x,y) = φ st in (x,min maxBound $ succ y) }
   | Char 'Y' <- k = st { φ = let (x,y) = φ st in (x,max 1 $ pred y) }
   | Char 's' <- k = st { σ = σ st + 0.01 }
   | Char 'S' <- k = st { σ = σ st - 0.01 }
   | Char 'r' <- k = st { ρ = min maxBound $ succ $ ρ st }
   | Char 'R' <- k = st { ρ = max 1 $ pred $ ρ st }
   | Char 'a' <- k = st { α = α st + 0.1 }
   | Char 'A' <- k = st { α = α st - 0.1 }
   | Char 'b' <- k = st { β = β st + 0.01 }
   | Char 'B' <- k = st { β = β st - 0.01 }
   | Char 'g' <- k = st { γ = succ $ γ st }
   | Char 'G' <- k = st { γ = pred $ γ st }
   | Char 'q' <- k = st { ω = Square }
   | Char 'w' <- k = st { ω = Saw }
   | Char 'e' <- k = st { ω = Sine }
   | Char 't' <- k = st { ω = Triangle }
   | Char 'k' <- k = st { κ = True }
   | Char 'K' <- k = st { κ = False }
catch _ s = s

step :: Float -> State -> State
step _ st = st { δ = δ st + σ st }

-- size of quadrant in pixels
size :: Float
size = 100

fps :: Int
fps = 30

