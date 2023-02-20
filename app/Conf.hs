module Conf where

-- size of quadrant in pixels
size :: Float
size = 100

-- line resolution (lower is better)
res :: Float
res = 1/60

-- curve size
zoom :: Float
zoom = 0.7 * size

-- x/y cycles
steps :: Float
steps = 9

fps :: Int
fps = 1200

-- rotation per frame in radians (lower is slower)
speed :: Float
speed = 1/100

-- line brightness
intensity :: Float
intensity = 0.2

