module Set8 where

import Data.Char (intToDigit)
import Codec.Picture

-- Color
data Color = Color Int Int Int
  deriving (Show, Eq)

getRed (Color r _ _) = r
getGreen (Color _ g _) = g
getBlue (Color _ _ b) = b

black, white, pink, red, yellow :: Color
black = Color 0 0 0
white = Color 255 255 255
pink = Color 255 105 180
red = Color 255 0 0
yellow = Color 255 240 0

-- Coord
data Coord = Coord Int Int

-- Picture
data Picture = Picture (Coord -> Color)

-- Picture utilities
justADot = Picture f
  where f (Coord 10 10) = white
        f _             = black

solid :: Color -> Picture
solid color = Picture (\_ -> color)

examplePicture1 = Picture f
  where f (Coord x y)
          | abs (x + y) < 100 = pink
          | max x y < 200     = white
          | otherwise         = black

render :: Picture -> Int -> Int -> String -> IO ()
render (Picture f) w h name =
  writePng name (generateImage (\x y -> colorToPixel (f (Coord x y))) w h)
  where colorToPixel (Color r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- Hex rendering
showHex :: Int -> String
showHex i = [digit (div i 16), digit (mod i 16)]
  where digit x | x >= 0 && x < 16 = intToDigit x
                | otherwise = 'X'

colorToHex :: Color -> String
colorToHex (Color r g b) = showHex r ++ showHex g ++ showHex b

getPixel :: Picture -> Int -> Int -> String
getPixel (Picture f) x y = colorToHex (f (Coord x y))

renderList :: Picture -> (Int,Int) -> (Int,Int) -> [[String]]
renderList picture (minx,maxx) (miny,maxy) =
  [[getPixel picture x y | x <- [minx..maxx]] | y <- [miny..maxy]]

renderListExample = renderList justADot (9,11) (9,11)

-- Ex 1: dotAndLine
dotAndLine :: Picture
dotAndLine = Picture f
  where
    f (Coord x y)
      | x == 3 && y == 4 = white
      | y == 8 = pink
      | otherwise = black

-- Ex 2: blendColor and combine
blendColor :: Color -> Color -> Color
blendColor (Color r1 g1 b1) (Color r2 g2 b2) =
  Color ((r1 + r2) `div` 2) ((g1 + g2) `div` 2) ((b1 + b2) `div` 2)

combine :: (Color -> Color -> Color) -> Picture -> Picture -> Picture
combine f (Picture p1) (Picture p2) = Picture (\coord -> f (p1 coord) (p2 coord))

blend :: Picture -> Picture -> Picture
blend = combine blendColor

-- Shapes
data Shape = Shape (Coord -> Bool)

contains :: Shape -> Int -> Int -> Bool
contains (Shape f) x y = f (Coord x y)

dot :: Int -> Int -> Shape
dot x y = Shape (\(Coord cx cy) -> x == cx && y == cy)

circle :: Int -> Int -> Int -> Shape
circle r cx cy = Shape (\(Coord x y) -> (x - cx)^2 + (y - cy)^2 < r^2)

fill :: Color -> Shape -> Picture
fill c (Shape f) = Picture (\coord -> if f coord then c else black)

exampleCircle :: Picture
exampleCircle = fill red (circle 80 100 200)

-- Ex 3: rectangle
rectangle :: Int -> Int -> Int -> Int -> Shape
rectangle x0 y0 w h = Shape (\(Coord x y) -> x >= x0 && x < x0 + w && y >= y0 && y < y0 + h)

-- Ex 4: union and cut
union :: Shape -> Shape -> Shape
union (Shape f1) (Shape f2) = Shape (\coord -> f1 coord || f2 coord)

cut :: Shape -> Shape -> Shape
cut (Shape f1) (Shape f2) = Shape (\coord -> f1 coord && not (f2 coord))

-- Ex 5: paintSolid
paintSolid :: Color -> Shape -> Picture -> Picture
paintSolid color (Shape f) (Picture base) =
  Picture (\coord -> if f coord then color else base coord)

allWhite :: Picture
allWhite = solid white

exampleSnowman :: Picture
exampleSnowman = fill white snowman
  where snowman = union (cut body mouth) hat
        mouth = rectangle 180 180 40 5
        body = union (circle 50 200 250) (circle 40 200 170)
        hat = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)

-- Ex 6: paint with pattern
paint :: Picture -> Shape -> Picture -> Picture
paint (Picture pat) (Shape f) (Picture base) =
  Picture (\coord -> if f coord then pat coord else base coord)

exampleColorful :: Picture
exampleColorful =
  (paintSolid black hat . paintSolid red legs . paintSolid pink body) allWhite
  where
    legs = circle 50 200 250
    body = circle 40 200 170
    hat  = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)

stipple :: Color -> Color -> Picture
stipple a b = Picture (\(Coord x y) -> if even x == even y then a else b)

stripes :: Color -> Color -> Picture
stripes a b = Picture (\(Coord x y) -> if even y then a else b)

examplePatterns :: Picture
examplePatterns =
  (paint (solid black) hat . paint (stripes red yellow) legs . paint (stipple pink black) body) allWhite
  where
    legs = circle 50 200 250
    body = circle 40 200 170
    hat  = union (rectangle 170 130 60 5) (rectangle 180 100 40 30)

-- Transforms
flipCoordXY (Coord x y) = Coord y x

flipXY :: Picture -> Picture
flipXY (Picture f) = Picture (f . flipCoordXY)

zoomCoord :: Int -> Coord -> Coord
zoomCoord z (Coord x y) = Coord (x `div` z) (y `div` z)

zoom :: Int -> Picture -> Picture
zoom z (Picture f) = Picture (f . zoomCoord z)

-- Ex 7: Fill, Zoom, Flip instances
data Fill = Fill Color

instance Transform Fill where
  apply (Fill color) _ = solid color

data Zoom = Zoom Int
  deriving Show

instance Transform Zoom where
  apply (Zoom z) (Picture f) = Picture (\coord -> f (zoomCoord z coord))

data Flip = FlipX | FlipY | FlipXY
  deriving Show

instance Transform Flip where
  apply FlipXY (Picture f) = Picture (\coord -> f (flipCoordXY coord))
  apply FlipX  (Picture f) = Picture (\(Coord x y) -> f (Coord (-x) y))
  apply FlipY  (Picture f) = Picture (\(Coord x y) -> f (Coord x (-y)))

-- Ex 8: Chain
data Chain a b = Chain a b
  deriving Show

instance (Transform a, Transform b) => Transform (Chain a b) where
  apply (Chain t1 t2) pic = apply t1 (apply t2 pic)

largeVerticalStripes2 :: Picture
largeVerticalStripes2 = apply (Chain (Zoom 5) FlipXY) (stripes red yellow)

-- Ex 9: Blur
data Blur = Blur
  deriving Show

instance Transform Blur where
  apply Blur (Picture f) = Picture g
    where
      g (Coord x y) =
        blend5 (f (Coord x y))
               (f (Coord (x-1) y))
               (f (Coord (x+1) y))
               (f (Coord x (y-1)))
               (f (Coord x (y+1)))

      blend5 (Color r1 g1 b1) (Color r2 g2 b2) (Color r3 g3 b3)
             (Color r4 g4 b4) (Color r5 g5 b5) =
        Color ((r1 + r2 + r3 + r4 + r5) `div` 5)
              ((g1 + g2 + g3 + g4 + g5) `div` 5)
              ((b1 + b2 + b3 + b4 + b5) `div` 5)

-- Ex 10: BlurMany
data BlurMany = BlurMany Int
  deriving Show

instance Transform BlurMany where
  apply (BlurMany 0) pic = pic
  apply (BlurMany n) pic = apply (BlurMany (n - 1)) (apply Blur pic)

-- Other utilities
class Transform t where
  apply :: t -> Picture -> Picture

xy :: Picture
xy = Picture (\(Coord x y) -> Color (x `mod` 256) (y `mod` 256) 0)

largeVerticalStripes = zoom 5 (flipXY (stripes red yellow))

flipBlend :: Picture -> Picture
flipBlend picture = blend picture (apply FlipXY picture)

checkered :: Picture
checkered = flipBlend largeVerticalStripes2

blurredSnowman :: Picture
blurredSnowman = apply (BlurMany 2) exampleSnowman
