import Haste
import Haste.Graphics.Canvas
import Data.IORef

data State = State

-- |Define triangle sides
trA = 1.0
trB = l6r/2
trC = sqrt $ trA^2 + trB^2

animate can time = do
  render can $ do
    bg
    translate (canW/2, canH/2) $ do
    color red    $ circ trC
    color green  $ stroke $ tria
    color yellow $ stroke $ arc_ hypo 0 arcang
    color white  $ scale (1,-1) $ stroke $ tria
    color yellow $ translate (-canW/2,0) $ statusBar
  setTimeout 10 $ animate can (time+1)
  where hypo = trC --sqrt $ (trR*trR)+((l6r/2)*(l6r/2))
        arcang = ang (l6r/2) hypo
        statusBar = do
          text (0, 0) $ "Hypo="  ++ (show hypo)
          text (0,16) $ "ArcL="  ++ (show $ arcLen hypo arcang)
          text (0,32) $ "CircL=" ++ (show $ l6r*2*pi)

main :: IO ()
main = do
  Just can <- getCanvasById "canvas"
  state <- newIORef $ State
  animate can 0

-- canvas params
canW = 800
canH = 600
scale_ = 100.0
-- various helpers
rad = pi/180
pi2 = pi*pi
toRad :: Double -> Angle
toRad = (rad*)
-- draw triangle with predefined sides
tria =
  path [p (0,0)
       ,p (trA,0)
       ,p (trA,trB)
       ,p (0,0)]
  where p (x,y) = (x*scale_,y*scale_)
-- draw arc of radius, start and end angles CW
arc_ r s e =
  arc (0,0) (r*scale_) s e
-- calc alpha angle of triangle by two sides, returns radians
ang a b = asin $ a / b
-- draw circle with radius and scale it
circ :: Double -> Picture ()
circ r = stroke $ circle (0,0) (r*scale_)
-- just draw background as filled rectangle
bg :: Picture ()
bg = color black $ fill $ rect (0,0) (canW,canH)
-- calculate arc length by radius and angle in radians
arcLen r = \a -> a * r
-- define radius of circle of 6 units length
l6r = 0.954929658551372014614
-- predefined colors
black   = RGBA 0 0 0 1
red     = RGBA 255 0 0 1
green   = RGBA 0 255 0 1
yellow  = RGBA 255 255 0 1
white   = RGBA 255 255 255 1
