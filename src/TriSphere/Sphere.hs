import Haste
import Haste.Graphics.Canvas
import Data.IORef

data State = State

animate can time = do
  render can $ do
    bg
    translate (canW/2, canH/2) $ do
      color red   $ stroke $ circ sphereR
      color green $ stroke $ triL triA triB
      color white $ scale (1,-1) $ stroke $ triL triA triB
      color green $ stroke $ arcD sphereR triAng
      --
      translate (-canW/2,0) $ color yellow $ statusBar
  setTimeout 10 $ animate can (time+1)
  where sphereR = triH
        triA = 1.7323
        triB = 1.0
        triH = hypo triA triB
        triAng = triAngleD triB triH
        aLen = arcLen sphereR triAng
        statusBar = do
          mapM_ (\(n,(t,v)) -> text (0,n*12) $ t ++ (show v)) $ zip [0..]
           [("arcL=",aLen)
           ,("triA=",triA)
           ,("triB=",triB)
           ,("triH=",triH)
           ,("triAng=",triAng)
           ,("linR=",2*pi*triB)
           ,("arcR=",2*pi*aLen)
           ,("raRatio=",2*pi*triB / aLen)
           ]

main :: IO ()
main = do
  Just can <- getCanvasById "canvas"
  state <- newIORef $ State
  animate can 0

-- canvas params
canW = 800
canH = 600
scale_ = 60.0
-- just draw background as filled rectangle
bg :: Picture ()
bg = color black $ fill $ rect (0,0) (canW,canH)
-- define radius of circle of 6 units length
l6r = 0.954929658551372014614
-- predefined colors
black   = RGBA 0 0 0 1
red     = RGBA 255 0 0 1
green   = RGBA 0 255 0 1
yellow  = RGBA 255 255 0 1
white   = RGBA 255 255 255 1

type Degree = Double
type Radian = Double

-- thriginometry functions
rad = pi/180
toRad :: Degree -> Radian
toRad = (rad*)
toGrad :: Radian -> Degree
toGrad r = r*(180/pi)

hypo a b = sqrt $ a*a + b*b

arcLen :: Double -> Degree -> Double
arcLen r a =  pi * r * a / 180

triAngleR :: Double -> Double -> Radian
triAngleR b h = asin $ b / h

triAngleD :: Double -> Double -> Degree
triAngleD b h = toGrad $ triAngleR b h

-- drawing functions
arcR r a = arc (0,0) (r*scale_) 0 a
arcD r a = arcR r (toRad a)

triL a b = path[(0,0),(a*scale_,0),(a*scale_,b*scale_),(0,0)]
circ r = circle (0,0) (r*scale_)
