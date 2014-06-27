import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLdouble, GLfloat, Size)
import Debug.Trace (trace, traceIO)
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (catch)

data Side = TL | TR | BL | BR deriving (Show, Eq)

type Point = (GLfloat, GLfloat)
type Ball  = (Point, Side)

sqrSize = 0.1 :: GLfloat

-- TODO make screen coords; change mouse position

field0 = [
  [0,1,1,1,1,1,1,0],
  [0,1,1,1,1,1,0,0],
  [0,1,1,1,1,0,0,0],
  [0,1,1,1,0,0,0,0]]

field1 :: [[Bool]]
field1 =
  let ff | 1 = True
         | 0 = False
  in map ff field0

enumerate = zip [0..]

main :: IO ()
main = do
  -- initialize has to come first. If it doesn't return True,
  -- this crashes with a pattern match error.
  True <- GLFW.initialize
  -- Set the RGB bits to get a color window.
  -- See the GLFW-b docs for all the options
  True <- GLFW.openWindow GLFW.defaultDisplayOptions
            { GLFW.displayOptions_numRedBits = 8
            , GLFW.displayOptions_numGreenBits = 8
            , GLFW.displayOptions_numBlueBits = 8
            , GLFW.displayOptions_numDepthBits = 1
            , GLFW.displayOptions_width = 640
            , GLFW.displayOptions_height = 480
            }
  screenSize <- newIORef (640, 480)
  GLFW.setWindowSizeCallback $ resize screenSize
  -- Use `$=` for assigning to GL values, `get` to read them.
  -- These functions basically hide IORefs.
  GL.depthFunc $= Just GL.Less
  GL.pointSize $= 20.0
  -- Use `finally` so that `quit` is called whether or
  -- not `mainLoop` throws an exception
  let ball = ((0.0,0.0), TL)
  finally (mainLoop screenSize 0.0 field1 ball) quit


calcCoords :: IORef (Int, Int) -> Int -> Int -> IO Point
calcCoords screenSize i j = do
  ss <- readIORef screenSize
  let x1 = fromIntegral i * 0.3 - 0.2
      y1 = fromIntegral j * 0.3 - 0.7
  return (x1,y1)


-- | Draw the window and handle input
mainLoop :: IORef (Int, Int) -> Double -> [[Bool]] -> Ball -> IO ()
mainLoop screenSize shovel objects ball = do
  now <- GLFW.getTime
  draw screenSize now objects ball
  -- Input is polled each time swapBuffers is called
  esc  <- GLFW.keyIsPressed GLFW.KeyEsc
  left  <- GLFW.keyIsPressed GLFW.KeyLeft
  right  <- GLFW.keyIsPressed GLFW.KeyRight
  isClosed <- fmap not GLFW.windowIsOpen
  -- DRAW IT!!!
  unless (esc || isClosed) $ do
    -- Sleep for the rest of the frame
    frameLeft <- fmap (spf + now -) GLFW.getTime
    when (frameLeft > 0) $
        threadDelay (truncate $ 1000000 * frameLeft)
    mainLoop screenSize shovel objects ball
  where
    -- maximum frame rate
    fps = 60
    spf = recip fps


-- | Draw a frame
draw :: IORef (Int, Int) -> Double -> [[Bool]] -> Ball -> IO ()
draw screenSize t objects ball = do
  -- Again, the functions in GL almost all map to standard OpenGL functions
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.loadIdentity
  drawBoxes screenSize objects
  --  GL.rotate theta axis
  -- renderPrimitive wraps the supplied action with glBegin and glEnd.
  -- We'll stop using this when we switch to shaders and vertex buffers.
  GL.renderPrimitive GL.Quads $
    -- Draw a unit square centered on the origin
    forM_ [(0, 0), (1, 0), (1, 1), (0, 1)] $ \(x, y) ->
        let vtx = GL.Vertex3 x y 0 :: GL.Vertex3 GL.GLfloat
            t = (x + y) / 2
            col = GL.Color4 t t t 0.5 :: GL.Color4 GL.GLfloat
        in GL.color col >> GL.vertex vtx
  drawPoint $ fst ball
  printErrors
  GL.flush
  GLFW.swapBuffers
    where
      -- GL.rotate takes the angle in degrees, not radians
      theta = realToFrac t * 360
      axis = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat


moveBall :: Ball -> Ball
moveBall ((x,y),st) =
  let d = 0.01
      p = case st of
            TL -> (x-d, y+d)
            TR -> (x+d, y+d)
            BL -> (x-d, y-d)
            BR -> (x+d, y-d)
  in (p, st)


near :: GLfloat -> Point -> Point -> Bool
near d (x0,y0) (x,y) = abs (x-x0) < d && abs (y-y0) < d

rotateBall (a,b) st (x,y) =
  let
    dx = a-x
    dy = a-y
    --rot | dy > 0 && dx > 0 = 0
  in  1

newState :: Ball -> [[Bool]] -> (Ball, [[Bool]])
newState (p,st) field =
  let update a = a && not $ near 0.1 p (coords a)
      field1 = map (map update) field0
      brokenBoxes = map near 0.1 p $ map coords $ foldl (++) [] field
  in (p, st)


distance2 :: Point -> Point -> GLfloat
distance2 (x1,y1) (x2,y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2


procButton :: Point -> [Point] -> [Bool] -> (Maybe Point, [Point], [Bool])
procButton mPos objects stat = 
    if (filter (\p -> (distance2 mPos p) < 1.0) objects) == [] then
      if (filter (== True) stat) == [] then
        -- create new
        (Nothing, (mPos:objects), (False:stat))
      else
        -- new mouse position
        (Just mPos, objects, stat)
    else
      if (filter (== True) stat) == [] then
        -- create new
        (Nothing, (mPos:objects), (False:stat))
      else
        -- switch square state
        (Nothing, a, b)
        where
          sel = (filter (\p -> (distance2 mPos p) < 1.0) objects)
          ff (p, s) = if (elem p sel) then (p, not s) else (p,s)
          (a,b) = unzip $ map ff $ zip objects stat


drawPoint :: Point -> IO ()
drawPoint (a,b) =
  GL.renderPrimitive GL.Points $
    -- Draw a unit square centered on the origin
     let half = sqrSize / 2
         vtx = GL.Vertex3 a b 0 :: GL.Vertex3 GLfloat
         col = GL.Color4 0 0 1.0 1 :: GL.Color4 GL.GLfloat
      in GL.color col >> GL.vertex vtx


drawBox :: Point -> IO ()
drawBox (a,b) =
  GL.renderPrimitive GL.Quads $
    -- Draw a unit square centered on the origin
    forM_ [(-1, -1), (1, -1), (1, 1), (-1, 1)] $ \(x, y) ->
       let h = sqrSize / 2
           vtx = GL.Vertex3 (x*h + b - 0.5) (y*h - a + 0.7) 0 :: GL.Vertex3 GLfloat
           col = GL.Color4 0.5 0.5 0 1 :: GL.Color4 GL.GLfloat
        in GL.color col >> GL.vertex vtx


drawBoxes :: IORef (Int, Int) -> [[Bool]] -> IO ()
drawBoxes screenSize pts = do
  forM_ (enumerate pts) $ \(i,ll) -> 
    forM_ (enumerate ll) $ \(j, p) ->
      -- let (a, b) = (fromIntegral (fst p) / 10, (fromIntegral (snd p) / 10))
      if p == 1
        then do
          xy <- calcCoords screenSize i j
          drawBox xy
      else return ()


-- | Resize the viewport and set the projection matrix
resize :: IORef (Int, Int) -> Int -> Int -> IO ()
resize screenSize w h = do
  writeIORef screenSize (w, h)
  -- These are all analogous to the standard OpenGL functions
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  let tmp = fromIntegral w / fromIntegral h
  GL.ortho2D (-tmp) tmp (-1.0) 1.0
  GL.matrixMode $= GL.Modelview 0


-- | Close the window and terminate GLFW
quit = GLFW.closeWindow >> GLFW.terminate


-- | This will print and clear the OpenGL errors
printErrors = GL.get GL.errors >>= mapM_ print



















