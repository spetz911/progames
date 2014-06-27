import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLdouble, GLfloat, Size)
import Debug.Trace (trace, traceIO)
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (catch)

type Point = (GLfloat, GLfloat)

sqrSize = 0.1 :: GLfloat

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
  -- Use `finally` so that `quit` is called whether or
  -- not `mainLoop` throws an exception
  finally (mainLoop screenSize False Nothing [] []) quit



-- | Draw the window and handle input
mainLoop :: IORef (Int, Int) -> Bool -> Maybe Point -> [Point] -> [Bool] -> IO ()
mainLoop screenSize prev prevPos objects stat = do
  now <- GLFW.getTime
  draw now objects stat
  -- Input is polled each time swapBuffers is called
  esc  <- GLFW.keyIsPressed GLFW.KeyEsc
  btn0 <- GLFW.mouseButtonIsPressed GLFW.MouseButton0
  mPos0 <- GLFW.getMousePosition
  ss <- readIORef screenSize
  let mPos = realCoords ss mPos0
      onPress     = btn0 == True && prev == False
      onRelease   = prev == True && btn0 == False
      (runToCenter, newObjects11, stat1) =
        if onPress
        then
          procButton mPos objects stat
        else
          (prevPos, objects, stat)
      (obj2,stat2) = (moveAllObjects runToCenter) .
                     (moveStopInCenter runToCenter) .
                     moveRunOther $ zip newObjects11 stat1 
  if onPress then
    print $ take 3 obj2
  else
    putStr ""
  isClosed <- fmap not GLFW.windowIsOpen
  -- DRAW IT!!!
  unless (esc || isClosed) $ do
    -- Sleep for the rest of the frame
    frameLeft <- fmap (spf + now -) GLFW.getTime
    when (frameLeft > 0) $
        threadDelay (truncate $ 1000000 * frameLeft)
    mainLoop screenSize btn0 runToCenter obj2 stat2
  where
    -- maximum frame rate
    fps = 60
    spf = recip fps


realCoords :: (Int, Int) -> (Int, Int) -> Point
realCoords (w0,h0) (a0,b0) =
  let (w,h) = (fromIntegral w0, fromIntegral h0)
      (a,b) = (fromIntegral a0, fromIntegral b0)
      y = (h - 2 * b) / h
      x = (2 * a - w) / h
  in (x,y)

identity x = x

near :: GLfloat -> Point -> Point -> Bool
near d (x0,y0) (x,y) = abs (x-x0) < d && abs (y-y0) < d

moveRunOther :: [(Point,Bool)] -> [(Point,Bool)]
moveRunOther status = 
  let actived = map fst $ filter snd status
      nearActive x = filter (near sqrSize x) actived /= []
      newState (p,st) = (p, st || (nearActive p))
  in map newState status

moveStopInCenter :: Maybe Point -> [(Point,Bool)] -> [(Point,Bool)]
moveStopInCenter (Just centerPos) status =
  let actived = map fst $ filter snd status
      newState (p,st) = (p, st && not (near 0.01 centerPos p))
  in map newState status

moveStopInCenter Nothing x = x


moveAllObjects :: Maybe Point -> [(Point,Bool)] -> ([Point], [Bool])
moveAllObjects (Just centerPos) status  = 
  let move (x0,y0) (x,y) = (x0 + (x-x0) * 0.99, y0 + (y-y0) * 0.99)
      newObjects (p,st) | st == True = move centerPos p
                        | otherwise  = p
  in (map newObjects status, map snd status)

moveAllObjects Nothing stat = unzip stat

distance2 (x1,y1) (x2,y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

procButton :: Point -> [Point] -> [Bool] -> (Maybe Point, [Point], [Bool])
procButton mPos objects stat = 
  let half = sqrSize / 2
      onHover (a,b) = abs (fst mPos - a) < half && abs (snd mPos - b) < half
      hasHover = filter onHover objects /= []
      hasActive = filter (== True) stat /= []
  in
    if hasHover then
      trace "hasHover" $
      let
        sel = filter onHover objects
        ff (p, s) = if (elem p sel) then (p, not s) else (p,s)
        (a,b) = unzip $ map ff $ zip objects stat
      in (Nothing, a, b)
    else
      trace "has no Hover" $
      if hasActive then
        -- new mouse position
        (Just mPos, objects, stat)
      else
        -- create new
        (Nothing, (mPos:objects), (False:stat))


drawSquare :: (Point, Bool) -> IO ()
drawSquare ((a,b), st) =
  GL.renderPrimitive GL.Quads $
    -- Draw a unit square centered on the origin
    forM_ [(0, 0), (sqrSize, 0), (sqrSize, sqrSize), (0, sqrSize)] $ \(x, y) ->
       let half = sqrSize / 2
           vtx = GL.Vertex3 (x - half + a) (y - half + b) 0 :: GL.Vertex3 GLfloat
           col =
            if st then
              GL.Color4 0.7 0.0 0.7 1 :: GL.Color4 GLfloat
            else
              GL.Color4 0.0 0.5 0.7 1 :: GL.Color4 GLfloat
        in GL.color col >> GL.vertex vtx


-- | Draw a frame
draw :: Double -> [Point] -> [Bool] -> IO ()
draw t objects stat = do
  -- Again, the functions in GL almost all map to standard OpenGL functions
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.loadIdentity
--  GL.translate $ GL.Vector3 0 0 (-1 :: GLfloat)
--  GL.scale 10 10 (1 :: GLfloat)
  forM_ (zip objects stat) drawSquare
  drawCoords
  printErrors
  GL.flush
  GLFW.swapBuffers
    where
      -- GL.rotate takes the angle in degrees, not radians
      theta = realToFrac t * 360
      axis = GL.Vector3 0 1 0 :: GL.Vector3 GLfloat

drawCoords :: IO ()
drawCoords =
   --renderPrimitive wraps the supplied action with glBegin and glEnd.
   --We'll stop using this when we switch to shaders and vertex buffers.
  GL.renderPrimitive GL.Quads $
    -- Draw a unit square centered on the origin
    forM_ [(0, 0), (1, 0), (1, 1), (0, 1)] $ \(x, y) ->
        -- Note that we have to explicitly type Vertex* and Vector*, because
        -- they are polymorphic in number field.
        let vtx = GL.Vertex3 x y 0 :: GL.Vertex3 GLfloat
            t = (x + y) / 2
            col = GL.Color4 t t t 0.5 :: GL.Color4 GLfloat
        in GL.color col >> GL.vertex vtx


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



















