import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (catch)

type Point = (Int, Int)

-- TODO make screen coords; change mouse position

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
  GLFW.setWindowSizeCallback $ resize
  -- Use `$=` for assigning to GL values, `get` to read them.
  -- These functions basically hide IORefs.
  GL.depthFunc $= Just GL.Less
  -- Use `finally` so that `quit` is called whether or
  -- not `mainLoop` throws an exception
  finally (mainLoop False Nothing [] []) quit

moveObjects :: [(Int, Int)] -> Maybe (Int, Int) -> [(Int, Int)]
moveObjects objects (Just (x0,y0)) = 
  map move objects
  where move (x,y) = (x0 + (x-x0) * 99 `div` 100, y0 + (y-y0) * 99 `div` 100)

moveObjects objects Nothing = objects


distance2 (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

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

-- | Draw the window and handle input
mainLoop :: Bool -> Maybe Point -> [Point] -> [Bool] -> IO ()
mainLoop prev prevPos objects stat = do
  now <- GLFW.getTime
  draw now objects stat
  -- Input is polled each time swapBuffers is called
  esc  <- GLFW.keyIsPressed GLFW.KeyEsc
  btn0 <- GLFW.mouseButtonIsPressed GLFW.MouseButton0
  mPos <- GLFW.getMousePosition
  let onPress     = btn0 == True && prev == False
      onRelease   = prev == True && btn0 == False
      newObjects1 = if onPress then (mPos:objects) else objects
      (runToCenter, newObjects11, stat1) = if onPress
        then 
          procButton mPos objects stat
        else
          (prevPos, objects, stat)
      newObjects2 = moveObjects newObjects11 runToCenter
  if onPress then
    print $ take 3 newObjects2
  else
    putStr ""
  isClosed <- fmap not GLFW.windowIsOpen
  -- DRAW IT!!!
  unless (esc || isClosed) $ do
    -- Sleep for the rest of the frame
    frameLeft <- fmap (spf + now -) GLFW.getTime
    when (frameLeft > 0) $
        threadDelay (truncate $ 1000000 * frameLeft)
    mainLoop btn0 runToCenter newObjects2 stat1
  where
    -- maximum frame rate
    fps = 60
    spf = recip fps

drawSquare :: (Point, Bool) -> IO ()
drawSquare ((a,b), st) =
  GL.renderPrimitive GL.Quads $
    -- Draw a unit square centered on the origin
    forM_ [(0, 0), (1, 0), (1, 1), (0, 1)] $ \(x, y) ->
       let a0 = fromIntegral a / 100 :: GL.GLfloat
           b0 = fromIntegral b / 100 :: GL.GLfloat
           vtx = GL.Vertex3 (x - 0.5 + a0) (y - 0.5 + b0) 0 :: GL.Vertex3 GL.GLfloat
           col = GL.Color4 x y 0 1 :: GL.Color4 GL.GLfloat
        in GL.color col >> GL.vertex vtx


-- | Draw a frame
draw :: Double -> [Point] -> [Bool] -> IO ()
draw t objects stat = do
  -- Again, the functions in GL almost all map to standard OpenGL functions
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.loadIdentity
  GL.translate $ GL.Vector3 0 0 (-50 :: GL.GLfloat)
  GL.scale 10 10 (1 :: GL.GLfloat)
  forM_ (zip objects stat) drawSquare
  GL.rotate theta axis
  -- renderPrimitive wraps the supplied action with glBegin and glEnd.
  -- We'll stop using this when we switch to shaders and vertex buffers.
  GL.renderPrimitive GL.Quads $
    -- Draw a unit square centered on the origin
    forM_ [(0, 0), (1, 0), (1, 1), (0, 1)] $ \(x, y) ->
        -- Note that we have to explicitly type Vertex* and Vector*, because
        -- they are polymorphic in number field.
        let vtx = GL.Vertex3 (x - 0.5) (y - 0.5) 0 :: GL.Vertex3 GL.GLfloat
        in GL.vertex vtx
  printErrors
  GL.flush
  GLFW.swapBuffers
    where
      -- GL.rotate takes the angle in degrees, not radians
      theta = realToFrac t * 360
      axis = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat


-- | Resize the viewport and set the projection matrix
resize :: Int -> Int -> IO ()
resize w h = do
  -- These are all analogous to the standard OpenGL functions
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 45 (fromIntegral w / fromIntegral h) 1 100
  GL.matrixMode $= GL.Modelview 0


-- | Close the window and terminate GLFW
quit = GLFW.closeWindow >> GLFW.terminate


-- | This will print and clear the OpenGL errors
printErrors = GL.get GL.errors >>= mapM_ print



















