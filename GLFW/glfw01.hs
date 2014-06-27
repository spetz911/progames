import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (catch)

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
  finally mainLoop quit

-- | Resize the viewport and set the projection matrix
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

-- | Draw the window and handle input
mainLoop = do
  now <- GLFW.getTime
  draw now

  -- Input is polled each time swapBuffers is called
  esc <- GLFW.keyIsPressed GLFW.KeyEsc
  isClosed <- fmap not GLFW.windowIsOpen
  unless (esc || isClosed) $ do
    -- Sleep for the rest of the frame
    frameLeft <- fmap (spf + now -) GLFW.getTime
    when (frameLeft > 0) $
        threadDelay (truncate $ 1000000 * frameLeft)
    
    mainLoop

  where
    -- maximum frame rate
    fps = 60
    spf = recip fps

-- | Draw a frame
draw :: Double -> IO ()
draw t = do
  -- Again, the functions in GL almost all map to standard OpenGL functions
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.loadIdentity
  GL.translate $ GL.Vector3 0 0 (-50 :: GL.GLfloat)
  GL.scale 10 10 (1 :: GL.GLfloat)
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
