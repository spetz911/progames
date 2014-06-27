import Data.ObjectName (genObjectNames)
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (textureBinding)
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (TextureTarget(..))

-- ...

-- Generate 1 texture object
[texObject] <- genObjectNames 1

-- Make it the "currently bound 2D texture"
textureBinding Texture2D $= Just texObject
import Data.Vector.Storable (unsafeWith)

import Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D, Level, Border, TextureSize2D(..))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (Proxy(..), PixelInternalFormat(..))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (PixelData(..))

-- ...


loadImage :: IO ()
loadImage = do image <- readPng "data/Picture.png"
               case image of 
                 (Left s) -> do print s
                                exitWith (ExitFailure 1)
                 (Right d) -> do case (ImageRGBA8 (Image width height dat)) ->
  -- Access the data vector pointer
  unsafeWith dat $ \ ptr
    -- Generate the texture
    texImage2D
      -- No cube map
      Nothing
      -- No proxy
      NoProxy
      -- No mipmaps
      0
      -- Internal storage format: use R8G8B8A8 as internal storage
      RGBA8
      -- Size of the image
      (TextureSize2D width height)
      -- No borders
      0
      -- The pixel data: the vector contains Bytes, in RGBA order
      (PixelData RGBA UnsignedByte ptr)


