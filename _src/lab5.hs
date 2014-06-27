import Control.Applicative
import Control.Monad (when)
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Storable (sizeOf)
import System.FilePath ((</>))
import TGA -- Small library for TGA file handling
import Graphics.GLUtil


-- | Load a texture and set some texturing parameters.
makeTexture :: FilePath -> IO TextureObject
makeTexture filename = 
    do (width,height,pixels) <- readTGA filename
       tex <- loadTexture $ texInfo width height TexBGR pixels
       textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
       textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
       textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
       return tex


-- | Geometry data is a list of four 2D vertices.
vertexBufferData :: [GLfloat]
vertexBufferData = [0, 0, 0.5,
                    1, 0, 0.5,
                    0, 1, 0.5,
                    0.9, 0.9, 0.5]



--vertexBufferData = [-1, -1, 0.5,
--                    1, -1, 0.5,
--                    -1, 1, 0.5,
--                    0.9, 0.9, 0.5]



vertexBuffer  :: IO BufferObject
vertexBuffer = makeBuffer ArrayBuffer vertexBufferData


texCoordBuffer  :: IO BufferObject
texCoordBuffer = makeBuffer ArrayBuffer [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0 :: GLfloat]


elementBuffer :: IO BufferObject
elementBuffer = makeBuffer ElementArrayBuffer [0,1,2,1,2,3::GLuint]


getTextures :: IO [TextureObject]
getTextures = do
    t1 <- makeTexture $ "images" </> "hello1.tga"
    t2 <- makeTexture $ "images" </> "hello2.tga"
    return [t1, t2]


fadeFactor :: GLfloat
fadeFactor = 0.0



draw fadeFactorU = do
    GL.clear [ColorBuffer]
    uniform fadeFactorU $= Index1 fadeFactor
    drawElements Triangles 6 UnsignedInt offset0
    swapBuffers
    pollEvents
    draw fadeFactorU


stride = fromIntegral $ sizeOf (undefined::GLfloat) * 3
vad = VertexArrayDescriptor 3 Float stride offset0

stride2 = fromIntegral $ sizeOf (undefined::GLfloat) * 2
tcd = VertexArrayDescriptor 2 Float stride2 offset0

main :: IO ()
main = do
    True <- GLFW.initialize
    True <- GLFW.openWindow defaultDisplayOptions
                { displayOptions_width = 500
                , displayOptions_height = 500}
    GLFW.setWindowTitle "Chapter 2"
    GL.clearColor $= Color4 1 1 1 1
    -- initShaders
    vs <- loadShader VertexShader $ "shaders" </> "hello-gl.vert"
    fs <- loadShader FragmentShader $ "shaders" </> "hello-gl.frag"
    thisShader <- linkShaderProgram [vs,fs]
    fadeFactorU <- get (uniformLocation thisShader "fade_factor")
    [tu1, tu2] <- mapM (get . uniformLocation thisShader) ["textures[0]", "textures[1]"]
    positionA <- get (attribLocation thisShader "position")
    texCoordA <- get (attribLocation thisShader "texcoordX")
    -- shaders ok
    GL.currentProgram $= Just thisShader
    -- set texture
    [t1, t2] <- getTextures
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just t1
    uniform tu1 $= Index1 (0::GLint)
    activeTexture $= TextureUnit 1
    textureBinding Texture2D $= Just t2
    uniform tu2 $= Index1 (1::GLint)

    -- setupGeometry 
    bo <- vertexBuffer
    bindBuffer ArrayBuffer $= Just bo
    vertexAttribPointer positionA $= (ToFloat, vad)
    vertexAttribArray positionA $= Enabled
    tc <- texCoordBuffer
    bindBuffer ArrayBuffer $= Just tc
    vertexAttribPointer texCoordA $= (ToFloat, tcd)
    vertexAttribArray texCoordA $= Enabled
    eb <- elementBuffer
    bindBuffer ElementArrayBuffer $= Just eb
    draw fadeFactorU







