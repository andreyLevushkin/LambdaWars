module GLUI (showBattle, showTestWorld) where

import Graphics.Rendering.OpenGL 
import Graphics.UI.GLUT

import Data.Vector.V2 hiding (Vector2)

import qualified Data.Vector.V2 as V
import Data.IORef ( IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Control.Applicative 

import Data.Angle

import System.IO
import Foreign.Marshal.Alloc
import Foreign.Ptr


import Paths_LambdaWars
import Core
import WorldRules
import GeometryUtils (xAxisVector, radiansToDegrees, angleDegrees)
import qualified GeometryUtils as Geom

import SimpleBots

-- | The type to contain all the OpenGL drawing state
data GLUI = GLUI {
    hullTexture   :: TextureObject,
    turretTexture :: TextureObject
}

arenaHeightP :: GLfloat
arenaHeightP = realToFrac arenaHeight

arenaWidthP :: GLfloat
arenaWidthP = realToFrac arenaWidth

-- | This is the main function in this module. Pass in the initial world
--   to draw and a world step function to update the world on every turn.
showBattle :: World -> (World -> World) -> IO ()
showBattle initial stepper = do
    putStrLn "Press SPACE to step through the battle and 'q' to quit." 
    (progname, _) <- getArgsAndInitialize
    
    createWindow "Lambda Wars"

    -- Create an OpenGL window the same size as the Arena and transform 
    -- it so that coordinate sytem of the window matches that of the arena.
    -- This will make drawing much easier later on.
    windowSize $= Size (round arenaWidth) (round arenaHeight)
    scale (realToFrac $ 2/arenaWidth) (realToFrac $ 2/arenaHeight) (1 :: GLfloat)
    translate $ Vector3 (-arenaHeightP/2) (-arenaWidthP/2) 0

    worldState <- newIORef initial
    glui       <- initRender

    displayCallback  $= (drawWorld glui worldState) 
    keyboardCallback $= (Just $ keyPressed worldState stepper)

    mainLoop    

-- | This function is here to help debug the bot display.
--   It draws a collection of bots on screen 
showTestWorld :: IO ()
showTestWorld = showBattle world id
    where
        world      = World bots bullets arenaBBox 
        bots       = zip (repeat (start sittingDuck)) $ map mkState [0..20]
        bullets    = map mkBullet [0..10]
        mkBullet n = Bullet (V.Vector2 (n * 10) 200) (V.Vector2 1 1 )
        mkState n  = BotState position velocity turret radar Fire
            where
                position = (V.Vector2 (n * 20) (n * 20))
                velocity = (V.Vector2 0 0)
                turret   = Geom.rotate (Degrees (10 * n)) (V.Vector2 1 0)
                radar    = Geom.rotate (Degrees (-10 * n)) (V.Vector2 1 0)

initRender :: IO GLUI
initRender = do
    bodyTexturePath   <- getDataFileName "resources/body.tex"
    turretTexturePath <- getDataFileName "resources/turret.tex"
    GLUI 
      <$> loadTexture (TextureSize2D 36 36) bodyTexturePath
      <*> loadTexture (TextureSize2D 20 54) turretTexturePath

drawWorld :: GLUI -> IORef World -> IO ()    
drawWorld glui worldRef = do
    (World  bots bullets _) <- readIORef worldRef

    clear [ColorBuffer,DepthBuffer] 
    texture Texture2D $= Enabled
    blend             $= Enabled
    blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)

    mapM_ (drawBot  glui . snd) bots
    mapM_ (drawBullet glui) bullets

    flush

-- | Draw a bot
drawBot :: GLUI -> BotState -> IO ()    
drawBot glui (BotState position velocity turretDirection radarDirection _) = do
    preservingMatrix $ do 
        translate $ vectorToVector3 position
        rotateZ   $ angleDegrees velocity xAxisVector
        drawHull   glui
        drawTurret glui turretDirection
        drawRadar  glui radarDirection

-- | Draws the main body of the bot
drawHull :: GLUI -> IO ()
drawHull glui = do
    textureBinding Texture2D $= Just (hullTexture glui)
    texturedQuad (realToFrac botSize) (realToFrac botSize)
    textureBinding Texture2D $= Nothing
    where
        width  = realToFrac $ botSize / 2 :: GLfloat
        height = realToFrac $ botSize / 2 :: GLfloat

-- | Draw a bot's turret. The direction is relative to the bot hull.
drawTurret :: GLUI -> Direction -> IO ()
drawTurret glui direction = preservingMatrix $ do
    rotateZ   $ angleDegrees direction xAxisVector
    texture        Texture2D $= Enabled
    blend $= Enabled
    textureBinding Texture2D $= Just (turretTexture glui)
    texturedQuad 54 20
    textureBinding Texture2D  $=  Nothing

texturedQuad :: GLfloat -> GLfloat -> IO ()
texturedQuad width height = do
    renderPrimitive Polygon $ do
        color $ Color4 1 1 1 (1::GLfloat)

        vertex   $ Vertex2  (-xOffset) (-yOffset)
        texCoord $ TexCoord2 0 (0::GLfloat)

        vertex   $ Vertex2   xOffset  (-yOffset)
        texCoord $ TexCoord2 1 (0::GLfloat)

        vertex   $ Vertex2   xOffset  ( yOffset)
        texCoord $ TexCoord2  1 (1 :: GLfloat)

        vertex   $ Vertex2 (-xOffset) ( yOffset)
        texCoord $ TexCoord2 0 (1 ::GLfloat)

    where
        xOffset = width  / 2
        yOffset = height / 2

-- | Draw a bots radar. The direction of the radar is relative to the bot hull.
drawRadar :: GLUI ->  Direction -> IO ()
drawRadar glui direction = preservingMatrix $ do
    rotateZ   $ angleDegrees direction xAxisVector
    renderPrimitive Lines $ do
        color radarColor
        vertex $ Vertex2 0 (0::GLint)
        vertex $ Vertex2 0 (100::GLint)

-- | Draw a single bullet
drawBullet :: GLUI -> Bullet -> IO ()
drawBullet glui (Bullet position velocity) = preservingMatrix $ do
    translate $ vectorToVector3 position
    renderPrimitive Polygon $ do
        color bulletColor
        vertex $ Vertex2 (-2) (-2::GLint)
        vertex $ Vertex2   2  (-2::GLint)
        vertex $ Vertex2   2  ( 2::GLint)
        vertex $ Vertex2 (-2) ( 2::GLint)

-- | Rotate the current transform matrix by the supplied number of degrees around the Z axis origin.
rotateZ :: GLdouble -> IO ()
rotateZ angle = rotate angle $ Vector3 0 0 1

-- | Converts an 2D AC-Vector vector to a 3D OpenGL vector
vectorToVector3 :: V.Vector2 -> Vector3 GLfloat
vectorToVector3 vec = Vector3 x y 0
    where 
        x = (realToFrac . v2x $ vec) 
        y = (realToFrac . v2y $ vec) 

-- | Handle ke presses
keyPressed :: (IORef World) -> (World -> World) -> Char -> Position -> IO ()    
keyPressed _     _         'q' _ = leaveMainLoop
keyPressed world worldStep ' ' _ = modifyIORef world worldStep >> postRedisplay Nothing
keyPressed _     _          _  _ = return ()

-- Texture Loading code

-- | Loads a texture from a file. The file must contain raw RGBA values 
--   with one byte per channel. There is a PNG to RGBA conversion tool on
--   github (https://github.com/andreyLevushkin/TextureConverter) but
--   it's very hacky and woun't work for all PNGs.
loadTexture :: TextureSize2D -> String -> IO TextureObject
loadTexture texSize path = do
    [name] <- genObjectNames 1               
    textureBinding Texture2D $= (Just name)
    tex <- loadTextureBuffer path
    let pixDat = PixelData RGBA UnsignedByte tex
    texImage2D Nothing NoProxy 0 RGBA' texSize 0 pixDat

    textureFilter   Texture2D   $= ((Linear', Nothing), Linear') 
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    free tex
    return name 

-- | Allocate a byte buffer outside of Haskell runtime and load the contents of 
--   of the file into it. You are responsible for freeing the buffer.
loadTextureBuffer :: String -> IO (Ptr a)
loadTextureBuffer path = do
    h        <- openBinaryFile path ReadMode
    fileSize <- hFileSize h
    buf      <- mallocBytes (fromIntegral fileSize)
    hGetBuf h buf (fromIntegral fileSize)
    hClose h
    return buf

-- | Colors
bulletColor  = Color3 1 1 (1 :: GLfloat) 
radarColor   = Color3 1 0 (0:: GLfloat) 

