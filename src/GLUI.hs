module GLUI where

import Graphics.Rendering.OpenGL 
import Graphics.UI.GLUT

import Data.Vector.V2 hiding (Vector2)
import Data.BoundingBox.B2 hiding (min_point, max_point)
import Data.BoundingBox.Range

import qualified Data.Vector.V2 as V
import Data.IORef ( IORef, newIORef, writeIORef, readIORef, modifyIORef)

import Data.Angle

import Core
import WorldRules
import GeometryUtils (xAxisVector, radiansToDegrees, angleDegrees)
import qualified GeometryUtils as Geom

import SimpleBots

arenaHeightP :: GLfloat
arenaHeightP = realToFrac arenaHeight

arenaWidthP :: GLfloat
arenaWidthP = realToFrac arenaWidth

showBattle :: World -> (World -> World) -> IO ()
showBattle initial stepper = do
    putStrLn "Press SPACE to step through the battle and 'q' to quit." 
    (progname, _) <- getArgsAndInitialize
    
    createWindow "Lambda Wars"
    windowSize $= Size (round arenaWidth) (round arenaHeight)
    scale (realToFrac $ 2/arenaWidth) (realToFrac $ 2/arenaHeight) (1 :: GLfloat)
    translate $ Vector3 (-arenaHeightP/2) (-arenaWidthP/2) 0
    worldState   <-  newIORef initial

    displayCallback  $= (drawWorld worldState) 
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

drawWorld :: IORef World -> IO ()    
drawWorld worldRef = do
    (World  bots bullets _) <- readIORef worldRef
    clear [ColorBuffer,DepthBuffer] 
    mapM_ (drawBot . snd) bots
    mapM_ drawBullet bullets
    flush

-- | Draw a bot
drawBot :: BotState -> IO ()    
drawBot (BotState position velocity turretDirection radarDirection _) = do
    preservingMatrix $ do 
        translate $ vectorToVector3 position
        rotateZ   $ angleDegrees velocity xAxisVector
        drawHull  
        drawTurret turretDirection
        drawRadar  radarDirection

-- | Draws the main body of the bot
drawHull :: IO ()
drawHull = do
    renderPrimitive Polygon $ do
        color botColour
        vertex $ Vertex2 (-width) (-height)
        vertex $ Vertex2   width  (-height)
        vertex $ Vertex2   width  ( height)
        vertex $ Vertex2 (-width) ( height)
    where
        width  = realToFrac $ botSize / 2 :: GLfloat
        height = realToFrac $ botSize / 2 :: GLfloat


-- | Draw a bot's turret. The direction is relative to the bot hull.
drawTurret :: Direction -> IO ()
drawTurret direction = preservingMatrix $ do
    rotateZ   $ angleDegrees direction xAxisVector
    renderPrimitive Polygon $ do
        color turretColour
        vertex $ Vertex2 (-2)  (-2::GLint)
        vertex $ Vertex2   0   ( 5::GLint) 
        vertex $ Vertex2   2   (-2::GLint)

-- | Draw a bots radar. The direction of the radar is relative to the bot hull.
drawRadar :: Direction -> IO ()
drawRadar direction = preservingMatrix $ do
    rotateZ   $ angleDegrees direction xAxisVector
    renderPrimitive Lines $ do
        color radarColor
        vertex $ Vertex2 0 (0::GLint)
        vertex $ Vertex2 0 (100::GLint)

-- | Draw a single bullet
drawBullet :: Bullet -> IO ()
drawBullet (Bullet position velocity) = preservingMatrix $ do
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

-- | Colors
botColour    = Color3 (166/255) (111/255) (0 :: GLfloat) 
turretColour = Color3 1 1 (0 :: GLfloat) 
bulletColor  = Color3 1 1 (1 :: GLfloat) 
radarColor   = Color3 1 0 (0:: GLfloat) 

