module GLUI where

import Graphics.Rendering.OpenGL 
import Graphics.UI.GLUT

import Data.Vector.V2 hiding (Vector2)
import Data.BoundingBox.B2 hiding (min_point, max_point)
import Data.BoundingBox.Range

import qualified Data.Vector.V2 as V
import Data.IORef ( IORef, newIORef, writeIORef, readIORef, modifyIORef)

import Core
import WorldRules
import GeometryUtils (xAxisVector, radiansToDegrees, angle)

showBattle :: World -> (World -> World) -> IO ()
showBattle initial stepper = do
    putStrLn "Press SPACE to step through the battle and 'q' to quit." 
    (progname, _) <- getArgsAndInitialize

    windowSize $= Size (round arenaWidth) (round arenaHeight)
    
    createWindow "Lambda Wars"

    get viewport >>= print
    get rgbaMode >>= print
    worldState   <-  newIORef initial
    displayCallback $= (drawWorld worldState) 
    keyboardCallback $= (Just $ keyPressed worldState stepper)
    mainLoop    


drawWorld :: IORef World -> IO ()    
drawWorld worldRef = do
    (World  bots bullets _) <- readIORef worldRef
    clear [ColorBuffer,DepthBuffer] 
    loadIdentity
    mapM_ (drawBot . snd) bots
    mapM_ drawBullet bullets
    swapBuffers

drawBot :: BotState -> IO ()    
drawBot (BotState position velocity turretDirection radarDirection _) = do
    putStrLn $ "Drawing bot at " ++ show position
    loadIdentity
    preservingMatrix $ do 
        drawHull   position velocity
        drawTurret position turretDirection
        drawRadar  position radarDirection


drawHull :: Point -> Point -> IO ()
drawHull position direction = do
        rotate hullAngle $ Vector3 0 (gX position) (gY position)
        color botColour
        renderSquare position botSize botSize
    where 
        hullAngle = 0 -- TODO work out the angle the bot is facing

-- TODO draw the bot turrent at given position
drawTurret :: Point -> Direction -> IO ()
drawTurret position direction = return ()

-- TODO draw the radar ray of the bot
drawRadar :: Point -> Direction -> IO ()
drawRadar position direction = return ()

drawBullet :: Bullet -> IO ()
drawBullet (Bullet position velocity) = preservingMatrix $ do
    color bulletColor
    renderSquare position 3 3


renderSquare :: Point -> Double -> Double -> IO ()
renderSquare position width height = do 
    renderPrimitive Polygon $ do 
            vertex $ Vertex2 (gX position - xOffset) (gY position - yOffset)
            vertex $ Vertex2 (gX position + xOffset) (gY position - yOffset)
            vertex $ Vertex2 (gX position + xOffset) (gY position + yOffset)
            vertex $ Vertex2 (gX position - xOffset) (gY position + yOffset)

    where 
        xOffset = realToFrac $ width / (2 * arenaWidth)
        yOffset = realToFrac $ height / (2 * arenaHeight)

gX :: V.Vector2 -> GLdouble
gX = realToFrac . (flip (/) width) . v2x
    where 
        minX = min_point . rangeX $ arenaBBox
        maxX = max_point . rangeX $ arenaBBox
        width = maxX - minX

gY :: V.Vector2 -> GLdouble
gY = realToFrac . (flip (/) height ) . v2y
    where 
        minY   = min_point . rangeY $ arenaBBox
        maxY   = max_point . rangeY $ arenaBBox
        height = maxY - minY 


keyPressed :: (IORef World) -> (World -> World) -> Char -> Position -> IO ()    
keyPressed _     _         'q' _ = leaveMainLoop
keyPressed world worldStep ' ' _ = modifyIORef world worldStep >> postRedisplay Nothing
keyPressed _     _          _  _ = return ()

botColour    = Color3 (166/255) (111/255) (0 :: GLfloat) 
turretColour = Color3 1 1 (1 :: GLfloat) 
bulletColor  = Color3 1 1 (1 :: GLfloat) 
