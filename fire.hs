import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad
import Control.Monad.State
import System.Random

data FireState = FireState
    { randomGen :: !StdGen
    }

type Fire = State FireState

xCells :: Int
xCells = 100
yCells :: Int
yCells = heatLayers

heatLayers :: Int
heatLayers = 100
heatLoss :: Fire GLfloat
heatLoss = rand >>= \r -> return $ (0.002 * r)

rand :: Fire GLfloat
rand = do
    rg <- gets randomGen
    let (r, newrg) = random rg
    modify $ \st -> st {randomGen = newrg}
    return $ fromInteger r

getScreenQuad :: GLfloat -> GLfloat -> [(GLfloat,GLfloat)]
getScreenQuad x y = [getScreenPoint x y, getScreenPoint x (y + 1), getScreenPoint (x + 1) (y + 1), getScreenPoint (x + 1) y]
    where
        getScreenPoint :: GLfloat -> GLfloat -> (GLfloat,GLfloat)
        getScreenPoint x y = (2*x/fromIntegral xCells - 1, 2*y/fromIntegral yCells - 1)

fireCoords :: [[[(GLfloat,GLfloat)]]]
fireCoords = [
  [getScreenQuad (fromIntegral x) (fromIntegral y) | x <- [0..(xCells-1)]]
    | y <- [0..(yCells-1)]]

-- white - yellow - red - black
colorHeat :: GLfloat -> (GLfloat,GLfloat,GLfloat)
colorHeat x = normalify x $
        if x <= borderRedYellow
        then blackRed x
        else if x <= borderYellowWhite
        then redYellow x
        else white x
    where
        --Color-functions:
        white :: GLfloat -> (GLfloat,GLfloat,GLfloat)
        white x = (1,1,sin (pi * 0.5 * (1 / (1 - borderYellowWhite)) * (x - borderYellowWhite)))
        redYellow :: GLfloat -> (GLfloat,GLfloat,GLfloat)
        redYellow x = (1,sin (pi * 0.5 * (1 / (borderYellowWhite - borderRedYellow)) * (x - borderRedYellow)),0)
        blackRed :: GLfloat -> (GLfloat,GLfloat,GLfloat)
        blackRed x = (sin (pi * 0.5 * (1/borderRedYellow) * x),0,0)
        --Little helpers:
        borderYellowWhite :: GLfloat
        borderYellowWhite = 1/2
        borderRedYellow :: GLfloat
        borderRedYellow = 1/4
        normalify :: GLfloat -> (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat)
        normalify x (a,b,c) = (norm x a, norm x b, norm x c)
             where
                 norm :: GLfloat -> GLfloat -> GLfloat
                 norm x t = (min 1 $ max 0 x) * t

calcHeat :: [GLfloat] -> Fire GLfloat
calcHeat x = heatLoss >>= \hl -> return $ min 1 (max 0 $ sum x / fromIntegral(length x) - hl)

generateNextHeatLine :: [GLfloat] -> Fire [GLfloat]
generateNextHeatLine x = sequence $ heatLine $ 0:x++[0]
   where
       heatLine :: [GLfloat] -> [Fire GLfloat]
       heatLine x@(a:b:c:_) = (calcHeat [a,b,c]) : (heatLine $ tail x)
       heatLine _           = []

-- lines -> initial line -> complete Lineset, including initial line
makeHeat :: Int -> [GLfloat] -> Fire [[GLfloat]]
makeHeat 0 x = return [x]
makeHeat remainingLines x = do
   nextHeatLine <- generateNextHeatLine x
   liftM2 (:) (return x) $ makeHeat (remainingLines - 1) nextHeatLine

madeHeat :: Fire [[GLfloat]]
madeHeat = do
   rl <- replicateM xCells rand
   heat <- makeHeat (heatLayers + 1) rl
   return $ drop 2 heat

fire :: Fire [[(GLfloat, [(GLfloat, GLfloat)])]]
fire = liftM (map $ uncurry zip) $ liftM2 zip madeHeat (return fireCoords)

colorify :: (GLfloat, GLfloat, GLfloat) -> Color3 GLfloat
colorify (x,y,z) = Color3 x y z

vertexify :: [(GLfloat, GLfloat)] -> [Vertex2 GLfloat]
vertexify = map (uncurry Vertex2)

display :: IO ()
display = do
   clear [ColorBuffer]
   rg <- newStdGen
   let f = evalState fire $ FireState { randomGen = rg }
   renderPrimitive Quads $ forM f $ mapM $ \value -> do
       color . colorify . colorHeat . fst $ value
       mapM vertex $ vertexify . snd $ value
   flush

main :: IO ()
main = do
 (progname, _) <- getArgsAndInitialize
 createWindow "Fire"
 displayCallback $= display
 mainLoop
