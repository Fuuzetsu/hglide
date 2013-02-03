module Main where

import System.Random
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Foreign.C.Types
-- import Control.Monad.State

data LifeState = Alive | Dead deriving (Show, Eq)
data Grid = Grid { sizeX :: Int
                 , sizeY :: Int
                 , sizeZ :: Int
                 , cells :: [[[LifeState]]]
                 } deriving Show
type Coord = (Int, Int, Int)
type Cell = LifeState

data State = State { gr :: Grid }

makeState :: IO State
makeState = do
  g <- randomGrid
  return $ State { gr = g }
  
putState :: Grid -> State
putState g = State { gr = g }

dimensions = 3 -- only used as helper for now

xsize = 10
ysize = xsize
zsize = xsize

makeGrid x y z = Grid { sizeX = x
                      , sizeY = y
                      , sizeZ = z
                      , cells = [ [ [ Dead | _ <- [0 .. z - 1] ] | _ <- [0 .. y - 1] ] | _ <- [0 .. x - 1] ]
                      }

advance :: Grid -> Grid
advance g = g { cells = [ [ [ rule g (x, y, z) | z <- [0 .. (sizeZ g) - 1] ] | y <- [0 .. (sizeY g) - 1] ] | x <- [0 .. (sizeX g) - 1] ] }

advanceN :: Grid -> Integer -> Grid
advanceN g 0 = g
advanceN g n = advanceN (advance g) (n - 1)


isUnderPopulated :: Grid -> Coord -> Bool
isUnderPopulated g c = (3 ^ dimensions) `div` 4 <= (length $ filter (\x -> x == Alive) (getNeighbours g c))

isOverCrowded :: Grid -> Coord -> Bool
isOverCrowded g c = (3 ^ dimensions) `div` 3 > (length $ filter (\x -> x == Alive) (getNeighbours g c))

shouldRevive :: Grid -> Coord -> Bool
shouldRevive g c
  | getCell g c == Alive = False
  | otherwise = getCell g c == Dead && (3 ^ dimensions) `div` 3 == (length $ filter (\x -> x == Alive) (getNeighbours g c))

rule :: Grid -> Coord -> Cell
rule g c@(x,y,z)
  | cl == Alive && undrPop = Dead
  | cl == Alive && overPop = Dead
  | cl == Dead && rev = Alive
  | otherwise = cl
  where cl = getCell g c
        undrPop = isUnderPopulated g c
        overPop = isOverCrowded g c
        rev = shouldRevive g c

getCell :: Grid -> Coord -> Cell
getCell g (x,y,z) = (cells g) !! x !! y !! z

gH :: Grid -> Coord -> Coord
gH g (x, y, z) = (f x $ sizeX g, f y $ sizeY g, f z $ sizeZ g)
  where f d m = if d > (m - 1)
                then 0
                else if d < 0
                     then m - 1
                     else d

getNeighbours :: Grid -> Coord -> [Cell]
getNeighbours g c = map (\(cl, _) -> cl) $ getNeighboursWithCoords g c

getNeighboursWithCoords :: Grid -> Coord -> [(Cell, Coord)]
getNeighboursWithCoords g (x,y,z) = [ (getCell g $ gH g (x + a, y + b, z + c), (x + a, y + b, z + c)) | a <- [-1, 0, 1]
                                                                                                      , b <- [-1, 0, 1]
                                                                                                      , c <- [-1, 0, 1]
                                                                                                      , (a, b, c) /= (0, 0, 0)
                                                                                                      ]

runOnCluster :: Grid -> Coord -> (Grid -> Coord -> Grid) -> Grid
runOnCluster g c f = foldl f g $ map (\(_, crd) -> crd) (getNeighboursWithCoords g c)

killCluster, invertCluster, resurrectCluster :: Grid -> Coord -> Grid
killCluster g c = runOnCluster g c killCell
invertCluster g c = runOnCluster g c invertCell
resurrectCluster g c = runOnCluster g c resurrectCell
                                  

putCell :: Grid -> Coord -> LifeState -> Grid
putCell g (x,y,z) cl = g { cells = replaceNth x (replaceNth y (replaceNth z cl ((cells g) !! x !! y)) ((cells g) !! x)) (cells g) }

resurrectCell, killCell, invertCell :: Grid -> Coord -> Grid
killCell g c = putCell g c Dead
resurrectCell g c = putCell g c Alive
invertCell g c = putCell g c (if getCell g c == Alive then Dead else Alive)

putRow :: Grid -> Int -> Int -> LifeState -> Grid
putRow g x y cl = g { cells = replaceNth x (replaceNth y (replicate (sizeZ g) cl) ((cells g) !! x)) (cells g) }

replaceRow :: Grid -> Int -> Int -> [LifeState] -> Grid
replaceRow g x y row = g { cells = replaceNth x (replaceNth y row ((cells g) !! x)) (cells g) }

resurrectRow, killRow :: Grid -> Int -> Int -> Grid
resurrectRow g x y = putRow g x y Alive
killRow g x y = putRow g x y Dead

defaultGrid :: Grid
defaultGrid = makeGrid xsize ysize zsize

getZ :: Grid -> Int -> Int -> [LifeState]
getZ g x y = ((cells g) !! x) !! y

replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs


getRndCl :: Bool -> LifeState
getRndCl True = Alive
getRndCl _ = Dead

randomRow :: Int -> IO [LifeState]
randomRow n = do
  rs <- mapM randomRIO $ replicate n (True, False)
  return $ map getRndCl rs

randomGrid :: IO Grid
randomGrid = do
  let g = defaultGrid
  row <- randomRow $ sizeZ g
  let ng = g { cells = [ [ row | _ <- [0 .. sizeY g - 1]] | _ <- [0 .. sizeX g - 1] ]}
  return ng


keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case c of
  '\27' -> exitWith ExitSuccess
  'd' -> display $ putState $ advance $ gr state
  'r' -> makeState >>= display
  _ -> return ()
keyboard _ _ _ _ _ = return ()

display :: State -> DisplayCallback
display state = do
  clear [ ColorBuffer, DepthBuffer ]
  putStrLn "Running display"
  -- resolve overloading, not needed in "real" programs
  let color3f = color :: Color3 GLfloat -> IO ()
  let scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
  color3f (Color3 1 1 1)
  let grid = gr state
  -- putStrLn . show $ grid
  -- putStrLn ""
  -- putStrLn . show $ advance grid
  let (x,y,z) = (sizeX grid, sizeY grid, sizeZ grid)
  let crds = [f a b c | a <- [0 .. x - 1]
                      , b <- [0 .. y - 1]
                      , c <- [0 .. z - 1]
                      , getCell grid (a,b,c) == Alive
              ]
        where f a b c = (CDouble x, CDouble y, CDouble z)
                where x = 0.01 * (fromIntegral a)
                      y = 0.01 * (fromIntegral b)
                      z = 0.01 * (fromIntegral c)
  let allc = [f a b c | a <- [0 .. x - 1]
                      , b <- [0 .. y - 1]
                      , c <- [0 .. z - 1]
              ]
        where f a b c = (CDouble x, CDouble y, CDouble z)
                where x = 0.01 * (fromIntegral a)
                      y = 0.01 * (fromIntegral b)
                      z = 0.01 * (fromIntegral c)
  -- putStrLn $ show crds
  preservingMatrix $ do
    rotate (20 :: GLfloat) (Vector3 1 0.5 0)
    scalef 30 30 30
    mapM (\(a,b,c) ->
           preservingMatrix $ do
             -- color (Color3 0 0 (0 :: GLfloat))
             translate (Vector3 a b c)
             -- cube (0.01 :: GLfloat)
             color (Color3 1 1 (1 :: GLfloat))
             cubeFrame (0.01 :: GLfloat)) allc
    mapM (\(a,b,c) ->
           preservingMatrix $ do
             -- scalef 7 7 7
             color (Color3 1 0 (0 :: GLfloat))
             translate (Vector3 a b c)
             cube (0.01 :: GLfloat)) crds
             -- color (Color3 1 1 (1 :: GLfloat))
             -- cubeFrame (0.01 :: GLfloat)) crds
    translate (Vector3 4.0 0.0 (-1.0 :: GLfloat))
    -- let (a,b,c) = last crds
    -- putStrLn $ show (a,b,c)
    -- preservingMatrix $ do
    --   translate (Vector3 (a + 3) b c)
    --   scalef 3 3 3
    --   color (Color3 1 1 (1 :: GLfloat))
    --   translate (Vector3 a b c)
    --   cube (0.1 :: GLfloat)
    --   color (Color3 1 0 (0 :: GLfloat))
    --   cubeFrame (0.1 :: GLfloat)
    color (Color3 1 1 (1 :: GLfloat))
    -- preservingMatrix $ do
    --   translate (Vector3 (-0.75) 0.5 (0 :: GLfloat))
    --   rotate (90 :: GLfloat) (Vector3 1 0 0)
    --   renderObject Solid (Torus 0.275 0.85 15 15)
    -- preservingMatrix $ do
    --   translate (Vector3 (-0.75) (-0.5) (0 :: GLfloat))
    --   rotate (270 :: GLfloat) (Vector3 1 0 0)
    --   renderObject Solid (Cone 1 2 15 15)
    -- preservingMatrix $ do
    --   translate (Vector3 0.75 0 (-1 :: GLfloat))
    --   renderObject Solid (Sphere' 1 15 15)

  --lookAt (Vertex3 20 10 5) (Vertex3 0 0 0) (Vector3 0 1 0)
  flush

myInit :: IO ()
myInit = do
  ambient (Light 0) $= Color4 0 0 0 1
  diffuse (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1
  position (Light 0) $= Vertex4 1 1 1 0
  lighting $= Disabled
  light (Light 0) $= Enabled
  depthFunc $= Just Less


reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   -- if w <= h
   --   then ortho (-50) 50 (-50 * hf/wf) (50 * hf/wf) (-1) 1
   --   else ortho (-50 * wf/hf) (50 * wf/hf) (-50) 50 (-1) 1
   if w <= h
     then ortho (-2.5) 2.5 (-2.5 * hf/wf) (2.5 * hf/wf) (-10) 10
     else ortho (-2.5 * wf/hf) (2.5 * wf/hf) (-2.5) 2.5 (-10) 10
   --perspective 45 ((fromIntegral h)/(fromIntegral w)) 0.1 100
   matrixMode $= Modelview 0


 
vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 verts = sequence_ $ map (\(a,b,c) -> vertex $ Vertex3 a b c) verts 

cubeFrame w = renderPrimitive Lines $ vertify3
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

cube w = renderPrimitive Quads $ vertify3
      [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

main :: IO ()
main = do
  -- gen <- getStdGen
  -- grid <- randomGrid
  -- putStrLn . show $ grid
  -- putStrLn ""
  -- putStrLn . show $ advance grid
  (progname, _args) <- getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 100
  createWindow "hglide"
  myInit
  state <- makeState
  -- let state = State { gr = invertCluster (invertCluster (resurrectCluster defaultGrid (2,2,2)) (2,3,2)) (2, 4, 3)}
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboard state)
  mainLoop