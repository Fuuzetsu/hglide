{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import System.Random
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitSuccess )
import Foreign.C.Types
import Data.List.Split
import System.IO.Unsafe
import Data.IORef
import Data.Map (toList, fromListWith)
-- import Control.Monad.State


data ThreeDGrid = ThreeDGrid { sizeX :: Int
                             , sizeY :: Int
                             , sizeZ :: Int
                             , cells :: [[[LifeState]]]
                             } deriving (Show, Eq)
type Coord = (Int, Int, Int)
type Cell = LifeState

data State a = State { grid :: IORef a
                     , cycle :: IORef Int
                     }

makeState :: IO (State ThreeDGrid)
makeState = do
  g <- randomGrid
  gr <- newIORef g
  c <- newIORef 0
  putStrLn "In makeState"
  return State { grid = gr
               , Main.cycle = c
               }
  

data LifeState = Alive | Dead deriving (Show, Eq)
data ThreeDCoord = ThreeDCoord Int Int Int

instance NDimCoord ThreeDCoord where
  coordSize c = 3
  coords (ThreeDCoord x y z) = [x, y, z]
  getNthDim c n = coords c !! (n - 1)

instance LifeGrid ThreeDGrid where
  dimensions g = 3
  size g = map (`id` g) [sizeX, sizeY, sizeZ]
  getElem g c = cells g !! gn 1 !! gn 2 !! gn 3
    where gn = getNthDim c
  setElem g c cl = g { cells = replaceNth x (replaceNth y (replaceNth z cl (cells g !! x !! y)) (cells g !! x)) (cells g) }
    where [x, y, z] = coords c
  defaultGrid szs = makeGrid (head szs) (szs !! 1) (szs !! 2)
    where
      makeGrid x y z = ThreeDGrid { sizeX = x
                                  , sizeY = y
                                  , sizeZ = z
                                  , cells = [ [ [ Dead | _ <- [0 .. z - 1] ] | _ <- [0 .. y - 1] ] | _ <- [0 .. x - 1] ]
                                  }
  neighbours g c = [ getElem g $ gH g (ThreeDCoord (x + a) (y + b) (z + c)) | a <- [-1, 0, 1]
                                                                            , b <- [-1, 0, 1]
                                                                            , c <- [-1, 0, 1]
                                                                            , (a, b, c) /= (0, 0, 0)
                                                                            ]
    where
      [x, y, z] = coords c
      gH g cr = ThreeDCoord (f (getNthDim cr 1) $ sizeX g) (f (getNthDim cr 2) $ sizeY g) (f (getNthDim cr 3) $ sizeZ g)
        where f d m
                | d > (m - 1) = 0
                | d < 0 = m - 1
                | otherwise = d

  advance g = g { cells = [ [ [ rule g (getElem g (ThreeDCoord x y z)) (neighbours g (ThreeDCoord x y z)) | z <- [0 .. (size g !! 2) - 1] ] | y <- [0 .. (size g !! 1) - 1] ] | x <- [0 .. head (size g) - 1] ] }
  applyToGrid g f = g { cells = [ [ [ f (getElem g (ThreeDCoord x y z)) | z <- [0 .. (size g !! 2) - 1] ] | y <- [0 .. (size g !! 1) - 1] ] | x <- [0 .. head (size g) - 1] ] }
  getAliveCellCoords g = [ ThreeDCoord a b c | a <- [0 .. head (size g) - 1]
                                             , b <- [0 .. size g !! 1 - 1]
                                             , c <- [0 .. size g !! 2 - 1]
                                             , getElem g (ThreeDCoord a b c) == Alive
                                             ]
         

instance ConvinientGrid ThreeDGrid where
  resurrectGrid g = applyToGrid g (\_ -> Alive)
  killGrid g = applyToGrid g (\_ -> Dead)
  invertGrid g = applyToGrid g (\x -> if x == Alive then Dead else Alive)
  countCellStates g = let crds = [ show (getElem g (ThreeDCoord a b c)) | a <- [0 .. x - 1]
                                                                        , b <- [0 .. y - 1]
                                                                        , c <- [0 .. z - 1]
                                                                        ]
                            where x = head $ size g
                                  y = size g !! 1
                                  z = size g !! 2
                      in toList $ fromListWith (+) [(str, 1) | str <- crds]

instance DrawableGrid ThreeDGrid where
  renderGrid g =  mapM ((\(Vector3 a b c) ->
                          preservingMatrix $ do
                            color (Color3 1 0 (0 :: GLfloat))
                            translate (Vector3 a b c)
                            cube (0.1 :: GLfloat)
                            color (Color3 1 1 (1 :: GLfloat))
                            cubeFrame (0.1 :: GLfloat))
                        . toOpenGLCoord)
                  $ getAliveCellCoords g

              

instance ConvertableCoord ThreeDCoord where
  toOpenGLCoord c = Vector3 (CDouble x) (CDouble y) (CDouble z)
    where x = fromIntegral $ getNthDim c 1
          y = fromIntegral $ getNthDim c 2
          z = fromIntegral $ getNthDim c 3


class NDimCoord a where
  coordSize :: a -> Int
  coords :: a -> [Int]
  getNthDim :: a -> Int -> Int

class LifeGrid a where
  dimensions :: a -> Int
  size :: a -> [Int]
  getElem :: (NDimCoord c) => a -> c -> LifeState
  setElem :: (NDimCoord c) => a -> c -> LifeState -> a
  defaultGrid :: [Int] -> a -- pass correct number of dimensions nodeptypinglel
  neighbours :: (NDimCoord c) => a -> c -> [LifeState]
  advance :: a -> a
  applyToGrid :: a -> (LifeState -> LifeState) -> a -- map
  getAliveCellCoords :: a -> [ThreeDCoord]
  rule :: a -> LifeState -> [LifeState] -> LifeState -- passes judgment, default rules
  rule g cl ngs
    | (3 ^ dimensions g) `div` 4 <= length (filter (== Alive) ngs) = Dead
    | (3 ^ dimensions g) `div` 3 > length (filter (== Alive) ngs) = Dead
    | (3 ^ dimensions g) `div` 3 == length (filter (== Alive) ngs) = Alive
    | otherwise = Dead

class (LifeGrid a) => ConvinientGrid a where
  advanceN :: a -> Int -> a
  advanceN grid 0 = grid
  advanceN grid n = advanceN (advance grid) (n - 1)
  invertGrid :: a -> a
  resurrectGrid :: a -> a
  killGrid :: a -> a
  countCellStates :: a -> [(String, Int)] 

class (NDimCoord a) => ConvertableCoord a where
  toOpenGLCoord :: a -> Vector3 CDouble

class (LifeGrid a) => DrawableGrid a where
  renderGrid :: a -> IO [()]
  

xsize = 5
ysize = xsize
zsize = xsize

replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs


getRndCl :: Int -> LifeState
getRndCl n | xsize - (xsize `div` 5) > n = Dead
getRndCl _ = Alive

randomRow :: Int -> IO [LifeState]
randomRow n = do
  rs <- mapM randomRIO $ replicate n (1, xsize)
  return $ map getRndCl rs

randomGrid :: IO ThreeDGrid
randomGrid = do
  let g = defaultGrid [5, 5, 5]
  row <- randomRow (head (size g) * size g !! 1 * size g !! 2)
  let csplit = splitE (sizeX g * sizeY g) row
  let rsplit = map (splitE (sizeY g)) csplit
  let ng = g { cells = rsplit }
  return ng


splitE :: Int -> [a] -> [[a]]
splitE n ls
  | length ls `mod` n /= 0 = error "Can't split the list evenly"
  | otherwise = chunksOf n ls


keyboard :: (LifeGrid g, ConvinientGrid g, DrawableGrid g) => State g -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case c of
  '\27' -> exitSuccess
  't' -> display state
  'd' -> do
    update grid advance
    display state
  'i' -> do
    update grid invertGrid
    display state
  'k' -> do
    update grid killGrid
    display state
  'l' -> do
    update grid resurrectGrid
    display state
  -- 'r' -> do
  --   newst <- makeState
  --   newg <- readIORef $ grid newst
  --   -- if aliveInFive newg then putStrLn "Will be alive in 5" else putStrLn "Will be dead in 5"
  --   -- if aliveInFive newg then return () else keyboard state (Char 'r') Down undefined undefined
  --   updateR grid $ \_ -> newg
  --   display state
  'n' -> display state
  _ -> return ()
  where update gr newg = do
          gr state $~ newg
          Main.cycle state $~ \x -> x + 1
        updateR gr newg = do
          gr state $~ newg
          Main.cycle state $~ \_ -> 0
        iog = readIORef $ grid state
          

keyboard _ _ _ _ _ = return ()

display :: (DrawableGrid a, ConvinientGrid a) => State a -> DisplayCallback
display state = do
  clear [ ColorBuffer, DepthBuffer ]
  -- resolve overloading, not needed in "real" programs
  let color3f = color :: Color3 GLfloat -> IO ()
  let scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
  color3f (Color3 1 1 1)
  gr <- readIORef $ grid state
  c <- readIORef $ Main.cycle state
  putStrLn . concat $ map (\(s, v) -> s ++ ": " ++ show v ++ " ") $ countCellStates gr
  putStrLn $ "Generation: " ++ show c
  rotate (20 :: GLfloat) (Vector3 1 0.5 0)
  renderGrid gr
  translate (Vector3 4.0 0.0 (-1.0 :: GLfloat))
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
   if w <= h
     then ortho (-2.5) 2.5 (-2.5 * hf/wf) (2.5 * hf/wf) (-10) 10
     else ortho (-2.5 * wf/hf) (2.5 * wf/hf) (-2.5) 2.5 (-10) 10

   matrixMode $= Modelview 0


 
vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 = mapM_ (\(a,b,c) -> vertex $ Vertex3 a b c)

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