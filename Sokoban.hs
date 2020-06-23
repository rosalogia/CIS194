{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Data Types

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C Integer Integer
data List a = Empty | Entry a (List a) deriving Eq
data State = State Coord Direction (List Coord) Integer deriving Eq
data SSState world = StartScreen | Running world
data Activity world = Activity
    world
    (Event -> world -> world)
    (world -> Picture)
data WithUndo a = WithUndo a (List a)
data Maze = Maze Coord (Coord -> Tile)


instance Eq Coord where
    C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
    c1 /= c2 = not (c1 == c2)

instance Eq s => Eq (SSState s) where
    StartScreen == StartScreen = True
    Running s == Running s' = s == s'
    _ == _ = False

-- Helper Functions

wall, ground, storage, box :: Picture

wall = colored gray (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = colored brown (solidCircle 0.5) & ground
box = colored brown (solidRectangle 1 1)

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity initialState eventHandler drawPic)
  = Activity initialState' eventHandler' drawPic'
  where
    initialState' = WithUndo initialState Empty
    
    eventHandler' (KeyPress key) (WithUndo s stack)
      | key == "U" = case stack of Entry s' stack' -> WithUndo s' stack'
                                   Empty           -> WithUndo s Empty
    eventHandler' e (WithUndo s stack)
        | s' == s     = WithUndo s stack
        | otherwise   = WithUndo (eventHandler e s) (Entry s stack)
      where s' = eventHandler e s
    drawPic' (WithUndo s _) = drawPic s

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

coordInList :: Coord -> List Coord -> Bool
coordInList c Empty = False
coordInList c (Entry lc lcs)
    | eqCoord c lc  = True
    | otherwise     = coordInList c lcs

elemList :: Eq a => a -> List a -> Bool
elemList value Empty = False
elemList value (Entry x xs)
    | value == x    = True
    | otherwise     = elemList value xs

listLength :: List a -> Integer
listLength Empty = 0
listLength l =
    length 0 l
    where
        length :: Integer -> List a -> Integer
        length n Empty = n
        length n (Entry _ xs) = length (n+1) xs

filterList :: (a -> Bool) -> List a -> List a
filterList predicate Empty = Empty
filterList predicate (Entry x xs)
    | predicate x   = Entry x (filterList predicate xs)
    | otherwise     = filterList predicate xs

nth :: List a -> Integer -> a
nth Empty _ = error "List too sort"
nth (Entry x xs) 0 = x
nth l n =
    nthItem 0 l
    where
        nthItem :: Integer -> List a -> a
        nthItem accumulator (Entry x xs)
            | n >= listLength l     = error "List too short"
            | n == accumulator      = x
            | otherwise             = nthItem (accumulator + 1) xs

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = process Empty (Entry initial Empty)
    where
        process processed Empty = True
        process processed (Entry x xs)
            | elemList x processed  = process processed xs
            | isOk x                = process (Entry x processed) (appendList (adjacent x) (xs))
            | otherwise             = False

tileIsOk :: (Coord -> Tile) -> Coord -> Bool
tileIsOk m c
    | m c == Ground || m c == Box || m c == Storage = True
    | otherwise                                     = False

adjacentTile :: (Coord -> Tile) -> Coord -> List Coord
adjacentTile m c = filterList (\t -> m t /= Wall) (mapList (\d -> adjacentCoord d c) allDirections)


isClosed :: Maze -> Bool
isClosed (Maze startingPoint mazeFunction) = (isPassable . mazeFunction $ startingPoint) && processMaze
    where
        processMaze = isGraphClosed startingPoint (adjacentTile mazeFunction) (tileIsOk mazeFunction)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

rightOf = adjacentCoord R
leftOf  = adjacentCoord L
above   = adjacentCoord U
beneath = adjacentCoord D

handleMovement :: Direction -> State -> State
handleMovement moveDirection (State position _ boxes level) = case (mazeWithBoxes (mazeFromLevel level) boxes) (adjacentCoord moveDirection position) of
    Ground  -> (State (adjacentCoord moveDirection position) moveDirection boxes level)
    Storage -> (State (adjacentCoord moveDirection position) moveDirection boxes level)
    Box     -> handleBoxes boxes
    _       -> (State position moveDirection boxes level)
    where
        handleBoxes bs
            | isPassable . (mazeWithBoxes (mazeFromLevel level) boxes) . nextTile $ boxLocation = State (nextTile position) moveDirection (mapList (moveFromTo boxLocation aboveBox) bs) level
            | otherwise                 = (State position moveDirection bs level)
            where
                nextTile t = adjacentCoord moveDirection t
                boxLocation = adjacentCoord moveDirection position
                aboveBox = adjacentCoord moveDirection boxLocation
    

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

isPassable :: Tile -> Bool
isPassable Ground = True
isPassable Storage = True
isPassable _ = False

isBox :: Tile -> Bool
isBox Box = True
isBox _ = False

checkCoord :: Coord -> Bool
checkCoord c = isPassable . maze $ c

appendList :: List a -> List a -> List a
appendList Empty Empty = Empty
appendList l Empty = l
appendList Empty l = l
appendList (Entry h hs) l2 = Entry h (appendList hs l2)

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

moveFromTo :: Eq a => a -> a -> a -> a
moveFromTo c1 c2 c | c1 == c   = c2
                   | otherwise = c

isStorage :: Tile -> Bool 
isStorage Storage = True
isStorage _ = False

isOnStorage :: Maze -> Coord -> Bool
isOnStorage m c
    | isStorage . noBoxMaze m $ c = True
    | otherwise                 = False


allList :: List Bool -> Bool
allList Empty = False
allList (Entry True Empty) = True
allList (Entry False as) = False
allList (Entry True as) = allList as

isWon :: State -> Bool
isWon (State _ _ boxes l) = allList (mapList (isOnStorage (mazeFromLevel l)) boxes)

loadLevel :: Integer -> State
loadLevel level = State (startingPosition level) U (initialBoxes . mazeFromLevel $ level) level

mazeFromLevel :: Integer -> Maze
mazeFromLevel l = nth extraMazes l

-- Level Generation

noBoxMaze :: Maze -> Coord -> Tile
noBoxMaze (Maze _ m) c
    | m c == Box = Ground
    | otherwise = m c

mazeWithBoxes :: Maze -> List Coord -> (Coord -> Tile)
mazeWithBoxes m l =
    noBoxMaze'
    where
        noBoxMaze' :: Coord -> Tile
        noBoxMaze' c
          | coordInList c l   = Box
          | otherwise         = noBoxMaze m c

traverseInnerBoxes :: Maze -> Integer -> Integer -> List Coord -> List Coord
traverseInnerBoxes (Maze _ m) x (-4) l
    | isBox . m $ (C x (-4)) = Entry (C x (-4)) l
    | otherwise                 = l
traverseInnerBoxes (Maze c m) x n l
    | isBox . m $ (C x n)    = Entry (C x n) (traverseInnerBoxes (Maze c m) x (n - 1) l)
    | otherwise                 = traverseInnerBoxes (Maze c m) x (n - 1) l

traverseBoxes :: Maze -> Integer -> List Coord -> List Coord
traverseBoxes m (-4) l = traverseInnerBoxes m (-4) 4 l
traverseBoxes m n l = appendList (traverseInnerBoxes m n 4 l) (traverseBoxes m (n - 1) l)

traverseInternal :: Maze -> Integer -> Integer -> Picture
traverseInternal m x (-4) = atCoord (C x (-4)) . drawTile $ noBoxMaze m (C x (-4))
traverseInternal m x n = (atCoord (C x n) . drawTile $ noBoxMaze m (C x n)) & (traverseInternal m x (n - 1))

traverseRow :: Maze -> Integer -> Picture
traverseRow m (-4) = traverseInternal m (-4) 4
traverseRow m n = traverseInternal m n 4 & traverseRow m (n - 1)

pictureOfMaze :: Maze -> Picture
pictureOfMaze m = traverseRow m 4

-- Box Manipulation Functions for Lesson 3

someBoxCoords :: List Coord
someBoxCoords = Entry (C 2 2) (Entry (C 3 3) (Entry (C (-1) 0) Empty))

firstBox :: List Coord -> Picture
firstBox Empty = blank
firstBox (Entry c _) = atCoord c (drawTile Box)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes coordinates = combine (mapList (\c -> atCoord c (drawTile Box)) coordinates)

-- Helper Values

allDirections :: List Direction
allDirections =
    Entry U $
    Entry D $
    Entry L $
    Entry R $
    Empty

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

player :: Picture
player = (colored white . solidPolygon $ [(-0.2,-0.15),(0.2,-0.15),(0,0.866*0.4-0.15)]) & colored blue . solidCircle $  0.5

startingPosition :: Integer -> Coord
startingPosition level = startingCoord (nth extraMazes level)
    where
        startingCoord :: Maze -> Coord
        startingCoord (Maze c _) = c

initialBoxes :: Maze -> List Coord
initialBoxes m = traverseBoxes m 4 Empty

-- Activity Modifiers

runActivity :: Activity state -> IO ()
runActivity (Activity initialState eventHandler drawPicture)
    = activityOf initialState eventHandler drawPicture
    
resetable :: Activity state -> Activity state
resetable (Activity initialState eventHandler drawPicture)  =
  Activity initialState eHandler drawPicture
  where eHandler (KeyPress key) s
          | key == "Esc" = initialState
          | otherwise = eventHandler (KeyPress key) s 
        eHandler event s = eventHandler event s 

withStartScreen :: Activity state -> Activity (SSState state)
withStartScreen (Activity initialState eventHandler drawPicture) =
    Activity initialState' eventHandler' drawPicture'
    where
        initialState' = StartScreen

        eventHandler' (KeyPress key) StartScreen
            | key == " "            = Running initialState
        eventHandler' _ StartScreen = StartScreen
        eventHandler' e (Running s) = Running (eventHandler e s)

        drawPicture' StartScreen = startScreen
        drawPicture' (Running s) = drawPicture s

-- Main Handlers

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) currentState
    | key == "Right"  = handleMovement R currentState 
    | key == "Up"     = handleMovement U currentState
    | key == "Left"   = handleMovement L currentState
    | key == "Down"   = handleMovement D currentState
handleEvent _ c      = c

finalEvent :: Event -> State -> State
finalEvent (KeyPress key) currentState
    | isWon currentState = nextLevelEvent (KeyPress key) currentState
    | otherwise = handleEvent (KeyPress key) currentState
finalEvent _ c = c

nextLevelEvent :: Event -> State -> State
nextLevelEvent (KeyPress key) (State c d b l)
    | l+1 > (listLength extraMazes) = (State c d b (l+1))
    | key == " " = finalEvent (KeyPress key) (loadLevel (l+1))
    | otherwise = (State c d b l)
nextLevelEvent _ c = c
    
drawState :: State -> Picture
drawState (State c U _ _) = atCoord c (player)
drawState (State c R _ _) = atCoord c (rotated (-pi/2) player)
drawState (State c L _ _) = atCoord c (rotated ((pi)/2) player)
drawState (State c D _ _) = atCoord c (rotated (pi) player)

draw :: State -> Picture
draw (State c d l level)
    | isWon (State c d l level)   = lettering "You Won!"
    | level >= (listLength extraMazes) = lettering "All done!"
    | otherwise             = (pictureOfBoxes l) & drawState (State c d l level) & pictureOfMaze (mazeFromLevel level)

pictureOfBools :: List Bool -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = listLength xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ Empty = blank
        go i (Entry b bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

myEvent :: Activity State
myEvent = Activity (loadLevel 1) finalEvent draw

main :: IO ()
main = runActivity (resetable (withUndo (withStartScreen (myEvent))))
-- main = drawingOf (pictureOfBoxes initialBoxes)
-- main = drawingOf (draw initialCoord)
-- main = drawingOf (pictureOfBools (mapList isClosed (extraMazes)))

-- Maze definitions, downloaded from CIS194

mazes :: List Maze
mazes =
  Entry (Maze (C 1 1)       maze9) $
  Entry (Maze (C 0 0)       maze8) $
  Entry (Maze (C (-3) 3)    maze7) $
  Entry (Maze (C (-2) 4)    maze6) $
  Entry (Maze (C 0 1)       maze5) $
  Entry (Maze (C 1 (-3))    maze4) $
  Entry (Maze (C (-4) 3)    maze3) $
  Entry (Maze (C 0 1)       maze1) $
  Empty
  
extraMazes :: List Maze
extraMazes =
  mazes

maze1 :: Coord -> Tile 
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5)   0 ) = Wall
maze3 (C (-5)   1 ) = Wall
maze3 (C (-5)   2 ) = Wall
maze3 (C (-5)   3 ) = Wall
maze3 (C (-5)   4 ) = Wall

maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4)   0 ) = Ground
maze3 (C (-4)   1 ) = Ground
maze3 (C (-4)   2 ) = Ground
maze3 (C (-4)   3 ) = Ground
maze3 (C (-4)   4 ) = Wall

maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3)   0 ) = Wall
maze3 (C (-3)   1 ) = Ground
maze3 (C (-3)   2 ) = Wall
maze3 (C (-3)   3 ) = Ground
maze3 (C (-3)   4 ) = Wall
maze3 (C (-3)   5 ) = Wall

maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2)   0 ) = Wall
maze3 (C (-2)   1 ) = Ground
maze3 (C (-2)   2 ) = Box
maze3 (C (-2)   3 ) = Box
maze3 (C (-2)   4 ) = Ground
maze3 (C (-2)   5 ) = Wall

maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1)   0 ) = Wall
maze3 (C (-1)   1 ) = Ground
maze3 (C (-1)   2 ) = Ground
maze3 (C (-1)   3 ) = Box
maze3 (C (-1)   4 ) = Ground
maze3 (C (-1)   5 ) = Wall
maze3 (C (-1)   6 ) = Wall

maze3 (C   0  (-6)) = Wall
maze3 (C   0  (-5)) = Ground
maze3 (C   0  (-4)) = Ground
maze3 (C   0  (-3)) = Ground
maze3 (C   0  (-2)) = Ground
maze3 (C   0  (-1)) = Ground
maze3 (C   0    0 ) = Wall
maze3 (C   0    1 ) = Wall
maze3 (C   0    2 ) = Wall
maze3 (C   0    3 ) = Wall
maze3 (C   0    4 ) = Ground
maze3 (C   0    5 ) = Ground
maze3 (C   0    6 ) = Wall

maze3 (C   1  (-6)) = Wall
maze3 (C   1  (-5)) = Ground
maze3 (C   1  (-4)) = Ground
maze3 (C   1  (-3)) = Ground
maze3 (C   1  (-2)) = Ground
maze3 (C   1  (-1)) = Ground
maze3 (C   1    0 ) = Wall
maze3 (C   1    1 ) = Storage
maze3 (C   1    2 ) = Storage
maze3 (C   1    3 ) = Storage
maze3 (C   1    4 ) = Ground
maze3 (C   1    5 ) = Ground
maze3 (C   1    6 ) = Wall

maze3 (C   2  (-6)) = Wall
maze3 (C   2  (-5)) = Wall
maze3 (C   2  (-4)) = Ground
maze3 (C   2  (-3)) = Ground
maze3 (C   2  (-2)) = Ground
maze3 (C   2  (-1)) = Ground
maze3 (C   2    0 ) = Wall
maze3 (C   2    1 ) = Wall
maze3 (C   2    2 ) = Wall
maze3 (C   2    3 ) = Wall
maze3 (C   2    4 ) = Wall
maze3 (C   2    5 ) = Wall
maze3 (C   2    6 ) = Wall

maze3 (C   3  (-5)) = Wall
maze3 (C   3  (-4)) = Ground
maze3 (C   3  (-3)) = Ground
maze3 (C   3  (-2)) = Storage
maze3 (C   3  (-1)) = Ground
maze3 (C   3    0 ) = Wall

maze3 (C   4  (-5)) = Wall
maze3 (C   4  (-4)) = Wall
maze3 (C   4  (-3)) = Wall
maze3 (C   4  (-2)) = Wall
maze3 (C   4  (-1)) = Wall
maze3 (C   4    0 ) = Wall

maze3 _ = Blank

maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

maze5 :: Coord -> Tile 
maze5 (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

maze6 :: Coord -> Tile 
maze6 (C x y)
  | abs x > 3  || abs y > 5                 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze7 :: Coord -> Tile
maze7 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | not (x == 2)  && y == 2   = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 2 && y == 2          = Box
  | otherwise                 = Ground
  
maze8 :: Coord -> Tile
maze8 (C x y)
  | abs x > 10 || abs y > 10    = Blank
  | x == 0 && y == 0            = Ground
  | abs x == 9 && abs y == 9    = Wall
  | abs x == 10 || abs y == 10  = Wall
  | x == y                      = Storage
  | abs x == abs y              = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0    = Storage
  | otherwise                   = Ground

maze9 :: Coord -> Tile 
maze9 (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c

maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c = maze4 c

maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9'  c      = maze9 c
