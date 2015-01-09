module Snake.Main where

import Snake.Types

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.ST

import Data.Array
import Data.Map(fromList, toList, Map(), update)
import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe
import Control.Monad(foldM, when)
import Math (floor)

import Data.DOM.Simple.Events(addKeyboardEventListener, KeyboardEventType(KeydownEvent), keyCode)
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window(setInterval, globalWindow)
import Graphics.Canvas (Canvas(), Context2D(), TextMetrics(), getCanvasElementById, getContext2D, setCanvasWidth, setCanvasHeight)
import Graphics.Canvas.Free

import Debug.Trace

-- Constants

worldSize :: Number
worldSize = 13

blockSize :: Number
blockSize = 20

canvasSize :: Number
canvasSize = (worldSize * blockSize)

startLevel :: Number
startLevel = 10

maxStep :: Number
maxStep = 20

-- Generator Functions

randomFood :: forall e. Number -> Number -> Snake -> Eff ( random :: Random | e ) Food
randomFood w h s = do
  xval <- randomRange 0 w
  yval <- randomRange 0 h

  let newFood = Point {x: floor xval, y: floor yval} :: Food

  case elemIndex newFood s.body of
    -1 -> return newFood
    _  -> randomFood w h s

  where randomRange lo hi = (\n -> lo + n * (hi - lo)) <$> random
        randomSign = (\n -> if n < 0.5 then (-1) else 1) <$> random

initialSnake :: Number -> Number -> Snake
initialSnake w h = {body: initialBody, direction: Null}
  where initialBody  = [Point {x: floor(w/2), y: floor(h/2)}]

initialWorld :: forall e. Number -> Number -> Eff ( random :: Random, trace :: Trace | e ) World
initialWorld w h = do
  let s   = initialSnake w h
      dps = fromList $ do x <- 0 .. (w - 1)
                          y <- 0 .. (h - 1)
                          return $ Tuple (Point {x: x, y: y}) 1

  f <- randomFood worldSize worldSize s
  return { dps: dps
         , food: f
         , snake: s
         , state: Alive
         , level: startLevel
         , untilStep: maxStep
         , nextDirection: Null
         }


-- Drawing

drawSnakePart :: Unit -> Point -> Graphics Unit
drawSnakePart _ bodypart = do rect $ { x: (getX bodypart * blockSize)
                                     , y: (getY bodypart * blockSize) 
                                     , w: blockSize
                                     , h: blockSize }

drawSnake :: World -> Graphics Unit
drawSnake w = do
    beginPath
    setFillStyle "#00FFFF"
    let s = w.snake
    foldM drawSnakePart unit s.body
    fill
    closePath

drawFood :: World -> Graphics Unit
drawFood w = do
  let f = w.food
  beginPath
  setFillStyle "#00FF00"
  arc { x: ((getX f) * blockSize) + blockSize / 2,
        y: ((getY f) * blockSize) + blockSize / 2,
        r: blockSize / 2, 
        start: 0, 
        end: Math.pi * 2
      }
  fill
  closePath

centerText :: String -> String -> String -> Number -> Graphics Unit
centerText style font message yOffset = do
  beginPath
  setFillStyle style
  setFont font
  txtsize <- measureText message
  fillText message ((canvasSize - txtsize.width)/2) yOffset

drawGameOver :: Graphics Unit
drawGameOver = do
  centerText "#222" "40px Serif" "GAME OVER" ((canvasSize/2) + 20)
  centerText "#222" "16px Serif" "Press Space to Try Again" ((canvasSize/2) + 40)
  closePath

clear :: Graphics Unit
clear = do
  beginPath
  setFillStyle "#f2f2f2"
  rect { x: 0, y: 0, w: canvasSize, h: canvasSize }
  fill
  closePath

-- single color range (green)
-- colorbrewer2
colorRange :: [String]
colorRange = ["#f7fcf5" ,"#e5f5e0" ,"#c7e9c0" ,"#a1d99b" ,"#74c476" ,"#41ab5d" ,"#238b45" ,"#006d2c" ,"#00441b"]

drawDeathPoint :: Number -> Number -> Unit -> Tuple Point Number -> Graphics Unit
drawDeathPoint minv maxv _ p = do
  let colorIndex = floor $ (((snd p)- minv) / maxv) * (length colorRange - 1)
      color      = fromJust $ colorRange !! colorIndex

  beginPath
  setFillStyle color
  rect { x: (getX (fst p) * blockSize), y: (getY (fst p) * blockSize), w: blockSize, h: blockSize }
  fill
  closePath


drawDeathPoints :: [Tuple Point Number] -> Graphics Unit
drawDeathPoints dplist = do
  let sorted = sortBy valueSort dplist
      min    = snd $ fromJust $ head $ sorted
      max    = snd $ fromJust $ last $ sorted

  foldM (drawDeathPoint min max) unit sorted

  where valueSort dp1 dp2 = compare (snd dp1 :: Number) (snd dp2 :: Number)




drawWorld :: forall e. Context2D -> World -> Eff (canvas :: Canvas | e) Unit
drawWorld ctx w = runGraphics ctx do
    clear
    drawDeathPoints (toList w.dps)
    case w.state of
         Alive -> do drawSnake w
                     drawFood w
         _     -> do drawGameOver

-- Update World

updateWorld :: forall s e. World -> Eff (st :: ST s, random :: Random, trace :: Debug.Trace.Trace | e) World
updateWorld w = if w.untilStep >= 0 then return $ w {untilStep = w.untilStep - w.level}
           else do 
                  let 
                    s    = w.snake
                    body = s.body
                    h    = fromJust $ head body
                    dir' = if w.nextDirection /= Null then w.nextDirection else s.direction
                    h'   = case dir' of
                                  U -> Point {x: getX h    , y: getY h - 1}
                                  R -> Point {x: getX h + 1, y: getY h    }
                                  D -> Point {x: getX h    , y: getY h + 1}
                                  L -> Point {x: getX h - 1, y: getY h    }
                                  _ -> h

                    ateFood = h' == w.food
                    tail'    = if ateFood then body else deleteAt (length body - 1) 1 body
                    snake'  = s {body = (h':tail'), direction = dir'}
                    state'  = if (getX h' <  0         ||
                                  getX h' >= worldSize ||
                                  getY h' <  0         ||
                                  getY h' >= worldSize ||
                                  elemIndex h' body > 0) then Dead else Alive

                    level'  = if (ateFood && w.level < 10) then w.level + 1 else w.level
                    dps     = w.dps :: Map Point Number
                    dps'    = case state' of 
                                     Alive -> w.dps
                                     Dead  -> update (\n -> Just (n + 1) :: Maybe Number) h dps

                  food' <- if ateFood then randomFood worldSize worldSize snake'
                                      else return $ w.food

                  return $ w {dps   = dps',
                              snake = snake',
                              state = state',
                              food  = food',
                              untilStep = maxStep,
                              level = level'}


-- Tick Handling 

tick :: forall s e. Context2D -> STRef s World -> Eff (st     :: ST s,
                                                       random :: Random,
                                                       canvas :: Canvas,
                                                       trace  :: Debug.Trace.Trace | e ) Unit
tick ctx st = do
  w  <- readSTRef st
  when (w.state == Alive) do w' <- updateWorld w
                             writeSTRef st w'
                             return unit

  drawWorld ctx w

-- Key Handling 

keypress :: forall s e. STRef s World -> DOMEvent -> Eff (st     :: ST s, 
                                                          dom    :: DOM,
                                                          random :: Random,
                                                          trace  :: Debug.Trace.Trace | e) Unit
keypress st event = do
  w <- readSTRef st
  code <- keyCode event
  case w.state of
    Alive | (code == 38 || code == 39 || code == 40 || code == 37)-> do
      let newDirection = case code of 
                          38 | w.snake.direction /= D -> U
                          39 | w.snake.direction /= L -> R
                          40 | w.snake.direction /= U -> D
                          37 | w.snake.direction /= R -> L
                          _  -> w.snake.direction

      writeSTRef st $ w {nextDirection = newDirection, untilStep = 0}
      return unit

    Dead | (code == 32) -> do
      let s = initialSnake worldSize worldSize

      f <- randomFood worldSize worldSize s
      writeSTRef st $ w { food          = f
                        , snake         = s
                        , state         = Alive
                        , level         = startLevel
                        , untilStep     = maxStep
                        , nextDirection = Null
                        }
      return unit

    _ -> return unit

main = do
  canvas <- getCanvasElementById "canvas"
  setCanvasWidth canvasSize canvas
  setCanvasHeight canvasSize canvas

  ctx <- getContext2D canvas
  w   <- initialWorld worldSize worldSize
  st  <- newSTRef w

  addKeyboardEventListener KeydownEvent (keypress st) globalWindow

  setInterval globalWindow 33 $ tick ctx st

  return unit
