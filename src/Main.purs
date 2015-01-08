module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.ST

import Data.Array
import Data.Maybe.Unsafe
import Control.Monad
import Math (floor)

import Data.DOM.Simple.Events
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window(setInterval, globalWindow)
import Graphics.Canvas (Canvas(), Context2D(), TextMetrics(), getCanvasElementById, getContext2D, setCanvasWidth, setCanvasHeight)
import Graphics.Canvas.Free

import Debug.Trace

worldSize :: Number
worldSize = 24

blockSize :: Number
blockSize = 10

canvasSize :: Number
canvasSize = (worldSize * blockSize)

data Direction = Null | U | R | D | L


instance showDirection :: Show Direction where
  show U = "U"
  show R = "R"
  show D = "D"
  show L = "L"

instance eqDirection :: Eq Direction where
  (==) U U = true
  (==) R R = true
  (==) D D = true
  (==) L L = true
  (==) _ _ = false
  (/=) d1 d2 = not $ d1 == d2

data GameState = Alive | Dead

instance eqGameState :: Eq GameState where
  (==) Alive Alive = true
  (==) Dead Dead   = true
  (==) _    _      = false
  (/=) g1 g2 = not $ g1 == g2

data Point = Point { 
    x :: Number
  , y :: Number
  }

getX :: Point -> Number
getX (Point p) = p.x

getY :: Point -> Number
getY (Point p) = p.y


instance showPoint :: Show Point where
  show p = "{"  ++ (show $ getX p) ++ " / " ++ (show $ getY p) ++ "}"

instance eqPoint :: Eq Point where
  (==) (Point p1) (Point p2) = (p1.x == p2.x) && (p1.y == p2.y)
  (/=) p1 p2 = not $ p1 == p2

type Food = Point

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

type Snake = {
    body :: [Point]
  , direction :: Direction
  }

defaultSnake :: Number -> Number -> Snake
defaultSnake w h = {body: initialBody, direction: Null}
  where initialBody  = [Point {x: floor(w/2), y: floor(h/2)}]

type DeathPoint = {
    point :: Point
  , value :: Number
  }

type World = {
    dps  :: [DeathPoint]
  , food :: Food
  , snake :: Snake
  , state :: GameState
  , level :: Number
  , untilStep :: Number
  , nextDirection :: Direction
  }


randomWorld :: forall e. Number -> Number -> Eff ( random :: Random | e ) World
randomWorld w h = do
  let initialSnake = defaultSnake w h
      initialDps   = do x <- 0 .. (w - 1)
                        y <- 0 .. (h - 1)
                        return $ {point: Point {x: x, y: y}, value: 0}
  initialFood      <- randomFood worldSize worldSize initialSnake

  return { dps: initialDps
         , food: initialFood
         , snake: initialSnake
         , state: Alive
         , level: 2
         , untilStep: 10
         , nextDirection: Null
         }

drawSnakePart _ bodypart = do rect $ { x: (getX bodypart * blockSize)
                                     , y: (getY bodypart * blockSize) 
                                     , w: blockSize
                                     , h: blockSize }

drawSnake w = do
    beginPath
    setFillStyle "#00FFFF"
    let s = w.snake
    foldM drawSnakePart unit s.body
    fill
    closePath

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

centerText style font message yOffset = do
  beginPath
  setFillStyle style
  setFont font
  txtsize <- measureText message     
  fillText message ((canvasSize - txtsize.width)/2) yOffset


drawGameOver = do
  centerText "#222" "40px Serif" "GAME OVER" ((canvasSize/2) + 20)
  centerText "#222" "16px Serif" "Press Space to Try Again" ((canvasSize/2) + 40)
  closePath

clear = do
  beginPath
  setFillStyle "#f2f2f2"
  rect { x: 0, y: 0, w: canvasSize, h: canvasSize }
  fill
  closePath

drawWorld :: forall e. Context2D -> World -> Eff (canvas :: Canvas | e) Unit
drawWorld ctx w = runGraphics ctx do
    clear
    case w.state of
         Alive -> do drawSnake w
                     drawFood w
         Dead  -> do drawGameOver

update :: forall s e. World -> Eff (st :: ST s, random :: Random, trace :: Debug.Trace.Trace | e) World
update w = if w.untilStep >= 0 then return $ w {untilStep = w.untilStep - w.level}
           else do 
                  let 
                    s    = w.snake
                    body = s.body
                    h    = fromJust $ head body
                    dir' = if w.nextDirection /= Null then w.nextDirection else s.direction
                    h'   = case dir' of
                                  U -> Point {x: getX h    , y: getY h - 1}
                                  R -> Point {x: getX h + 1, y: getY h}
                                  D -> Point {x: getX h    , y: getY h + 1}
                                  L -> Point {x: getX h - 1, y: getY h}
                                  _ -> h

                    ateFood = h' == w.food

                    t      = if ateFood then body else deleteAt (length body - 1) 1 body
                    snake' = s {body = (h':t), direction = dir'}
                    state' = if (getX h' <  0         ||
                                 getX h' >= worldSize ||
                                 getY h' <  0         ||
                                 getY h' >= worldSize ||
                                 elemIndex h' body > 0) then Dead else Alive

                    level' = if (ateFood && w.level < 10) then w.level + 1 else w.level

                  food' <- if ateFood then randomFood worldSize worldSize snake'
                                      else return $ w.food

                  -- print $ show h'

                  return $ w {snake = snake',
                              state = state',
                              food  = food',
                              untilStep = 20,
                              level = level'}



tick :: forall s e. Context2D -> STRef s World -> Eff (st :: ST s, random :: Random, canvas :: Canvas, trace :: Debug.Trace.Trace | e ) Unit
tick ctx st = do
  w  <- readSTRef st
  when (w.state == Alive) do w' <- update w
                             writeSTRef st w'
                             return unit

  drawWorld ctx w

keypress :: forall s e. STRef s World -> DOMEvent -> Eff (st :: ST s, dom :: DOM, random :: Random,trace :: Debug.Trace.Trace | e) Unit
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

      -- print $ show newDirection
      writeSTRef st $ w {nextDirection = newDirection}
      return unit

    Dead | (code == 32) -> do
      w' <- randomWorld worldSize worldSize
      writeSTRef st $ w'
      return unit

    _ -> return unit

main = do
  canvas <- getCanvasElementById "canvas"
  setCanvasWidth canvasSize canvas
  setCanvasHeight canvasSize canvas

  ctx <- getContext2D canvas
  w   <- randomWorld worldSize worldSize
  st  <- newSTRef w

  addKeyboardEventListener KeydownEvent (keypress st) globalWindow

  setInterval globalWindow 33 $ tick ctx st

  return unit
