module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.ST

import Data.Array
import Data.Maybe.Unsafe
import Control.Monad
import Math (floor)

import Data.DOM.Simple.Events
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

data GameState = Alive | Dead

data Point = Point { 
    x :: Number
  , y :: Number
  }

getX :: Point -> Number
getX (Point p) = p.x

getY :: Point -> Number
getY (Point p) = p.y

instance eqPoint :: Eq Point where
  (==) (Point p1) (Point p2) = (p1.x == p2.x) && (p1.y == p2.y)
  (/=) p1 p2 = not $ (p1 == p2)

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

drawGameOver = do
  let message = "GAME OVER"
  beginPath
  setFillStyle "#222" 
  setFont "40px Serif"
  txtsize <- measureText message
  fillText message ((canvasSize - txtsize.width)/2) ((canvasSize/2) + 20)
  fill
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

update :: forall s e. World -> Eff (st :: ST s, random :: Random | e) World
update w = do
  let
    s    = w.snake
    body = s.body
    h    = fromJust $ head body
    h'   = case s.direction of
              U -> Point {x: getX h    , y: getY h - 1}
              R -> Point {x: getX h + 1, y: getY h}
              D -> Point {x: getX h    , y: getY h + 1}
              L -> Point {x: getX h - 1, y: getY h}
              _ -> h

    t = if h' == w.food then body
                        else deleteAt (length body - 1) 1 body
    snake' = s {body = (h':t)}
    state' = if (getX h' <  0         ||
                 getX h' >= worldSize ||
                 getY h' <  0         ||
                 getY h' >= worldSize  ) then Dead else Alive

  return $ w {snake = snake', state = state'}


tick :: forall s e. Context2D -> STRef s World -> Eff (st :: ST s, random :: Random, canvas :: Canvas | e ) Unit
tick ctx st = do
  w  <- readSTRef st
  w' <- update w
  writeSTRef st w'
  drawWorld ctx w

main = do
  canvas <- getCanvasElementById "canvas"
  setCanvasWidth canvasSize canvas
  setCanvasHeight canvasSize canvas

  ctx <- getContext2D canvas
  w   <- randomWorld worldSize worldSize
  st  <- newSTRef w

  setInterval globalWindow 200 $ tick ctx st

  return unit
