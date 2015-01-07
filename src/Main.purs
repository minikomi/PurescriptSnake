module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Data.Array
import Data.Foldable
import Math

import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Canvas.Free

import Debug.Trace

data Direction = U | R | D | L

randomDirection :: forall e. Eff ( random :: Random | e ) Direction
randomDirection = do
  r <- random
  return $ case r of
            r | r <  0.25             -> U
            r | r >= 0.25 && r < 0.50 -> R
            r | r >= 0.50 && r < 0.75 -> D
            r | r >= 0.75             -> L

data Point = Point { 
    x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point p) = "{" ++ (show p.x) ++ "/" ++ (show p.y) ++ "}"

type Food = Point

randomFood :: forall e. Number -> Number -> Eff ( random :: Random | e ) Food
randomFood w h = do
  xval <- randomRange 0 w
  yval <- randomRange 0 h
  return $ Point {x: floor xval, y: floor yval}
  where randomRange lo hi = (\n -> lo + n * (hi - lo)) <$> random
        randomSign = (\n -> if n < 0.5 then (-1) else 1) <$> random

data Snake = Snake {
    body :: [Point]
  , direction :: Direction
  }

data DeathPoint = DeathPoint {
    point :: Point
  , value :: Number
  }

instance showDeathPoint :: Show DeathPoint where
  show (DeathPoint dp) = show dp.point ++ "-" ++ show dp.value

type World = {
    dps  :: [DeathPoint]
  , food :: Food
  , snake :: Snake
  }

randomWorld :: forall e. Number -> Number -> Eff ( random :: Random | e ) World
randomWorld w h = do
  initialFood      <- randomFood worldSize worldSize
  initialDirection <- randomDirection

  let initialBody  = [Point {x: floor(w/2), y: floor(h/2)}]
      initialSnake = Snake {body: initialBody, direction: initialDirection}
      initialDps   = do x <- 0 .. (w - 1)
                        y <- 0 .. (h - 1)
                        return $ DeathPoint {point: Point {x: x, y: y}, value: 0}

  return { dps: initialDps
         , food: initialFood
         , snake: Snake {body: [], direction: U}
         }

worldSize = 24

main = do
  initialWorld <- randomWorld worldSize worldSize

