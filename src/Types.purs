module Snake.Types where

import Data.Map


data Direction = Null | U | R | D | L

instance showDirection :: Show Direction where
  show Null = "Null"
  show U = "U"
  show R = "R"
  show D = "D"
  show L = "L"

instance eqDirection :: Eq Direction where
  (==) U U = true
  (==) R R = true
  (==) D D = true
  (==) L L = true
  (==) Null Null = true
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

instance ordPoint :: Ord Point where
  compare (Point p1) (Point p2) = case compare p1.x p2.x of
                                       EQ -> compare p1.y p2.y
                                       o  -> o

type Food = Point

type Snake = {
    body :: [Point]
  , direction :: Direction
  }

type DeathPoints = Map Point Number

type World = {
    dps  :: DeathPoints
  , food :: Food
  , snake :: Snake
  , state :: GameState
  , level :: Number
  , untilStep :: Number
  , nextDirection :: Direction
  }
