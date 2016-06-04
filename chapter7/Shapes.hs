module Shapes(
Point(..),
Shape(..),
area,
nudge,
baseCircle,
baseRect 
    )where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float 
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape 
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape 
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person {firstName::String,
                      lastName:: String,
                      age::Int,
                      height::Float,
                      phoneNumber::String,
                      flavor::String 
                      } deriving (Show)


