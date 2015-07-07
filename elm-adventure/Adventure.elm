module Adventure where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Markdown
import Time exposing (..)
import Window
import Touch



-- MODEL


type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : String
  }


hero : Model
hero =
  Model 0 0 0 0 "south"


-- UPDATE

update : (Time, { x:Int, y:Int }, (Int,Int)) -> Model -> Model
update (timeDelta, direction, (dim_x,dim_y)) model =
  model
    |> newVelocity direction
    |> setDirection direction
    |> updatePosition timeDelta (dim_x,dim_y) 


newVelocity : { x:Int, y:Int } -> Model -> Model
newVelocity  {x,y} model =
  let
    scale = 1
    newVel n =
      if x == 0 || y == 0 then
        scale * toFloat n
      else
        scale * toFloat n / sqrt 2
  in
      { model |
          vx <- newVel x,
          vy <- newVel y
      }


setDirection : { x:Int, y:Int } -> Model -> Model
setDirection {x,y} model =
  { model |
      dir <-
        if  | x > 0 -> "east"
            | x < 0 -> "west"
            | y < 0 -> "south"
            | y > 0 -> "north"
            | otherwise -> model.dir
  }


updatePosition : Time -> (Int,Int) -> Model -> Model
updatePosition dt (dim_x,dim_y) ({x,y,vx,vy} as model) =
 let f = toFloat
 in
  { model |
      x <- clamp (f(-dim_x//2)) (f(dim_y//2)) (x + dt * vx),
      y <- clamp (f(-dim_x//2)) (f(dim_y//2)) (y + dt * vy)
  }


-- VIEW

view : Float -> (Int,Int) -> Model -> Element
view t (w,h) {x,y,vx,vy,dir} =
  let
    verb = if vx == 0 && vy == 0 then "stand" else "walk"
    src = "imgs/hero/" ++ verb ++ "/" ++ dir ++ ".gif"
    f = toFloat
  in
    container w h middle <|
    collage w h
      [ toForm (image w h "imgs/desert_test.png")
      , toForm (image 42 48 src)
          |> move (x,y)
      , toForm (Markdown.toElement (toString (1000/t)))
          |> move (f( 100-w//2),f (30-h//2))
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map3 view delta Window.dimensions (Signal.foldp update hero input)

input : Signal (Float, { x:Int, y:Int }, (Int,Int))
input =
  Signal.map3 (,,) delta (Signal.map2 scaleTouches Window.dimensions Touch.taps) Window.dimensions

dimensions : Signal (Int,Int)
dimensions = 
  Window.dimensions

scaleTouches : (Int,Int) -> {x:Int,y:Int} -> {x:Int,y:Int}
scaleTouches (dim_x,dim_y) {x,y} =
  let
     x' = if x>(dim_x//2) then 1 else -1
     y' = if y>(dim_y//2) then -1 else 1
     recent = (x,y)/=(0,0)
  in 
     if recent then {x=x',y=y'} else {x=0,y=0}

delta : Signal Float
delta =
  Signal.map (\t -> t ) (fps 200)

