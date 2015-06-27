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

areaW = 500
areaH = 600


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

update : (Time, { x:Int, y:Int }) -> Model -> Model
update (timeDelta, direction) model =
  model
    |> newVelocity direction
    |> setDirection direction
    |> updatePosition timeDelta


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


updatePosition : Time -> Model -> Model
updatePosition dt ({x,y,vx,vy} as model) =
  { model |
      x <- clamp (-areaW/2) (areaW/2) (x + dt * vx),
      y <- clamp (-areaH/2) (areaH/2) (y + dt * vy)
  }


-- VIEW

view : (Int,Int) -> Model -> Element
view (w,h) {x,y,vx,vy,dir} =
  let
    verb = if vx == 0 && vy == 0 then "stand" else "walk"
    src = "imgs/hero/" ++ verb ++ "/" ++ dir ++ ".gif"
  in
    container w h middle <|
    collage areaW areaH
      [ toForm (image areaW areaH "imgs/desert.png")
      , toForm (image 42 48 src)
          |> move (x,y)
      , toForm (Markdown.toElement "Arrows to move<br/>Shift to run")
          |> move (70-areaW/2, 30-areaH/2)
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update hero input)


input : Signal (Time, { x:Int, y:Int })
input =
  Signal.map2 (,) delta (Signal.map scaleTouches Touch.taps)

scaleTouches : {x:Int,y:Int} -> {x:Int,y:Int}
scaleTouches {x,y} =
  let
     x' = if x>(areaW/2) then 1 else -1
     y' = if y>(areaH/2) then -1 else 1
     recent = (x,y)/=(0,0)
  in 
     if recent then {x=x',y=y'} else {x=0,y=0}

delta : Signal Time
delta =
  Signal.map (\t -> t / 20) (fps 25)

