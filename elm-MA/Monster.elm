module Monster where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Keyboard
import Markdown
import Time exposing (..)
import Window
import Touch
import List.Extra


-- MODEL


type alias Card =
  {
    cal : Int
  , pos : (Int,Int)
  , selected : Bool
  , name : String
  }


testCard : Card
testCard =
  Card 100 (0,0) False "hotdog"

type alias Board = List Card

def_card : (Int,Int) -> Card
def_card x = {testCard | pos <- x}

board = 
  let 
   poses = concat <| take 9 <| map (\y-> map (\x -> (x,y)) [-1,0,1]) [-1,0,1]
  in
   map def_card poses

-- UPDATE

update : (Time, { x:Int, y:Int }, (Int,Int)) -> Board -> Board
update (timeDelta, tap, dim) board =
  processTaps tap board

processTaps : {x:Int,y:Int} -> Board -> Board
processTaps {x,y} b =
  let 
   f c =  if ((x,y)==c.pos) then {c | selected<-True} else c
  in
   map f b

{-
newVelocity : { x:Int, y:Int } -> Card -> Card
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


setDirection : { x:Int, y:Int } -> Card -> Card
setDirection {x,y} model =
  { model |
      dir <-
        if  | x > 0 -> "east"
            | x < 0 -> "west"
            | y < 0 -> "south"
            | y > 0 -> "north"
            | otherwise -> model.dir
  }
-}

-- VIEW

view : (Int,Int) -> Board -> Element
view (w,h) board =
  let
    s = \c -> if not c.selected then c.name++".png" else c.name++"_clicked.png"
    f =  (\x -> toForm (image (w//9) (h//9) ("imgs/"++(s x))))
    imgs = map f board
    mapT (x,y) = (toFloat <| 150*x, toFloat <| 80*y)
    locs = map (\c-> (mapT c.pos)) board
    fin = map2 move locs imgs
  in
    container w h middle <|
    collage w h <|
      [(group fin)]

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update board input)

input : Signal (Time, { x:Int, y:Int }, (Int,Int))
input =
  Signal.map3 (,,) delta (Signal.map2 scaleTouches Window.dimensions Touch.taps) Window.dimensions

dimensions : Signal (Int,Int)
dimensions = 
  Window.dimensions

scaleTouches : (Int,Int) -> {x:Int,y:Int} -> {x:Int,y:Int}
scaleTouches (dim_x,dim_y) {x,y} =
  let
     x' = (x//dim_x)*3-4
     y' = (y//dim_y)*3-4
     recent = (x,y)/=(0,0)
  in 
     if recent then {x=x',y=y'} else {x=0,y=0}

delta : Signal Time
delta =
  Signal.map (\t -> t / 20) (fps 25)


