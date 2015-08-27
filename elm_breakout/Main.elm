module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (button)
import Keyboard
import Touch
import Signal exposing (..)
import Time exposing (fps, inSeconds, Time)
import Window
import Text
import List
import Markdown

{-- Part 1: Model the user input ----------------------------------------------
What information do you need to represent all relevant user input?
Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.
------------------------------------------------------------------------------}


{-| Check if the user touched one of the four screen quadrants. -}
touchInQuadrant : Int -> (Int,Int) -> Touch.Touch -> Maybe Bool
touchInQuadrant q (w,h) touch =
  let
    (centerX,centerY) = (toFloat w / 2, toFloat h / 2)
    (x,y) = (toFloat touch.x, toFloat touch.y)
    (qExists, xCmp, yCmp) = case q of
                              1 -> (True, (>), (<))
                              2 -> (True, (<), (<))
                              3 -> (True, (<), (>))
                              4 -> (True, (>), (>))
                              _ -> (False, (==), (==))
  in
    if qExists then Just (x `xCmp` centerX && y `yCmp` centerY) else Nothing


maybe : b -> (a -> b) -> Maybe.Maybe a -> b
maybe def f val = Maybe.withDefault def (Maybe.map f val)

touchUpperRight : (Int,Int) -> Touch.Touch -> Bool
touchUpperRight = (<<) (maybe False identity) << touchInQuadrant 1

touchUpperLeft : (Int,Int) -> Touch.Touch -> Bool
touchUpperLeft = (<<) (maybe False identity) << touchInQuadrant 2

touchLowerLeft : (Int,Int) -> Touch.Touch -> Bool
touchLowerLeft = (<<) (maybe False identity) << touchInQuadrant 3

touchLowerRight : (Int,Int) -> Touch.Touch -> Bool
touchLowerRight = (<<) (maybe False identity) << touchInQuadrant 4

{-| Was the upper half of the screen touched? -}
touchUpper : (Int,Int) -> Touch.Touch -> Bool
touchUpper (w,h) t = touchUpperLeft (w,h) t || touchUpperRight (w,h) t

{-| Touching the upper quadrant can be used to serve like the space key. -}
spaceSignal : Signal.Signal Bool
spaceSignal =
  let
    f space touches (w,h) = space || List.any (touchUpper (w,h)) touches
  in
    Signal.map3 f Keyboard.space Touch.touches Window.dimensions

{-| The paddle can be moved with the arrow keys
or by touching the lower quadrants. -}
dirSignal : Signal.Signal Int
dirSignal =
  let
    f arrows touches (w,h) =
      let
        touchLeft = if List.any (touchLowerLeft (w,h)) touches then 1 else 0
        touchRight = if List.any (touchLowerRight (w,h)) touches then 1 else 0
      in
        arrows.x + touchRight - touchLeft
  in
    Signal.map3 f Keyboard.arrows Touch.touches Window.dimensions


type alias UserInput = { dir : Int }

userInput : Signal UserInput
userInput = UserInput <~ (dirSignal)

type alias Input =
    { delta : Float
    , userInput : UserInput
    }
    

{-- Part 2: Model the game ----------------------------------------------------
What information do you need to represent the entire game?
Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.
For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):
    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }
------------------------------------------------------------------------------}

(gameWidth,gameHeight) = (400,600)
(halfWidth,halfHeight) = (gameWidth/2,gameHeight/2)

type alias Ball = { x:Float, y:Float, vx:Float, vy:Float, r:Float }
type alias Player = { x:Float, y:Float, vx:Float, vy:Float, w:Float, h:Float }
type alias Block = { x:Float, y:Float, w:Float, h:Float }

block x y w h = { x=x, y=y, w=w, h=h }
(blockWidth, blockHeight) = (40,15)

type alias GameState =
  { ball : Ball
  , player : Player
  , blocks : List Block
  , fps : Float
  }

blockRow y = List.map (\x -> block (blockWidth * 2 * x) (100+blockHeight*2.5*y) blockWidth blockHeight) [-2..2]
blockGrid =  List.foldr (\y l -> l ++ (blockRow y)) [] [0..3]

defaultGame : GameState
defaultGame =
  { ball = { x=0, y=0, vx=200, vy=200, r=8 }
  , player = { x=0, y=20-halfHeight, vx=0, vy=0, w=40, h=10 }
  , blocks = blockGrid
  , fps = 0
  }

{-- Part 3: Update the game ---------------------------------------------------
How does the game step from one state to another based on user input?
Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.
------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame  {delta,userInput} ({ball,player,blocks,fps} as gameState) =
  let
    (ball', blocks') = stepBall delta ball player blocks
    player' = stepPlayer delta userInput.dir player
    fps' = if (floor ball.x%10)==0 then delta else fps
    restart' = if ball'.y<(-gameHeight) then True else False
  in
    if restart'
    then defaultGame
    else 
      { gameState | ball <- ball'
                  , blocks <- blocks'
                  , player <- player'
                  , fps <- fps'}

stepPlayer delta dir player =
  let player' = stepObj delta { player | vx <- toFloat dir * 200 }
  in
    { player' |
        x <- clamp (22-halfWidth) (halfWidth-22) player'.x
    }

stepBall : Time -> Ball -> Player -> List Block -> (Ball, List Block)
stepBall delta ({x,y,vx,vy} as ball) player blocks =
  let
    hitPlayer = (ball `within` player)
    hitCeiling = (y > halfHeight - ball.r)
    ball' = stepObj delta
      { ball | vx <- stepV vx (x < ball.r - halfWidth) (x > halfWidth - ball.r)
             , vy <- stepV vy hitPlayer hitCeiling
      }
  in
    (List.foldr goBlockHits (ball',[]) blocks)

goBlockHits : Block -> (Ball,List Block) -> (Ball,List Block)
goBlockHits block (ball,blocks) =
  let
    hit = ball `within` block
    blocks' = if hit then blocks else block::blocks
    ball' = if hit then { ball | vy <- -ball.vy } else ball
  in
    (ball', blocks')

near k c n =
    n >= k-c && n <= k+c

within ball box = (ball.x |> near box.x (ball.r + box.w / 2))
               && (ball.y |> near box.y (ball.r + box.h / 2))

stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

stepObj delta ({x,y,vx,vy} as obj) =
  { obj |
      x <- x + vx * delta,
      y <- y + vy * delta
  }

{-- Part 4: Display the game --------------------------------------------------
How should the GameState be displayed to the user?
Task: redefine `display` to use the GameState you defined in part 2.
------------------------------------------------------------------------------}

display : (Int,Int) -> GameState -> Element
display (w,h) ({ball,player,blocks,fps} as gameState) =
  container w h middle <|
    collage gameWidth gameHeight <|
      [ rect gameWidth gameHeight
          |> filled blue
      , circle ball.r
          |> make ball white
      , rect player.w player.h
          |> make player white
      , group <| List.map (\b -> rect b.w b.h |> make b white) blocks
      , toForm (Markdown.toElement (toString (floor (1/fps))++" fps"))
      ]
      

make obj col shape =
  shape
    |> filled col
    |> move (obj.x, obj.y)

{-- That's all folks! ---------------------------------------------------------
The following code puts it all together and shows it on screen.
------------------------------------------------------------------------------}

input : Signal Input
input =
  let
    delta = inSeconds <~ fps 60
  in
    sampleOn delta (Input <~ delta ~ userInput)


gameState : Signal GameState
gameState =
  foldp stepGame defaultGame input


main : Signal Element
main = display <~ Window.dimensions ~ gameState
