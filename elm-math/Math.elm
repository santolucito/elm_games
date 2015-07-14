module Math where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Markdown
import Time exposing (..)
import Window
import Touch

import Html exposing (div, button, text, fromElement)
import Html.Events exposing (onClick)
import StartApp

-- MODEL

--should be able to have any thinf that can be transformed
-- Int, List a, Set a, Tree a
type alias Model =
    { listSeq : List ( List (Int)),
      goal : List (Int)
    }

initM : Model
initM =
    { listSeq = [[1,2,3]],
      goal = [1,2,3]
    }

type alias Exp = String

-- UPDATE

type Action
    = ApplyExp Exp
    | RemoveExp


update : Action -> Model -> Model
update action model =
  case action of
    ApplyExp exp ->
      let 
          newL = eval exp <| lastList model.listSeq
      in 
        { model |
            listSeq <- List.append model.listSeq [newL]
        }
     
    RemoveExp ->
      { model |
          listSeq <- initList model.listSeq
      }

eval : Exp -> List (Int) -> List(Int)
eval exp l = 
 case exp of
   "+" -> List.map (\x->(x+1) % 4) l 
   "-" -> List.map (\x->(x-1) % 4) l 
   "*" -> List.map (\x->(x*2) % 4) l 
   "t" -> case l of
           [] -> []
           (x::xs) -> xs
   "d" -> l++l
   "r" -> List.reverse l

wonGame : Model -> String
wonGame m =
  let
    done = (List.length m.listSeq) == 5 
    win = (lastList m.listSeq) == m.goal
  in
    if done
      then if win then "winner" else "loser"
      else "" 

-- VIEW
view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    f l d = group (List.map2 toImgs l [0..100])
           |> moveY (d*(-50))
    toImgs i x = 
      toForm (image 42 42 ("imgs/"++toString i++".png"))
      |> moveX (x*50)
    lists = [fromElement <| collage 300 250 <| [move (-120,100) <| group <| List.map2 f model.listSeq [0..100]]]
    result = 
      toForm (Markdown.toElement (wonGame model))
    goal = [fromElement <| collage 300 100 <| [move (-120,75) <| f model.goal 1,move (-50,-25) <|result]]
    mkButt exp = button [onClick address (ApplyExp exp)] [ Html.text (toString <| exp) ]
    buttons =
      List.map mkButt ["+","-","*", "t","d","r"]
  in 
    div []
      (buttons++ lists++ goal)


-- SIGNALS

main =
  StartApp.start { model = initM, view = view, update = update }

-- EXTRAS
initList : List a -> List a
initList l = (List.reverse <| (Maybe.withDefault l) <| List.tail <| List.reverse l)

lastList : List (List (Int)) -> List (Int)
lastList l = Maybe.withDefault [] (List.head <| List.reverse l)
