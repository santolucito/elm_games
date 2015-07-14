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
type alias Item =
    { rot : Int,
      color : Int
    } 

type alias Model =
    { listSeq : List ( List (Item)),
      goal : List (Item)
    }

mkI (r,c) = { rot = r, color = c}

initM : Model
initM =
    { listSeq = [List.map mkI [(1,1),(1,2),(1,3)]],
      goal = List.map mkI [(1,1),(1,2),(1,3)]
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

eval : Exp -> List (Item) -> List(Item)
eval exp l = 
 case exp of
   "+h" -> case l of
            [] -> []
            (x::xs) -> (mkI (((x.rot+1) % 4),x.color)) :: xs
   "d" -> l++l
   "+" -> List.map (\m->mkI (((m.rot+1) % 4),m.color)) l 
   "-" -> List.map (\m->mkI (((m.rot-1) % 4),m.color)) l 
   --"*" -> List.map (\m->(m*2) % 4) l 
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
      toForm (image 42 42 ("imgs/"++toString i.rot++"."++toString i.color++".png"))
      |> moveX (x*50)
    lists = [fromElement <| collage 300 250 <| [move (-120,100) <| group <| List.map2 f model.listSeq [0..100]]]
    result = 
      toForm (Markdown.toElement (wonGame model))
    goal = [fromElement <| collage 300 100 <| [move (-120,75) <| f model.goal 1,move (-50,-25) <|result]]
    mkButt exp = button [onClick address (ApplyExp exp)] [ Html.text (toString <| exp) ]
    buttons =
      List.map mkButt ["+h","+","-","t","d","r"]
  in 
    div []
      (buttons++ lists++ goal)


-- SIGNALS

main =
  StartApp.start { model = initM, view = view, update = update }

-- EXTRAS
initList : List a -> List a
initList l = (List.reverse <| (Maybe.withDefault l) <| List.tail <| List.reverse l)

lastList : List (List (Item)) -> List (Item)
lastList l = Maybe.withDefault [] (List.head <| List.reverse l)
