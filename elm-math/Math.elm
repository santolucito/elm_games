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
    { listSeq : List ( List (Int))
    }

initM : Model
initM =
    { listSeq = [[1,2,3]]
    }

type alias Exp = String

--anything that has :: [a] -> [a]
-- map (+1) 
-- head
-- tail
-- rev 
type Exp
    = ListToList
    | ListToXToList

type ListToList
    = String

type ListToXToList
    = String X

type 

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
   "+" -> List.map (\x->x+1) l 
   "-" -> List.map (\x->x-1) l 

-- VIEW
view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    f s d = toForm (Markdown.toElement (toString s))
            |> moveY (d*(-30))

    lists = [fromElement <| collage 100 300 <| List.map2 f model.listSeq [0..100]]
    mkButt exp = button [ onClick address (ApplyExp exp)] [ Html.text (toString <| exp) ]
    buttons =
      List.map mkButt ["+","-"]
  in 
    div []
      (List.append buttons lists)


-- SIGNALS

main =
  StartApp.start { model = initM, view = view, update = update }

-- EXTRAS
initList : List a -> List a
initList l = (List.reverse <| (Maybe.withDefault l) <| List.tail <| List.reverse l)

lastList : List (List (Int)) -> List (Int)
lastList l = Maybe.withDefault [] (List.head <| List.reverse l)
