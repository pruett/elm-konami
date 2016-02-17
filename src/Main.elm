module Main where

import Html
import Graphics.Element as Element
import String
import Keyboard
import Char

type alias Arrow =
  { x: Int
  , y: Int
  }

type Action
  = PressArrow Arrow
  | PressKey Char
  | NoOp

-- MODEL

type alias Model = List String

initialModel : Model
initialModel = []

-- UPDATE

update : String -> Model -> Model
update direction model  =
  List.append model [direction]

direction : Arrow -> Maybe String
direction { x, y } =
  case (x, y) of
    (1, 0) -> Just "Right"
    (-1, 0) -> Just "Left"
    (0, 1) -> Just "Up"
    (0, -1) -> Just "Down"
    _ -> Nothing

parseInt : Char ->  Maybe Int
parseInt character =
  case String.toInt (String.fromChar character) of
    Ok value ->
      Just value
    Err error ->
      Nothing

characters : Signal Char
characters =
  Signal.map Char.fromCode Keyboard.presses

integers : Signal Int
integers =
  Signal.filterMap parseInt 0 characters

arrows : Signal String
arrows =
  Signal.filterMap direction "" Keyboard.arrows

model : Signal Model
model =
  Signal.foldp update initialModel arrows

-- VIEW

view : Model -> Element.Element
view model =
  Element.show model

main : Signal Element.Element
main =
  Signal.map view model
