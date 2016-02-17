module Konami where

import Konami.Arrow exposing (Arrow, direction)
import Konami.Keyboard exposing (characters)

import Html
import Graphics.Element as Element
import String
import Keyboard
import Char
import Time

-- ACTIONS

type Action
  = ArrowPress String
  | KeyPress Char
  | Tick
  | NoOp

-- MODEL

type alias Model =
  { sequence : List String
  , countDown : Int
  }

countDownClock : Int
countDownClock =
  3

initialModel : Model
initialModel =
  Model [] countDownClock

-- UPDATE

update : Action -> Model -> Model
update action model  =
  case action of
    ArrowPress direction ->
      { model |
        sequence = List.append model.sequence [direction],
        countDown = countDownClock }

    KeyPress character ->
      { model |
        sequence = List.append model.sequence [String.fromChar character],
        countDown = countDownClock }

    Tick ->
      if model.countDown - 1 > 0 then
        { model |
          countDown = model.countDown - 1 }
      else
        { model |
          sequence = [], countDown = 3 }

    NoOp ->
      model

-- SIGNALS

keyboardSignal : Signal Action
keyboardSignal =
  Signal.map (\char -> KeyPress char) characters

arrowSignal : Signal Action
arrowSignal =
  let
    sgnl = Signal.filterMap direction "" Keyboard.arrows
  in
    Signal.map (\str -> ArrowPress str) sgnl

countdownSignal : Signal Action
countdownSignal =
  Signal.map (\_ -> Tick) (Time.fps 1)

actionSignals : Signal Action
actionSignals =
  Signal.mergeMany [keyboardSignal, arrowSignal, countdownSignal]

modelSignal : Signal Model
modelSignal =
  Signal.foldp update initialModel actionSignals

-- VIEW

view : Model -> Element.Element
view model =
  Element.show model

main : Signal Element.Element
main =
  Signal.map view modelSignal
