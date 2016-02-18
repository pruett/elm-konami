module Konami where

import Konami.Arrow exposing (Arrow, direction)
import Konami.Keyboard exposing (characters)
import Konami.Constants as Constants

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
  | CheckSequence
  | Tick
  | NoOp

-- MODEL

type alias Model =
  { sequence : List String
  , countDown : Int
  , correct : Bool
  }

initialModel : Model
initialModel =
  Model [] Constants.countDownClock False

-- UPDATE

update : Action -> Model -> Model
update action model  =
  case action of
    ArrowPress direction ->
      { model |
        sequence = List.append model.sequence [direction],
        countDown = Constants.countDownClock }

    KeyPress character ->
      let
        str =
          character
            |> String.fromChar
            |> String.toUpper
      in
        { model |
          sequence = List.append model.sequence [str],
          countDown = Constants.countDownClock }

    CheckSequence ->
      let
        currentSequence =
          model.sequence
            |> List.reverse
            |> List.take 9
      in
        if currentSequence == Constants.konamiCode then
          { model |
            sequence = [],
            countDown = Constants.countDownClock,
            correct = True }
        else
          { model |
            sequence = List.append model.sequence ["A"],
            countDown = Constants.countDownClock }

    Tick ->
      if model.countDown - 1 > 0 then
        { model |
          countDown = model.countDown - 1 }
      else
        { model |
          sequence = [], countDown = Constants.countDownClock }

    NoOp ->
      model


-- SIGNALS

handleKeypress : Char -> Action
handleKeypress char =
  if Char.toUpper char == 'A' then
    CheckSequence
  else
    KeyPress char

keyboardSignal : Signal Action
keyboardSignal =
  Signal.map handleKeypress characters

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
