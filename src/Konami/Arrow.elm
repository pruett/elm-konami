module Konami.Arrow (Arrow, getDirection) where
import Keyboard


type alias Arrow =
  { x : Int
  , y : Int
  }


parseDirection : Arrow -> Maybe String
parseDirection { x, y } =
  case ( x, y ) of
    ( 1, 0 ) ->
      Just "Right"

    ( -1, 0 ) ->
      Just "Left"

    ( 0, 1 ) ->
      Just "Up"

    ( 0, -1 ) ->
      Just "Down"

    _ ->
      Nothing


getDirection : Signal String
getDirection =
  Signal.filterMap parseDirection "" Keyboard.arrows
