module Konami.Arrow (..) where


type alias Arrow =
  { x : Int
  , y : Int
  }


direction : Arrow -> Maybe String
direction { x, y } =
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
