module Konami.Keyboard where

import Char
import Keyboard

characters : Signal Char
characters =
  Signal.map Char.fromCode Keyboard.presses
