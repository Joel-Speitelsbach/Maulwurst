
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Style
import Style.Color as Color
import Color exposing (..)
import Style.Font as Font
import Html as H
import Tabelle as Tab
import Stil

main = viewport Stil.stylesheet <| el Stil.Neutral [center] <|
  Tab.reiheHeight [] 20 [300] <|
    [ Input.multiline Stil.TextField [height fill] { onChange = \_ -> () , value = "" , label = Input.hiddenLabel "" , options = [] }
    ]
