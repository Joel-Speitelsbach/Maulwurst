
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
import DatePicker

main = H.program
  { subscriptions = always Sub.none
  , init = DatePicker.init
  , update = update
  , view = \datePicker ->
      viewport Stil.stylesheet <| el Stil.Neutral [center,verticalCenter] <| html <|
        DatePicker.view Nothing DatePicker.defaultSettings datePicker
  }

update msg datePicker =
  let
    _ = Debug.log "event" event
    _ = Debug.log "currDate" <| DatePicker.focusedDate newDatePicker
    (newDatePicker, cmd, event) =
      DatePicker.update DatePicker.defaultSettings msg datePicker
  in (newDatePicker, cmd)

mainu = viewport Stil.stylesheet <| el Stil.Neutral [center] <|
  Tab.reiheHeight [] 20 [300] <|
    [ Input.multiline Stil.TextField [height fill] { onChange = \_ -> () , value = "" , label = Input.hiddenLabel "" , options = [] }
    ]
