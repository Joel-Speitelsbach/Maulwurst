module Anzeige exposing (Farbe(Hinschauen,Neutral),tabelle,tabelleText)

import Html exposing (..)

type Farbe
  = Hinschauen
  | Neutral

type alias Element
  = Html ()

tabelle : List (List (Html msg)) -> Html msg
tabelle hss = table [] <| List.map (tr []) hss

tabelleText : List (List String) -> Html msg
tabelleText sss = tabelle <| List.map (List.map (\str -> td [] [text str])) sss
