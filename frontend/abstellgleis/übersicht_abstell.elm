

import Html exposing (..)
-- import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Anzeige exposing (..)

type Msg
  = A

main = tabelle
  [ [text "Apel", ml]
  , [text "Käseeee", text "Wurst"]
  ]

tab = table

ml = div []
  [ text "Birne"
  , text "Kirsche"
  ]

group html = div [] [html]

set = div []
  [ cell "Pamplemousse"
  , cell "Ananas"
  , cell "Jus d'orange"
  , cell "Boeuf"
  , cell "Soupe du jour"
  , cell "Camembert"
  , cell "Jacques Cousteau"
  , cell "Baguette"
  ]

cell str =
  button [ onClick A ] [ text str ]

stylesheet =
  Style.styleSheet
    [ Style.style Big
      [ Font.size (scale 3)
      ]
    , Style.style Medium
      [ Font.size (scale 2)
      ]
    , Style.style Small
      [ Font.size (scale 1)
      ]
    , Style.style NiceBackground
      [ Color.background yellow
      ]
    ]
