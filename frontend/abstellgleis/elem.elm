
import Element exposing (..)
import Element.Attributes exposing (..)
import Style
import Style.Color as Color
import Color exposing (..)
import Style.Font as Font

main =
  Element.viewport stylesheet <|
    el Neutral [center, verticalCenter ] <|
      tab

type MyStyles
  = Big
  | Medium
  | Neutral
  | Small
  | TableElement

stylesheet =
  Style.styleSheet
    [ Style.style Big
      [ Color.text darkYellow
      , Color.background white
      , Font.size 50 -- all units given as px
      ]
    , Style.style Medium
      [ Color.background white
      , Font.size 20
      ]
    , Style.style Small
      [ Font.size 13
      ]
    ]

tab =
  grid Neutral []
    { columns = [ px 100, px 100, px 100, px 100 ]
     , rows =
         [ px 100
         , px 100
         , px 100
         , px 100
         ]
     , cells =
         [ cell
             { start = ( 0, 0 )
             , width = 1
             , height = 1
             , content = el Medium [] (text "boxxxxxxxxxxxxxxxxxxx")
             }
         , cell
             { start = ( 1, 1 )
             , width = 1
             , height = 2
             , content = el Medium [] (text "box")
             }

         ]
     }

view =
  Element.viewport stylesheet <|
    el Neutral [center, verticalCenter ] <|
      row Big [spacing 10 ] <|
        [ e alignRight "hello!"
        , e (moveUp 40) "blub"
        , e center "hello!"
        , e alignRight "hello!"
        , e alignRight "hello!"
        , paragraph Medium []
            [ text "lots of text ....", el Big [] (text "this is bold"), text "lots of text ...."]
        ]

e attr str = el Medium [attr ] (text str)
