
import Element exposing (..)
import Element.Attributes exposing (..)
import Style
import Style.Color as Color
import Color exposing (..)
import Style.Font as Font
import Html as H
import Tabelle as Tab
import Stil

type MyStyles
  = Big
  | Medium
  | Neutral
  | Small
  | TableElement

main = inCenter tab3

tab3 =
  full Stil.Button [] <|
    table Stil.Neutral [center]
      [ [ text "text", text "k"]
      , [ text "blub", text "reihe2"]
      ]

tab2 =
  let sizes = [100,100]
  in
    column Stil.Neutral [] <|
      [ Tab.reihe sizes
          [ text "rudolf"
          , text "gummi"
            ]
      , button Stil.Button [] <|
          Tab.reihe sizes
            [ text "ganz"
            , text "gar nicht"
            ]
      ]

stylesheetTest =
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

inCenter = inCenterStyle Stil.stylesheet Stil.Neutral
inCenterStyle stylesheet topStyle content =
  Element.viewport stylesheet <|
    el topStyle [center, verticalCenter ] <|
      content

tab =
  grid Neutral [alignRight]
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
             , width = 6
             , height = 1
             , content = button Medium [] (text "dtrn")
             }
         , cell
             { start = ( 0, 0 )
             , width = 1
             , height = 1
             , content = el Medium [] (text "boxxxxxxxxxxxxxxxxxxx")
             }
         , cell
             { start = ( 1, 0 )
             , width = 1
             , height = 1
             , content = el Medium [] (text "box")
             }
         ]
     }

view =
  Element.viewport stylesheetTest <|
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
