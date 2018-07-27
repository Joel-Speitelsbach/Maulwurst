module Stil exposing (..)

import Element.Attributes as Attr
import Style
import Style.Color as Color
import Style.Scale as Scale
import Style.Font as Font
import Style.Border as Border
import Color exposing (..)

import CommonTypes exposing (Status(Neu,InBearbeitung,Fertig),Bestelltyp(..))

type Stil
  = Big
  | Medium
  | Small
  | Neutral
  | TableElement
  | NiceBackground
  | Button
  | ButtonSmall
  | SortButton
  | HiddenButton
  | TextField
  | TextFeld Bool
  | LöschButton
  | Stat (Maybe Status)
  | TabelleSpaltenName
  | Btyp Bestelltyp
  | WithBorder
  | Day DayColor

type DayColor
  = DayColor1
  | DayColor2
  | DayColor3

spacin = Attr.spacing << vergr
pading = Attr.padding << vergr
pxx    = Attr.px      << vergr

vergr i = i * scale 1 / 16

scale i =
  17 * 1.618 ^ (i-1)

stylesheet =
  Style.styleSheet
    [ Style.style Neutral        [small]
    , Style.style Small          [small]
    , Style.style Medium         [medium]
    , Style.style Big            [big]
    , Style.style NiceBackground [Color.background yellow]
    , Style.style TableElement   [small]
    , Style.style TabelleSpaltenName [small, Font.center ]
    , Style.style TextField
        [ border
        , Color.background (grayscale 0.8)
        , Color.text       white
        , small
        ]
    , Style.style (TextFeld True)
        [ border
        , Color.background (grayscale 0.8)
        , Color.text       white
        , small
        ]
    , Style.style (TextFeld False)
        [ border
        , Color.background lightRed
        , Color.text       black
        , small
        ]
    , Style.style Button
        [ Style.hover [Color.background hoverColor]
        , Color.background lightBlue
        , Color.text black
        , Font.size (scale 1.5)
        ]
    , Style.style ButtonSmall
        [ Style.hover [Color.background hoverColor]
        , Color.background lightBlue
        , Color.text black
        , small
        ]
    , Style.style HiddenButton
        [ Style.hover [Color.background hoverColor]
        , Color.background white
        , Color.text black
        , Font.size (scale 1)
        ]
    , Style.style SortButton
        [ Style.hover [Color.background hoverColor ]
        , Color.background lightBlue
        , Color.text black
        , Font.size (scale 1)
        ]
    , Style.style LöschButton
        [ Style.hover [Color.background red]
        , Color.background lightRed
        , Color.text black
        , Font.size (scale 1)
        , Border.rounded (scale 0)
        ]
    , Style.style (Stat <| Just Neu)
        [ Color.background grey
        ]
    , Style.style (Stat <| Just InBearbeitung)
        [ Color.background lightYellow
        ]
    , Style.style (Stat <| Just Fertig)
        [ Color.background lightGreen
        ]
    , Style.style (Stat Nothing)
        papierkorb
    , Style.style (Btyp Merchingen)
        [ Color.background lightBrown
        ]
    , Style.style (Btyp Adelsheim)
        [ Color.background (rgb 230 160 160)
        ]
    , Style.style (Btyp Partyservice)
        [ Color.background lightPurple
        ]
    , Style.style WithBorder
        [ border
        ]
    , Style.style (Day DayColor1)
        [ Color.background (rgb 150 150 150)
        ]
    , Style.style (Day DayColor2)
        [ Color.background (rgb 200 200 200)
        ]
    , Style.style (Day DayColor3)
        [ Color.background (rgb 250 250 250)
        ]
    ]

papierkorb =
  [ Style.hover [Color.background grey]
  , Color.text black
  , Color.background white
  , Border.rounded 20
  , Border.left 1, Border.right 1, Border.bottom 1
  ]

hoverColor = rgb 170 170 210
featherBlue = rgb 170 170 250

small  = Font.size (scale 1)
medium = Font.size (scale 2)
big    = Font.size (scale 3)

border = Border.all 1

-- partyservice =
