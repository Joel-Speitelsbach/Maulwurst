module Stil exposing (..)

import Style
import Style.Color as Color
import Style.Scale as Scale
import Style.Font as Font
import Style.Border as Border
import Color exposing (..)
import Types exposing (Status(Neu,InBearbeitung,Fertig))

type Stil
  = Big
  | Medium
  | Small
  | Neutral
  | TableElement
  | NiceBackground
  | Button
  | SortButton
  | TextField
  | LöschButton
  | Stat Status

scale =
  Scale.modular 16 1.618

stylesheet =
  Style.styleSheet
    [ Style.style Neutral        []
    , Style.style Small          [small]
    , Style.style Medium         [medium]
    , Style.style Big            [big]
    , Style.style NiceBackground [Color.background yellow]
    , Style.style TableElement   [small]
    , Style.style TextField
        [ border
        , Color.background (grayscale 0.8)
        , Color.text       white
        ]
    , Style.style Button
        [ Style.hover [Color.background lightPurple]
        , Color.background lightBlue
        , Color.text black
        , Font.size (scale 1)
        ]
    , Style.style SortButton
        [ Style.hover [Color.background lightYellow ]
        , Color.background lightYellow
        , Color.text black
        , Font.size (scale 1)
        ]
    , Style.style LöschButton
        [ Style.hover [Color.background red]
        , Color.background lightRed
        , Color.text black
        , Font.size (scale 1)
        ]
    , Style.style (Stat Neu) [Color.background lightBrown]
    , Style.style (Stat InBearbeitung) [Color.background lightYellow]
    , Style.style (Stat Fertig) [Color.background lightGreen]
    ]

small  = Font.size (scale 1)
medium = Font.size (scale 2)
big    = Font.size (scale 3)

border = Border.all 1
