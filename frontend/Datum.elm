module Datum exposing (..)

import Date exposing (Date)
import Element as El exposing (..)
import Element.Input as Input
import Regex
import Stil exposing (Stil, scale, spacin, pading, pxx )

-- MODEL
type Model
  = Datum Date
  | DatumStr String

-- UPDATE
type Msg
  = Change String

update : Msg -> Model -> Model
update msg _ =
  case msg of
    Change str -> parse str

parse : String -> Model
parse str =
  str
  |> String.toLower
  |> Regex.replace Regex.All (Regex.regex "uhr")  (always "")
  |> Regex.replace Regex.All (Regex.regex "märz") (always "march")
  |> Regex.replace Regex.All (Regex.regex "mär")  (always "march")
  |> Regex.replace Regex.All (Regex.regex "mä")   (always "march")
  |> Regex.replace Regex.All (Regex.regex "dez")  (always "dec")
  |> Regex.replace Regex.All (Regex.regex "mai")  (always "may")
  |> Regex.replace Regex.All (Regex.regex "juni") (always "june")
  |> Regex.replace Regex.All (Regex.regex "juli") (always "july")
  |> Regex.replace Regex.All (Regex.regex "ok")   (always "oc")
  |> String.map
      (\char ->
          case char of
            '.' -> ','
            _   -> char
      )
  |> Date.fromString
  |> Result.map Datum
  |> Result.withDefault (DatumStr str)

-- VIEW
type alias Elem var = Element Stil var Msg
type alias Attrs variation = List (Attribute variation Msg)

viewPickDate : Model -> Elem var
viewPickDate model = El.row Stil.Neutral [spacin 20]
    [ text "Lieferdatum:"
    , let
        txt = format model
        istValide =
          case model of
            DatumStr _ -> False
            Datum _ -> True
      in
        textInput Input.text (Stil.TextFeld istValide) [] txt Change
    ]

format : Model -> String
format model =
  case model of
    DatumStr str -> str
    Datum datum ->
      let
        zahlen =
          List.map ((|>) datum)
            [ toString << Date.day
            , toString << Date.month
            , toString << Date.year
            , toString << Date.hour
            , String.padLeft 2 '0' << toString << Date.minute
            ]
          ++ [""]
        format = String.split "%" "%. % %, %:% Uhr"
      in String.concat <| List.map2 (++) format zahlen

type alias TextInput variation =
  Stil
  -> List (Attribute variation Msg)
  -> Input.Text Stil variation Msg
  -> Element Stil variation Msg

textInput : TextInput var -> Stil -> Attrs var -> String -> (String -> Msg) -> Elem var
textInput inputElement stil attrs content onChange =
  inputElement stil attrs
     { onChange = onChange
     , value    = content
     , label    = Input.hiddenLabel ""
     , options  = []
     }
