module Datum
  exposing (..)
  -- exposing
  --   ( Model(..)
  --   , update
  --   , viewPickDate
  --   , toStr
  --   )

import Date exposing (Date)
import Time
import Element as El exposing (..)
import Element.Input as Input
import Regex
import Stil exposing (Stil, scale, spacin, pading, pxx )


-- MODEL

type Model
  = Datum Date
  | DatumStr String


init : Model
init = DatumStr ""



-- UPDATE

type Msg
  = Change String


update : { msg : Msg, model : Model, today : Date } -> Model
update { msg, today } =
  case msg of
    Change str -> parseToday { str = str, today = today }


parseToday : { today : Date, str : String } -> Model
parseToday { today, str } =
  parseWeekday { str = str }
  |> Maybe.map
      (\weekday ->
          Datum <| nextWeekday { today = today, weekday = weekday })
  |> Maybe.withDefault (parse str)


parseWeekday : { str : String } -> Maybe Date.Day
parseWeekday { str } =
  case String.left 2 <| String.toLower str of
    "mo" -> Just Date.Mon
    "di" -> Just Date.Tue
    "tu" -> Just Date.Tue
    "mi" -> Just Date.Wed
    "we" -> Just Date.Wed
    "do" -> Just Date.Thu
    "th" -> Just Date.Thu
    "fr" -> Just Date.Fri
    "sa" -> Just Date.Sat
    "so" -> Just Date.Sun
    "su" -> Just Date.Sun
    _    -> Nothing


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
            _   -> char)
  |> Date.fromString
  |> Result.map Datum
  |> Result.withDefault (DatumStr str)



-- VIEW

type alias Elem = Element Stil Never Msg
type alias Attrs = List (Attribute Never Msg)


viewPickDate : { model : Model, label : Maybe String } -> Elem
viewPickDate arg =
  let
    txt = toStr arg.model
    istValide =
      case arg.model of
        DatumStr _ -> False
        Datum    _ -> True
    label =
      case arg.label of
        Just str -> Input.labelLeft (text str)
        Nothing -> Input.hiddenLabel ""
  in
    Input.text (Stil.TextFeld istValide) []
      { onChange = Change
      , value    = txt
      , label    = label
      , options  = []
      }


toStr : Model -> String
toStr model =
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


type alias TextInput =
  Stil
  -> List (Attribute Never Msg)
  -> Input.Text Stil Never Msg
  -> Element Stil Never Msg



-- MISC

textInput : TextInput    -> Stil -> Attrs -> String  -> (String -> Msg) -> Elem
textInput   inputElement    stil    attrs    content    onChange           =
  inputElement stil attrs
     { onChange = onChange
     , value    = content
     , label    = Input.hiddenLabel ""
     , options  = []
     }


nextWeekday : { today : Date, weekday : Date.Day } -> Date
nextWeekday { today, weekday } =
  let dayDiffFloat = toFloat <| dayDiff { from = Date.dayOfWeek today, to = weekday }
  in
    Date.fromTime <|
      (Date.toTime <| startOfTheDay { now = today } )
      +
      (Time.hour * 24)
      *
      dayDiffFloat


dayDiff : { from : Date.Day, to : Date.Day } -> Int
dayDiff { from, to } =
  (dayToInt to - dayToInt from) % 7


dayToInt : Date.Day -> Int
dayToInt day =
  case day of
    Date.Mon -> 0
    Date.Tue -> 1
    Date.Wed -> 2
    Date.Thu -> 3
    Date.Fri -> 4
    Date.Sat -> 5
    Date.Sun -> 6


startOfTheDay : { now : Date } -> Date
startOfTheDay { now } =
  Date.fromTime <|
    Date.toTime now
    - toFloat (Date.hour now) * Time.hour
    - toFloat (Date.minute now) * Time.minute
    - toFloat (Date.second now) * Time.second
