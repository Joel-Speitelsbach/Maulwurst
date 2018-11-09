module DayColors exposing (..)

import Date exposing (Date)
import Element exposing (Element)
import Stil exposing (Stil)

type alias Elem var msg = Element Stil var msg

stil i = case i % 3 of
  0 -> Stil.Day Stil.DayColor1
  1 -> Stil.Day Stil.DayColor2
  2 -> Stil.Day Stil.DayColor3
  _ -> Debug.log "error stil in DayColors.elm" Stil.Neutral

farbSegemente : List (Maybe Date) -> List Stil
farbSegemente els =
  List.concat <|
    List.indexedMap
      (\i split ->
          List.repeat (List.length split) (stil i)
      )
      (splitOnChange differentMaybeDay els)

different : (a -> b) -> a -> a -> Bool
different map a b = map a /= map b

differentDay : Date -> Date -> Bool
differentDay d t =
     different Date.year  d t
  || different Date.month d t
  || different Date.day   d t

differentMaybeDay : Maybe Date -> Maybe Date -> Bool
differentMaybeDay md mt =
  case md of
    Nothing -> True
    Just d ->
      case mt of
        Nothing -> True
        Just t -> differentDay d t

splitOnChange : (a -> a -> Bool) -> List a -> List (List a)
splitOnChange isDifferent list =
  let
    go : List (List a) -> List a -> List (List a)
    go fin rest =
      case rest of
        [] -> fin
        next :: newRest ->
          case fin of
            group :: tfin ->
              case group of
                [] -> Debug.log "DayColors: splitOnChange: empty group" []
                curr :: _ ->
                  if isDifferent curr next
                    then go ([next] :: fin) newRest
                    else go ((next :: group) :: tfin) newRest
            [] -> Debug.log "DayColors: splitOnChange: empty fin" []
    reversed =
      case list of
        x::xs -> go [[x]] xs
        [] -> []
  in
    reversed
    |> List.map List.reverse
    |> List.reverse
