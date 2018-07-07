module Tabelle exposing (..)

import Element exposing (Attribute,Element)
import Element.Attributes exposing (px)
import Stil

reihe sizes columns =
  let
    addColNumber = List.indexedMap (,)
    indexedList = addColNumber columns
    indexedToCell (colNum,content) =
      Element.cell
         { start = ( colNum, 0 )
         , width = 1
         , height = 1
         , content = content
         }
  in
    Element.grid Stil.Neutral []
      { columns = List.map px sizes
      , rows    = [ px (Stil.scale 2) ]
      , cells   = List.map indexedToCell indexedList
      }

tableTransposed
         : style
        -> List (Attribute variation msg)
        -> List Float
        -> Float
        -> List Float
        -> List (List (Element style variation msg))
        -> Element style variation msg
tableTransposed style attrs initRows rowSize columnSizes matrix =
  let
    height = List.length matrix
    width  = List.length <| List.foldr pickMaxLen [] matrix
    pickMaxLen l ll =
      if List.length l >= List.length ll then l else ll
    addColNumber = List.indexedMap (,)
    indexedList =
      List.concat <| List.indexedMap (\i col -> List.map (\cell -> (i,cell)) (addColNumber col)) matrix
    indexedToCell (linNum,(colNum,content)) =
      Element.cell
         { start = ( colNum, linNum )
         , width = 1
         , height = 1
         , content = content
         }
    rows = initRows ++ List.repeat (height - List.length initRows) rowSize
  in
    Element.grid style attrs
      { columns = List.map px columnSizes
      , rows    = List.map px rows
      , cells   = List.map indexedToCell indexedList
      }

--abstellgleis
-- someLine = case matrix of
-- fstLine :: _ -> fstLine
-- []           -> []
