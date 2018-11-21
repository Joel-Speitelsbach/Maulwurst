module Tabelle exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Stil exposing (pxx,Stil)
import CommonTypes exposing (..)

reihe       = reiheHeight 25.888
reiheHeight = reiheStil   Stil.Neutral

reiheStil : Stil -> Float -> Attrs msg -> List Float -> List (Elem msg) -> Elem msg
reiheStil   stil    height   attrs        sizes         columns            =
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
    Element.grid stil attrs
      { columns = List.map pxx sizes
      , rows    = [ pxx height ]
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
      { columns = List.map pxx columnSizes
      , rows    = List.map pxx rows
      , cells   = List.map indexedToCell indexedList
      }
