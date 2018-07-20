module Details exposing (view,update,subscriptions,Msg)

import Element exposing (..)
import Element.Input as Input
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Types exposing (..)
import Date exposing (Date)
import ToServer
import Html exposing (Html)
import Stil exposing (Stil, scale, spacin, pading, pxx)
import Tabelle as Tab
import Time exposing (minute)

-----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

acceptCheck : Lieferung -> Bool
acceptCheck lieferung =
  Date.fromString lieferung.lieferdatum
  |> Result.map (\_ -> True)
  |> Result.withDefault False

ixLieferung lieferungen lieferungsId =
  List.filter (\l -> l.id == lieferungsId) lieferungen
  |> List.head

---------------------------------------------------------------------
-------------------------- UPDATE ---------------------------------

type Msg
  = ZuÜbersicht
  | StopReloading
  | LöscheBestellung Lieferung Bestellung
  | Change Lieferung
  | NeueBestellung Lieferung
  | LöscheLieferung Lieferung

type alias Elem variation = Element Stil variation Msg
type alias Attrs variation = List (Attribute variation Msg)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Change lieferung ->
      ( model
      , ToServer.send (ToServer.updateLieferung lieferung)
      )
    LöscheLieferung lieferung ->
      ( model
      , ToServer.send (ToServer.löscheLieferung lieferung.id)
      )
    NeueBestellung lieferung ->
      ( model
      , ToServer.send (ToServer.neueBestellung lieferung.id)
      )
    LöscheBestellung lieferung bestellung ->
      ( model
      , ToServer.send (ToServer.löscheBestellung lieferung.id bestellung.id)
      )
    StopReloading ->
      ( { model
        | ansicht = case model.ansicht of
            Details a -> Details { a | reloading = False}
            _ -> model.ansicht
        }
      , Cmd.none
      )
    ZuÜbersicht ->
      ( { model
        | ansicht =
            case model.ansicht of
              Details details ->
                if
                  ixLieferung model.lieferungen details.id
                  |> Maybe.map acceptCheck
                  |> Maybe.withDefault True
                then Übersicht
                else model.ansicht
              a -> a
        }
      , Cmd.none
      )

updateBestellung : Lieferung -> (val -> Bestellung) -> val -> Msg
updateBestellung lieferung onString val =
  let
    bestellung = onString val
    update b =
      if b.id == bestellung.id
        then bestellung
        else b
  in Change {lieferung | bestellungen = List.map update lieferung.bestellungen}

--------------------------------------------------------------------------------
------------------------ SUBSCRIPTIONS ------------------------------------------

subscriptions model =
  case model.ansicht of
    Details ansicht ->
      if ansicht.reloading
      then Time.every 10 <| (\_ -> StopReloading)
      else Sub.none
    _ -> Sub.none

----------------------------------------------------------------------------
------------------------------ VIEW ----------------------------------------

view ansicht lieferungen =
  if ansicht.reloading
  then Just (Html.text "reloading")
  else
    ixLieferung lieferungen ansicht.id
    |> Maybe.map viewLieferung

viewLieferung lieferung =
  viewport Stil.stylesheet <| el Stil.Neutral [center, pading 20] <|
    column Stil.Neutral [spacin 20] <|
      let sizes = [{-80,-} 35,180,80,240]
      in
        [ Tab.reiheStil Stil.TabelleSpaltenName [] 5 sizes [{-empty,-} text "PLU", text "Artikelbezeichnung", text "Menge", text "Freitext"]
        , column Stil.Neutral [spacin 20] <|
            List.map (viewBestellung sizes lieferung) lieferung.bestellungen
        , flip (button Stil.Button) (text "+") <|
            [ onClick (NeueBestellung lieferung), height (pxx 30) ]
        , row Stil.Neutral [spacin 20]
            [ text "Kunde:"
            , textfeld lieferung.kundenname <| \str -> Change {lieferung | kundenname = str}
            ]
        , row Stil.Neutral [spacin 20]
            [ text "Lieferdatum:"
            , let
                istValide =
                  Result.map (\_ -> True) (Date.fromString lieferung.lieferdatum)
                  |> Result.withDefault False
              in
                textInput Input.text (Stil.TextFeld istValide) [] lieferung.lieferdatum <| \str -> Change {lieferung | lieferdatum = str}
            ]
        , row Stil.Neutral [spacin 100] <|
            let höhe = height (pxx 70)
            in
              [ column Stil.Neutral [spacin 6, width (fillPortion 3), höhe]
                  [ button Stil.Button [attribute "onClick" "window.print()", height fill, width fill] (text "Drucken")
                  , button Stil.Button [onClick ZuÜbersicht, width fill, height fill] (text "Speichern")
                  ]
              , column Stil.Neutral [spacin 6, width (fillPortion 1), höhe]
                  [ el Stil.Neutral         [height (fillPortion 5)] empty
                  , button Stil.LöschButton [height (fillPortion 3), onClick (LöscheLieferung lieferung)] (text "(Entgültig) Löschen")
                  ]
              ]
        ]

viewBestellung sizes lieferung bestellung =
  let
    update = updateBestellung lieferung
  in
    row Stil.Neutral []
      [ {-el Stil.Neutral [verticalCenter, moveLeft 50] <|
          button Stil.LöschButton [width (pxx <| Maybe.withDefault 50 (List.head sizes)), onClick (LöscheBestellung lieferung bestellung)] (text "Löschen")
      ,-} column Stil.Neutral []
            [ Tab.reiheHeight [] 52 ({-List.drop 1-} sizes)
              [ textfeld bestellung.plu                 <| update (\str -> {bestellung | plu                = str})
              , textfeld bestellung.artikelbezeichnung  <| update (\str -> {bestellung | artikelbezeichnung = str})
              , textfeld bestellung.menge               <| update (\str -> {bestellung | menge              = str})
              , multiline bestellung.freitext           <| update (\str -> {bestellung | freitext           = str})
              ]
          , Input.radioRow Stil.Neutral [ pading 0, spacin 10, center ]
              { onChange = update <| \status -> {bestellung | status = status}
              , selected = Just bestellung.status
              , label = Input.hiddenLabel ""
              , options = []
              , choices =
                  [ statusChoice Neu
                  , statusChoice InBearbeitung
                  , statusChoice Fertig
                  ]
              }
            ]
        ]

statusChoice status =
  let txt = text <| statusString status
  in
    Input.styledChoice status <|
      \selected ->
        if selected
          then el (Stil.Stat status) [] txt
          else el (Stil.Neutral)     [] txt

type alias TextInput variation = Stil -> List (Attribute variation Msg) -> Input.Text Stil variation Msg -> Element Stil variation Msg

textfeld : String -> (String -> Msg) -> Elem variation
textfeld = (textInput Input.text Stil.TextField [])

multiline : String -> (String -> Msg) -> Elem variation
multiline = textInput Input.multiline Stil.TextField [height fill]

textInput : TextInput var -> Stil -> Attrs var -> String -> (String -> Msg) -> Elem var
textInput inputElement stil attrs content onChange =
  inputElement stil attrs
     { onChange = onChange
     , value    = content
     , label    = Input.hiddenLabel ""
     , options  = []
     }
