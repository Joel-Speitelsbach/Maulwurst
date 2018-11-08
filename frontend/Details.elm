module Details exposing (..)

import Element exposing (..)
import Element.Input as Input
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Types exposing (..)
import CommonTypes exposing (..)
import Date exposing (Date)
import ToServer
import Html exposing (Html)
import Stil exposing (Stil, scale, spacin, pading, pxx, vergr)
import Tabelle as Tab
import Time exposing (minute)
import Datum


----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

acceptCheck : Lieferung -> Bool
acceptCheck lieferung =
  case lieferung.lieferdatum of
    Datum.Datum _ -> True
    Datum.DatumStr _ -> False

ixLieferung lieferungen lieferungsId =
  List.filter (\l -> l.id == lieferungsId) lieferungen
  |> List.head

leereParty : PartyserviceData
leereParty =
  { adresse = ""
  , telefon = ""
  , veranstaltungsort = ""
  , personenanzahl = ""
  }


---------------------------------------------------------------------
-------------------------- UPDATE ---------------------------------

type Msg
  = ZuÜbersicht
  | StopReloading
  | LöscheBestellung Lieferung Bestellung
  | Change Lieferung
  | NeueBestellung Lieferung
  | TogglePapierkorb Lieferung
  | Reload
  | DoNothing
  | PapTatsächlich Lieferung Bool

type alias Elem variation = Element Stil variation Msg
type alias Attrs variation = List (Attribute variation Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change lieferung ->
      ( model
      , ToServer.send (ToServer.updateLieferung lieferung)
      )
    TogglePapierkorb lieferung ->
      if inPapierkorbBool lieferung
      then
        ( model
        , ToServer.send <| ToServer.papierkorbLieferung lieferung.id False
        )
      else
        ( { model
          | ansicht =
              case model.ansicht of
                Details details ->
                  Details
                    { details
                    | modus = LöschDialog
                    }
                a -> a
          }
        , Cmd.none
        )
    PapTatsächlich lieferung bool ->
      ( { model
        | ansicht =
            case model.ansicht of
              Details d ->
                if bool
                then Übersicht
                else Details { d | modus = DetailsNormal}
              a -> a
        }
      , if bool
        then ToServer.send <| ToServer.papierkorbLieferung lieferung.id True
        else Cmd.none
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
            Details a -> Details { a | modus = DetailsNormal}
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
    DoNothing -> (model, Cmd.none)
    Reload ->
      ( { model
        | ansicht =
            let curr = model.ansicht
            in
              case curr of
                Details d -> Details { d | modus = Reloading }
                _ -> curr
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
      if ansicht.modus == Reloading
      then Time.every 10 <| (\_ -> StopReloading)
      else Sub.none
    _ -> Sub.none


----------------------------------------------------------------------------
------------------------------ VIEW ----------------------------------------

view ansicht lieferungen =
  case ansicht.modus of
    Reloading -> Just (text "reloading")
    LöschDialog ->
      ixLieferung lieferungen ansicht.id
      |> Maybe.map viewLöschDialog
    _ ->
      ixLieferung lieferungen ansicht.id
      |> Maybe.map viewLieferung
      |> Maybe.map (el Stil.Neutral [center, spacin 20])

viewLieferung lieferung =
  column Stil.Neutral [spacin 20] <|
      [ viewBestellTyp lieferung
      , viewBestellungTabelle lieferung
      , row Stil.Neutral [spacin 20]
          [ text "Kunde:"
          , textfeld lieferung.kundenname <| \str -> Change {lieferung | kundenname = str}
          ]
      , empty -- lieferdatum
      , if lieferung.bestelltyp == Partyservice
        then viewPartyservice lieferung
        else empty
      , viewControlArea lieferung
      , Element.map
          (\datumMsg ->
              Change
                { lieferung
                | lieferdatum =
                    Datum.update datumMsg lieferung.lieferdatum
                }
          ) <|
          Datum.viewPickDate lieferung.lieferdatum
      ]

viewBestellTyp : Lieferung -> Elem var
viewBestellTyp lieferung =
  Input.radioRow Stil.Neutral [ pading 0, spacin 10, center ]
      { onChange = \btyp ->
          Change {lieferung | bestelltyp = btyp}
      , selected = Just lieferung.bestelltyp
      , label = Input.hiddenLabel ""
      , options = []
      , choices =
          [ btypChoice Merchingen
          , btypChoice Adelsheim
          , btypChoice Partyservice
          ]
      }

btypChoice btypStr =
  let txt = text <| bestelltypString btypStr
  in
    Input.styledChoice btypStr <|
      \selected ->
        if selected
          then el (Stil.Btyp btypStr) [] txt
          else el (Stil.Neutral)      [] txt

viewLieferungEintrag lieferung label content changer =
  row Stil.Neutral [spacin 20]
    [ text (label ++ ":")
    , textfeld content <| \str -> Change (changer str lieferung)
    ]

partyChanger newParty str lieferung =
  { lieferung
  | partyserviceData = newParty str
  }

viewPartyservice : Lieferung -> Elem Msg
viewPartyservice lieferung =
  let party = lieferung.partyserviceData
  in
    column Stil.Neutral [spacin 20] <|
      [ viewLieferungEintrag lieferung "Adresse" party.adresse <|
          partyChanger <| \str -> { party | adresse = str }
      , viewLieferungEintrag lieferung "Telefon" party.telefon <|
          partyChanger <| \str -> { party | telefon = str }
      , viewLieferungEintrag lieferung "Veranstaltungsort" party.veranstaltungsort <|
          partyChanger <| \str -> { party | veranstaltungsort = str }
      , viewLieferungEintrag lieferung "Personenanzahl" party.personenanzahl <|
          partyChanger <| \str -> { party | personenanzahl = str }
      ]

viewControlArea lieferung =
  row Stil.Neutral [spacin 100] <|
    let höhe = height (pxx 70)
    in
      [ column Stil.Neutral [spacin 6, width (fillPortion 3), höhe]
          [ button Stil.Button [attribute "onClick" "window.print()", height fill, width fill] (text "Drucken")
          , button Stil.Button [onClick ZuÜbersicht, width fill, height fill] (text "Speichern")
          ]
      , column Stil.Neutral [spacin 6, width (fillPortion 1), höhe]
          [ el Stil.Neutral         [height (fillPortion 5)] empty
          , viewPapierkorbButton lieferung
          ]
      ]

viewPapierkorbButton lieferung =
  button (Stil.Stat Nothing)
    [ height (fillPortion 3)
    , onClick (TogglePapierkorb lieferung)
    ] <|
    case lieferung.inPapierkorb of
      Nothing -> text "In Papierkorb legen"
      Just _  -> text "Aus dem Papierkorb herausholen"

viewLöschDialog : Lieferung -> Elem var
viewLöschDialog lieferung =
  el Stil.Neutral [center, verticalCenter] <|
    row Stil.Neutral [spacin 90]
      [ button (Stil.Stat Nothing) [onClick <| PapTatsächlich lieferung True] (text "In Papierkorb legen")
      , button Stil.Button [onClick <| PapTatsächlich lieferung False] (text "Behalten")
      ]

viewBestellungTabelle  lieferung=
  let sizes = [{-80,-} 37,180,80,240]
  in
    column Stil.Neutral [spacin 20] <|
      [ Tab.reiheStil Stil.TabelleSpaltenName [] 5 sizes [{-empty,-} text "PLU", text "Artikelbezeichnung", text "Menge", text "Freitext"]
      , column Stil.Neutral [spacin 20] <|
          List.map (viewBestellung sizes lieferung) lieferung.bestellungen
      , flip (button Stil.ButtonSmall) (text "Artikel hinzufügen") <|
          [ onClick (NeueBestellung lieferung), height (pxx 30) ]
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
  let txt = text <| statusString <| Just status
  in
    Input.styledChoice status <|
      \selected ->
        if selected
          then el (Stil.Stat <| Just status) [] txt
          else el (Stil.Neutral)             [] txt

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
