module Details exposing (..)

import CommonnTypes exposing (..)
import CommonTypes exposing (..)
import Date exposing (Date)
import Datum
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Stil exposing (Stil, scale, spacin, pading, pxx, vergr)
import Tabelle as Tab
import Time exposing (minute)
import ToServer
import Druckansicht


----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

type alias Model =
  { ansicht     : Ansicht
  }


type Ansicht
  = NormalAnsicht
  | Reloading
  | LöschDialog
  | Druckansicht


init : Model
init = { ansicht = NormalAnsicht }


checkLieferung : Lieferung -> { ok : Bool}
checkLieferung lieferung =
  { ok = case lieferung.lieferdatum of
           Datum.Datum    _ -> True
           Datum.DatumStr _ -> False
  }


leereParty : PartyserviceData
leereParty =
  { adresse           = ""
  , telefon           = ""
  , veranstaltungsort = ""
  , personenanzahl    = ""
  , lieferservice     = False
  , mitChafingDish    = False
  }


---------------------------------------------------------------------
-------------------------- UPDATE ---------------------------------

type Msg
  = ZuÜbersicht
  | StopReloading
  | LöscheBestellung Lieferung Bestellung
  | Change Lieferung
  | LieferdatumMsg Datum.Msg
  | NeueBestellung Lieferung
  | TogglePapierkorb Lieferung
  | Reload
  | DoNothing
  | PapTatsächlich Lieferung Bool
  | Drucke
  | DruckansichtMsg Druckansicht.Msg


type Update
  = NotLeave Model
  | Leave


update :
  { msg : Msg
  , model : Model
  , lieferung : Lieferung
  , today : Date
  }
  -> (Update, Cmd Msg)
update { msg, model, lieferung, today } =
  case msg of
    Change geänderte_lieferung ->
      ( NotLeave model
      , ToServer.send (ToServer.updateLieferung geänderte_lieferung)
      )
    LieferdatumMsg msg ->
      ( NotLeave model
      , ToServer.send <|
          ToServer.updateLieferung
            { lieferung
            | lieferdatum =
                Datum.update
                  { msg = msg
                  , model = lieferung.lieferdatum
                  , today = today
                  }
            }
      )
    TogglePapierkorb lieferung ->
      if inPapierkorbBool lieferung
      then
        ( NotLeave model
        , ToServer.send <| ToServer.papierkorbLieferung lieferung.id False
        )
      else
        ( NotLeave { model | ansicht = LöschDialog }
        , Cmd.none
        )
    PapTatsächlich lieferung bool ->
      if bool
      then ( Leave
           , ToServer.send <| ToServer.papierkorbLieferung lieferung.id True
           )
      else ( NotLeave model
           , Cmd.none
           )
    NeueBestellung lieferung ->
      ( NotLeave model
      , ToServer.send (ToServer.neueBestellung lieferung.id)
      )
    LöscheBestellung lieferung bestellung ->
      ( NotLeave model
      , ToServer.send (ToServer.löscheBestellung lieferung.id bestellung.id)
      )
    StopReloading ->
      ( NotLeave { model | ansicht = NormalAnsicht }
      , Cmd.none
      )
    ZuÜbersicht ->
      ( if (checkLieferung lieferung).ok
        then Leave
        else NotLeave model
      , Cmd.none
      )
    DoNothing -> (NotLeave model, Cmd.none)
    Reload ->
      ( NotLeave { model | ansicht = Reloading }
      , Cmd.none
      )
    Drucke ->
      ( NotLeave { model | ansicht = Druckansicht }
      , Cmd.none
      )
    DruckansichtMsg () -> (NotLeave model, Cmd.none)


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

subscriptions : { model : Model } -> Sub Msg
subscriptions { model } =
    if model.ansicht == Reloading
    then Time.every 10 <| (\_ -> StopReloading)
    else Sub.none


----------------------------------------------------------------------------
------------------------------ VIEW ----------------------------------------

view { ansicht, lieferung } =
  case ansicht of
    Reloading -> text "reloading"
    LöschDialog -> viewLöschDialog lieferung
    NormalAnsicht ->
        viewLieferung lieferung
        |> el Stil.Neutral [center, spacin 20]
    Druckansicht ->
        Druckansicht.view { lieferung = lieferung }
        |> Element.map DruckansichtMsg


viewLieferung : Lieferung -> Elem Msg
viewLieferung lieferung =
  column Stil.Neutral [spacin 20] <|
      [ viewBestellTyp lieferung
      , viewBestellungTabelle lieferung
      , row Stil.Neutral [spacin 20]
          [ text "Kunde:"
          , textfeld lieferung.kundenname <| \str -> Change {lieferung | kundenname = str}
          ]
      , if lieferung.bestelltyp == Partyservice
        then viewPartyservice lieferung
        else empty
      , viewLieferdatum { lieferung = lieferung }
      , viewControlArea lieferung
      ]


viewLieferdatum : { lieferung : Lieferung } -> Elem Msg
viewLieferdatum { lieferung } =
  Element.map
    LieferdatumMsg
    (Datum.viewPickDate { model = lieferung.lieferdatum, label = Just "Lieferdatum:" })


viewBestellTyp : Lieferung -> Elem Msg
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


viewLieferungEintrag : Lieferung -> String -> String -> (String -> Lieferung -> Lieferung) -> Elem Msg
viewLieferungEintrag   lieferung    label     content   changer                               =
  row Stil.Neutral [spacin 20]
    [ text (label ++ ":")
    , textfeld content <| \str -> Change (changer str lieferung)
    ]


partyChanger : (String -> PartyserviceData) -> String -> Lieferung -> Lieferung
partyChanger   newParty                        str       lieferung    =
  { lieferung
  | partyserviceData = newParty str
  }


viewPartyservice : Lieferung -> Elem Msg
viewPartyservice lieferung =
  let
    party = lieferung.partyserviceData
    mkPartyCheckbox arg =
      Input.checkbox Stil.Neutral []
        { onChange = \bool -> Change <|
            { lieferung
            | partyserviceData = arg.setter bool party
            }
        , checked = arg.getter party
        , label = el Stil.Neutral [] <| text arg.label
        , options = []
        }
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
      , row Stil.Neutral [] <|
          [ mkPartyCheckbox
              { setter = \bool party -> { party | lieferservice = bool }
              , getter = .lieferservice
              , label = "Lieferservice"
              }
          , mkPartyCheckbox
              { setter = \bool party -> { party | mitChafingDish = bool }
              , getter = .mitChafingDish
              , label = "mit Chafing Dish"
              }
          ]
      ]


viewControlArea lieferung =
  row Stil.Neutral [spacin 100] <|
    let höhe = height (pxx 70)
    in
      [ column Stil.Neutral [spacin 6, width (fillPortion 3), höhe]
          [ button Stil.Button
              [onClick Drucke, height fill, width fill]
              (text "Drucken")
          , button Stil.Button
              [onClick ZuÜbersicht, height fill, width fill]
              (text "Speichern")
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


viewLöschDialog : Lieferung -> Elem Msg
viewLöschDialog lieferung =
  el Stil.Neutral [center, verticalCenter] <|
    row Stil.Neutral [spacin 90]
      [ button (Stil.Stat Nothing) [onClick <| PapTatsächlich lieferung True] (text "In Papierkorb legen")
      , button Stil.Button [onClick <| PapTatsächlich lieferung False] (text "Behalten")
      ]


viewBestellungTabelle lieferung =
  let sizes = [{-80,-} 37,180,80,240]
  in
    column Stil.Neutral [spacin 20] <|
      [ Tab.reiheStil Stil.TabelleSpaltenName 5 [] sizes [{-empty,-} text "PLU", text "Artikelbezeichnung", text "Menge", text "Freitext"]
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
            [ Tab.reiheHeight 52 [] ({-List.drop 1-} sizes)
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


type alias TextInput =
  Stil
  -> Attrs Msg
  -> Input.Text Stil Never Msg
  -> Elem Msg


-- MISC


textfeld : String -> (String -> Msg) -> Elem Msg
textfeld = textInput Input.text Stil.TextField []


multiline : String -> (String -> Msg) -> Elem Msg
multiline = textInput Input.multiline Stil.TextField [height fill]


textInput : TextInput    -> Stil -> Attrs Msg -> String -> (String -> Msg) -> Elem Msg
textInput   inputElement    stil    attrs        content   onChange           =
  inputElement stil attrs
     { onChange = onChange
     , value    = content
     , label    = Input.hiddenLabel ""
     , options  = []
     }
