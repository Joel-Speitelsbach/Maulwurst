module Main exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Style
import Style.Color as Color
import Color exposing (..)
import Style.Font as Font
import Date exposing (Date)
import Stil exposing (Stil)
import Types exposing (..)
import Tabelle exposing (tableTransposed)
import Html exposing (Html)
import Time exposing (minute)
import WebSocket
import Json.Decode as Dec
import Json.Encode as Enc
import Decoding as FromServer exposing (ServerMsg(..))
import Encoding as ToServer

main = Html.program {view=view, init=(init,Cmd.none), update=update, subscriptions=subscriptions}

-- MODEL

init : Model
init =
  { lieferungen = []
  , ansicht = Übersicht
  , jetzt = Date.fromTime 0
  , sortby =
      { kategorie = Bestelldatum
      , vorwärts = True
      }
  , testmsg = "oiaeo"
  }

type Prog
  = Running Model
  | Login
  | Fetching

type alias Model =
  { lieferungen : List Lieferung
  , ansicht : Ansicht
  , jetzt : Date
  , sortby :
      { kategorie : Sortby
      , vorwärts : Bool
      }
  , testmsg : String
  }

defLieferung : Int -> Lieferung
defLieferung bestellungStartId =
  { bestelldatum = Date.fromTime 0
  , lieferdatum = "10 Jun 2018"
  , bestellungen = beispielBestellungen 1 bestellungStartId
  , kundenname = "Grand Smith"
  , id = 0
  }
neueLieferung : Date -> Int -> Lieferung
neueLieferung jetzt id =
  { bestelldatum = jetzt
  , lieferdatum = ""
  , bestellungen = []
  , kundenname = ""
  , id = id
  }

defBestellung : Bestellung
defBestellung =
  { plu = "noplu"
  , artikelbezeichnung = ""
  , menge = "nix"
  , status = Neu
  , freitext = "hier kann man irgendwas reinschreiben"
  , id = 0
  }
neueBestellung : Int -> Bestellung
neueBestellung id =
  { plu = ""
  , artikelbezeichnung = ""
  , menge = ""
  , status = Neu
  , freitext = ""
  , id = id
  }

type Ansicht
  = Übersicht
  | Details Int
  | Reloading Int

lieferStatus : Lieferung -> Status
lieferStatus {bestellungen} =
  let
    stati = List.map .status bestellungen
    fertig = List.all identity
      [ List.isEmpty <| List.filter (\s -> s /= Fertig) stati
      , not <| List.isEmpty stati
      ]
    inBearbeitung = not <| List.isEmpty <| List.filter (\s -> s /= Neu) stati
  in
    if      fertig        then Fertig
    else if inBearbeitung then InBearbeitung
                          else Neu

nextLieferId lieferungen =
  List.maximum (List.map .id lieferungen)
  |> Maybe.withDefault 0
  |> \i -> i + 1

nextBestellungId : List Lieferung -> Int
nextBestellungId lieferungen =
  List.concatMap (\l -> List.map .id l.bestellungen) lieferungen
  |> List.maximum
  |> Maybe.withDefault 0
  |> \i -> i + 1

type Sortby
  = Bestelldatum
  | Lieferdatum
  | SortStatus
  | Kunde

lieferungLeer : Lieferung -> Bool
lieferungLeer lieferung =
  lieferung.lieferdatum == ""
  && lieferung.kundenname == ""
  && lieferung.bestellungen == []

findeLieferung : Int -> List Lieferung -> Maybe Lieferung
findeLieferung id lieferungen =
  List.head <| List.filter (\l -> l.id == id) lieferungen

zusammenfassung : Lieferung -> String
zusammenfassung lieferung =
  toString lieferung.bestelldatum ++ " "
  ++ lieferung.lieferdatum ++ " "
  ++ lieferung.kundenname ++ " "
  ++ (String.concat <| List.map zusammenfassungBestellung
        lieferung.bestellungen
     )

zusammenfassungBestellung : Bestellung -> String
zusammenfassungBestellung bestellung =
  bestellung.plu ++ " "
  ++ bestellung.artikelbezeichnung ++ " "
  ++ bestellung.menge ++ " "
  ++ toString bestellung.status ++ " "
  ++ bestellung.freitext ++ " "

-- UPDATE

type Msg
  = Goto Ansicht
  | Change Lieferung
  | NeueBestellung Lieferung
  | NeueLieferung
  | LöscheBestellung Lieferung Bestellung
  | LöscheLieferung Lieferung
  | UpdateTime Date
  | ChangeSort Sortby
  | FromServer ServerMsg

reactServer msg model =
  case msg of
    TestMsg str -> { model | testmsg = str }
    ServerChangeLieferung lieferung ->
      { model
      | lieferungen = changeLieferungen lieferung model.lieferungen
      }
    ServerZeigeNeueLieferung id ->
      { model
      | ansicht = Details id
      }
    ServerLöscheBestellung lid bid ->
      { model
      | lieferungen = List.map (löscheBestellung bid) model.lieferungen
      , ansicht =
          case model.ansicht of
            Details id -> if lid == id then Reloading id
                                       else Details   id
            a          -> a
      }
    ServerLöscheLieferung id ->
      löscheLieferung id model
    ServerSetzeAllesZurück ->
      { model | lieferungen = [] }
    MalformedMsg str -> { model | testmsg = Debug.log "" <| "malformed server msg: \n" ++ str }

update msg model =
  case msg of
    Change lieferung ->
      ( model
      , send (ToServer.updateLieferung lieferung)
      )
    LöscheBestellung lieferung bestellung ->
      ( model
      , send (ToServer.löscheBestellung lieferung.id bestellung.id)
      )
    LöscheLieferung lieferung ->
      ( model
      , send (ToServer.löscheLieferung lieferung.id)
      )
    NeueLieferung ->
      ( model
      , send (ToServer.neueLieferung model.jetzt)
      )
    NeueBestellung lieferung ->
      ( model
      , send (ToServer.neueBestellung lieferung.id)
      )
    _ -> (updateNoCmd msg model, Cmd.none)

send msg =
  WebSocket.send serverUrl (Enc.encode 0 msg)

updateNoCmd msg model =
  case msg of
    Goto a -> {model | ansicht = a}
    UpdateTime jetzt -> {model | jetzt = jetzt}
    ChangeSort sortby ->
      { model
      | sortby =
          if sortby == model.sortby.kategorie
            then { kategorie = sortby, vorwärts = not model.sortby.vorwärts}
            else { kategorie = sortby, vorwärts = True}
      }
    FromServer msg -> reactServer msg model
    _ -> model

löscheLieferung : Int -> Model -> Model
löscheLieferung id model =
  { model
  | lieferungen = List.filter (\l -> l.id /= id) model.lieferungen
  }

changeLieferungen : Lieferung -> List Lieferung -> List Lieferung
changeLieferungen lieferung lieferungen =
  lieferung :: List.filter (\l -> l.id /= lieferung.id) lieferungen

löscheBestellung : Int -> Lieferung -> Lieferung
löscheBestellung id lieferung =
  { lieferung
  | bestellungen = List.filter (\b -> b.id /= id) lieferung.bestellungen
  }

toInt string = case String.toInt string of
  Ok int -> int
  Err _  -> -1

updateBestellung : Lieferung -> (val -> Bestellung) -> val -> Msg
updateBestellung lieferung onString val =
  let
    bestellung = onString val
    update b =
      if b.id == bestellung.id
        then bestellung
        else b
  in Change {lieferung | bestellungen = List.map update lieferung.bestellungen}

-- SUBSCRIPTIONS

subscriptions model =
  let
    time = Time.every Time.second <| UpdateTime << Date.fromTime
    websocket = WebSocket.listen serverUrl (FromServer << FromServer.parseServerMsg)
    reloading =
      case model.ansicht of
        Reloading id -> Time.every 10 <| (\_ -> Goto (Details id))
        _            -> Sub.none
  in
    Sub.batch [time,reloading,websocket]

serverUrl = "ws://37.221.194.181:3000"

-- VIEW

view : Model -> Html Msg
view model =
  case model.ansicht of
    Übersicht -> viewÜbersicht model
    Reloading _ -> Html.text "reloading"
    Details lieferungsId ->
      List.filter (\l -> l.id == lieferungsId) model.lieferungen
      |> List.head
      |> Maybe.map (viewDetails model.lieferungen)
      |> Maybe.withDefault (viewÜbersicht model)

------------ view Details
viewDetails lieferungen lieferung =
  viewport Stil.stylesheet <| el Stil.Neutral [center, padding 20] <|
    column Stil.Neutral [spacing 20]
      [ tableTransposed Stil.Neutral [] [30] 100 [80, 35,300,80,400,100] <|
          [ [empty, text "PLU", el Stil.Neutral [moveRight 50] (text "Artikelbezeichnung"), text "Menge", el Stil.Neutral [moveRight 130] (text "Freitext")]
          ] ++ List.map (viewBestellung lieferung) lieferung.bestellungen
      , flip (button Stil.Button) (text "+") <|
          [ onClick (NeueBestellung lieferung) ]
      , row Stil.Neutral [spacing 20]
          [ text "Kunde:"
          , textfeld lieferung.kundenname <| \str -> Change {lieferung | kundenname = str}
          ]
      , row Stil.Neutral [spacing 20]
          [ text "Lieferdatum:"
          , textfeld lieferung.lieferdatum <| \str -> Change {lieferung | lieferdatum = str}
          ]
      , button Stil.Button [onClick (Goto Übersicht), height (px 30)] (text "Zurück")
      ]

viewBestellung lieferung bestellung =
  let
    update = updateBestellung lieferung
  in
    [ el Stil.Neutral [] <|
        button Stil.LöschButton [onClick (LöscheBestellung lieferung bestellung)] (text "Löschen")
    , textfeld bestellung.plu                 <| update (\str -> {bestellung | plu                = str})
    , textfeld bestellung.artikelbezeichnung  <| update (\str -> {bestellung | artikelbezeichnung = str})
    , textfeld bestellung.menge               <| update (\str -> {bestellung | menge              = str})
    , multiline bestellung.freitext           <| update (\str -> {bestellung | freitext           = str})
    , Input.radio Stil.Neutral [ padding 0, spacing 10 ]
        { onChange = update <| \status -> {bestellung | status = status}
        , selected = Just bestellung.status
        , label = Input.hiddenLabel ""
        , options = []
        , choices =
            [ statusChoice Neu "Neu"
            , statusChoice InBearbeitung "In Bearbeitung"
            , statusChoice Fertig "Fertig"
            ]
        }
    ]

statusChoice status txt =
  Input.styledChoice status <|
    \selected ->
      if selected
        then el (Stil.Stat status) [] (text txt)
        else el (Stil.Neutral)     [] (text txt)

type alias Elem variation = Element Stil variation Msg
type alias Attrs variation = List (Attribute variation Msg)
type alias TextInput variation = Stil -> List (Attribute variation Msg) -> Input.Text Stil variation Msg -> Element Stil variation Msg

textfeld : String -> (String -> Msg) -> Elem variation
textfeld  = textInput Input.text      []

multiline : String -> (String -> Msg) -> Elem variation
multiline = textInput Input.multiline [height fill]

textInput : TextInput variation -> Attrs variation -> String -> (String -> Msg) -> Elem variation
textInput inputElement attrs content onChange =
  inputElement Stil.TextField attrs
     { onChange = onChange
     , value    = content
     , label    = Input.hiddenLabel ""
     , options  = []
     }

------------ view Übersicht
viewÜbersicht : Model -> Html Msg
viewÜbersicht model =
  viewport Stil.stylesheet <| el Stil.Neutral [center, padding 20] <|
    el Stil.Neutral [ spacing 20] <|
      row Stil.Neutral [spacing 10] <|
        [ tabelleÜbersicht model
        , flip (button Stil.Button) (text "+") <|
            [ onClick  NeueLieferung, width (px 30) ]
        ]

tabelleÜbersicht : Model -> Elem var
tabelleÜbersicht model =
  tableTransposed Stil.Neutral [spacing 10] [] 20 [70, 120, 200,200,120] <|
    let
      sortButton sortby txt =
        let
          arrow =
            if sortby == model.sortby.kategorie
              then if model.sortby.vorwärts then " ↓" else " ↑"
              else ""
        in button Stil.SortButton [center, onClick (ChangeSort sortby)] (text (txt ++ arrow))
    in
      [ empty
      , sortButton Kunde "Kunde"
      , sortButton Bestelldatum "Bestelldatum"
      , sortButton Lieferdatum "Lieferdatum"
      , sortButton SortStatus "Status"
      ]
      :: List.map lieferungReihe (sortiere model.sortby.vorwärts model.sortby.kategorie model.lieferungen)

lieferungReihe : Lieferung -> List (Element Stil variation Msg)
lieferungReihe lieferung =
  let
    viewBut = button Stil.Button [onClick (Goto (Details lieferung.id))] (text "Zeige Details")
    viewStatus =
      let status = lieferStatus lieferung
      in el (Stil.Stat status) [] <| text <| toString status
  in
    [ el Stil.Neutral [] <|
        button Stil.LöschButton [onClick (LöscheLieferung lieferung)] (text "Löschen")
    , text (lieferung.kundenname)
    , text (viewDate lieferung.bestelldatum)
    , text lieferung.lieferdatum
    , viewStatus
    , viewBut
    ]

sortiere : Bool -> Sortby -> List Lieferung -> List Lieferung
sortiere vorwärts sortby lieferungen =
  let
    sortiert =
      case sortby of
        Bestelldatum -> List.sortBy (.bestelldatum >> Date.toTime >> (\d -> -d)) lieferungen
        Lieferdatum ->
          let
            by l =
                Date.fromString l.lieferdatum
                |> Result.map Date.toTime
                |> Result.withDefault 0
            filtered =
              List.filter
                (\l ->
                  Result.toMaybe (Date.fromString l.lieferdatum) /= Nothing
                )
                lieferungen
          in List.sortBy by filtered
        SortStatus ->
          let
            by l =
              case lieferStatus l of
                Neu           -> 0
                InBearbeitung -> 1
                Fertig        -> 2
          in List.sortBy by lieferungen
        Kunde -> List.sortBy .kundenname lieferungen
  in
    if vorwärts then sortiert else List.reverse sortiert

viewDate : Date -> String
viewDate datum =
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

-- Testing

beispielBestellungen anzahl startId =
  let
    bestellung i = {defBestellung | plu = toString i, id = startId+i}
  in
    List.map bestellung (List.range 1 anzahl)

textss =
  [ [text "uiae", text "öäpz"]
  , [text "uie", text "dtrn", text "qfgh", text "dtrn"]
  , [text "vflgcfvlg"]
  ]
