module Druckansicht exposing (..)

import CommonnTypes exposing (..)
import CommonTypes exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Attributes exposing (..)
import Stil exposing (Stil, scale, spacin, pading, pxx, vergr)
import Datum


-------------------------------------------------------------------------
-------------------------------- MODEL -----------------------------------

-- type alias Read = Lieferung


-- type Navigation
--   = Leave
--   | Stay


--------------------------------------------------------------------------
------------------------------- UPDATE -----------------------------------

type alias Msg = ()


-- update : Read -> (Navigation, Cmd Msg)


------------------------------------------------------------------------
------------------------------- VIEW -----------------------------------

view : { lieferung : Lieferung } -> Elem Msg
view { lieferung } =
  button Stil.HiddenButton [attribute "onClick" "window.print()"] <|
    column Stil.Neutral [eintragSpace]
      [ viewBestellungen { bestellungen = lieferung.bestellungen }
      , viewLieferInfos { lieferung = lieferung }
      , if lieferung.bestelltyp == Partyservice
          then viewPartyservice { party = lieferung.partyserviceData }
          else empty
      ]


viewBestellungen : { bestellungen : List Bestellung } -> Elem Msg
viewBestellungen { bestellungen } =
  let
    indent : Elem msg -> Elem msg
    indent elem =
      row Stil.Neutral []
        [ el Stil.Neutral [pading 10] empty
        , elem
        ]
  in
    column Stil.Neutral [eintragSpace] <|
      List.indexedMap
        (\ix bestellung ->
            column Stil.Neutral [eintragSpace, alignLeft, paddingBottom << vergr <| eintragNum * 2]
              [ text <| "Bestellung " ++ toString (ix + 1) ++ ":"
              , indent
                  (viewBestellung { bestellung = bestellung })
              ])
        bestellungen


viewBestellung : { bestellung : Bestellung } -> Elem Msg
viewBestellung { bestellung } =
  column Stil.Neutral [eintragSpace, alignLeft]
    [ text <| "PLU: " ++ bestellung.plu
    , text <| "Artikel: " ++ bestellung.artikelbezeichnung
    , text <| "Menge: " ++ bestellung.menge
    , if bestellung.freitext /= ""
        then text <| "Kommentar: " ++ bestellung.freitext
        else empty
    ]


viewLieferInfos : { lieferung : Lieferung } -> Elem Msg
viewLieferInfos { lieferung } =
  column Stil.Neutral [eintragSpace, alignLeft]
    [ text <| "Bestelldatum: " ++ Datum.toStr (Datum.Datum lieferung.bestelldatum)
    , text <| "Lieferdatum: " ++ Datum.toStr lieferung.lieferdatum
    , text <| "Kunde: " ++ lieferung.kundenname
    ]


viewPartyservice : { party : PartyserviceData } -> Elem Msg
viewPartyservice { party } =
  let
    mkPartyCheckbox : { getter : PartyserviceData -> Bool, label : String } -> Elem Msg
    mkPartyCheckbox arg =
      Input.checkbox Stil.Neutral []
        { onChange = always ()
        , checked = arg.getter party
        , label = el Stil.Neutral [] <| text arg.label
        , options = []
        }
  in
    column Stil.Neutral [eintragSpace, alignLeft]
      [ text <| "Adresse: " ++ party.adresse
      , text <| "Telefon: " ++ party.telefon
      , text <| "Ort: " ++ party.veranstaltungsort
      , text <| "Personen: " ++ party.personenanzahl
      , mkPartyCheckbox { getter = .lieferservice, label = "Lieferservice" }
      , mkPartyCheckbox { getter = .mitChafingDish, label = "mit Chafing Dish" }
      ]

eintragNum = 5
-- eintragPadd = pading eintragNum
eintragSpace = spacin eintragNum
eintragSpaceRel fac = spacin (eintragNum * fac)
