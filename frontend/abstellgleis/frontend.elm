import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


serverUrl : String
serverUrl =
  "wss://localhost:3000"



-- MODEL

type alias Model =
  { appMode : AppMode
  -- , serverData = ServerData
  -- , clientData = ClientData
  }


init : (Model, Cmd Msg)
init =
  ( { appMode = Select
    }
  , Cmd.none )

type AppMode
  = Server
  | Client
  | Select

type alias Bestellung =
  { produkt : Produkt
  , menge   : Int
  }

type alias Produkt = String


-- UPDATE

type Msg
  = SwitchAppMode AppMode
  | ClientMsg ClientMsg
  | NewMessage String --TODO: remove

type ClientMsg
  = PlaceOrder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  SwitchAppMode mode -> noc {model | appMode = mode}
  NewMessage str -> noc model
  ClientMsg msg -> case msg of
    PlaceOrder -> noc model

noc model = (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen serverUrl NewMessage



-- VIEW

view : Model -> Html Msg
view model =
  let
    app = case model.appMode of
      Server -> viewServer
      Client -> viewClient
      Select -> viewSelect
  in div []
      [app]

viewSelect =
  div []
    [ button [onClick (SwitchAppMode Server) ] [text "Server"]
    , button [onClick (SwitchAppMode Client) ] [text "Client"]
    ]

viewServer = div []
  [goBack]

viewClient =
  div []
    [ textarea [placeholder "Bestellungsinformation"] []
    , goBack
    , button [onClick (ClientMsg PlaceOrder)] [text "Bestellung aufgeben"]
    ]

goBack = button [onClick (SwitchAppMode Select)] [text "Zurück"]
