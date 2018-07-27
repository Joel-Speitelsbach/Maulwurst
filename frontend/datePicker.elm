

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings)
import Html exposing (Html, div, h1, text)


type Msg
    = ToDatePicker DatePicker.Msg


type alias Model =
    { date : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


init : ( Model, Cmd Msg )
init =
    let
        isDisabled date =
            dayOfWeek date
                |> flip List.member [ Sat, Sun ]

        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
        { date = Nothing
        , datePicker = datePicker
        }
            ! [ Cmd.map ToDatePicker datePickerFx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ datePicker } as model) =
    case msg of
        ToDatePicker msg ->
            let
                ( newDatePicker, datePickerFx, mDate ) =
                    DatePicker.update defaultSettings msg datePicker

                date =
                    case mDate of


                        DatePicker.Changed date ->
                            date

                        _ ->
                            model.date
            in
                { model
                    | date = date
                    , datePicker = newDatePicker
                }
                    ! [ Cmd.none ]


view : Model -> Html Msg
view ({ date, datePicker } as model) =
    div []
        [ case date of
            Nothing ->
                h1 [] [ text "Pick a date" ]

            Just date ->
                h1 [] [ text <| formatDate date ]
        , (DatePicker.view date defaultSettings datePicker)
            |> Html.map ToDatePicker
        ]


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
