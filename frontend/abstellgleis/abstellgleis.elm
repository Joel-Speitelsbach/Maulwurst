

string2bestelltyp : String -> Bestelltyp
string2bestelltyp char =
case char of
"Merchingen"   -> Merchingen
"Adelsheim"    -> Adelsheim
"Partyservice" -> Partyservice leereParty
_              -> Merchingen

bestelltyp2string : Bestelltyp -> String
bestelltyp2string btyp =
case btyp of
Merchingen     -> "Merchingen"
Adelsheim      -> "Adelsheim"
Partyservice _ -> "Partyservice"

-- Details
DatePickerMsg msg ->
  let
    (datePicker, datePickerCmd, dateEvent) =
      DatePicker.update
        DatePicker.defaultSettings
        msg
        model.datePicker
  in
    ( { model
      | datePicker = datePicker
      }
    , Cmd.map DatePickerMsg datePickerCmd
    )


viewDatePicker datePicker  =
Element.map DatePickerMsg <| html <|
DatePicker.view
Nothing
DatePicker.defaultSettings
datePicker
