

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
