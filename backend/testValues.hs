
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as Ap
import qualified Data.ByteString.Lazy.Char8 as B
import Types

testLieferung = Lieferung
  { _bestelldatum     = 1
  , _lieferdatum      = Left "5"
  , _kundenname       = ""
  , _bestelltyp       = Merchingen
  , _partyserviceData = leereParty
  , _inPapierkorb     = Nothing
  , _lid              = 0
  , _bestellungen     = []
  }

leereParty = PartyserviceData
  { _adresse           = ""
  , _telefon           = ""
  , _veranstaltungsort = ""
  , _personenanzahl    = ""
  }

test :: A.ToJSON val => val -> IO ()
test = B.putStrLn . Ap.encodePretty
