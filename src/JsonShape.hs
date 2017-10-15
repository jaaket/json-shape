module JsonShape where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Text as T
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint as PP ((<+>), ($+$))
import Control.Arrow ((***))


data JsonType =
    TString
  | TNumber
  | TObject (Map.Map String JsonType)
  | TArray JsonType
  | TBool
  | TNull
  | TUnion (Set.Set JsonType)
  deriving (Ord, Show)

instance Eq JsonType where
  TString == TString = True
  TNumber == TNumber = True
  TObject obj1 == TObject obj2 = obj1 == obj2
  TArray t1 == TArray t2 = t1 == t2
  TBool == TBool = True
  TNull == TNull = True
  TUnion ts1 == TUnion ts2 = ts1 == ts2
  TUnion ts == t2 =
    case Set.minView ts of
      Just (t, _) -> t == t2
      Nothing -> False -- ts is empty
  _ == _ = False

inferType :: Json.Value -> JsonType
inferType Json.Null = TNull
inferType (Json.Bool _) = TBool
inferType (Json.String _ ) = TString
inferType (Json.Number _) =
  TNumber
inferType (Json.Array vs) =
  TArray $
    let types = map inferType (Vector.toList vs)
    in  case types of
      []  -> TUnion Set.empty
      [t] -> t
      _   -> TUnion (Set.fromList types)
inferType (Json.Object obj) =
  TObject (Map.fromList (map (T.unpack *** inferType) (HashMap.toList obj)))

prettyPrint :: JsonType -> String
prettyPrint = PP.render . prettyPrint'

prettyPrint' :: JsonType -> PP.Doc
prettyPrint' TNull = PP.text "null"
prettyPrint' TBool = PP.text "bool"
prettyPrint' TString = PP.text "str"
prettyPrint' TNumber = PP.text "num"
prettyPrint' (TArray t) =
  PP.lbrack $+$ prettyPrint' t $+$ PP.rbrack
prettyPrint' (TObject obj) =
  PP.lbrace $+$ PP.hsep (PP.punctuate PP.comma (map (\(key, value) -> PP.doubleQuotes (PP.text key) <+> PP.char ':' <+> prettyPrint' value) (Map.toList obj)))
    $+$ PP.rbrace
prettyPrint' (TUnion ts) =
  PP.hsep (PP.punctuate (PP.text " |") (map prettyPrint' (Set.toList ts)))

