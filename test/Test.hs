import Test.Tasty
import Test.Tasty.HUnit
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Aeson as Json
import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import JsonShape


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ jsonTypeTests
  , typeEquivalenceTests
  ]

jsonTypeTests :: TestTree
jsonTypeTests = testGroup "jsonTypeTests"
  [ jtt "null" TNull
  , jtt "1.0" TNumber
  , jtt "true" TBool
  , jtt "false" TBool
  , jtt "\"foo\"" TString
  , jtt "{\"foo\": \"bar\"}" (TObject (Map.fromList [("foo", TString)]))
  , jtt "[]" (TArray (TUnion Set.empty))
  , jtt "[\"foo\"]" (TArray TString)
  , jtt "[1.0, 2]" (TArray TNumber)
  , jtt "[1.0, \"bar\"]" (TArray (TUnion (Set.fromList [TNumber, TString])))
  ]

typeEquivalenceTests :: TestTree
typeEquivalenceTests = testGroup "typeEquivalenceTests"
  [ tet (TUnion Set.empty)
        (TUnion Set.empty)
  , tet (TUnion (Set.fromList [TNumber]))
        TNumber
  , tet (TUnion (Set.fromList [TNumber, TNull]))
        (TUnion (Set.fromList [TNull, TNumber]))
  ]

tet :: JsonType -> JsonType -> TestTree
tet input expected =
  testCase
    (show input ++ " ~ " ++ show expected)
    (expected @=? input)

jtt :: String -> JsonType -> TestTree
jtt input expected =
  testCase (input ++ " : " ++ show expected) $ do
    let Just json = Json.decode (encodeUtf8 (T.pack input)) :: Maybe Json.Value
    expected @=? inferType json
