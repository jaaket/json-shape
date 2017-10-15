module Main where

import JsonShape
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Aeson as Json


main :: IO ()
main = do
  input <- getContents
  let Just json = Json.decode (encodeUtf8 (T.pack input)) :: Maybe Json.Value
  let t = inferType json
  putStrLn (prettyPrint t)
