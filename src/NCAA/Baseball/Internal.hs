module NCAA.Baseball.Internal where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import Text.HTML.Scalpel

type TeamId = Text
type Year = Int

fetchHtml :: Text -> IO (Maybe Text)
fetchHtml url = do
  request <- parseRequest (T.unpack url)
  response <- httpBS request
  pure $ case TE.decodeUtf8' (getResponseBody response) of
    Left _ -> Nothing
    Right body -> Just body

maybeText :: Selector -> Scraper Text (Maybe Text)
maybeText selector = do
  t <- text selector
  pure $ if T.null t then Nothing else Just t

textToMaybeInt :: Text -> Maybe Int
textToMaybeInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _ -> Nothing
