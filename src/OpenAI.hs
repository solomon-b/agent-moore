{-# LANGUAGE OverloadedStrings #-}
module OpenAI where
--------------------------------------------------------------------------------

import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Data.ByteString.Lazy (ByteString, toStrict)
import Control.Exception (throwIO)
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

--------------------------------------------------------------------------------

newtype OpenAICredential = OpenAICredential Text

mkRequest :: OpenAICredential -> Text -> IO HTTP.Request
mkRequest (OpenAICredential credential) path = do
  initRequest <- HTTP.parseUrlThrow (Text.unpack $ "http://api.openai.com" <> path)
  pure $
    initRequest
      { HTTP.requestHeaders =
          [("Content-Type", "application/json"),
           ("Authorization", "Bearer " <> TE.encodeUtf8 credential)
          ],
        HTTP.checkResponse = checkResponse
      }

checkResponse :: HTTP.Request -> HTTP.Response HTTP.BodyReader -> IO ()
checkResponse req res =
  unless (200 <= code && code < 500) $ do
    chunk <- HTTP.brReadSome (HTTP.responseBody res) 1024
    throwResponseError req res chunk
  where
    HTTP.Status code _ = HTTP.responseStatus res

throwResponseError :: HTTP.Request -> HTTP.Response body -> ByteString -> IO a
throwResponseError req res chunk =
  throwIO $ HTTP.HttpExceptionRequest req ex
  where
    ex = HTTP.StatusCodeException (void res) (toStrict chunk)

--------------------------------------------------------------------------------
-- Chat Completions

data Model
data Message
data MessageResponse

chatCompletion :: OpenAICredential -> [Message] -> Model -> IO MessageResponse
chatCompletion credential _messages _model = do
  request <- mkRequest credential $ "/v1/chat/completions"

  undefined credential request
