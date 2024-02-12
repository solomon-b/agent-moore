{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module OpenAI where

--------------------------------------------------------------------------------

import Control.Exception (throwIO)
import Control.Monad
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Deriving.Aeson
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types qualified as HTTP

--------------------------------------------------------------------------------

newtype OpenAICredential = OpenAICredential Text

data ClientSession = ClientSession
  { credential :: OpenAICredential,
    manager :: HTTP.Manager
  }

createSession :: OpenAICredential -> IO ClientSession
createSession credential = ClientSession credential <$> HTTP.newManager HTTP.tlsManagerSettings

mkRequest :: OpenAICredential -> Text -> IO HTTP.Request
mkRequest (OpenAICredential credential') path = do
  initRequest <- HTTP.parseUrlThrow (Text.unpack $ "http://api.openai.com" <> path)
  pure $
    initRequest
      { HTTP.requestHeaders =
          [ ("Content-Type", "application/json"),
            ("Authorization", "Bearer " <> TE.encodeUtf8 credential')
          ],
        HTTP.checkResponse = checkResponse,
        HTTP.method = "POST"
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

data ApiError = ApiError
  { apiErrMessage :: Text,
    apiErrType :: Text,
    apiErrParam :: Maybe Text,
    apiErrCode :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON
          '[ SumObjectWithSingleField,
             NoAllNullaryToStringTag,
             TagSingleConstructors,
             ConstructorTagModifier (Rename "ApiError" "error"),
             FieldLabelModifier '[StripPrefix "apiErr", CamelToSnake]
           ]
          ApiError

doRequest :: (Aeson.FromJSON a) => HTTP.Manager -> HTTP.Request -> IO (Either ApiError a)
doRequest manager request = do
  response <- HTTP.httpLbs request manager
  case decodeResp $ HTTP.responseBody response of
    Right x -> pure x
    Left e ->
      if HTTP.statusIsSuccessful $ HTTP.responseStatus response
        then fail e
        else throwResponseError request response (HTTP.responseBody response)

decodeResp :: (Aeson.FromJSON a) => ByteString -> Either String (Either ApiError a)
decodeResp resp = case Aeson.eitherDecode resp of
  Right a -> Right $ pure a
  Left e -> case Aeson.eitherDecode resp of
    Right me -> Right $ Left me
    Left _ -> Left e

--------------------------------------------------------------------------------
-- Chat Completions

newtype Model = Model Text
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | A list of messages comprising the conversation so far.
data Message = SystemMessage' SystemMessage | UserMessage' UserMessage | AssistantMessage' AssistantMessage | ToolMessage' ToolMessage
  deriving stock (Show)

instance FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \ob -> do
    ob .: "role" >>= \case
      ("system" :: Text) -> fmap SystemMessage' $ Aeson.parseJSON $ Aeson.Object ob
      "user" -> fmap UserMessage' $ Aeson.parseJSON $ Aeson.Object ob
      "assistant" -> fmap AssistantMessage' $ Aeson.parseJSON $ Aeson.Object ob
      "tool" -> fmap ToolMessage' $ Aeson.parseJSON $ Aeson.Object ob
      _ -> fail "Invalid role"

instance ToJSON Message where
  toJSON = \case
    SystemMessage' sm ->
      case Aeson.toJSON sm of
        Aeson.Object km -> Aeson.Object $ KeyMap.fromList [("role", "system")] <> km
        val -> val
    UserMessage' um ->
      case Aeson.toJSON um of
        Aeson.Object km -> Aeson.Object $ KeyMap.fromList [("role", "user")] <> km
        val -> val
    AssistantMessage' am ->
      case Aeson.toJSON am of
        Aeson.Object km -> Aeson.Object $ KeyMap.fromList [("role", "assistant")] <> km
        val -> val
    ToolMessage' tm ->
      case Aeson.toJSON tm of
        Aeson.Object km -> Aeson.Object $ KeyMap.fromList [("role", "tool")] <> km
        val -> val

data SystemMessage = SystemMessage
  { smContent :: Text,
    smName :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "sm", CamelToSnake]] SystemMessage

data UserMessage = UserMessage
  { umContent :: Text,
    umName :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "um", CamelToSnake], OmitNothingFields] UserMessage

data Function = Function
  { fName :: Text,
    fArguments :: Text
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "f", CamelToSnake]] Function

data ToolCall = ToolCall
  { tcId :: Text,
    tcType :: Text,
    tcFunction :: Function
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "tc", CamelToSnake]] ToolCall

data AssistantMessage = AssistantMessage
  { amContent :: Text,
    amName :: Maybe Text,
    amToolCalls :: Maybe [ToolCall]
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "am", CamelToSnake]] AssistantMessage

data ToolMessage = ToolMessage
  { tmContent :: Text,
    tmToolContentId :: String
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "tm", CamelToSnake]] ToolMessage

data ChatCompletionRequest = ChatCompletionRequest
  { ccrModel :: Model,
    ccrMessages :: [Message]
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ccr", CamelToSnake]] ChatCompletionRequest

data Usage = Usage
  { uPromptTokens :: Int,
    uCompletionTokens :: Int,
    uTotalTokens :: Int
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "u", CamelToSnake]] Usage

newtype Choice = Choice {cMessage :: Message}
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "c", CamelToSnake]] Choice

data ChatCompletionResponse = ChatCompletionResponse
  { ccreId :: Text,
    ccreObject :: Text,
    ccreCreated :: Int,
    ccreModel :: Model,
    ccreSystemFingerprint :: Maybe Text,
    ccreChoices :: [Choice],
    ccreUsage :: Usage
  }
  deriving stock (Show, Generic)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ccre", CamelToSnake]] ChatCompletionResponse

chatCompletion :: ClientSession -> ChatCompletionRequest -> IO (Either ApiError ChatCompletionResponse)
chatCompletion ClientSession {..} requestBody = do
  request <- mkRequest credential "/v1/chat/completions"

  doRequest manager $ request {HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode requestBody)}
