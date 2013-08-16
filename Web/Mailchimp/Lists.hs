module Web.Mailchimp.Lists where

import Web.Mailchimp.Client
import Data.Text (Text(..), unpack)
import Data.Aeson (Value, object, (.=), decode, ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (Pair)
import Data.Aeson.Encode (encode)
import Network.HTTP.Conduit (parseUrl, requestBody, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..))
import Data.Default (def)
import Control.Monad.Error (throwError)
import Control.Exception.Lifted (throwIO, catch)
import Control.Exception.Base (Exception(..))
import Data.Conduit (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero, liftM)
import Control.Applicative ((<*>), (<$>))
import Data.ByteString.Lazy (fromStrict)

newtype ListId = ListId {unListId :: Text}

data EmailId = Email Text
             | EmailUniqueId Text
             | ListEmailId Text
  deriving (Show)

instance ToJSON EmailId where
  toJSON (Email t) = object ["email" .= t]
  toJSON (EmailUniqueId t) = object ["euid" .= t]
  toJSON (ListEmailId t) = object ["leid" .= t]

unEmailId :: EmailId -> Text
unEmailId (Email t) = t
unEmailId (EmailUniqueId t) = t
unEmailId (ListEmailId t) = t

data EmailReturn = EmailReturn { erEmail :: Maybe EmailId
                               , erEmailUniqueId :: Maybe EmailId
                               , erListEmailId :: Maybe EmailId
                               }
  deriving (Show)

instance FromJSON (EmailId, EmailId, EmailId) where
  parseJSON (Object v) = do
    email <- v .: "email"
    euid <- v .: "euid"
    leid <- v .: "leid"
    return ((Email email), (EmailUniqueId euid), (ListEmailId leid))
  parseJSON _ = mzero

instance FromJSON EmailReturn where
  parseJSON (Object v) = do
    email <- v .:? "email"
    euid <- v .:? "euid"
    leid <- v .:? "leid"
    return $ EmailReturn (fmap Email email) (fmap EmailUniqueId euid) (fmap ListEmailId leid)
  parseJSON _ = mzero

type MergeVarsItem = Pair

data EmailType = EmailTypeHTML
               | EmailTypeText
  deriving (Show)

instance ToJSON EmailType where
  toJSON EmailTypeHTML = "html"
  toJSON EmailTypeText = "text"

subscribeUser :: MailchimpApiKey 
              -> ListId 
              -> EmailId 
              -> Maybe [MergeVarsItem] 
              -> Maybe EmailType
              -> Maybe Bool 
              -> Maybe Bool 
              -> Maybe Bool 
              -> Maybe Bool 
              -> IO EmailReturn
subscribeUser apiKey 
              listId 
              emailId 
              mergeVars 
              emailType 
              doubleOptin 
              updateExisting 
              replaceInterests 
              sendWelcome = runResourceT $ do
  initReq <- parseUrl $ unpack $ apiEndpointUrl (makDatacenter apiKey) "lists" "subscribe"
  let requestJson = object [ "apikey" .= makApiKey apiKey
                           , "id" .= unListId listId
                           , "email" .= emailId
                           , "merge_vars" .= fmap object mergeVars
                           , "email_type" .= emailType
                           , "double_optin" .= doubleOptin
                           , "update_existing" .= updateExisting
                           , "replace_interests" .= replaceInterests
                           , "send_welcome" .= sendWelcome
                           ]
  let req = initReq { requestBody = RequestBodyLBS $ encode requestJson }
  man <- liftIO $ newManager def 
  
  response <- catch (httpLbs req man) 
    (\e -> 
      case e :: HttpException of
        StatusCodeException _ headers _ -> do
          let (mResponse :: Maybe MailchimpError) = fromStrict `fmap` (lookup "X-Response-Body-Start" headers) >>= decode
          liftIO $ print (lookup "X-Response-Body-Start" headers)
          liftIO $ print mResponse
          maybe (throwIO e) id (throwIO `fmap` mResponse)
        _ -> throwIO e)
  liftIO $ print $ responseBody response
  let meResult = maybe (fmap Right $ decode $ responseBody response) id (fmap (Just . Left) $ decode $ responseBody response)
  case meResult of
    Just (Left mcError) -> throwIO (mcError :: MailchimpError)
    Just (Right emailIdTriple) -> return emailIdTriple
    Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"

