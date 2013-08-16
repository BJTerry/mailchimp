module Web.Mailchimp.Lists where

import Web.Mailchimp.Client
import Data.Text (Text(..), unpack)
import Data.Aeson (Value, object, (.=), decode, ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (Pair)
import Data.Aeson.Encode (encode)
import Network.HTTP.Conduit (parseUrl, requestBody, newManager, httpLbs, Response(..), HttpException(..))
import Data.Default (def)
import Control.Monad.Error (throwError)
import Control.Exception.Lifted (throwIO, catch)
import Control.Exception.Base (Exception(..))
import Data.Conduit (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero, liftM)
import Control.Applicative ((<*>), (<$>))
import Data.ByteString.Lazy (fromStrict)
import Network.HTTP.Conduit (parseUrl, requestBody, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..))


-- | Represents an individual mailing list
newtype ListId = ListId {unListId :: Text}

-- | Represents one of the canonical ways of identifying subscribers
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

data EmailReturn = EmailReturn { erEmail :: EmailId
                               , erEmailUniqueId :: EmailId
                               , erListEmailId :: EmailId
                               }
  deriving (Show)

instance FromJSON EmailReturn where
  parseJSON (Object v) = do
    email <- v .: "email"
    euid <- v .: "euid"
    leid <- v .: "leid"
    return $ EmailReturn (Email email) (EmailUniqueId euid) (ListEmailId leid)
  parseJSON _ = mzero

-- | Represents an individual merge variable
type MergeVarsItem = Pair

-- | The type of e-mail your user will receive
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
              sendWelcome = 
  query apiKey "lists" "subscribe" request
 where
  request = object [ "apikey" .= makApiKey apiKey
                           , "id" .= unListId listId
                           , "email" .= emailId
                           , "merge_vars" .= fmap object mergeVars
                           , "email_type" .= emailType
                           , "double_optin" .= doubleOptin
                           , "update_existing" .= updateExisting
                           , "replace_interests" .= replaceInterests
                           , "send_welcome" .= sendWelcome
                           ]
