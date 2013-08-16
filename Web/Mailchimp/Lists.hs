module Web.Mailchimp.Lists 
  ( ListId (..)
  , EmailId (..)
  , EmailReturn (..)
  , MergeVarsItem
  , EmailType (..)
  , subscribeUser
  , AbuseReport (..)
  , AbuseResults (..)
  , abuseReports
  )
  where

import Web.Mailchimp.Client
import Web.Mailchimp.Campaigns (CampaignId)

import Data.Text (Text)
import Data.Aeson (Value(..), object, (.=), ToJSON(..), FromJSON(..), (.:), Value(..))
import Data.Aeson.Types (Pair)
import Control.Monad (mzero)
import Data.Time.Clock (UTCTime)
import Control.Applicative ((<*>), (<$>))
import Control.Monad.IO.Class (liftIO)

-- | Represents an individual mailing list
newtype ListId = ListId {unListId :: Text}

instance ToJSON ListId where
  toJSON lid = String $ unListId lid

-- | Represents one of the canonical ways of identifying subscribers
data EmailId = Email Text
             | EmailUniqueId Text
             | ListEmailId Text
  deriving (Show)

instance ToJSON EmailId where
  toJSON (Email t) = object ["email" .= t]
  toJSON (EmailUniqueId t) = object ["euid" .= t]
  toJSON (ListEmailId t) = object ["leid" .= t]

-- | The result of calling subscribeUser. Provides three ways of identifying the user for subsequent calls.
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

-- | Represents an individual merge variable. For example @(\"FNAME\", \"John\")@ or @(\"mc_location\", object [(\"latitude\", \"37.723614\"), (\"longitude\", \"-122.21878\"])@
type MergeVarsItem = Pair

-- | The type of e-mail your user will receive
data EmailType = EmailTypeHTML
               | EmailTypeText
  deriving (Show)

instance ToJSON EmailType where
  toJSON EmailTypeHTML = "html"
  toJSON EmailTypeText = "text"

-- | Subscribes a user to the given list. 
--   See <http://apidocs.mailchimp.com/api/2.0/lists/subscribe.php> for details.
--
--   Example Usage:
-- 
-- >>> subscribeUser listId emailId (Just [("FNAME", "John"), ("LNAME", "Doe")]) (Just doubleOptin) (Just updateExisting) (Just replaceInterests) (Just sendWelcome)
-- 
subscribeUser :: ListId 
              -> EmailId 
              -> Maybe [MergeVarsItem] 
              -> Maybe EmailType
              -> Maybe Bool 
              -> Maybe Bool 
              -> Maybe Bool 
              -> Maybe Bool 
              -> Mailchimp EmailReturn
subscribeUser listId 
              emailId 
              mergeVars 
              emailType 
              doubleOptin 
              updateExisting 
              replaceInterests 
              sendWelcome = do
  apiKey <- askApiKey
  query "lists" "subscribe" $ request apiKey
 where
  request apiKey = filterObject [ "apikey" .= makApiKey apiKey
                                , "id" .= unListId listId
                                , "email" .= emailId
                                , "merge_vars" .= fmap object mergeVars
                                , "email_type" .= emailType
                                , "double_optin" .= doubleOptin
                                , "update_existing" .= updateExisting
                                , "replace_interests" .= replaceInterests
                                , "send_welcome" .= sendWelcome
                                ]

data AbuseReport = AbuseReport
  { arDate :: UTCTime
  , arEmail :: Text
  , arCampaignId :: CampaignId
  , arType :: Text
  }

instance FromJSON AbuseReport where
  parseJSON (Object v) = AbuseReport         <$> 
            unMCTime <$> v .: "date"         <*>
                         v .: "email"        <*>
                         v .: "campaign_id"  <*>
                         v .: "type"
  parseJSON _ = mzero

data AbuseResults = AbuseResults 
  { arTotal :: Int
  , arData :: [AbuseReport]
  }

instance FromJSON AbuseResults where
  parseJSON (Object v) = AbuseResults <$> v .: "total" <*> v.: "data"
  parseJSON _ = mzero

abuseReports :: ListId -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Mailchimp AbuseResults
abuseReports listId start limit since = do
  apiKey <- askApiKey
  liftIO $ print $ request apiKey
  query "lists" "abuse-reports" $ request apiKey
 where
  request apiKey = filterObject [ "apikey" .= makApiKey apiKey
                                , "id" .= listId
                                , "start" .= start
                                , "limit" .= limit
                                , "since" .= (MCTime `fmap` since)
                                ]
