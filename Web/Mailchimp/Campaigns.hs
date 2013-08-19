module Web.Mailchimp.Campaigns
  ( CampaignId
  , CampaignInfo (..)
  )
  where

import Web.Mailchimp.Client

import Data.Text (Text)
import Data.Aeson (Value(..), object, (.=), decode, ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))


newtype CampaignId = CampaignId {unCampaignId :: Text}
  deriving (Show, Eq)

instance FromJSON CampaignId where
  parseJSON (String v) = return $ CampaignId v
  parseJSON _ = mzero

data CampaignTracking = CampaignTracking
  { ctHtmlClicks :: Bool
  , ctTextClicks :: Bool
  , ctOpens :: Bool
  }

$(deriveFromJSON (convertName 2) ''CampaignTracking)

data CampaignInfo = CampaignInfo
  { ciId :: CampaignId
  , ciWebId :: Int
  , ciListId :: Text
  , ciFolderId :: Int
  , ciTemplateId :: Int
  , ciContentType :: Text -- TODO: Needs type
  , ciTitle :: Text
  , ciType :: Text
  , ciCreateTime :: MCTime
  , ciSendTime :: MCTime
  , ciEmailsSent :: Int
  , ciStatus :: Text -- TODO: Needs type
  , ciFromName :: Text
  , ciFromEmail :: Text
  , ciSubject :: Text
  , ciToName :: Text
  , ciArchiveUrl :: Text
  , ciInlineCss :: Bool
  , ciAnalytics :: Text
  , ciAnalyticsTag :: Text
  , ciAuthenticate :: Bool
  , ciEcomm360 :: Bool
  , ciAutoTweet :: Bool
  , ciAutoFbPost :: Text
  , ciAutoFooter :: Bool
  , ciTimewarp :: Bool
  , ciParentId :: Text
  , ciTracking :: CampaignTracking
  , ciSegmentText :: Text
  , ciSegmentOpts :: Value -- TODO: Needs type
  , ciTypeOpts :: Value -- TODO: Needs type
  , ciCommentsTotal :: Int
  , ciCommentsUnread :: Int
  , ciSummary :: Value -- TODO: Needs type
  }

$(deriveFromJSON (convertName 2) ''CampaignInfo)
