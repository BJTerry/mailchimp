module Web.Mailchimp.Campaigns
  (CampaignId
  )
  where
    
import Data.Text (Text)
import Data.Aeson (Value(..), object, (.=), decode, ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))


newtype CampaignId = CampaignId {unCampaignId :: Text}
  deriving (Show, Eq)

instance FromJSON CampaignId where
  parseJSON (String v) = return $ CampaignId v
  parseJSON _ = mzero
