-- |
-- Implements the \"lists\" section of the Mailchimp JSON API.
--
module Web.Mailchimp.Lists
  ( 
  -- * Mailchimp API Methods
    abuseReports
  , listActivity
  , batchSubscribe
  , batchUnsubscribe
  , clients
  , growthHistory
  , interestGroupAdd
  , interestGroupDelete
  , interestGroupUpdate
  , interestGroupingAdd
  , interestGroupingDelete
  , interestGroupingUpdate
  , interestGroupings
  , listInfo
  , listLocations
  , memberActivity
  , memberInfo
  , members
  , mergeVarAdd
  , mergeVarDelete
  , mergeVarReset
  , mergeVarSet
  , mergeVarUpdate
  , mergeVarInfo
  , staticSegmentAdd
  , staticSegmentDelete
  , staticSegmentMembersAdd
  , staticSegmentMembersDelete
  , staticSegmentReset
  , staticSegments
  , subscribeUser
  , unsubscribe
  , updateMember
  , webhookAdd
  , webhookDelete
  , webhooks
  -- * Parameter types
  , ListId(..)
  , EmailId(..)
  , MergeVarsItem
  , EmailType(..)
  , AbuseReport(..)
  , GroupingId
  , GroupingType(..)
  , ListFilters(..)
  , SortField(..)
  , SortDir(..)
  , MemberStatus(..)
  , MergeVarType(..)
  , MergeVarOptions(..)
  , SegmentId
  , WebhookActions(..)
  , WebhookSources(..)
  , WebhookId

  
  -- * Result types
  , EmailResult(..)
  , AbuseResults(..)
  , UserResultError(..)
  , BatchSubscribeResult(..)
  , BatchUnsubscribeResult(..)
  , ClientItem(..)
  , ClientResultsCategory(..)
  , ClientResults(..)
  , GrowthHistoryResult(..)
  , CompleteResult(..)
  , InterestGroupingAddResult(..)
  , InterestGroupDetail(..)
  , InterestGrouping(..)
  , ListStats(..)
  , ListInfo(..)
  , ListResultError(..)
  , ListsResult(..)
  , ListLocationsResult(..)
  , ActivityAction(..)
  , ActivityRecord(..)
  , MemberActivityResult(..)
  , MemberList(..)
  , MemberGeo(..)
  , MemberClient(..)
  , MemberStaticSegment(..)
  , MemberNote(..)
  , MemberInfoFields(..)
  , MemberInfo(..)
  , MemberInfoError(..)
  , MemberInfoResult(..)
  , MembersResult(..)
  , MergeVarResult(..)
  , MergeVarInfo(..)
  , MergeVarError(..)
  , MergeVarInfoResult(..)
  , StaticSegmentAddResult(..)
  , StaticSegmentMembersAddResult(..)
  , StaticSegmentMembersDelResult(..)
  , StaticSegmentResult(..)
  , WebhookAddResult(..)
  , WebhookResult(..)
  )
  where

import Web.Mailchimp
import Web.Mailchimp.Campaigns (CampaignId, CampaignInfo(..) )
import Web.Mailchimp.Util

import Data.Text (Text)
import Data.Aeson (Value(..), object, (.=), ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (Pair)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Control.Monad (mzero)
import Data.Time.Clock (UTCTime)
import Data.Maybe (catMaybes)
import Data.Default (Default(..))
import Text.Read (readMaybe)
import Data.HashMap.Strict (lookup)

-- | Represents an individual mailing list
newtype ListId = ListId {unListId :: Text}
  deriving (Show, Eq)

instance ToJSON ListId where
  toJSON lid = String $ unListId lid

instance FromJSON ListId where
  parseJSON (String s) = return $ ListId s
  parseJSON _ = mzero

-- | Represents one of the canonical ways of identifying subscribers
data EmailId = Email Text
             | EmailUniqueId Text
             | ListEmailId Text
  deriving (Show, Eq)

instance ToJSON EmailId where
  toJSON (Email t) = object ["email" .= t]
  toJSON (EmailUniqueId t) = object ["euid" .= t]
  toJSON (ListEmailId t) = object ["leid" .= t]

instance FromJSON EmailId where
  parseJSON (Object v) = do
    email <- fmap (fmap Email) $ v .:? "email"
    euid <- fmap (fmap EmailUniqueId) $ v .:? "euid"
    leid <- fmap (fmap ListEmailId) $ v .:? "leid"
    case catMaybes [email, euid, leid] of
      (x:_) -> return x
      _ -> mzero
  parseJSON _ = mzero

-- | Provides three ways of identifying the user for subsequent calls.
data EmailResult = EmailResult { erEmail :: EmailId
                               , erEmailUniqueId :: EmailId
                               , erListEmailId :: EmailId
                               }
  deriving (Show, Eq)

instance FromJSON EmailResult where
  parseJSON (Object v) = do
    email <- v .: "email"
    euid <- v .: "euid"
    leid <- v .: "leid"
    return $ EmailResult (Email email) (EmailUniqueId euid) (ListEmailId leid)
  parseJSON _ = mzero

-- | Represents an individual merge variable. For example @(\"FNAME\", \"John\")@ or @(\"mc_location\", object [(\"latitude\", \"37.723614\"), (\"longitude\", \"-122.21878\"])@
type MergeVarsItem = Pair

-- | The type of e-mail your user will receive
data EmailType = EmailTypeHTML
               | EmailTypeText
  deriving (Show, Eq)

instance ToJSON EmailType where
  toJSON EmailTypeHTML = "html"
  toJSON EmailTypeText = "text"

instance FromJSON EmailType where
  parseJSON (String "html") = return EmailTypeHTML
  parseJSON (String "text") = return EmailTypeText
  parseJSON _ = mzero

-- | Subscribes a user to the given list. 
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/subscribe.php> for details.
--
--   Example Usage:
-- 
-- > subscribeUser listId emailId (Just [("FNAME", "John"), ("LNAME", "Doe")]) (Just doubleOptin) (Just updateExisting) (Just replaceInterests) (Just sendWelcome)
-- 
subscribeUser :: ListId 
              -- ^ The list to subscribe the user to
              -> EmailId 
              -- ^ Subscriber's email              
              -> Maybe [MergeVarsItem] 
              -- ^ List of merge vars for the subscriber
              -> Maybe EmailType
              -- ^ Type of e-mail
              -> Maybe Bool 
              -- ^ Send the user an optin email
              -> Maybe Bool 
              -- ^ Update existing subscriber
              -> Maybe Bool 
              -- ^ Replace user interests
              -> Maybe Bool 
              -- ^ Send welcome email
              -> MailchimpT m EmailResult
subscribeUser listId 
              emailId 
              mergeVars 
              emailType 
              doubleOptin 
              updateExisting 
              replaceInterests 
              sendWelcome = 
  query "lists" "subscribe" request 
 where
  request = [ "id" .= unListId listId
            , "email" .= emailId
            , "merge_vars" .= fmap filterObject mergeVars
            , "email_type" .= emailType
            , "double_optin" .= doubleOptin
            , "update_existing" .= updateExisting
            , "replace_interests" .= replaceInterests
            , "send_welcome" .= sendWelcome
            ]

-- | The reports produced by abuseReports
data AbuseReport = AbuseReport
  { arDate :: UTCTime
  , arEmail :: Text
  , arCampaignId :: CampaignId
  , arType :: Text
  }
  deriving (Show, Eq)
 
instance FromJSON AbuseReport where
  parseJSON (Object v) = do
    ar_date <- v .: "date"
    ar_email <- v .: "email"
    ar_campaign_id <- v .: "campaign_id"
    ar_type <- v .: "type"
    return $ AbuseReport ar_date ar_email ar_campaign_id ar_type
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''AbuseReport)


-- | Aggregate abuse results returnd by abuseReports
data AbuseResults = AbuseResults 
  { arTotal :: Int
  , arData :: [AbuseReport]
  }
  deriving (Show, Eq)

instance FromJSON AbuseResults where
  parseJSON (Object v) = do
    ar_total <- v .: "total"
    ar_data <- v .: "data"
    return $ AbuseResults ar_total ar_data
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''AbuseResults)

-- | Queries Mailchimp for the abuse reports for a given mailing list
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/abuse-reports.php> for details.
--
--   Example Usage: 
--
-- > abuseReports listId (Just startingPage [indexed at 0]) (Just limitResults) (Just sinceDate)
abuseReports :: ListId
             -- ^ List
             -> Maybe Int
             -- ^ Start page (from 0)
             -> Maybe Int
             -- ^ Limit number of records
             -> Maybe UTCTime
             -- ^ Since time
             -> MailchimpT m AbuseResults
abuseReports listId start limit since = 
  query "lists" "abuse-reports" request
 where
  request = [ "id" .= listId
            , "start" .= start
            , "limit" .= limit
            , "since" .= (MCTime `fmap` since)
            ]

-- | Provides unspecified data relating to list activity
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/activity.php>
listActivity :: ListId -> MailchimpT m [Value]
listActivity listId = 
  query "lists" "activity" [ "id" .= listId ]

-- | An error result from batch calls, such as subscribeUser
data UserResultError = UserResultError
  { bsreEmail :: Maybe EmailId
  , bsreCode :: Int
  , bsreError :: Text
  , bsreRow :: Maybe Value -- This value doesn't actually seem to be returned by the API
  }
  deriving (Show, Eq)

instance FromJSON UserResultError where
  parseJSON (Object v) = do
    bsre_email <- v .:? "email_id"
    bsre_code <- v .: "code"
    bsre_error <- v .: "error"
    bsre_row <- v .:? "value"
    return $ UserResultError bsre_email bsre_code bsre_error bsre_row
  parseJSON _ = mzero

$(deriveToJSON (convertName 4) ''UserResultError)

-- | The result type of batchSubscribe
data BatchSubscribeResult = BatchSubscribeResult
  { bsrAddCount :: Int
  , bsrAdds :: [EmailResult]
  , bsrUpdateCount :: Int
  , bsrUpdates :: [EmailResult]
  , bsrErrorCount :: Int
  , bsrErrors :: [UserResultError]
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 3) ''BatchSubscribeResult)


-- | Subscribe many users at once to a particular list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/batch-subscribe.php>
--
--   Usage:
--
-- > batchSubscribe listId [(Email "example@example.com", EmailTypeHTML, [("FNAME", "John")]] (Just doubleOptin) (Just updateExisting) (Just replaceInterests)
batchSubscribe :: ListId
               -- ^ List
               -> [(EmailId, EmailType, [MergeVarsItem])]
               -- ^ Emails, email type, and merge vars for the user               
               -> Maybe Bool
               -- ^ Send double optin email
               -> Maybe Bool
               -- ^ Update existing users
               -> Maybe Bool
               -- ^ Replace user interests
               -> MailchimpT m BatchSubscribeResult
batchSubscribe listId batchItems doubleOptin updateExisting replaceInterests =
  query "lists" "batch-subscribe" request
 where
  request = [ "id" .= listId
            , "batch" .= buildBatch batchItems
            , "double_optin" .= doubleOptin
            , "update_existing" .= updateExisting
            , "replace_interests" .= replaceInterests
            ]
  buildBatch :: [(EmailId, EmailType, [MergeVarsItem])] -> [Value]
  buildBatch = map buildBatchItem
  buildBatchItem (email, emailType, mergeVars) = 
    filterObject [ "email" .= email
                 , "email_type" .= emailType
                 , "merge_vars" .= filterObject mergeVars
                 ]

data BatchUnsubscribeResult = BatchUnsubscribeResult
  { burSuccessCount :: Int
  , burErrorCount :: Int
  --, burOf :: Maybe Value -- This is in the documentation, but not being returned by the server
  , burErrors :: [UserResultError]
  }
  deriving (Show, Eq)

instance FromJSON BatchUnsubscribeResult where
  parseJSON (Object v) = do
    bur_success_count <- v .: "success_count"
    bur_error_count <- v .: "error_count"
    --bur_of <- v .:? "of"
    bur_errors <- v .: "errors"
    return $ BatchUnsubscribeResult bur_success_count bur_error_count bur_errors
  parseJSON _ = mzero
-- $(deriveFromJSON (convertName 3) ''BatchUnsubscribeResult)


-- | Unsubscribe multiple users from a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/batch-unsubscribe.php>
--
--   Usage:
--
-- > batchUnsubscribe listId [Email "example@example.com"] (Just deleteMember) (Just sendGoodbye) (Just sendNotify)
batchUnsubscribe :: ListId
                 -- ^ List
                 -> [EmailId]
                 -- ^ Emails to unsubscribe
                 -> Maybe Bool
                 -- ^ Delete member after unsubscribing
                 -> Maybe Bool
                 -- ^ Send goodbye email
                 -> Maybe Bool
                 -- ^ Send notification email to list administrator
                 -> MailchimpT m BatchUnsubscribeResult
batchUnsubscribe  listId batchItems deleteMember sendGoodbye sendNotify = 
  query "lists" "batch-unsubscribe" 
    [ "id" .= listId
    , "batch" .= batchItems
    , "delete_member" .= deleteMember
    , "send_goodbye" .= sendGoodbye
    , "send_notify" .= sendNotify
    ]

data ClientItem = ClientItem
  { ciClient :: Text
  , ciPercent :: Double
  , ciMembers :: Int
  }
  deriving (Show, Eq)

instance FromJSON ClientItem where
  parseJSON (Object v) = do
    ci_client <- v .: "client"
    ci_percent <- v .: "percent"
    ci_members <- v .: "members"
    return $ ClientItem ci_client ci_percent ci_members
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''ClientItem)


data ClientResultsCategory = ClientResultsCategory
  { crPenetration :: Double
  , crClients :: [ClientItem]
  }
  deriving (Show, Eq)

instance FromJSON ClientResultsCategory where
  parseJSON (Object v) = do
    cr_penetration <- v .: "penetration"
    cr_clients <- v .: "clients"
    return $ ClientResultsCategory cr_penetration cr_clients
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''ClientResultsCategory)


data ClientResults = ClientResults
  { crDesktop :: Maybe ClientResultsCategory
  , crMobile :: Maybe ClientResultsCategory
  }
  deriving (Show, Eq)

instance FromJSON ClientResults where
  parseJSON (Object v) = do
    cr_desktop <- resultsOrArray "desktop"
    cr_mobile <- resultsOrArray "mobile"
    -- Returns empty lists if there are no clients
    return $ ClientResults cr_desktop cr_mobile
   where
    resultsOrArray k = case Data.HashMap.Strict.lookup k v of
                         Just (Object _) -> v .:? k
                         _ -> return Nothing
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''ClientResults)


-- | Gets the user agent info for members of the list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/clients.php>
clients :: ListId -> MailchimpT m ClientResults
clients listId = 
  query "lists" "clients" [ "id" .= listId ]


data GrowthHistoryResult = GrowthHistoryResult
  { ghrMonth :: Text
  , ghrExisting :: Maybe Int
  , ghrImports :: Maybe Int
  , ghrOptins :: Maybe Int
  }
  deriving (Show, Eq)

instance FromJSON GrowthHistoryResult where
  parseJSON (Object v) = do
    ghr_month <- v .: "month" 
    ghr_existing <- numberOrString "existing"
    ghr_imports <- numberOrString "imports"
    ghr_optins <- numberOrString "optins"
    return GrowthHistoryResult { ghrMonth = ghr_month 
                               , ghrExisting = ghr_existing 
                               , ghrImports = ghr_imports
                               , ghrOptins = ghr_optins
                               }
   where
    numberOrString k = case Data.HashMap.Strict.lookup k v of
                         Just (String _) -> fmap readMaybe $ v .: k
                         Just (Number _) -> v .:? k
                         _ -> return Nothing
  parseJSON _ = mzero


$(deriveToJSON (convertName 3) ''GrowthHistoryResult)

-- $(deriveFromJSON (convertName 3) ''GrowthHistoryResult)

-- | Gets the growth history by month for an account or list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/growth-history.php>
growthHistory :: Maybe ListId -> MailchimpT m [GrowthHistoryResult]
growthHistory listId = 
  query "lists" "growth-history" [ "id" .= listId ]


-- | Grouping IDs for interest groups (not the idenfier for the interest group itself, which are identified by name only)
type GroupingId = Int

data CompleteResult = CompleteResult 
  { crComplete :: Bool
  }
  deriving (Show, Eq)

instance FromJSON CompleteResult where
  parseJSON (Object v) = do
    cr_complete <- v .: "complete"
    return $ CompleteResult cr_complete
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''CompleteResult)

-- | Adds an interest group to a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-group-add.php>
interestGroupAdd :: ListId
                 -- ^ List
                 -> Text
                 -- ^ Group name
                 -> Maybe GroupingId
                 -- ^ Grouping to put group in
                 -> MailchimpT m Bool
interestGroupAdd listId name groupId =
  fmap crComplete $ query "lists" "interest-group-add" [ "id" .= listId
                                                       , "group_name" .= name
                                                       , "grouping_id" .= groupId
                                                       ]
-- | Removes an interest group from a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-group-del.php>
interestGroupDelete :: ListId
                    -- ^ List
                    -> Text
                    -- ^ Group name
                    -> Maybe GroupingId
                    -- ^ Optional grouping
                    -> MailchimpT m Bool
interestGroupDelete listId name groupId =
  fmap crComplete $ query "lists" "interest-group-del" [ "id" .= listId
                                                       , "group_name" .= name
                                                       , "grouping_id" .= groupId
                                                       ]

-- | Changes the name of an interest group
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-group-update.php>
interestGroupUpdate :: ListId
                    -- ^ List
                    -> Text
                    -- ^ Old group name
                    -> Text
                    -- ^ New group name
                    -> Maybe GroupingId
                    -- ^ Optional grouping
                    -> MailchimpT m Bool
interestGroupUpdate listId oldName newName groupId = 
  fmap crComplete $ query "lists" "interest-group-update" [ "id" .= listId
                                                          , "old_name" .= oldName
                                                          , "new_name" .= newName
                                                          , "grouping_id" .= groupId
                                                          ]

data GroupingType = CheckboxesGrouping
                  | HiddenGrouping
                  | DropdownGrouping
                  | RadioGrouping
  deriving (Show, Eq)

instance ToJSON GroupingType where
  toJSON CheckboxesGrouping = String "checkboxes"
  toJSON HiddenGrouping = String "hidden"
  toJSON DropdownGrouping = String "dropdown"
  toJSON RadioGrouping = String "radio"

instance FromJSON GroupingType where
  parseJSON (String "checkboxes") = return CheckboxesGrouping
  parseJSON (String "hidden") = return HiddenGrouping
  parseJSON (String "dropdown") = return DropdownGrouping
  parseJSON (String "radio") = return RadioGrouping
  parseJSON _ = mzero

data InterestGroupingAddResult = InterestGroupingAddResult
  { igarId :: GroupingId
  }
  deriving (Show, Eq)

instance FromJSON InterestGroupingAddResult where
  parseJSON (Object v) = do
    igar_id <- v .: "id"
    return $ InterestGroupingAddResult igar_id
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 4) ''InterestGroupingAddResult)

-- | Creates a new Interest Grouping
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-grouping-add.php>
interestGroupingAdd :: ListId
                    -- ^ List
                    -> Text
                    -- ^ New grouping name
                    -> GroupingType
                    -- ^ Type of grouping
                    -> [Text]
                    -- ^ New groups in grouping
                    -> MailchimpT m GroupingId
interestGroupingAdd listId name groupingType groups =
  fmap igarId $ query "lists" "interest-grouping-add" [ "id" .= listId
                                                      , "name" .= name
                                                      , "type" .= groupingType
                                                      , "groups" .= groups
                                                      ]
-- | Deletes an Interest Grouping
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-grouping-del.php>
interestGroupingDelete :: GroupingId -> MailchimpT m Bool
interestGroupingDelete groupId = 
  fmap crComplete $ query "lists" "interest-grouping-del" ["grouping_id" .= groupId ]

-- | Updates an Interest Grouping. 
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-grouping-update.php>
--
--   Usage
--
-- > interestGroupingUpdate groupId field value
--
--   Where "field" is either "name" or "type" and value is the new value for that field.
interestGroupingUpdate :: GroupingId
                       -- ^ Grouping
                       -> Text
                       -- ^ Field to update, either "name" or "type"
                       -> Text
                       -- ^ New value for field
                       -> MailchimpT m Bool
interestGroupingUpdate groupId name value =
  fmap crComplete $ query "lists" "interest-grouping-update" [ "grouping_id" .= groupId
                                                             , "name" .= name
                                                             , "value" .= value
                                                             ]
data InterestGroupDetail = InterestGroupDetail
  { igdBit :: Text
  , igdName :: Text
  , igdDisplayOrder :: Text
  , igdSubscribers :: Int
  }
  deriving (Show, Eq)

instance FromJSON InterestGroupDetail where
  parseJSON (Object v) = do
    igd_bit <- v .: "bit"
    igd_name <- v .: "name"
    igd_display_order <- v .: "display_order"
    igd_subscribers <- v .: "subscribers"
    return $ InterestGroupDetail igd_bit igd_name igd_display_order igd_subscribers
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''InterestGroupDetail)

data InterestGrouping = InterestGrouping
  { igId :: GroupingId
  , igName :: Text
  , igFormField :: GroupingType
  , igGroups :: [InterestGroupDetail]
  }
  deriving (Show, Eq)

instance FromJSON InterestGrouping where
  parseJSON (Object v) = do
    ig_id <- v .: "id"
    ig_name <- v .: "name"
    ig_form_field <- v .: "form_field"
    ig_groups <- v .: "groups"
    return $ InterestGrouping ig_id ig_name ig_form_field ig_groups
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''InterestGrouping)

-- | Lists the Interest Groupings for a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-groupings.php>
interestGroupings :: ListId
                  -- ^ List
                  -> Maybe Bool
                  -- ^ Calculate counts for the groupings, which is slower
                  -> MailchimpT m [InterestGrouping]
interestGroupings listId counts =
  query "lists" "interest-groupings" [ "id" .= listId
                                     , "counts" .= counts
                                     ]

data ListStats = ListStats
  { lsMemberCount :: Double
  , lsUnsubscribeCount :: Double
  , lsCleanedCount :: Double
  , lsMemberCountSinceSend :: Double
  , lsUnsubscribeCountSinceSend :: Double
  , lsCleanedCountSinceSend :: Double
  , lsCampaignCount :: Double
  , lsGroupingCount :: Double
  , lsGroupCount :: Double
  , lsMergeVarCount :: Double
  , lsAvgSubRate :: Double
  , lsAvgUnsubRate :: Double
  , lsTargetSubRate :: Double
  , lsOpenRate :: Double
  , lsClickRate :: Double
  }
  deriving (Show, Eq)

instance FromJSON ListStats where
  parseJSON (Object v) = do
    ls_member_count <- v .: "member_count"
    ls_unsubscribe_count <- v .: "unsubscribe_count"
    ls_cleaned_count <- v .: "cleaned_count"
    ls_member_count_since_send <- v .: "member_count_since_send"
    ls_unsubscribe_count_since_send <- v .: "unsubscribe_count_since_send"
    ls_cleaned_count_since_send <- v .: "cleaned_count_since_send"
    ls_campaign_count <- v .: "campaign_count"
    ls_grouping_count <- v .: "grouping_count"
    ls_group_count <- v .: "group_count"
    ls_merge_var_count <- v .: "merge_var_count"
    ls_avg_sub_rate <- v .: "avg_sub_rate"
    ls_avg_unsub_rate <- v .: "avg_unsub_rate"
    ls_target_sub_rate <- v .: "target_sub_rate"
    ls_open_rate <- v .: "open_rate"
    ls_click_rate <- v .: "click_rate"
    return $ ListStats ls_member_count ls_unsubscribe_count ls_cleaned_count ls_member_count_since_send ls_unsubscribe_count_since_send ls_cleaned_count_since_send ls_campaign_count ls_grouping_count ls_group_count ls_merge_var_count ls_avg_sub_rate ls_avg_unsub_rate ls_target_sub_rate ls_open_rate ls_click_rate
  parseJSON _ = mzero


-- $(deriveFromJSON (convertName 2) ''ListStats)

$(deriveToJSON (convertName 2) ''ListStats)

data ListInfo = ListInfo
  { liId :: ListId
  , liWebId :: Int
  , liName :: Text
  , liDateCreated :: UTCTime
  , liEmailTypeOption :: Bool
  , liUseAwesomebar :: Bool
  , liDefaultFromName :: Text
  , liDefaultFromEmail :: Text
  , liDefaultSubject :: Text
  , liDefaultLanguage :: Text
  , liListRating :: Double
  , liSubscribeUrlShort :: Text
  , liBeamerAddress :: Text
  , liVisibility :: Text
  , liStats :: ListStats
  , liModules :: [Text]
  }
  deriving (Show, Eq)
  
instance FromJSON ListInfo where
  parseJSON (Object v) = do
    li_id <- v .: "id"
    li_web_id <- v .: "web_id"
    li_name <- v .: "name"
    li_date_created <- fmap unMCTime $ v .: "date_created"
    li_email_type_option <- v .: "email_type_option"
    li_use_awesomebar <- v .: "use_awesomebar"
    li_default_from_name <- v .: "default_from_name"
    li_default_from_email <- v .: "default_from_email"
    li_default_subject <- v .: "default_subject"
    li_default_language <- v .: "default_language"
    li_list_rating <- v .: "list_rating"
    li_subscribe_url_short <- v .: "subscribe_url_short"
    li_beamer_address <- v .: "beamer_address"
    li_visibility <- v .: "visibility"
    li_stats <- v .: "stats"
    li_modules <- v .: "modules"
    return $ ListInfo li_id li_web_id li_name li_date_created li_email_type_option li_use_awesomebar li_default_from_name li_default_from_email li_default_subject li_default_language li_list_rating li_subscribe_url_short li_beamer_address li_visibility li_stats li_modules
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''ListInfo)

data ListResultError = ListResultError 
  { lreParam :: Text
  , lreCode :: Int
  , lreError :: Text
  }
  deriving (Show, Eq)

instance FromJSON ListResultError where
  parseJSON (Object v) = do
    lre_param <- v .: "param"
    lre_code <- v .: "code"
    lre_error <- v .: "error"
    return $ ListResultError lre_param lre_code lre_error
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''ListResultError)


data ListsResult = ListsResult
  { lrTotal :: Int
  , lrData :: [ListInfo]
  , lrErrors :: [ListResultError]
  }
  deriving (Show, Eq)

instance FromJSON ListsResult where
  parseJSON (Object v) = do
    lr_total <- v .: "total"
    lr_data <- v .: "data"
    lr_errors <- v .: "errors"
    return $ ListsResult lr_total lr_data lr_errors
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''ListsResult)

data ListFilters = ListFilters
  { lfListId :: Maybe ListId
  , lfListName :: Maybe Text
  , lfFromName :: Maybe Text
  , lfFromEmail :: Maybe Text
  , lfFromSubject :: Maybe Text
  , lfCreatedBefore :: Maybe UTCTime
  , lfCreatedAfter :: Maybe UTCTime
  , lfExact :: Maybe Bool
  }
  deriving (Show, Eq)

instance FromJSON ListFilters where
  parseJSON (Object v) = do
    lf_list_id <- v .: "list_id"
    lf_list_name <- v .: "list_name"
    lf_from_name <- v .: "from_name"
    lf_from_email <- v .: "from_email"
    lf_from_subject <- v .: "from_subject"
    lf_created_before <- (fmap . fmap) unMCTime $ v .: "created_before"
    lf_created_after <- (fmap . fmap) unMCTime $ v .: "created_after"
    lf_exact <- v .: "exact"
    return $ ListFilters lf_list_id lf_list_name lf_from_name lf_from_email lf_from_subject lf_created_before lf_created_after lf_exact
  parseJSON _ = mzero

$(deriveToJSON (convertName 2) ''ListFilters)

instance Default ListFilters where
  def = ListFilters Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data SortField = SortFieldCreated
               | SortFieldWeb

instance ToJSON SortField where
  toJSON SortFieldCreated = String "created"
  toJSON SortFieldWeb = String "web"

data SortDir = SortDirDesc
             | SortDirAsc
  deriving (Show, Eq)

instance ToJSON SortDir where
  toJSON SortDirDesc = String "DESC"
  toJSON SortDirAsc = String "ASC"

-- | Get information for all of the lists in the account.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/list.php>
--
--   Usage:
--
-- > listInfo (Just listFilter) (Just startPage) (Just limitCount) (Just sortField) (Just sortDir)
listInfo :: Maybe ListFilters
         -- ^ Filters for lists to return
         -> Maybe Int
         -- ^ Start page, from 0
         -> Maybe Int
         -- ^ Limit number of lists to return
         -> Maybe SortField
         -- ^ Field to sort on
         -> Maybe SortDir
         -- ^ Sort direction
         -> MailchimpT m ListsResult
listInfo filters start limit sortField sortDir =
  query "lists" "list" [ "filters" .= filters
                       , "start" .= start
                       , "limit" .= limit
                       , "sort_field" .= sortField
                       , "sort_dir" .= sortDir
                       ]

data ListLocationsResult = ListLocationsResult
  { llrCountry :: Text
  , llrCc :: Text
  , llrPercent :: Double
  , llrTotal :: Double
  }
  deriving (Show, Eq)

instance FromJSON ListLocationsResult where
  parseJSON (Object v) = do
    llr_country <- v .: "country"
    llr_cc <- v .: "cc"
    llr_percent <- v .: "percent"
    llr_total <- v .: "total"
    return $ ListLocationsResult llr_country llr_cc llr_percent llr_total
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''ListLocationsResult)

-- | Get the locations for the subscribers of a list
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/locations.php>
listLocations :: ListId -> MailchimpT m [ListLocationsResult]
listLocations listId =
  query "lists" "locations" [ "id" .= listId ]

data ActivityAction = ActivityAction
  { aaAction :: Text
  , aaTimestamp :: UTCTime
  , aaUrl :: Text
  , aaType :: Text
  , aaCampaignId :: CampaignId
  , aaCampaignData :: Maybe CampaignInfo
  }
  deriving (Show, Eq)

instance FromJSON ActivityAction where
  parseJSON (Object v) = do
    aa_action <- v .: "action"
    aa_timestamp <- fmap unMCTime $ v .: "timestamp"
    aa_url <- v .: "url"
    aa_type <- v .: "type"
    aa_campaign_id <- v .: "campaign_id"
    aa_campaign_data <- v .:? "campaign_data"
    return $ ActivityAction aa_action aa_timestamp aa_url aa_type aa_campaign_id aa_campaign_data
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''ActivityAction)

data ActivityRecord = ActivityRecord
  { arecEmail :: EmailId
  , arecActivity :: [ActivityAction]
  }
  deriving (Show, Eq)

instance FromJSON ActivityRecord where
  parseJSON (Object v) = do
    arec_email <- v .: "email"
    arec_activity <- v .: "activity"
    return $ ActivityRecord arec_activity arec_email
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 4) ''ActivityRecord)

data MemberActivityResult = MemberActivityResult
  { marSuccessCount :: Int
  , marErrorCount :: Int
  , marErrors :: [UserResultError]
  , marData :: [ActivityRecord]
  }
  deriving (Show, Eq)

instance FromJSON MemberActivityResult where
  parseJSON (Object v) = do
    mar_success_count <- v .: "success_count"
    mar_error_count <- v .: "error_count"
    mar_errors <- v .: "errors"
    mar_data <- v .: "data"
    return $ MemberActivityResult mar_success_count mar_error_count mar_errors mar_data
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''MemberActivityResult)

-- | Get the activity of a list of members for a list
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/member-activity.php>
memberActivity :: ListId -> [EmailId] -> MailchimpT m MemberActivityResult
memberActivity listId emails = 
  query "lists" "member-activity" [ "id" .= listId, "emails" .= emails ]

data MemberList = MemberList
  { mlId :: ListId
  , mlStatus :: Text
  }
  deriving (Show, Eq)

instance FromJSON MemberList where
  parseJSON (Object v) = do
    ml_id <- v .: "id"
    ml_status <- v .: "status"
    return $ MemberList ml_id ml_status
  parseJSON _ = mzero
  
-- $(deriveFromJSON (convertName 2) ''MemberList)

data MemberGeo = MemberGeo
  { mgLatitude :: Text
  , mgLongitude :: Text
  , mgGmtoff :: Text
  , mgDstoff :: Text
  , mgTimezone :: Text
  , mgCc :: Text
  , mgRegion :: Text
  }
  deriving (Show, Eq)

instance FromJSON MemberGeo where
  parseJSON (Object v) = do
    mg_latitude <- v .: "latitude"
    mg_longitude <- v .: "longitude"
    mg_gmtoff <- v .: "gmtoff"
    mg_dstoff <- v .: "dstoff"
    mg_timezone <- v .: "timezone"
    mg_cc <- v .: "cc"
    mg_region <- v .: "region"
    return $ MemberGeo mg_latitude mg_longitude mg_gmtoff mg_dstoff mg_timezone mg_cc mg_region
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''MemberGeo)

data MemberClient = MemberClient
  { mcName :: Text
  , mcIconUrl :: Text
  }
  deriving (Show, Eq)

instance FromJSON MemberClient where
  parseJSON (Object v) = do
    mc_name <- v .: "name"
    mc_icon_url <- v .: "icon_url"
    return $ MemberClient mc_name mc_icon_url
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''MemberClient)

data MemberStaticSegment = MemberStaticSegment
  { mssId :: Int
  , mssName :: Text
  , mssAdded :: Text
  }
  deriving (Show, Eq)

instance FromJSON MemberStaticSegment where
  parseJSON (Object v) = do
    mss_id <- v .: "id"
    mss_name <- v .: "name"
    mss_added <- v .: "added"
    return $ MemberStaticSegment mss_id mss_name mss_added
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''MemberStaticSegment)

data MemberNote = MemberNote
  { mnId :: Int
  , mnNote :: Text
  , mnCreated :: UTCTime
  , mnUpdated :: UTCTime
  , mnCreatedByName :: Text
  }
  deriving (Show, Eq)
  
instance FromJSON MemberNote where
  parseJSON (Object v) = do
    mn_id <- v .: "id"
    mn_note <- v .: "note"
    mn_created <- fmap unMCTime $ v .: "created"
    mn_updated <- fmap unMCTime $ v .: "updated"
    mn_created_by_name <- v .: "created_by_name"
    return $ MemberNote mn_id mn_note mn_created mn_updated mn_created_by_name
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''MemberNote)


data MemberInfoFields = MemberInfoFields
  { miEmailType :: EmailType
  , miMerges :: Value -- Keys are merge var names, except GROUPINGS
  , miStatus :: Text
  , miIpSignup :: Text
  , miTimestampSignup :: Text
  , miIpOpt :: Text
  , miTimestampOpt :: UTCTime
  , miMemberRating :: Int
  , miCampaignId :: CampaignId
  , miLists :: [MemberList]
  , miTimestamp :: UTCTime
  , miInfoChanged :: UTCTime
  , miWebId :: Int
  , miListId :: ListId
  , miListName :: Text
  , miLanguage :: Text
  , miIsGmonkey :: Bool
  , miGeo :: MemberGeo
  , miClients :: MemberClient
  , miStaticSegments :: [MemberStaticSegment]
  , miNotes :: [MemberNote]
  }
  deriving (Show, Eq)

instance FromJSON MemberInfoFields where
  parseJSON (Object v) = do
    mi_email_type <- v .: "email_type"
    mi_merges <- v .: "merges"
    mi_status <- v .: "status"
    mi_ip_signup <- v .: "ip_signup"
    mi_timestamp_signup <- v .: "timestamp_signup"
    mi_ip_opt <- v .: "ip_opt"
    mi_timestamp_opt <- fmap unMCTime $ v .: "timestamp_opt"
    mi_member_rating <- v .: "member_rating"
    mi_campaign_id <- v .: "campaign_id"
    mi_lists <- v .: "lists"
    mi_timestamp <- fmap unMCTime $ v .: "timestamp"
    mi_info_changed <- fmap unMCTime $ v .: "info_changed"
    mi_web_id <- v .: "web_id"
    mi_list_id <- v .: "list_id"
    mi_list_name <- v .: "list_name"
    mi_language <- v .: "language"
    mi_is_gmonkey <- v .: "is_gmonkey"
    mi_geo <- v .: "geo"
    mi_clients <- v .: "clients"
    mi_static_segments <- v .: "static_segments"
    mi_notes <- v .: "notes"
    return $ MemberInfoFields mi_email_type mi_merges mi_status mi_ip_signup mi_timestamp_signup mi_ip_opt mi_timestamp_opt mi_member_rating mi_campaign_id mi_lists mi_timestamp mi_info_changed mi_web_id mi_list_id mi_list_name mi_language mi_is_gmonkey mi_geo mi_clients mi_static_segments mi_notes
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''MemberInfoFields)

-- | MemberInfo holds the results from calling "members". It doesn't match the layout
--   of the Mailchimp API so that the EmailResult can be pulled out as one type.
data MemberInfo = MemberInfo
  { miEmailResult :: EmailResult
  , miFields :: MemberInfoFields
  }
  deriving (Show, Eq)

instance FromJSON MemberInfo where
  parseJSON (Object v) = do
    emailResult <- parseJSON (Object v)
    memberFields <- parseJSON (Object v)
    return MemberInfo { miEmailResult = emailResult, miFields = memberFields }
  parseJSON _ = mzero

data MemberInfoError = MemberInfoError
  { mieEmail :: EmailId
  , mieError :: Text
  }
  deriving (Show, Eq)

instance FromJSON MemberInfoError where
  parseJSON (Object v) = do
    mie_email <- v .: "email"
    mie_error <- v .: "error"
    return $ MemberInfoError mie_email mie_error
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''MemberInfoError)

data MemberInfoResult = MemberInfoResult
  { mirSuccessCount :: Int
  , mirErrorCount :: Int
  , mirErrors :: [MemberInfoError]
  , mirData :: [MemberInfo]
  }
  deriving (Show, Eq)

instance FromJSON MemberInfoResult where
  parseJSON (Object v) = do
    mir_success_count <- v .: "success_count"
    mir_error_count <- v .: "error_count"
    mir_errors <- v .: "errors"
    mir_data <- v .: "data"
    return $ MemberInfoResult mir_success_count mir_error_count mir_errors mir_data
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''MemberInfoResult)

-- | Get information for members of a list
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/member-info.php>
memberInfo :: ListId -> [EmailId] -> MailchimpT m MemberInfoResult
memberInfo listId emails =
  query "lists" "member-info" [ "id" .= listId, "emails" .= emails ]


data MemberStatus = SubscribedStatus
                  | UnsubscribedStatus
                  | CleanedStatus

instance ToJSON MemberStatus where
  toJSON SubscribedStatus = String "subscribed"
  toJSON UnsubscribedStatus = String "unsubscribed"
  toJSON CleanedStatus = String "cleaned"


data MembersResult = MembersResult
  { mrTotal :: Int
  , mrData :: [MemberInfo]
  }
  deriving (Show, Eq)

instance FromJSON MembersResult where
  parseJSON (Object v) = do
    mr_total <- v .: "total"
    mr_data <- v .: "data"
    return $ MembersResult mr_total mr_data
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''MembersResult)


-- | Get all the members of a list matching a query.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/members.php>
members :: ListId
        -- ^ List
        -> Maybe MemberStatus
        -- ^ Member status filter
        -> Maybe Int
        -- ^ Start page, from 0
        -> Maybe Int
        -- ^ Limit of records returned
        -> Maybe Text
        -- ^ Field to sort on
        -> Maybe SortDir
        -- ^ Ascending or descending
        -> Maybe Value
        -- ^ Segment to return
        -> MailchimpT m MembersResult
members listId status start limit sortField sortDir segment = 
  query "lists" "members" [ "id" .= listId
                          , "status" .= status
                          , "opts" .= filterObject [ "start" .= start
                                                   , "limit" .= limit
                                                   , "sort_field" .= sortField
                                                   , "sort_dir" .= sortDir
                                                   , "segment" .= segment
                                                   ]
                          ]
data MergeVarType = EmailMergeVarType
                  | TextMergeVarType
                  | NumberMergeVarType
                  | RadioMergeVarType
                  | DropdownMergeVarType
                  | DateMergeVarType
                  | AddressMergeVarType
                  | PhoneMergeVarType
                  | UrlMergeVarType
                  | ImageurlMergeVarType
                  | ZipMergeVarType
                  | BirthdayMergeVarType
  deriving (Show, Eq)

instance ToJSON MergeVarType where
  toJSON EmailMergeVarType = String "email"  
  toJSON TextMergeVarType = String "text"
  toJSON NumberMergeVarType = String "number"
  toJSON RadioMergeVarType = String "radio"
  toJSON DropdownMergeVarType = String "dropdown"
  toJSON DateMergeVarType = String "date"
  toJSON AddressMergeVarType = String "address"
  toJSON PhoneMergeVarType = String "phone"
  toJSON UrlMergeVarType = String "url"
  toJSON ImageurlMergeVarType = String "imageurl"
  toJSON ZipMergeVarType = String "zip"
  toJSON BirthdayMergeVarType = String "birthday"

instance FromJSON MergeVarType where
  parseJSON (String "email") = return EmailMergeVarType
  parseJSON (String "text") = return TextMergeVarType
  parseJSON (String "number") = return NumberMergeVarType
  parseJSON (String "radio") = return RadioMergeVarType
  parseJSON (String "dropdown") = return DropdownMergeVarType
  parseJSON (String "date") = return DateMergeVarType
  parseJSON (String "address") = return AddressMergeVarType
  parseJSON (String "phone") = return PhoneMergeVarType
  parseJSON (String "url") = return UrlMergeVarType
  parseJSON (String "imageurl") = return ImageurlMergeVarType
  parseJSON (String "zip") = return ZipMergeVarType
  parseJSON (String "birthday") = return BirthdayMergeVarType
  parseJSON _ = mzero



-- | Options used to create merge vars. Default instance is defined on this so you can easily
--   create new ones. E.g.
--
-- > def {mvoFieldType = Just TextMergeVarType, mvoHelptext = Just "Help text" }
data MergeVarOptions = MergeVarOptions
  { mvoFieldType :: Maybe MergeVarType
  , mvoReq :: Maybe Bool
  , mvoPublic :: Maybe Bool
  , mvoShow :: Maybe Bool
  , mvoOrder :: Maybe Int
  , mvoDefaultValue :: Maybe Text
  , mvoHelptext :: Maybe Text
  , mvoChoices :: Maybe [Text]
  , mvoDateformat :: Maybe Text
  , mvoPhoneformat :: Maybe Text
  , mvoDefaultCountry :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON MergeVarOptions where
  parseJSON (Object v) = do
    mvo_field_type <- v .:? "field_type"
    mvo_req <- v .:? "req"
    mvo_public <- v .:? "public"
    mvo_show <- v .:? "show"
    mvo_order <- v .:? "order"
    mvo_default_value <- v .:? "default_value"
    mvo_helptext <- v .:? "helptext"
    mvo_choices <- v .:? "choices"
    mvo_dateformat <- v .:? "dateformat"
    mvo_phoneformat <- v .:? "phoneformat"
    mvo_default_country <- v .:? "default_country"
    return $ MergeVarOptions mvo_field_type mvo_req mvo_public mvo_show mvo_order mvo_default_value mvo_helptext mvo_choices mvo_dateformat mvo_phoneformat mvo_default_country
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''MergeVarOptions)

instance Default MergeVarOptions where
  def = MergeVarOptions { mvoFieldType = Nothing
                        , mvoReq = Nothing
                        , mvoPublic = Nothing
                        , mvoShow = Nothing
                        , mvoOrder = Nothing
                        , mvoDefaultValue = Nothing
                        , mvoHelptext = Nothing
                        , mvoChoices = Nothing
                        , mvoDateformat = Nothing
                        , mvoPhoneformat = Nothing
                        , mvoDefaultCountry = Nothing
                        }
$(deriveToJSON (convertName 3) ''MergeVarOptions)

data MergeVarResult = MergeVarResult
  { mvrName :: Text
  , mvrReq :: Bool
  , mvrFieldType :: MergeVarType
  , mvrPublic :: Bool
  , mvrShow :: Bool
  , mvrOrder :: Text
  , mvrDefault :: Maybe Text
  , mvrHelptext :: Maybe Text
  , mvrSize :: Text
  , mvrTag :: Text
  , mvrChoices :: Maybe [Text]
  , mvrId :: Int
  }
  deriving (Show, Eq)

instance FromJSON MergeVarResult where
  parseJSON (Object v) = do
    mvr_name <- v .: "name"
    mvr_req <- v .: "req"
    mvr_field_type <- v .: "field_type"
    mvr_public <- v .: "public"
    mvr_show <- v .: "show"
    mvr_order <- v .: "order"
    mvr_default <- v .: "default"
    mvr_helptext <- v .: "helptext"
    mvr_size <- v .: "size"
    mvr_tag <- v .: "tag"
    mvr_choices <- v .:? "choices"
    mvr_id <- v .: "id"
    return $ MergeVarResult mvr_name mvr_req mvr_field_type mvr_public mvr_show mvr_order mvr_default mvr_helptext mvr_size mvr_tag mvr_choices mvr_id
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''MergeVarResult)

$(deriveToJSON (convertName 3) ''MergeVarResult)

-- | Add a merge tag to a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-add.php>
mergeVarAdd :: ListId
            -- ^ List
            -> Text
            -- ^ Tag for merge var
            -> Text
            -- ^ Name of merge var
            -> Maybe MergeVarOptions
            -- ^ Options for merge var
            -> MailchimpT m MergeVarResult
mergeVarAdd listId tag name options =
  query "lists" "merge-var-add" [ "id" .= listId
                                , "tag" .= tag
                                , "name" .= name
                                , "options" .= options
                                ]

-- | Delete a merge tag from all members
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-del.php>
mergeVarDelete :: ListId
               -- ^ List
               -> Text
               -- ^ Merge var tag
               -> MailchimpT m Bool
mergeVarDelete listId tag =
  fmap crComplete $ query "lists" "merge-var-del" [ "id" .= listId
                                                  , "tag" .= tag
                                                  ]

-- | Reset all data stored in a merge var.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-reset.php>
mergeVarReset :: ListId
              -- ^ List
              -> Text
              -- ^ Merge var tag
              -> MailchimpT m Bool
mergeVarReset listId tag = 
  fmap crComplete $ query "lists" "merge-var-reset" [ "id" .= listId
                                                    , "tag" .= tag
                                                    ]
                                                  
-- | Set a merge var for all users. Only works for merge var 1-30
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-set.php>
mergeVarSet :: ListId
            -- ^ List
            -> Text
            -- ^ Merge var tag
            -> Text
            -- ^ New merge var value
            -> MailchimpT m Bool
mergeVarSet listId tag value =
  fmap crComplete $ query "lists" "merge-var-set" [ "id" .= listId
                                                  , "tag" .= tag
                                                  , "value" .= value
                                                  ]

-- | Update the options of a merge var.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-update.php>
mergeVarUpdate :: ListId
               -- ^ List
               -> Text
               -- ^ Merge var tag
               -> MergeVarOptions
               -- ^ Merge var options
               -> MailchimpT m MergeVarResult
mergeVarUpdate listId tag options =
  query "lists" "merge-var-update" [ "id" .= listId
                                   , "tag" .=  tag
                                   , "options" .= options
                                   ]

data MergeVarInfo = MergeVarInfo
  { mviId :: ListId
  , mviName :: Text
  , mviMergeVars :: [MergeVarResult]
  }
  deriving (Show, Eq)

instance FromJSON MergeVarInfo where
  parseJSON (Object v) = do
    mvi_id <- v .: "id"
    mvi_name <- v .: "name"
    mvi_merge_vars <- v .: "merge_vars"
    return $ MergeVarInfo mvi_id mvi_name mvi_merge_vars
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''MergeVarInfo)

data MergeVarError = MergeVarError
  { mveId :: ListId
  , mveCode :: Int
  , mveMsg :: Text
  }
  deriving (Show, Eq)

instance FromJSON MergeVarError where
  parseJSON (Object v) = do
    mve_id <- v .: "id"
    mve_code <- v .: "code"
    mve_msg <- v .: "msg"
    return $ MergeVarError mve_id mve_code mve_msg
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''MergeVarError)

data MergeVarInfoResult = MergeVarInfoResult
  { mvirSuccessCount :: Int
  , mvirErrorCount :: Int
  , mvirData :: [MergeVarInfo]
  , mvirErrors :: [MergeVarError]
  }
  deriving (Show, Eq)

instance FromJSON MergeVarInfoResult where
  parseJSON (Object v) = do
    mvir_success_count <- v .: "success_count"
    mvir_error_count <- v .: "error_count"
    mvir_data <- v .: "data"
    mvir_errors <- v .: "errors"
    return $ MergeVarInfoResult mvir_success_count mvir_error_count mvir_data mvir_errors
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 4) ''MergeVarInfoResult)

-- | Retrieve list of marge vars for a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-vars.php>
mergeVarInfo :: [ListId] -> MailchimpT m MergeVarInfoResult
mergeVarInfo listId =
  query "lists" "merge-vars" [ "id" .= listId ]

type SegmentId = Int

data StaticSegmentAddResult = StaticSegmentAddResult
  { ssarId :: SegmentId
  }
  deriving (Show, Eq)

instance FromJSON StaticSegmentAddResult where
  parseJSON (Object v) = do
    ssar_id <- v .: "id"
    return $ StaticSegmentAddResult ssar_id
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 4) ''StaticSegmentAddResult)

-- | Add a static segment for later use.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segment-add.php>
staticSegmentAdd :: ListId
                 -- ^ List
                 -> Text
                 -- ^ New static segment name
                 -> MailchimpT m SegmentId
staticSegmentAdd listId name =
  fmap ssarId $ query "lists" "static-segment-add" [ "id" .= listId
                                                   , "name" .= name
                                                   ]

-- | Delete a static segment
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segment-del.php>
staticSegmentDelete :: ListId -> SegmentId -> MailchimpT m Bool
staticSegmentDelete listId segmentId =
  fmap crComplete $ query "lists" "static-segment-del" [ "id" .= listId
                                                       , "seg_id" .= segmentId
                                                       ]

data StaticSegmentMembersAddResult = StaticSegmentMembersAddResult
  { ssmarSuccessCount :: Int
  , ssmarErrorCount :: Int
  , ssmarErrors :: [UserResultError]
  }
  deriving (Show, Eq)

instance FromJSON StaticSegmentMembersAddResult where
  parseJSON (Object v) = do
    ssmar_success_count <- v .: "success_count"
    ssmar_error_count <- v .: "error_count"
    ssmar_errors <- v .: "errors"
    return $ StaticSegmentMembersAddResult ssmar_success_count ssmar_error_count ssmar_errors
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 5) ''StaticSegmentMembersAddResult)


-- | Add list members to a static segment
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segment-members-add.php>
staticSegmentMembersAdd :: ListId -> SegmentId -> [EmailId] -> MailchimpT m StaticSegmentMembersAddResult
staticSegmentMembersAdd listId segmentId emails =
  query "lists" "static-segment-members-add" [ "id" .= listId
                                             , "seg_id" .= segmentId
                                             , "batch" .= emails
                                             ]

-- | The Mailchimp API docs show a slight difference between the return JSON of adds and deletes.
data StaticSegmentMembersDelResult = StaticSegmentMembersDelResult
  { ssmdrSuccessCount :: Int
  , ssmdrErrorCount :: Int
  , ssmdrErrors :: [UserResultError]
  }
  deriving (Show, Eq)

instance FromJSON StaticSegmentMembersDelResult where
  parseJSON (Object v) = do
    ssmdr_success_count <- v .: "success_count"
    ssmdr_error_count <- v .: "error_count"
    ssmdr_errors <- v .: "errors"
    return $ StaticSegmentMembersDelResult ssmdr_success_count ssmdr_error_count ssmdr_errors
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 5) ''StaticSegmentMembersDelResult)


staticSegmentMembersDelete :: ListId -> SegmentId -> [EmailId] -> MailchimpT m StaticSegmentMembersDelResult
staticSegmentMembersDelete listId segmentId emails =
  query "lists" "static-segment-members-del" [ "id" .= listId
                                             , "seg_id" .= segmentId
                                             , "batch" .= emails
                                             ]

-- | Reset a static segment and remove all members.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segment-reset.php>
staticSegmentReset :: ListId -> SegmentId -> MailchimpT m Bool
staticSegmentReset listId segmentId = 
  fmap crComplete $ query "lists" "static-segment-reset" [ "id" .= listId
                                                         , "seg_id" .= segmentId
                                                         ]

data StaticSegmentResult = StaticSegmentResult
  { ssrId :: SegmentId
  , ssrName :: Text
  , ssrMemberCount :: Int
  , ssrCreatedDate :: Maybe UTCTime
  , ssrLastUpdate :: Maybe UTCTime
  , ssrLastReset :: Maybe UTCTime
  }
  deriving (Show, Eq)

instance FromJSON StaticSegmentResult where
  parseJSON (Object v) = do
    ssr_id <- v .: "id"
    ssr_name <- v .: "name"
    ssr_member_count <- v .: "member_count"
    ssr_created_date <- (fmap . fmap) unMCTime $ v .: "created_date"
    ssr_last_update <- (fmap . fmap) unMCTime $ v .: "last_update"
    ssr_last_reset <- (fmap . fmap) unMCTime $  v .: "last_reset"
    return $ StaticSegmentResult ssr_id ssr_name ssr_member_count ssr_created_date ssr_last_update ssr_last_reset
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''StaticSegmentResult)

-- | Get all of the static segments
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segments.php>
staticSegments :: ListId -> MailchimpT m [StaticSegmentResult]
staticSegments listId =
  query "lists" "static-segments" [ "id" .= listId ]

-- | Unsubscribe a user from a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/unsubscribe.php>
unsubscribe :: ListId
            -- ^ List
            -> EmailId
            -- ^ Email to unsubscribe
            -> Maybe Bool
            -- ^ Delete member after unsubscribing
            -> Maybe Bool
            -- ^ Send goodbye email
            -> Maybe Bool
            -- ^ Send notification email to administrator
            -> MailchimpT m Bool
unsubscribe listId emailId deleteMember sendGoodbye sendNotify =
  fmap crComplete $ query "lists" "unsubscribe" [ "id" .= listId
                                                , "email" .= emailId
                                                , "delete_member" .= deleteMember
                                                , "send_goodbye" .= sendGoodbye
                                                , "send_notify" .= sendNotify
                                                ]

-- | Edit the information for a member
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/update-member.php>
updateMember :: ListId
             -- ^ List
             -> EmailId
             -- ^ Email of member
             -> [MergeVarsItem]
             -- ^ Merge vars
             -> Maybe EmailType
             -- ^ Email type to send
             -> Maybe Bool
             -- ^ Replace interests
             -> MailchimpT m EmailResult
updateMember listId emailId mergeVars emailType replaceInterests = 
  query "lists" "update-member" [ "id" .= listId
                                , "email" .= emailId
                                , "merge_vars" .= mergeVars
                                , "email_type" .= emailType
                                , "replace_interests" .= replaceInterests
                                ]


data WebhookActions = WebhookActions
  { waSubscribe :: Maybe Bool
  , waUnsubscribe :: Maybe Bool
  , waProfile :: Maybe Bool
  , waCleaned :: Maybe Bool
  , waUpemail :: Maybe Bool
  , waCampaign :: Maybe Bool
  }
  deriving (Show, Eq)

instance FromJSON WebhookActions where
  parseJSON (Object v) = do
    wa_subscribe <- v .:? "subscribe"
    wa_unsubscribe <- v .:? "unsubscribe"
    wa_profile <- v .:? "profile"
    wa_cleaned <- v .:? "cleaned"
    wa_upemail <- v .:? "upemail"
    wa_campaign <- v .:? "campaign"
    return $ WebhookActions wa_subscribe wa_unsubscribe wa_profile wa_cleaned wa_upemail wa_campaign
  parseJSON _ = mzero


$(deriveToJSON (convertName 2) ''WebhookActions)

-- $(deriveFromJSON (convertName 2) ''WebhookActions)

instance Default WebhookActions where
  def = WebhookActions Nothing Nothing Nothing Nothing Nothing Nothing

data WebhookSources = WebhookSources
  { wsUser :: Maybe Bool
  , wsAdmin :: Maybe Bool
  , wsApi :: Maybe Bool
  }
  deriving (Show, Eq)

instance FromJSON WebhookSources where
  parseJSON (Object v) = do
    ws_user <- v .:? "user"
    ws_admin <- v .:? "admin"
    ws_api <- v .:? "api"
    return $ WebhookSources ws_user ws_admin ws_api
  parseJSON _ = mzero

$(deriveToJSON (convertName 2) ''WebhookSources)

-- $(deriveFromJSON (convertName 2) ''WebhookSources)

type WebhookId = Text

data WebhookAddResult = WebhookAddResult
  { warId :: WebhookId
  }
  deriving (Show, Eq)

instance FromJSON WebhookAddResult where
  parseJSON (Object v) = do
    war_id <- v .: "id"
    return $ WebhookAddResult war_id
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 3) ''WebhookAddResult)

-- | Add a webhook URL to a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/webhook-add.php>
webhookAdd :: ListId
           -- ^ List
           -> Text
           -- ^ URL of new webhook
           -> Maybe WebhookActions
           -- ^ Webhook actions
           -> Maybe WebhookSources
           -- ^ Webhook sources
           -> MailchimpT m WebhookAddResult
webhookAdd listId url actions sources =
  query "lists" "webhook-add" [ "id" .= listId
                              , "url" .= url
                              , "actions" .= actions
                              , "sources" .= sources
                              ]

-- | Delete a webhook URL from a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/webhook-del.php>
webhookDelete :: ListId
              -- ^ List
              -> Text
              -- ^ URL to delete
              -> MailchimpT m Bool
webhookDelete listId url =
  fmap crComplete $ query "lists" "webhook-del" [ "id" .= listId
                              , "url" .= url
                              ]

data WebhookResult = WebhookResult
  { wrUrl :: Text
  , wrActions :: WebhookActions
  , wrSources :: WebhookSources
  }
  deriving (Show, Eq)

instance FromJSON WebhookResult where
  parseJSON (Object v) = do
    wr_url <- v .: "url"
    wr_actions <- v .: "actions"
    wr_sources <- v .: "sources"
    return $ WebhookResult wr_url wr_actions wr_sources
  parseJSON _ = mzero

-- $(deriveFromJSON (convertName 2) ''WebhookResult)

-- | List all the webhooks for a list.
--
--   See <http://apidocs.mailchimp.com/api/2.0/lists/webhooks.php>
webhooks :: ListId -> MailchimpT m [WebhookResult]
webhooks listId =
  query "lists" "webhooks" [ "id" .= listId ]
