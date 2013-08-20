module Web.Mailchimp.Lists
  where

import Web.Mailchimp.Client
import Web.Mailchimp.Campaigns (CampaignId, CampaignInfo(..) )

import Data.Text (Text, pack, unpack)
import Data.Aeson (Value(..), object, (.=), ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (Pair)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Control.Monad (mzero)
import Data.Time.Clock (UTCTime)
import Control.Applicative ((<*>), (<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe, isJust)
import Text.Read (readMaybe)


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
    case (catMaybes [email, euid, leid]) of
      (x:_) -> return x
      _ -> mzero
  parseJSON _ = mzero

-- | The result of calling subscribeUser. Provides three ways of identifying the user for subsequent calls.
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
              -> MailchimpT m EmailResult
subscribeUser listId 
              emailId 
              mergeVars 
              emailType 
              doubleOptin 
              updateExisting 
              replaceInterests 
              sendWelcome = do
  query "lists" "subscribe" request 
 where
  request = [ "id" .= unListId listId
            , "email" .= emailId
            , "merge_vars" .= fmap object mergeVars
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
 
$(deriveFromJSON (convertName 2) ''AbuseReport)


-- | Aggregate abuse results returnd by abuseReports
data AbuseResults = AbuseResults 
  { arTotal :: Int
  , arData :: [AbuseReport]
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 2) ''AbuseResults)

-- | Queries Mailchimp for the abuse reports for a given mailing list
--   See <http://apidocs.mailchimp.com/api/2.0/lists/abuse-reports.php> for details.
--
--   Example Usage: 
--
-- >>> abuseReports listId (Just startingPage [indexed at 0]) (Just limitResults) (Just sinceDate)
abuseReports :: ListId -> Maybe Int -> Maybe Int -> Maybe UTCTime -> MailchimpT m AbuseResults
abuseReports listId start limit since = 
  query "lists" "abuse-reports" request
 where
  request = [ "id" .= listId
            , "start" .= start
            , "limit" .= limit
            , "since" .= (MCTime `fmap` since)
            ]

-- | Provides unspecified data relating to list activity
--   See <http://apidocs.mailchimp.com/api/2.0/lists/activity.php>
listActivity :: ListId -> MailchimpT m [Value]
listActivity listId = 
  query "lists" "activity" [ "id" .= listId ]

-- | An error result from a call to batchSubscribe
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
--   See <http://apidocs.mailchimp.com/api/2.0/lists/batch-subscribe.php>
--
--   Usage:
--
-- >>> batchSubscribe listId [(Email "example@example.com", EmailTypeHTML, [("FNAME", "John")]] (Just doubleOptin) (Just updateExisting) (Just replaceInterests)
batchSubscribe :: ListId -> [(EmailId, EmailType, [MergeVarsItem])] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> MailchimpT m BatchSubscribeResult
batchSubscribe listId batchItems doubleOptin updateExisting replaceInterests = do
  query "lists" "batch-subscribe" request
 where
  request = [ "id" .= listId
            , "batch" .= buildBatch batchItems
            , "double_optin" .= doubleOptin
            , "update_existing" .= updateExisting
            , "replace_interests" .= replaceInterests
            ]
  buildBatch :: [(EmailId, EmailType, [MergeVarsItem])] -> [Value]
  buildBatch items = map buildBatchItem items
  buildBatchItem (email, emailType, mergeVars) = 
    filterObject [ "email" .= email
                 , "email_type" .= emailType
                 , "merge_vars" .= object mergeVars
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
--   See <http://apidocs.mailchimp.com/api/2.0/lists/batch-unsubscribe.php>
--
--   Usage:
--
-- >>> batchUnsubscribe listId [Email "example@example.com"] (Just deleteMember) (Just sendGoodbye) (Just sendNotify)
batchUnsubscribe :: ListId -> [EmailId] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> MailchimpT m BatchUnsubscribeResult
batchUnsubscribe  listId batchItems deleteMember sendGoodbye sendNotify = 
  query "lists" "batch-unsubscribe" $ 
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

$(deriveFromJSON (convertName 2) ''ClientItem)


data ClientResultsCategory = ClientResultsCategory
  { crPenetration :: Double
  , crClients :: [ClientItem]
  }

$(deriveFromJSON (convertName 2) ''ClientResultsCategory)


data ClientResults = ClientResults
  { crDesktop :: Maybe ClientResultsCategory
  , crMobile :: Maybe ClientResultsCategory
  }

instance FromJSON ClientResults where
  parseJSON (Object v) = do
    cr_desktop <- v .:? "desktop"
    cr_mobile <- v .:? "mobile"
    return $ ClientResults cr_desktop cr_mobile
  parseJSON _ = mzero


-- $(deriveFromJSON (convertName 2) ''ClientResults)


-- | Gets the user agent info for members of the list.
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
    ghr_existing <- v .:? "existing"
    ghr_imports <- v .:? "imports"
    ghr_optins <- v .:? "optins"
    ghr_existing2 <- v .:? "existing"
    ghr_imports2 <- v .:? "imports"
    ghr_optins2 <- v .:? "optins" 

    return $ GrowthHistoryResult { ghrMonth = ghr_month 
                                 , ghrExisting = firstJust ghr_existing ghr_existing2
                                 , ghrImports = firstJust ghr_imports ghr_imports2
                                 , ghrOptins = firstJust ghr_optins ghr_optins2
                                 }
   where
    firstJust :: Maybe Int -> Maybe Text -> Maybe Int
    firstJust a b = if isJust a then a else unpack `fmap` b >>= readMaybe

-- $(deriveFromJSON (convertName 3) ''GrowthHistoryResult)

-- | Gets the growth history by month for an account or list.
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

$(deriveFromJSON (convertName 2) ''CompleteResult)

-- | Adds an interest group to a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-group-add.php>
interestGroupAdd :: ListId -> Text -> Maybe GroupingId -> MailchimpT m Bool
interestGroupAdd listId name groupId =
  fmap crComplete $ query "lists" "interest-group-add" [ "id" .= listId
                                                       , "group_name" .= name
                                                       , "grouping_id" .= groupId
                                                       ]
-- | Removes an interest group from a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-group-del.php>
interestGroupDelete :: ListId -> Text -> Maybe GroupingId -> MailchimpT m Bool
interestGroupDelete listId name groupId =
  fmap crComplete $ query "lists" "interest-group-del" [ "id" .= listId
                                                       , "group_name" .= name
                                                       , "grouping_id" .= groupId
                                                       ]

-- | Changes the name of an interest group
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-group-update.php>
interestGroupUpdate :: ListId -> Text -> Text -> Maybe GroupingId -> MailchimpT m Bool
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

$(deriveFromJSON (convertName 4) ''InterestGroupingAddResult)

-- | Creates a new Interest Grouping
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-grouping-add.php>
interestGroupingAdd :: ListId -> Text -> GroupingType -> [Text] -> MailchimpT m GroupingId
interestGroupingAdd listId name groupingType groups = do
  fmap igarId $ query "lists" "interest-grouping-add" [ "id" .= listId
                                                      , "name" .= name
                                                      , "type" .= groupingType
                                                      , "groups" .= groups
                                                      ]
-- | Deletes an Interest Grouping
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-grouping-del.php>
interestGroupingDelete :: GroupingId -> MailchimpT m Bool
interestGroupingDelete groupId = 
  fmap crComplete $ query "lists" "interest-grouping-del" ["grouping_id" .= groupId ]

-- | Updates an Interest Grouping. 
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-grouping-update.php>
--
--   Usage
-- >>> interestGroupingUpdate groupId field value
--
--   Where "field" is either "name" or "type" and value is the new value for that field.
interestGroupingUpdate :: GroupingId -> Text -> Text -> MailchimpT m Bool
interestGroupingUpdate groupId name value = do
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

$(deriveFromJSON (convertName 3) ''InterestGroupDetail)

data InterestGrouping = InterestGrouping
  { igId :: GroupingId
  , igName :: Text
  , igFormField :: GroupingType
  , igGroups :: [InterestGroupDetail]
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 2) ''InterestGrouping)

-- | Lists the Interest Groupings for a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/interest-groupings.php>
interestGroupings :: ListId -> Maybe Bool -> MailchimpT m [InterestGrouping]
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

$(deriveFromJSON (convertName 2) ''ListStats)
$(deriveToJSON (convertName 2) ''ListStats)

data ListInfo = ListInfo
  { liId :: ListId
  , liWebId :: Int
  , liName :: Text
  , liDateCreated :: MCTime
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

$(deriveFromJSON (convertName 2) ''ListInfo)

data ListResultError = ListResultError 
  { lreParam :: Text
  , lreCode :: Int
  , lreError :: Text
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 2) ''ListResultError)


data ListsResult = ListsResult
  { lrTotal :: Int
  , lrData :: [ListInfo]
  , lrErrors :: [ListResultError]
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 2) ''ListsResult)

data ListFilters = ListFilters
  { lfListId :: Maybe ListId
  , lfListName :: Maybe Text
  , lfFromName :: Maybe Text
  , lfFromEmail :: Maybe Text
  , lfFromSubject :: Maybe Text
  , lfCreatedBefore :: Maybe MCTime
  , lfCreatedAfter :: Maybe MCTime
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
    lf_created_before <- v .: "created_before"
    lf_created_after <- v .: "created_after"
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
--   See <http://apidocs.mailchimp.com/api/2.0/lists/list.php>
--
--   Usage:
-- >>> listInfo (Just listFilter) (Just startPage) (Just limitCount) (Just sortField) (Just sortDir)
listInfo :: Maybe ListFilters -> Maybe Int -> Maybe Int -> Maybe SortField -> Maybe SortDir -> MailchimpT m ListsResult
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

$(deriveFromJSON (convertName 3) ''ListLocationsResult)

-- | Get the locations for the subscribers of a list
--   See <http://apidocs.mailchimp.com/api/2.0/lists/locations.php>
listLocations :: ListId -> MailchimpT m ListLocationsResult
listLocations listId =
  query "lists" "locations" [ "id" .= listId ]

data ActivityAction = ActivityAction
  { aaAction :: Text
  , aaTimestamp :: MCTime
  , aaUrl :: Text
  , aaType :: Text
  , aaCampaignId :: CampaignId
  , aaCampaignData :: Maybe CampaignInfo
  }
  deriving (Show, Eq)

instance FromJSON ActivityAction where
  parseJSON (Object v) = do
    aa_action <- v .: "action"
    aa_timestamp <- v .: "timestamp"
    aa_url <- v .: "url"
    aa_type <- v .: "type"
    aa_campaign_id <- v .: "campaign_id"
    aa_campaign_data <- v .:? "campaign_data"
    return $ ActivityAction aa_action aa_timestamp aa_url aa_type aa_campaign_id aa_campaign_data

-- $(deriveFromJSON (convertName 2) ''ActivityAction)

data ActivityRecord = ActivityRecord
  { arecEmail :: EmailId
  , arecActivity :: [ActivityAction]
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 4) ''ActivityRecord)

data MemberActivityResult = MemberActivityResult
  { marSuccessCount :: Int
  , marErrorCount :: Int
  , marErrors :: [UserResultError]
  , marData :: [ActivityRecord]
  }
  deriving (Show, Eq)


$(deriveFromJSON (convertName 3) ''MemberActivityResult)

-- | Get the activity of a list of members for a list
--   See <http://apidocs.mailchimp.com/api/2.0/lists/member-activity.php>
memberActivity :: ListId -> [EmailId] -> MailchimpT m MemberActivityResult
memberActivity listId emails = 
  query "lists" "member-activity" [ "id" .= listId, "emails" .= emails ]

data MemberList = MemberList
  { mlId :: ListId
  , mlStatus :: Text
  }
  deriving (Show, Eq)
  
$(deriveFromJSON (convertName 2) ''MemberList)

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

$(deriveFromJSON (convertName 2) ''MemberGeo)

data MemberClient = MemberClient
  { mcName :: Text
  , mcIconUrl :: Text
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 2) ''MemberClient)

data MemberStaticSegment = MemberStaticSegment
  { mssId :: Int
  , mssName :: Text
  , mssAdded :: Text
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 2) ''MemberStaticSegment)

data MemberNote = MemberNote
  { mnId :: Int
  , mnNote :: Text
  , mnCreated :: MCTime
  , mnUpdated :: MCTime
  , mnCreatedByName :: Text
  }
  deriving (Show, Eq)
  
$(deriveFromJSON (convertName 2) ''MemberNote)


data MemberInfoFields = MemberInfoFields
  { miEmailType :: EmailType
  , miMerges :: Value -- Keys are merge var names, except GROUPINGS
  , miStatus :: Text
  , miIpSignup :: Text
  , miTimestampSignup :: Text
  , miIpOpt :: Text
  , miTimestampOpt :: MCTime
  , miMemberRating :: Int
  , miCampaignId :: CampaignId
  , miLists :: [MemberList]
  , miTimestamp :: MCTime
  , miInfoChanged :: MCTime
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

$(deriveFromJSON (convertName 2) ''MemberInfoFields)

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
    return $ MemberInfo { miEmailResult = emailResult, miFields = memberFields }
  parseJSON _ = mzero

data MemberInfoError = MemberInfoError
  { mieEmail :: EmailId
  , mieError :: Text
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 3) ''MemberInfoError)

data MemberInfoResult = MemberInfoResult
  { mirSuccessCount :: Int
  , mirErrorCount :: Int
  , mirErrors :: [MemberInfoError]
  , mirData :: MemberInfo
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 3) ''MemberInfoResult)

-- | Get information for members of a list
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

$(deriveFromJSON (convertName 2) ''MembersResult)


-- | Get all the members of a list matching a query.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/members.php>
members :: ListId -> Maybe MemberStatus -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe SortDir -> Maybe Value -> MailchimpT m MembersResult
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
data MergeVarType = TextMergeVarType
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



-- | Options used to create merge vars. Default instance is defined on this so you can easily
--   create new ones. E.g.
--
-- >>> def {mvoFieldType = Just TextMergeVarType, mvoHelptext = Just "Help text" }
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
  , mvrDefault :: Text
  , mvrHelptext :: Text
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
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-add.php>
mergeVarAdd :: ListId -> Text -> Text -> Maybe MergeVarOptions -> MailchimpT m MergeVarResult
mergeVarAdd listId tag name options =
  query "lists" "merge-var-add" [ "id" .= listId
                                , "tag" .= tag
                                , "name" .= name
                                , "options" .= options
                                ]

-- | Delete a merge tag from all members
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-del.php>
mergeVarDelete :: ListId -> Text -> MailchimpT m Bool
mergeVarDelete listId tag =
  fmap crComplete $ query "lists" "merge-var-del" [ "id" .= listId
                                                  , "tag" .= tag
                                                  ]

-- | Reset all data stored in a merge var.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-reset.php>
mergeVarReset :: ListId -> Text -> MailchimpT m Bool
mergeVarReset listId tag = 
  fmap crComplete $ query "lists" "merge-var-reset" [ "id" .= listId
                                                    , "tag" .= tag
                                                    ]
                                                  
-- | Set a merge var for a specific user. Only works for merge var 1-30
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-set.php>
mergeVarSet :: ListId -> Text -> Text -> MailchimpT m Bool
mergeVarSet listId tag value =
  fmap crComplete $ query "lists" "merge-var-set" [ "id" .= listId
                                                  , "tag" .= tag
                                                  , "value" .= value
                                                  ]

-- | Update the options of a merge var.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-var-update.php>
mergeVarUpdate :: ListId -> Text -> MergeVarOptions -> MailchimpT m MergeVarResult
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

$(deriveFromJSON (convertName 3) ''MergeVarInfo)

data MergeVarError = MergeVarError
  { mveId :: ListId
  , mveCode :: Int
  , mveMsg :: Text
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 3) ''MergeVarError)

data ListMergeVarsResult = ListMergeVarsResult
  { lmvrSuccessCount :: Int
  , lmvrErrorCount :: Int
  , lmvrData :: [MergeVarInfo]
  , lmvrErrors :: [MergeVarError]
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 4) ''ListMergeVarsResult)

-- | Retrieve list of marge vars for a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/merge-vars.php>
listMergeVars :: ListId -> MailchimpT m ListMergeVarsResult
listMergeVars listId =
  query "lists" "merge-vars" [ "id" .= listId ]

type SegmentId = Int

data StaticSegmentAddResult = StaticSegmentAddResult
  { ssarId :: SegmentId
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 4) ''StaticSegmentAddResult)

-- | Add a static segment for later use.
--   See <ListMergeVarsResult>
staticSegmentAdd :: ListId -> Text -> MailchimpT m SegmentId
staticSegmentAdd listId name =
  fmap ssarId $ query "lists" "static-segment-add" [ "id" .= listId
                                                   , "name" .= name
                                                   ]

-- | Delete a static segment
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segment-del.php>
staticSegmentDelete :: ListId -> SegmentId -> MailchimpT m Bool
staticSegmentDelete listId segmentId =
  fmap crComplete $ query "lists" "static-segment-del" [ "id" .= listId
                                                       , "seg_id" .= segmentId
                                                       ]

data BatchEmailItem = BatchEmailItem
  { beiEmail :: EmailId }
  deriving (Show, Eq)

$(deriveToJSON (convertName 3) ''BatchEmailItem)

data StaticSegmentMembersAddResult = StaticSegmentMembersAddResult
  { ssmarSuccessCount :: Int
  , ssmarErrors :: UserResultError
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 5) ''StaticSegmentMembersAddResult)


-- | Add list members to a static segment
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segment-members-add.php>
staticSegmentMembersAdd :: ListId -> SegmentId -> [EmailId] -> MailchimpT m StaticSegmentMembersAddResult
staticSegmentMembersAdd listId segmentId emails = do
  let batch = map BatchEmailItem emails
  query "lists" "static-segment-members-add" [ "id" .= listId
                                             , "seg_id" .= segmentId
                                             , "batch" .= batch
                                             ]

-- | The Mailchimp API docs show a slight difference between the return JSON of adds and deletes.
data StaticSegmentMembersDelResult = StaticSegmentMembersDelResult
  { ssmdrSuccessCount :: Int
  , ssmdrErrorCount :: Int
  , ssmdrErrors :: [UserResultError]
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 5) ''StaticSegmentMembersDelResult)


staticSegmentMembersDelete :: ListId -> SegmentId -> [EmailId] -> MailchimpT m StaticSegmentMembersDelResult
staticSegmentMembersDelete listId segmentId emails = do
  let batch = map BatchEmailItem emails
  query "lists" "static-segment-members-del" [ "id" .= listId
                                             , "seg_id" .= segmentId
                                             , "batch" .= batch
                                             ]

-- | Reset a static segment and remove all members.
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
  , ssrCreatedDate :: MCTime
  , ssrLastUpdate :: MCTime
  , ssrLastReset :: MCTime
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 3) ''StaticSegmentResult)

-- | Get all of the static segments
--   See <http://apidocs.mailchimp.com/api/2.0/lists/static-segments.php>
staticSegments :: ListId -> MailchimpT m [StaticSegmentResult]
staticSegments listId =
  query "lists" "static-segments" [ "id" .= listId ]

-- | Unsubscribe a user from a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/unsubscribe.php>
unsubscribe :: ListId -> EmailId -> Maybe Bool -> Maybe Bool -> Maybe Bool -> MailchimpT m Bool
unsubscribe listId emailId deleteMember sendGoodbye sendNotify =
  fmap crComplete $ query "lists" "unsubscribe" [ "id" .= listId
                                                , "email" .= emailId
                                                , "delete_member" .= deleteMember
                                                , "send_goodbye" .= sendGoodbye
                                                , "send_notify" .= sendNotify
                                                ]

-- | Edit the information for a member
--   See <http://apidocs.mailchimp.com/api/2.0/lists/update-member.php>
updateMember :: ListId -> EmailId -> [MergeVarsItem] -> Maybe EmailType -> Maybe Bool -> MailchimpT m EmailResult
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

type WebhookId = Int

data WebhookAddResult = WebhookAddResult
  { warId :: WebhookId
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 3) ''WebhookAddResult)

-- | Add a webhook URL to a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/webhook-add.php>
webhookAdd :: ListId -> Text -> Maybe WebhookActions -> Maybe WebhookSources -> MailchimpT m WebhookAddResult
webhookAdd listId url actions sources =
  query "lists" "webhook-add" [ "id" .= listId
                              , "url" .= url
                              , "actions" .= actions
                              , "sources" .= sources
                              ]

-- | Delete a webhook URL from a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/webhook-del.php>
webhookDelete :: ListId -> Text -> MailchimpT m Bool
webhookDelete listId url =
  query "lists" "webhook-del" [ "id" .= listId
                              , "url" .= url
                              ]

data WebhookResult = WebhookResult
  { wrUrl :: Text
  , wrActions :: WebhookActions
  , wrSources :: WebhookSources
  }
  deriving (Show, Eq)

$(deriveFromJSON (convertName 2) ''WebhookResult)

-- | List all the webhooks for a list.
--   See <http://apidocs.mailchimp.com/api/2.0/lists/webhooks.php>
webhooks :: ListId -> MailchimpT m [WebhookResult]
webhooks listId =
  query "lists" "webhooks" [ "id" .= listId ]
