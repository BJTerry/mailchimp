{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Text (pack)
import System.Environment (lookupEnv)
import Data.Aeson (Value(..), Value(..), decode)
import Data.HashMap.Strict (lookup)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)

import Web.Mailchimp
import Web.Mailchimp.Lists
import Web.Mailchimp.Campaigns
import Web.Mailchimp.Util

main = do
  apiKey <- fmap ((=<<) mailchimpKey) $ (fmap (fmap pack)) $ lookupEnv "MAILCHIMPKEY" :: IO (Maybe MailchimpApiKey)
  listId <- (fmap . fmap $ ListId . pack) (lookupEnv "MAILCHIMPTESTLIST")
  testAddress <- (fmap . fmap $ pack) (lookupEnv "MAILCHIMPTESTEMAIL")
  case (apiKey, listId, testAddress) of
    (Just key, Just lid, Just testA) -> do
      config <- defaultMailchimpConfig key 
      defaultMain $ tests config lid testA
    _ -> fail "Could not find valid API Key, test list ID or test email address. Please check that MAILCHIMPKEY, MAILCHIMPTESTLIST, MAILCHIMPTESTEMAIL environment variables are set."

tests cfg lid testAddress = 
  [ testGroup "Client" 
    [ testCase "Ping Mailchimp" $ ping_mailchimp cfg
    , testCase "Parse API Key" parse_key
    , testCase "Parse Mailchimp Error" parse_mailchimp_error
    , testCase "Convert JSON names" convert_name
    ]
  , testGroup "Lists"
    [ testCase "Abuse Reports" $ lists_abuse_reports cfg lid
    , testCase "List Activity" $ lists_activity cfg lid
    , testCase "Batch Subscribe Invalid" $ lists_batch_subscribe_invalid cfg lid
    , testCase "Batch Subscribe / Unsubscribe" $ lists_batch_subscribe cfg lid testAddress
    , testCase "Clients" $ lists_clients cfg lid
    , testCase "Growth History" $ lists_growth_history cfg lid
    , testCase "Interest Groups" $ lists_interest_group_add cfg lid
    , testCase "Interest Groupings" $ lists_interest_grouping cfg lid
    , testCase "List Info" $ lists_list_info cfg lid
    , testCase "List Locations" $ lists_list_locations cfg lid
    , testCase "Member Activity" $ lists_member_activity cfg lid testAddress
    , testCase "Member Info" $ lists_member_info cfg lid testAddress
    , testCase "Members" $ lists_members cfg lid testAddress
    , testCase "Merge Vars" $ lists_merge_vars cfg lid
    , testCase "Static Segments" $ lists_static_segments cfg lid testAddress
    , testCase "Subscribe / Unsubscribe" $ lists_subscribe cfg lid testAddress
    , testCase "Webhooks" $ lists_webhooks cfg lid
    ]
  ]

--
-- Web.Mailchimp.Client
--

ping_mailchimp cfg = do
  Object o <- runMailchimp cfg $ query "helper" "ping" []
  Data.HashMap.Strict.lookup "msg" o @?= (Just $ String "Everything's Chimpy!")

parse_key = 
  mailchimpKey "00000000000000000000000000000000-us1" @?= 
    (Just $ MailchimpApiKey "00000000000000000000000000000000-us1" "us1")

parse_mailchimp_error =
  decode "{\"status\":\"error\",\"code\":104,\"name\":\"Invalid_ApiKey\",\"error\":\"Invalid Mailchimp API Key: 00000000000000000000000000000000-us1 . You are accessing the wrong datacenter - your client library may not properly support our datacenter mapping scheme.\"}" @?=
    (Just $ InvalidApiKey 104 "Invalid_ApiKey" "Invalid Mailchimp API Key: 00000000000000000000000000000000-us1 . You are accessing the wrong datacenter - your client library may not properly support our datacenter mapping scheme.")

convert_name =
  convertName 3 "123456789" @?= "456789"


--
-- Web.Mailchimp.Lists
--

lists_abuse_reports cfg lid = do
  runMailchimp cfg $ do
    _ <- abuseReports lid (Just 0) (Just 100) (parseTime defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 1970")
    return ()

lists_activity cfg lid = do
  runMailchimp cfg $ do
    !_ <- listActivity lid
    return ()

lists_batch_subscribe_invalid cfg lid = do
  runMailchimp cfg $ do
    x <- batchSubscribe lid [(Email "Test@example.com", EmailTypeHTML, [])] (Just False) Nothing Nothing
    liftIO $ bsrErrorCount x @?= 1 -- Fails because of example.com domain

lists_batch_subscribe cfg lid testAddress = do
  runMailchimp cfg $ do
    x <- batchSubscribe lid [(Email testAddress, EmailTypeHTML, [])] (Just False) Nothing Nothing
    liftIO $ bsrAddCount x @?= 1 
    y <- batchUnsubscribe lid [Email testAddress] (Just True) (Just False) (Just False)
    liftIO $ burSuccessCount y @?= 1

lists_clients cfg lid = do
  runMailchimp cfg $ do
    !_ <- clients lid
    return ()


lists_growth_history cfg lid = do
  runMailchimp cfg $ do
    !_ <- growthHistory Nothing
    return ()

lists_interest_group_add cfg lid = do
  runMailchimp cfg $ do
    x <- interestGroupAdd lid "InterestGroup" Nothing
    liftIO $ x @?= True
    y <- interestGroupUpdate lid "InterestGroup" "InterestGroup2" Nothing
    liftIO $ y @?= True
    z <- interestGroupDelete lid "InterestGroup2" Nothing
    liftIO $ z @?= True

lists_interest_grouping cfg lid = do
  runMailchimp cfg $ do
    x <- interestGroupingAdd lid "InterestGrouping" CheckboxesGrouping ["Group1", "Group2"]
    y <- interestGroupingUpdate (x) "name" "InterestGrouping2"
    liftIO $ y @?= True
    z <- interestGroupings lid (Just True)
    liftIO $ Prelude.length z @?= 2 -- A default grouping is created when a user is added
    !_ <- interestGroupingDelete (x)
    w <- interestGroupings lid (Just True)
    liftIO $ Prelude.length w @?= 1

lists_list_info cfg lid = do
  runMailchimp cfg $ do
    x <- listInfo (Just $ def {lfListId = Just lid}) Nothing Nothing Nothing Nothing
    liftIO $ lrTotal x @?= 1

lists_list_locations cfg lid = do
  runMailchimp cfg $ do
    !_ <- listLocations lid
    return ()

lists_member_activity cfg lid testAddress = do
  runMailchimp cfg $ do
    !_ <- memberActivity lid [Email testAddress]
    return ()


lists_member_info cfg lid testAddress = do
  runMailchimp cfg $ do
    !_ <- memberInfo lid [Email testAddress]
    return ()

lists_members cfg lid testAddress = do
  runMailchimp cfg $ do
    !_ <- members lid (Just UnsubscribedStatus) (Just 0) (Just 100) (Just "email") Nothing Nothing
    return ()

lists_merge_vars cfg lid = do
  runMailchimp cfg $ do
    x <- mergeVarAdd lid "TEST" "Test Tag" (Just $ def {mvoShow = Just True}) 
    y <- mergeVarReset lid "TEST"
    liftIO $ y @?= True
    z <- mergeVarSet lid "TEST" "Something"
    liftIO $ z @?= True
    w <- mergeVarUpdate lid "TEST" (def {mvoPublic = Just True})
    liftIO $ mvrName w @?= "Test Tag"    
    q <- mergeVarInfo [lid]
    liftIO $ mvirSuccessCount q @?= 1
    v <- mergeVarDelete lid "TEST"
    liftIO $ v @?= True

lists_static_segments cfg lid testAddress = do
  runMailchimp cfg $ do
    x <- staticSegmentAdd lid "TestSegment"
    y <- staticSegmentMembersAdd lid x [Email testAddress]
    liftIO $ ssmarSuccessCount y @?= 0
    z <- staticSegmentReset lid x
    w <- staticSegmentMembersDelete lid x [Email testAddress]
    v <- staticSegments lid
    liftIO $ Prelude.length v @?= 1
    q <- staticSegmentDelete lid x
    liftIO $ q @?= True

lists_subscribe cfg lid testAddress = do
  runMailchimp cfg $ do
    x <- subscribeUser lid (Email testAddress) Nothing (Just EmailTypeHTML) (Just False) Nothing Nothing (Just False)
    liftIO $ erEmail x @?= (Email testAddress)
    y <- updateMember lid (Email testAddress) [] (Just EmailTypeText) Nothing
    liftIO $ erEmail y @?= (Email testAddress)
    z <- unsubscribe lid (Email testAddress) (Just True) (Just False) (Just False)
    liftIO $ z @?= True

lists_webhooks cfg lid = do
  runMailchimp cfg $ do
    x <- webhookAdd lid "http://www.example.com" (Just $ def {waSubscribe = Just False}) Nothing
    y <- webhooks lid
    liftIO $ Prelude.length y @?= 1
    z <- webhookDelete lid "http://www.example.com"
    liftIO $ z @?= True
