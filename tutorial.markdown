The world is full of RESTful JSON APIs that are of interest to developers. While you can often use them in an ad hoc manner if you only need a few features for your Haskell application, sometimes you will be using so much of the API that a complete Haskell interface becomes useful. This tutorial will help you build packages to interface with RESTful JSON APIs.  For the sake of the tutorial will implement an interface for the Mailchimp API, which allows developers to manage e-mail marketing mailing lists. By the end of the tutorial you should be well-prepared to create your own packages. This tutorial assumes an intermediate understanding of Haskell.

# Create Your Package

Create a folder for your package. Initialize with cabal init.

    mkdir mailchimp
    cabal init
    # Select default package name, package version, choose your license, etc.

Your package will be created with a basic .cabal file. I recommend adding declarations for the OverloadedStrings GHC extension right out of the gate, as any useful library will make heavy use of the Data.Text datatype. OverloadedStrings allows you to use literal strings in your Haskell code as Data.Text (and a few other types) rather than as String types, which are very inefficient.

# Representing API Keys

The first step to supporting the Mailchimp API is parsing the the API key to build request URLs. In Mailchimp's API, unlike many others, the API key includes the datacenter within it, which must be extracted for the URL. We use the following type to represent an API key, which we will put in the Web.Mailchimp module (the complete source for the mailchimp package is on github):

    -- | Represents a mailchimp API key, which implicitly includes a datacenter.
    data MailchimpApiKey = MailchimpApiKey
      { makApiKey :: Text -- Full API key including datacenter
      , makDatacenter :: Text -- 3-letter datacenter code
      }

While we could require users of the package to pass properly constructed MailchimpApiKeys to the library, we will probably want to make it easy to parse the API keys as provided by Mailchimp, in case someone wants to load keys dynamically. Haskell Platform includes a parser (Parsec) that makes this simple. One could also use regular expressions, which are likewise provided by the Haskell Platform. A full discussion of how to use either of these tools are outside of the scope of this tutorial, but you can find examples of each in [Real World Haskell](http://book.realworldhaskell.org/).

    -- | Create a MailchimpApiKey from Text
    mailchimpKey :: Text -> Maybe MailchimpApiKey
    mailchimpKey apiKey = 
      case parse parseKey "(unknown)" apiKey of
        Left _ -> Nothing
        Right (_, dcString) ->
          Just MailchimpApiKey 
            { makApiKey = apiKey 
            , makDatacenter = pack dcString
            }

    parseKey :: GenParser st (String, String)
    parseKey = do
      key <- many1 hexDigit
      _ <- char '-'
      dc <- many1 alphaNum
      return (key, dc)

#Building the Endpoint URL and Other Types

With any normal JSON API you will need a function that builds the endpoint URL for the method you are calling. URLs in Mailchimp's JSON API have the form https://**dc**.api.mailchimp.com/2.0/**section**/**method**.json:

    -- | Builds the mailchimp endpoint URL
    apiEndpointUrl :: Text -> Text -> Text -> Text
    apiEndpointUrl datacenter section method = 
      Data.Text.concat ["https://", datacenter, ".api.mailchimp.com/2.0/", section, "/", method, ".json"]

We will start by supporting the the "lists/subscribe" method of the API, which subscribes a user to a Mailchimp mailing list. It may be helpful to reference the Mailchimp [documentation](http://apidocs.mailchimp.com/api/2.0/lists/subscribe.php) for this method. (Although during the creation of this tutorial I found several errors in the documentation. I imagine you will find the same with APIs you are accessing!) We create the Web.Mailchimp.Lists module which will hold the following types:

    -- | Represents an individual mailing list
    newtype ListId = ListId {unListId :: Text}
      deriving (Show, Eq)
    
    -- | Represents one of the canonical ways of identifying subscribers
    data EmailId = Email Text
                 | EmailUniqueId Text
                 | ListEmailId Text
      deriving (Show, Eq)

    -- | Represents an individual merge variable    
    type MergeVarsItem = Pair -- From the Aeson library. Represents the individual fields in a JSON object

    -- | The type of e-mail your user will receive
    data EmailType = EmailTypeHTML
                   | EmailTypeText
   
E-mail addresses in the Mailchimp API can always be sent in one of three forms. As an e-mail address, as an ID associated with the e-mail address, or as the ID of a particular subscriber within a list. MergeVarsItem represents data associated with the user, such as FNAME (for first name) or LNAME (for last name) or one of a number of special purpose variables. These are used to insert user data into e-mails that you are sending out. We use the Pair type from the Aeson JSON library to represent them, which is just a tuple of (Text, Value), and Value is a JSON object, string, etc., because, while not frequently used, Mailchimp has some special-purpose merge variables that can be sent as JSON objects.

# subscribeUser, Version 0

We can now write a simple function to perform the lists/subscribe request. It's helpful to write a specific case before deciding how to move up to a higher level of abstraction. In many cases, such as if you only need to support a single JSON method, you can pretty much stop after writing one function.

    -- | The result of calling subscribeUser. Provides three ways of identifying the user for subsequent calls.
    data EmailResult = EmailResult { erEmail :: EmailId
                                   , erEmailUniqueId :: EmailId
                                   , erListEmailId :: EmailId
                                   }
      deriving (Show, Eq)

    subscribeUser :: MailchimpApiKey 
                  -> ListId 
                  -> EmailId 
                  -> Maybe [MergeVarsItem] 
                  -> Maybe EmailType
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> Maybe Bool 
                  -> IO EmailResult
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
      let req = initReq { requestBody = RequestBodyLBS $ encode requestJson 
                        , method = methodPost
                        }
      man <- liftIO $ newManager def 
      response <- httpLbs req man 
      let meResult = decode $ responseBody response
      case meResult of
        Just emailResult -> return emailResult
        Nothing -> liftIO $ mzero

Now, this version has a couple of problems (and doesn't compile), but it's a good first try. EmailResult is just a record to wrap up the returned value from the "lists/subscribe" method call. Mailchimp always returns subscriber info with all three representations. A typical response might look like:

    {"email":"example@example.com","euid":"abc123","leid":"abc123"} 

To actually perform the request, we use the [http-conduit package](http://hackage.haskell.org/package/http-conduit), by Michael Snoyman of Yesod fame. That package requires everything be run in a [ResourceT](http://hackage.haskell.org/packages/archive/resourcet/latest/doc/html/Control-Monad-Trans-Resource.html#t:ResourceT) monad transformer (which manages creating and freeing resources, such as network connections), so the function starts with runResourceT. Within this context (ResourceT IO), any actions in the IO monad must be called with [liftIO](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-IO-Class.html#t:MonadIO). 

First, we parse the endpoint URL to create the intial request (initReq). Then we set the request options to include our request body and HTTP method. Creating the request body uses the [Aeson](http://hackage.haskell.org/package/aeson) library (maintained by Bryan O'Sullivan) to create and encode JSON using the object, encode and decode functions. HttpLbs (from http-conduit) is the function that actually performs the request, and we are going to expect back a result of Maybe EmailResult. If we couldn't parse the response we will fail with an error (mzero).

# Creating JSON Instances

The first problem with the code is that we currently don't know how to convert EmailId and EmailType to JSON, and we don't know how to convert EmailResult from JSON. We will use the Aeson library to create instances that tell Haskell how to do these conversions. The instances look like this:

    instance ToJSON EmailType where
      toJSON EmailTypeHTML = "html"
      toJSON EmailTypeText = "text"

    instance ToJSON EmailId where
      toJSON (Email t) = object ["email" .= t]
      toJSON (EmailUniqueId t) = object ["euid" .= t]
      toJSON (ListEmailId t) = object ["leid" .= t]

    instance FromJSON EmailResult where
      parseJSON (Object v) = do
        email <- v .: "email"
        euid <- v .: "euid"
        leid <- v .: "leid"
        return $ EmailResult (Email email) (EmailUniqueId euid) (ListEmailId leid)
      parseJSON _ = mzero 

The ToJSON instances simply take one of their respective values and create the appropriate JSON objects. FromJSON is a little more complex. The parseJSON function of FromJSON runs in the Parser monad. Within Parser, the .: operator accesses object values by key, allowing us to construct the final value. The parseJSON instance function can parse any type of JSON value (not only objects, but integers, arrays, etc.) but anything other than an object is not an EmailReturn, so we call mzero. If one were reading a field that had a Maybe value (i.e. an optional field), one would instead use the (.:?) operator from Aeson.

While Aeson includes template haskell functions to create ToJSON and FromJSON instances from record definitions for you, beware that the created instances for FromJSON in the current version on Hackage (0.6.1) have a couple major failings when it comes to RESTful JSON APIs. First, it will fail to parse objects that have *extra* keys that do not appear in your records. Second, it requires that optional "Maybe" fields *actually be present* in the parsed JSON with "null" value, otherwise the parse fails. The function to derive ToJSON instances also adds "null" entries for Nothing values, which would be fine in most APIs but actually causes problems in Mailchimp. A future version is intended to address some of these problems, but is not on Hackage yet. 

More generally, APIs are usually being served in dynamic programming languages, and not from Aeson. There will be lots of edge cases that aren't handled in the way you expect, along with errors. For example, Mailchimp in some instances serializes numbers as JSON strings rather than integers, and it sometimes returns empty lists instead of null values. The only way to know is with extensive unit testing of the API (you can see some example unit tests of Mailchimp in the repository, though they are far from complete). 

# Better Error Handling

The next issue with subscribeUser is that it doesn't currently handle errors gracefully. When the Mailchimp API signals an error, it always returns an HTTP status other than 200. Using httpLbs, this will immediately cause an HttpException to be thrown. We would rather throw Exceptions specific to the Mailchimp API so that users of our library can handle them as appropriate for their application.

Let's create a type to represent API exceptions in Web.Mailchimp.Client. For now we will just include the method-specific error for this particular method in addition to the API-wide errors.

    data MailchimpError = InvalidApiKey Int Text Text
                        | UserDisabled Int Text Text
                        | UserInvalidRole Int Text Text
                        | TooManyConnections Int Text Text
                        | UserUnderMaintenance Int Text Text
                        | UserInvalidAction Int Text Text
                        | ValidationError Int Text Text
                        | ListDoesNotExist Int Text Text
                        | ListAlreadySubscribed Int Text Text -- Undocumented Error
                        | OtherMailchimpError Int Text Text
      deriving (Typeable, Show, Eq)

    instance Exception MailchimpError -- Requires Typeable

    instance FromJSON MailchimpError where
      parseJSON (Object v) = do
        status <- v .: "status"
        when (status /= ("error" :: Text)) mzero
        name <- v .: "name"
        code <- v .: "code"
        message <- v .: "error" -- Documentation shows this is under the "message" key, but it is incorrect
        return $ (errConstructor name) code name message
       where
          errConstructor name = case (name :: Text) of
            "Invalid_ApiKey" -> InvalidApiKey 
            "User_Disabled" -> UserDisabled 
            "User_InvalidRole" -> UserInvalidRole 
            "Too_Many_Connections" -> TooManyConnections 
            "User_UnderMaintenance" -> UserUnderMaintenance 
            "User_InvalidAction" -> UserInvalidAction 
            "ValidationError" -> ValidationError 
            "List_DoesNotExist" -> ListDoesNotExist 
            "List_AlreadySubscribed" -> ListAlreadySubscribed
            _ -> OtherMailchimpError 
      parseJSON _ = mzero

Exception is defined in [Control.Exception.Base](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception-Base.html) and will allow us to throw and catch MailchimpErrors. Next, let's revise subscribeUser to include our error handling code:

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
      let req = initReq { requestBody = RequestBodyLBS $ encode requestJson 
                        , method = methodPost
                        }
      man <- liftIO $ newManager def 
      response <- catch (httpLbs req man) 
        (\e -> 
          case e :: HttpException of
            StatusCodeException _ headers _ -> do
              let (mResponse :: Maybe MailchimpError) = fromStrict `fmap` (lookup "X-Response-Body-Start" headers) >>= decode
              maybe (throwIO e) id (throwIO `fmap` mResponse)
            _ -> throwIO e)
      let mResult = decode $ responseBody response
      case mResult of
        Just emailResult -> return emailResult
        Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"

Because we are in the ResourceT IO monad, we use throwIO and catch from [lifted-base](http://hackage.haskell.org/packages/archive/lifted-base/latest/doc/html/Control-Exception-Lifted.html). They work the same as their analogs in base, but are generalized to a wider variety of monadic contexts. When httpLbs gets a non-200 result, it throws a StatusCodeException, which we catch. Mailchimp's API sends the response body of errors in the X-Response-Body-Start header of the result, so we attempt to decode that and if possible, we throw the appropriate MailchimpError, otherwise we throw the original HttpException. We will also throw an OtherMailchimpError if we couldn't parse the result JSON.

# Trying it Out

We now have a working function, so lets try it in ghci. Unfortunately, Mailchimp doesn't have a test mode for their API, so I used my API key and a test list:

    ~ ghci -fglasgow-exts
    > :set -XOverloadedStrings
    > :l Web.Mailchimp.Lists Web.Mailchimp.Client
    > :m Web.Mailchimp.Lists Web.Mailchimp.Client
    > let key = MailchimpApiKey "<your key here>" "<dc code>"
    > subscribeUser key (ListId "<list id>") (Email "example@example.com") Nothing (Just EmailTypeHTML) Nothing Nothing Nothing Nothing
    EmailReturn {erEmail = Email "example@example.com", erEmailUniqueId = EmailUniqueId "<id>", erListEmailId = ListEmailId "<id>"}

The function worked. And if you run it again, you see that error handling also works:

    > subscribeUser key (ListId "<list id>") (Email "example@example.com") Nothing (Just EmailTypeHTML) Nothing Nothing Nothing Nothing
    *** Exception: ListAlreadySubscribed 214 "List_AlreadySubscribed" "example@example.com is already subscribed to list Users Newsletter. Click here to update your profile."

# Create query Function

Wunderbar! Of course, we don't want to write such a long function for each of Mailchimp's 104 API methods, so let's extract some common functionality from subscribeUser that we'll use over and over:

    query :: FromJSON x => MailchimpApiKey -> Text -> Text -> Value -> IO x
    query apiKey section method request = runResourceT $ do
      initReq <- parseUrl $ unpack $ apiEndpointUrl (makDatacenter apiKey) section method
      let req = initReq { requestBody = RequestBodyLBS $ encode request 
                        , method = methodPost
                        }
      man <- liftIO $ newManager def
      response <- catch (httpLbs req man) catchHttpException
      case decode $ responseBody response of
        Just result -> return result
        Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"
     where
      catchHttpException :: HttpException -> ResourceT (IO a)
      catchHttpException e@(StatusCodeException _ headers _) =
        maybe (throwIO e) id (throwIO `fmap` (decodeError headers))
      catchHttpException e = throwIO e
      decodeError :: ResponseHeaders -> Maybe MailchimpError
      decodeError headers = fromStrict `fmap` (lookup "X-Response-Body-Start" headers) >>= decode


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

# Using ReaderT

There are still a couple improvements that could be made to the interface. First, we are creating a new Manager each time query is called, which is very inefficient as it's an expensive operation. Second, if you are making multiple calls within the library, the current syntax could be slightly improved. Suppose that you want to delete all your folders created before a certain date, and you are in a non-IO monadic context, such as in a Yesod Handler. The code to perform that operation would look like this:

    liftIO $ do
      now <- getCurrentTime
      folders <- listFolders apiKey "campaign"
      mapM_ (\f -> deleteFolder apiKey (folderId f) (folderType f)) $ 
        filter (\folder -> dateCreated folder < (now - 60 * 60 * 24 * 7)) folders

For every action, we are passing in apiKey even though the key is not changing. One can imagine even more complex actions where it would be more of a burden. To solve this problem and the problem of creating potentially hundreds of managers, let's wrap up our actions in a monad, specfically in [ReaderT](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Reader.html#v:ReaderT). We'll also create a MailchimpConfig type to carry the apiKey and the shared Manager:

    data MailchimpConfig = MailchimpConfig 
      { mcApiKey :: MailchimpApiKey
      , mcManager :: Manager
      }

    -- | Creates a MailchimpConfig with a new Manager
    defaultMailchimpConfig :: MonadIO m => MailchimpApiKey -> m MailchimpConfig
    defaultMailchimpConfig apiKey = do
      man <- liftIO $ newManager def
      return MailchimpConfig { mcApiKey = apiKey
                             , mcManager = man
                             }

    type Mailchimp a = ReaderT MailchimpConfig (ResourceT IO) a

    runMailchimp :: (MonadIO m) => MailchimpConfig -> Mailchimp a -> m a
    runMailchimp config action = 
      liftIO $ runResourceT $ flip runReaderT config $ action

MailchimpConfig is just a simple type to carry around the configuration, and defaultMailchimpConfig is a way to create a default configuration with a new Manager. We will run all of our actions in the Mailchimp monad stack, which is IO, ResourceT (used by Network.HTTP.Conduit) and a ReaderT, which will allow us to query the MailchimpConfig without having to pass it around in every function.

With these changes query and subscribeUser now look like this:

    query :: (FromJSON x) => Text -> Text -> Value -> Mailchimp x
    query section method request = do
      config <- ask
      initReq <- liftIO $ parseUrl $ unpack $ apiEndpointUrl (makDatacenter $ mcApiKey config) section method
      let req = initReq { requestBody = RequestBodyLBS $ encode request 
                        , method = methodPost
                        }
      response <- catch (httpLbs req $ mcManager config) catchHttpException
      case decode $ responseBody response of
        Just result -> return result
        Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"
     where
      catchHttpException :: HttpException -> Mailchimp a
      catchHttpException e@(StatusCodeException _ headers _) = 
        maybe (throwIO e) id (throwIO `fmap` (decodeError headers))
      catchHttpException e = throwIO e
      decodeError :: ResponseHeaders -> Maybe MailchimpError
      decodeError headers = fromStrict `fmap` (lookup "X-Response-Body-Start" headers) >>= decode

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
      request apiKey = object [ "apikey" .= makApiKey apiKey
                              , "id" .= unListId listId
                              , "email" .= emailId
                              , "merge_vars" .= fmap object mergeVars
                              , "email_type" .= emailType
                              , "double_optin" .= doubleOptin
                              , "update_existing" .= updateExisting
                              , "replace_interests" .= replaceInterests
                              , "send_welcome" .= sendWelcome
                              ]

# Dealing with Mailchimp Idiosyncrasies

As is the case with many JSON APIs, Mailchimp has a couple idiosyncrasies that are not immediately apparent. First, Aeson assumes that Nothing in a JSON object that's being created should create the associated key with a "null" value, but the Mailchimp API actually checks for the existence of the key rather than a non-null value and treats it differently (it is treated as a "false" for optional parameters), so we must filter out Nothing entries before the object is created. To replace the "object" function of Aeson, we create filterObject:

    filterObject list =
      object $ filter notNothing list
     where
      notNothing (_, Null) = False
      notNothing _ = True

Another issue with the API is Mailchimp's time format. Mailchimp sends and receives times as "YYYY-MM-DD HH:MM:SS" in GMT, while Aeson only tries to read UTCTime as ECMA-262 ISO-8601 format (YYYY-MM-DDTHH:MM:SS.sZ). Instead of relying on Aeson's instance of ToJSON UTCTime and FromJSON UTCTime, we newtype UTCTime and instance the new type:

    newtype MCTime = MCTime {unMCTime :: UTCTime}

    mcFormatString :: String
    mcFormatString = "%F %T"

    instance ToJSON MCTime where
      toJSON (MCTime t) = String $ pack $ formatTime defaultTimeLocale mcFormatString t
    instance FromJSON MCTime where
      parseJSON (String s) = maybe mzero (return . MCTime) $ parseTime defaultTimeLocale mcFormatString (unpack s)
      parseJSON _ = mzero

Obviously these two issues may not effect future APIs you are interfacing with, but it may give you a flavor of the sort of problems that can crop up. With those problems out of the way, let's try running the code with our new Monad.

    ~ ghci -fglasgow-exts
    > :set -XOverloadedStrings
    > :l Web.Mailchimp.Lists Web.Mailchimp.Client
    > :m Web.Mailchimp.Lists Web.Mailchimp.Client
    > let key = MailchimpApiKey "<your key here>" "<dc code>"
    > cfg <- defaultMailchimpConfig key
    > runMailchimp cfg $ subscribeUser (ListId "f771e2e2be") (Email "example@cardsharp.ly") Nothing (Just EmailTypeHTML) Nothing Nothing Nothing Nothing 
    > EmailReturn {erEmail = Email "example@example.com", erEmailUniqueId = EmailUniqueId "<id>", erListEmailId = ListEmailId "<id>""

# Adding Logging

Everything seems to be working, but after implementing a few more API methods, a problem has come up. While we could add "liftIO $ print x" to debug, it's tedious and not great for production. Let's add real logging to make debugging somewhat easier. You can use the [LoggerT](hackage.haskell.org/packages/archive/monad-logger/latest/doc/html/Control-Monad-Logger.html) monad transformer to add logging to your monad, which requires only a couple changes to the Mailchimp definition:

    type Mailchimp a = LoggingT (ReaderT MailchimpConfig (ResourceT IO)) a

    runMailchimp :: (MonadIO m) => MailchimpConfig -> Mailchimp a -> m a
    runMailchimp config action = 
      liftIO $ runResourceT $ flip runReaderT config $ runStderrLoggingT action

Now we can add debugging statements anywhere in the Mailchimp monad and they will be printed to stderr. For example, to log the request and response bodies in query, note the added $(logDebug) statements, which use Template Haskell to create log messages that include line numbers.

    query :: (FromJSON x) => Text -> Text -> Value -> Mailchimp x
    query section apiMethod request = do
      config <- ask
      initReq <- liftIO $ parseUrl $ unpack $ apiEndpointUrl (makDatacenter $ mcApiKey config) section apiMethod
      let req = initReq { requestBody = RequestBodyLBS $ encode request 
                        , method = methodPost
                        }
      $(logDebug) $ pack . show $ requestBody req
      response <- catch (httpLbs req $ mcManager config) catchHttpException
      $(logDebug) $ pack . show $ responseBody response
      case decode $ responseBody response of
        Just result -> return result
        Nothing -> throwIO $ OtherMailchimpError (-1) "ParseError" "Could not parse result JSON from Mailchimp"
     where
      catchHttpException :: HttpException -> Mailchimp a
      catchHttpException e@(StatusCodeException _ headers _) = do
        $(logDebug) $ pack . show $ decodeError headers
        maybe (throwIO e) id (throwIO `fmap` (decodeError headers))
      catchHttpException e = throwIO e
      decodeError :: ResponseHeaders -> Maybe MailchimpError
      decodeError headers = fromStrict `fmap` (lookup "X-Response-Body-Start" headers) >>= decode

If you are just using your package outside of the context of Yesod applications, you can probably stop here and still have a nice, full-featured library that's easy to use in IO. If you ARE using Yesod, though, there is one more useful change.

# MailchimpT Monad Transformer

The way Mailchimp is currently written it might interact negatively with existing logging functionality in a user's application. They may want to bring their own logging to the table. For example, if you are developing a Yesod application, your Handler monad already includes MonadLogger, and you can control the output level for development vs. production. Instead of giving Mailchimp a defined monad stack, let's make it into a monad transformer that merely requires logging.

By making Mailchimp into a monad transformer, we also allow users to interleave actions from the Mailchimp monad and their own monad in a single do-block (or other standard monadic operations). To make the change, we enable RankNTypes (this allows the type constraints in the below type definition) and specify in our Mailchimp type the monadic instances that we require to run, in this case MonadIO (for IO), MonadLogger (for logging) and MonadBaseControl IO (used by query to throw and catch exceptions from ResourceT). Using monad transformers in this manner basically specifies a required set of capabilities. To make life easier, we move uses of runResourceT down into query only where they are needed, because we don't need it's functionality everywhere and because getting all of its constraints to typecheck is a pain.

    type MailchimpT m a =  (MonadIO m, MonadLogger m, MonadBaseControl IO m) => ReaderT MailchimpConfig m a

    runMailchimpT :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => MailchimpConfig -> MailchimpT m a -> m a
    runMailchimpT config action = 
      flip runReaderT config action

    -- | Runs Mailchimp in IO, ignoring the existing monadic context and logging to stderr
    runMailchimp :: (MonadIO m) => (MailchimpConfig -> MailchimpT (LoggingT IO) a -> m a)
    runMailchimp config action =
      liftIO $ runStderrLoggingT $ flip runReaderT config action

If people don't want to use MailchimpT, they can instead use runMailchimp in IO which will build up the monad stack itself.

# Conclusion

At this point the core of the package is written, and the rest is just translating the API documentation into Haskell code, i.e. extensive boilerplate. If you want to see what all this boilerplate looks like, you can check out the complete source for the Web.Mailchimp.Lists on GitHub, as well as a simple setup for performing tests with HUnit. Hopefully you have found this tutorial useful, thanks for reading!
