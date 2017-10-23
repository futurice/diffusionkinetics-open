{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell, Rank2Types #-}
{-# LANGUAGE DataKinds, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dashdo.Servant where

import Dashdo
import Dashdo.Types
import Dashdo.FileEmbed

import Servant
import Servant.HTML.Lucid

import Network.Wai (Application)
import Control.Monad.Trans (liftIO)
import System.Random
import qualified Data.UUID as UUID
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Map as Map

import Data.Monoid
import Lucid
import Control.Exception.Safe (catchAnyDeep)

type RunInIO m = forall a. m a -> IO a

getRandomUUID :: IO UUID.UUID
getRandomUUID = randomIO

dashdoJS :: Text
dashdoJS = TLE.decodeUtf8 $ BLS.fromStrict $(embedFile "public/js/dashdo.js")

dashdoJSrunnerRDashdo :: Text
dashdoJSrunnerRDashdo = TLE.decodeUtf8 $ BLS.fromStrict $(embedFile "public/js/runners/rdashdo.js")

data JavaScript

instance Accept JavaScript where
    contentType _ = "application/javascript"

instance MimeRender JavaScript Text where
    mimeRender _ = TLE.encodeUtf8

type P = (Text, Text)

type DashdoAPI = Get '[HTML] (Html ())
    :<|> "uuid" :> Get '[JSON] UUID.UUID
    :<|> "js" :> "dashdo.js" :> Get '[JavaScript] Text
    :<|> "js" :> "runners" :> "rdashdo.js" :> Get '[JavaScript] Text
    :<|> Capture "handler" T.Text :> ReqBody '[FormUrlEncoded] [P] :> Post '[HTML] (Html ())

dashdoServer :: forall m. Monad m => RunInIO m -> Text -> [RDashdo m] -> IO (Server DashdoAPI)
dashdoServer r iniHtml ds = do
    uuid <- getRandomUUID
    pure $ pure (toHtmlRaw iniHtml)
        :<|> pure uuid
        :<|> pure dashdoJS
        :<|> pure dashdoJSrunnerRDashdo
        :<|> handler
  where
    handler :: T.Text -> [P] -> Handler (Html ())
    handler n f = case Map.lookup n hs of
        Nothing -> fail "unknown" -- TODO
        Just h  -> liftIO $ h >>= ($ f)

    hs :: Map.Map T.Text (IO ([P] -> IO (Html ())))
    hs = Map.fromList $ map (\(RDashdo n _ d) -> (T.pack n, makeHandler d)) ds
      where
        makeHandler :: Dashdo m a -> IO ([P] -> IO (Html ()))
        makeHandler d = do
            (_iniHtml, ff, _) <- r $ dashdoGenOut d (initial d) []
            pure $ \ps -> do
                let newVal = parseForm (initial d) ff ps
                (thisHtml, _, _) <- liftIO $ (r $ dashdoGenOut d newVal ps) `catchAnyDeep` \e -> do
                    let es :: String
                        es = "<div  class=\"alert alert-danger\" role=\"alert\"><pre>Error: " <> show e <> "</pre></div>"

                        errorHtml = TL.pack es <> iniHtml
                    pure (errorHtml, [], [])
                print ps
                pure $ toHtmlRaw thisHtml


dashdoApplication :: Monad m => RunInIO m -> Text -> [RDashdo m] -> IO Application
dashdoApplication r iniHtml ds = do
    server <- dashdoServer r iniHtml ds
    pure $ serve (Proxy :: Proxy DashdoAPI) server

