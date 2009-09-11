{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Controller where 

import HSP
import Happstack.Server
import Happstack.Helpers
import Happstack.State
import Happstack.Data (defaultValue)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Monoid (mappend)
import System.Time (getClockTime)

import State
import View

import Data.ByteString.Char8 (pack, unpack)

appHandler :: ServerPartT IO Response
appHandler = msum [ methodM GET >> seeOther "/blog" (toResponse ()) -- matches /  
                  , dir "img" (fileServe [] "images")
                  , dir "css" (fileServe [] "public")
                  , dir "blog" (viewWeblog)
                  , dir "post" (methodSP GET  (viewPostArticle) `mappend` 
                                methodSP POST (processformPostArticle)) 
--                  , adminHandler  >>= renderFromBody "funFrogger's Admin" 
                  ]

weblogHandler :: ServerPartT IO (HSP XML)
weblogHandler = dir "blog" $ msum [putArticle, getWeblog]

-- adminHandler :: ServerPartT IO (HSP XML)
-- adminHandler  = dir "post" $ msum [renderInputForm]

viewWeblog :: ServerPartT IO Response
viewWeblog = getWeblog >>= renderFromBody "funFrogger's Weblog"

getWeblog :: ServerPartT IO (HSP XML)
getWeblog = methodM GET >> 
                        do wblg <- query ReadWeblog 
                           ok $ <div><% wblg %></div>


viewPostArticle :: ServerPartT IO Response
viewPostArticle = do x <- renderInputForm
                     return x

processformPostArticle :: ServerPartT IO Response
processformPostArticle = do Just posting <- getData
                            now          <- liftIO getClockTime
                            update $ PostArticle posting
                            seeOther "/blog" (toResponse ())


putArticle :: ServerPartT IO (HSP XML)
putArticle = methodM POST >> 
                          do Just posting <- getData
                             now          <- liftIO getClockTime
                             update $ PostArticle posting
                             seeOther "/entries" (seeOtherXML "/entries")

instance FromData Article where
  fromData = do
    headline <- look "headline"
    text     <- look "text"
    return $ MkArticle (pack headline) (pack text) -- defaultValue
