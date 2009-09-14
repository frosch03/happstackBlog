{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Controller where 

import HSP
import Happstack.Server
import Happstack.Server.SURI (ToSURI)
import Happstack.Server.SimpleHTTP
import Happstack.Helpers
import Happstack.State
import Happstack.Data (defaultValue)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Monoid (mappend)
import System.Time

import State
import View

import Data.ByteString.Char8 (pack, unpack)


redir :: (FilterMonad Response m, ToSURI uri) => uri -> m Response
redir url = seeOther url (toResponse "")

appHandler :: ServerPartT IO Response
appHandler = msum [ methodM GET >> seeOther "/blog" (toResponse ()) -- matches /  
                  , dir "img" (fileServe [] "images")
                  , dir "css" (fileServe [] "public")
                  , dir "blog" (viewWeblog)
                  , dir "post" postHandler
                  ]

viewWeblog :: ServerPartT IO Response
viewWeblog = getWeblog >>= renderFromBody "funFrogger's Weblog"



postHandler :: ServerPartT IO Response
postHandler  = msum [ methodSP GET  (viewPostArticle)
                    , methodSP POST (processformPostArticle)
                    ]

viewPostArticle :: ServerPartT IO Response
viewPostArticle = do x <- (renderInput "new article")
                     return x

processformPostArticle :: ServerPartT IO Response
processformPostArticle = do Just posting <- getData
                            (TOD sec _)  <- liftIO getClockTime
                            update $ PostArticleTest (posting {time = sec})
                            redir "/blog"




getWeblog :: ServerPartT IO (HSP XML)
getWeblog = methodM GET >> 
                        do wblg <- query ReadWeblog 
                           ok $ <div><% wblg %></div>


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
    return $ MkArticle (pack headline) (pack text) 0 -- defaultValue
