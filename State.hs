{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction, ScopedTypeVariables,
    TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}

module State where

import Happstack.State
import Control.Monad.State
import Control.Monad.Reader
import Data.Generics
import qualified Data.ByteString as B

import System.Time (ClockTime (), getClockTime)


type BlogString = B.ByteString
type Headline   = BlogString
type Text       = BlogString

data Article = MkArticle { headline :: Headline
                         , text     :: Text
                         , time     :: ClockTime
                         } 
    deriving (Show, Read, Ord, Eq, Typeable, Data)

instance Version Article
$(deriveSerialize ''Article) 

newtype Weblog = MkWeblog { articles :: [Article] }
    deriving (Show, Read, Ord, Eq, Typeable, Data)

instance Version Weblog
$(deriveSerialize ''Weblog)


instance Component Weblog where
    type Dependencies Weblog = End
    initialValue = MkWeblog { articles = [] }


readWeblog :: Query Weblog [Article]
readWeblog = 
    do (MkWeblog x) <- ask
       return x

postToArticle :: Headline -> Text -> Weblog -> Update Weblog ()
postToArticle headline text (MkWeblog weblog) = put $ MkWeblog (newArticle:weblog)
    where newArticle = MkArticle headline text

postArticle :: Article -> Update Weblog ()
postArticle article = modify $ \(MkWeblog weblog) -> MkWeblog (article:weblog)

postArticleTest :: Article -> IO ClockTime -> Update Weblog ()
postArticleTest article time = modify $ \(MkWeblog weblog) -> MkWeblog (article:weblog)



$(mkMethods ''Weblog ['readWeblog, 'postToArticle, 'postArticle])
