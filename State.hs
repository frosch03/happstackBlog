{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction, ScopedTypeVariables,
    TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}

module State where

import Happstack.State
import Control.Monad.State
import Control.Monad.Reader
import Data.Generics
import qualified Data.ByteString as B

import System.Time 


type BlogString = B.ByteString
type Headline   = BlogString
type Text       = BlogString
type Date       = Integer

data Article = MkArticle { headline :: Headline
                         , text     :: Text
                         , time     :: Date
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
    where newArticle = MkArticle headline text 0

postArticle :: Article -> Update Weblog ()
postArticle article = modify $ \(MkWeblog weblog) -> MkWeblog (article:weblog)

postArticleTest :: Article -> Update Weblog ()
postArticleTest article = modify $ \(MkWeblog weblog) -> MkWeblog (article:weblog)



$(mkMethods ''Weblog ['readWeblog, 'postToArticle, 'postArticle, 'postArticleTest])
