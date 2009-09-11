{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module View where 

import HSP
import qualified HSX.XMLGenerator as HSX (XML)
import Control.Monad.Trans (MonadIO)
import Happstack.Server (Response, ok, ServerPartT)
import Happstack.Server.HSP.HTML (webHSP)
import State

import Data.ByteString.Char8 (pack, unpack)

newtype WithLineBreak = MkWithLineBreak String

instance (XMLGenerator m) => (EmbedAsChild m (Article)) where
    asChild (MkArticle hl tx) = 
        <%
            <div class="posting">
                <div class="headline"><% unpack $ hl %></div>
                <br />
                <% map p (lines.unpack $ tx) %>
            </div>
        %>
       where p str = <br><% str %></br>

--instance (XMLGenerator m) => (EmbedAsChild m (Weblog)) where
--    asChild (MkWeblog arts) = 
--        <%
--            <div>
--            <h2 id="ueberschrift">Blub</h2>
--            <ul>
--            <% arts %>
--            </ul>
--            </div>
--        %>

seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>

renderFromBody :: (MonadIO m, EmbedAsChild (HSPT' IO) xml) => String -> xml -> m Response
renderFromBody title = webHSP . pageFromBody title

pageFromBody :: (EmbedAsChild (HSPT' IO) xml) => String -> xml -> HSP XML
pageFromBody title body = 
    withMetaData html4Strict $ 
        <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
          <head>
            <title><% title %></title>

            <link rel="stylesheet" href="/css/log.css" type="text/css" />
          </head>
          <body>

            <div class="top">
            <a name="top"></a>

            <img src="/img/blog.png" alt="www.frosch03.de/blog" />
            </div>

            <div class="left">
            <pre class="code"> 
  Table of Content <br /> <br />

  1) <a href="index.html#intro">introduction</a><br />
  2) <a href="index.html#about">about me</a><br />
  3) <a href="index.html#publi">pulications</a><br />

  4) <a href="index.html#secur">security project</a><br />
  5) <a href="index.html#picts">pictures</a><br />
  6) <a href="index.html#brain">brainfuck</a><br />
  7) <a href="index.html#conts">contact me</a><br />
  8) <a href="index.html#links">links</a><br />

            </pre>
            </div>

            <div class="box">
            <div class="layer">
              <% body %>
            </div>
            </div>
            <p />

            <div>
            <a href="http://validator.w3.org/check/referer">
             <img style="border:0;width:88px;height:31px"
                  src="http://www.w3.org/Icons/valid-xhtml10"
                  alt="Valid XHTML 1.0!" height="31" width="88" /></a>

            <a href="http://jigsaw.w3.org/css-validator/">
             <img style="border:0;width:88px;height:31px"
                  src="http://jigsaw.w3.org/css-validator/images/vcss" 
                  alt="Valid CSS!" /></a>
            </div>

          </body>
        </html>


renderInput :: (MonadIO m) => String -> m Response
renderInput title = webHSP . pageFromBody title $
    <form action="/post" method="post" enctype="multipart/form-data;charset=UTF-8" accept-charset="UTF-8">
        <p>
            <label for="headline"><span class="accesskey">H</span>eadline</label><br />
            <input type="text" name="headline" id="headline" tabindex="1" accesskey="H" />
        </p>
        <p>
            <label for="text"><span class="accesskey">T</span>ext</label><br />
            <textarea cols="80" rows="10" name="text" id="text" tabindex="2" accesskey="T"></textarea>
        </p>
        <p>
            <input type="submit" tabindex="3" accesskey="P" value="Post Article" />
        </p>
    </form>
