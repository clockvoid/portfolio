module Site.Lib
  ( postCtx
  , postPattern
  , archiveIdentifier
  , indexPattern
  , archiveTemplate
  , defaultTemplate
  , postTemplate
  ) where

import Hakyll

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postPattern :: Pattern
postPattern = fromRegex "posts/*"

archiveTemplate :: Identifier
archiveTemplate = fromFilePath "_html/templates/blog.html"

defaultTemplate :: Identifier
defaultTemplate = fromFilePath "templates/default.html"

archiveIdentifier :: Identifier
archiveIdentifier = fromFilePath "blog.html"

indexPattern :: Pattern
indexPattern = fromRegex "_html/index.html"

postTemplate :: Identifier 
postTemplate = fromFilePath "templates/post.html"

