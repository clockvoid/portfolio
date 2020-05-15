module Lib
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
archiveTemplate = fromFilePath "templates/archive.html"

defaultTemplate :: Identifier
defaultTemplate = fromFilePath "templates/default.html"

archiveIdentifier :: Identifier
archiveIdentifier = fromFilePath "archive.html"

indexPattern :: Pattern
indexPattern = fromRegex "index.html"

postTemplate :: Identifier 
postTemplate = fromFilePath "templates/post.html"
