module Site.Lib
  ( postCtx
  , postPattern
  , archiveIdentifier
  , indexPattern
  , archiveTemplate
  , defaultTemplate
  , postTemplate
  , articleContext
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

articleContext :: Context String
articleContext = 
  constField "index-link" "index.html" `mappend`
  constField "about-link" "about.html" `mappend`
  constField "blog-link" "blog.html" `mappend`
  constField "works-link" "works.html" `mappend`
  constField "github-link" "https://github.com/clockvoid/portfolio" `mappend`
  defaultContext

