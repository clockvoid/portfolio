module Site.Lib
  ( postCtx
  , postPattern
  , indexPattern
  , articleTemplate
  , postTemplate
  ) where

import Hakyll

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postPattern :: Pattern
postPattern = fromRegex "posts/*"

articleTemplate :: Identifier
articleTemplate = fromFilePath "_html/templates/article.html"

indexPattern :: Pattern
indexPattern = fromRegex "_html/index.html"

postTemplate :: Identifier 
postTemplate = fromFilePath "templates/post.html"

