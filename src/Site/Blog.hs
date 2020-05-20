module Site.Blog
  ( blog
  ) where

import Data.Monoid (mappend)
import Hakyll

import Site.Lib

blogIdentifier :: Identifier
blogIdentifier = fromFilePath "blog.html"

blogTemplate :: Identifier
blogTemplate = fromFilePath "_html/templates/blog.html"

blog :: Rules ()
blog = create [blogIdentifier] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll postPattern
        let archiveCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Blog" `mappend`
              constField "github-link" "https://github.com/clockvoid/portfolio/blob/master/src/Html/Blog.hs" `mappend`
              isEmpty (length posts) `mappend`
              defaultContext
                where
                  isEmpty 0 = constField "empty" "true"
                  isEmpty _ = mempty

        makeItem ""
            >>= loadAndApplyTemplate blogTemplate archiveCtx
            >>= loadAndApplyTemplate articleTemplate archiveCtx
            >>= relativizeUrls
