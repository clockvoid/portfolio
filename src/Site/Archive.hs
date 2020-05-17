module Site.Archive
  ( archive
  ) where

import Data.Monoid (mappend)
import Hakyll

import Site.Lib

archive :: Rules ()
archive = create [archiveIdentifier] $ do
    route idRoute
    compile $ do
        --posts <- recentFirst =<< loadAll postPattern
        let archiveCtx =
        --        listField "posts" postCtx (return posts) `mappend`
                  constField "title-link" "Blog" `mappend`
                  articleContext

        makeItem ""
            >>= loadAndApplyTemplate archiveTemplate archiveCtx
         --   >>= loadAndApplyTemplate defaultTemplate archiveCtx
            >>= relativizeUrls
