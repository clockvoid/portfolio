module Index
  ( posts
  ) where

import Data.Monoid (mappend)
import Hakyll
import Lib

posts :: Rules ()
posts = match indexIdentifier $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll postPattern
        let indexCtx =
                listField "posts" postCtx (return posts) `mappend`
                constField "title" "Home"                `mappend`
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate defaultTemplate indexCtx
            >>= relativizeUrls

