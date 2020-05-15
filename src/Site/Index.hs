module Site.Index
  ( index
  ) where

import Data.Monoid (mappend)
import Hakyll

import Site.Lib

index :: Rules ()
index = match indexPattern $ do
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

