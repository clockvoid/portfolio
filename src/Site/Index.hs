module Site.Index
  ( index,
  )
where

import Data.Monoid (mappend)
import Hakyll
import Site.Lib

index :: Rules ()
index = match indexPattern $ do
  route $ constRoute "index.html"
  compile $ do
    let indexCtx =
          constField "title" "Home"
            `mappend` defaultContext

    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= relativizeUrls
