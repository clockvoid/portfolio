{-# LANGUAGE OverloadedStrings #-}

module Html.Index
  ( indexPage
  ) where

import Lucid

indexPage :: (FilePath, Html ())
indexPage = (indexPath, indexHtml)

indexPath :: FilePath
indexPath = "index.html"

indexHtml :: Html ()
indexHtml = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "clockvoid - $title$"
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [rel_ "stylesheet", href_ "/css/default.css"]

