{-# LANGUAGE OverloadedStrings #-}

module Html.Index
  ( indexPage
  ) where

import           Data.Text
import           Lucid

imgClockvoid :: Text
imgClockvoid = "https://i.imgur.com/SBwBOuB.png"

indexPage :: (FilePath, Html ())
indexPage = (indexPath, indexHtml)

indexPath :: FilePath
indexPath = "index.html"

indexHtml :: Html ()
indexHtml = do
  doctype_
  html_ [class_ "disable-scrolling"] $ do
    head_ $ do
      title_ "clockvoid - $title$"
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      meta_ [name_ "twitter:card", content_ "summary"]
      meta_ [name_ "og:site", content_ "@clockvoid"]
      meta_ [name_ "og:title", content_ "$title$"]
      meta_ [name_ "og:image", content_ imgClockvoid]
      meta_ [name_ "og:type", content_ "website"]
      meta_ [name_ "og:url", content_ "https://clockvoid.tk"]
      link_ [rel_ "shortcut icon", href_ "/favicon.ico", type_ "image/x-icon"]
      link_ [rel_ "stylesheet", href_ "/css/mystyles.css"]
    body_ $
      section_ [class_ "hero is-primary is-fullheight is-bold"] $ do
        div_ [class_ "hero-head"] $
          div_ [class_ "navbar is-transparent"] $
            div_ [class_ "navbar-brand"] $ do
              a_ [class_ "navbar-item", href_ "https://github.com/clockvoid"] $
                span_ [class_ "icon media-icon is-medium"] $ i_ [class_ "fab fa-lg fa-github"] ""
              a_ [class_ "navbar-item", href_ "https://twitter.com/clock_void"] $
                span_ [class_ "icon media-icon is-medium"] $ i_ [class_ "fab fa-lg fa-twitter"] ""
        div_ [class_ "hero-body"] $
          div_ [class_ "container has-text-centered"] $ do
            figure_ [class_ "image is-128x128", style_ "margin: 0 auto 1.5rem auto"] $
              img_ [class_ "is-rounded", src_ imgClockvoid]
            h1_ [class_ "title is-size-1-desktop is-size-2-touch"] "clockvoid"
            nav_ [class_ "home-breadcrumb is-centered has-bullet-separator"] $
              ul_ $ do
                li_ $ a_ [href_ "./about.html", style_ "padding-left: 12px;"] "About"
                li_ $ a_ [href_ "./blog.html"] "Blog"
                li_ $ a_ [href_ "./works.html"] "Works"
        div_ [class_ "hero-foot"] $
          div_ [class_ "navbar is-transparent"] mempty
