{-# LANGUAGE OverloadedStrings #-}

module Html.Blog
  ( blogTemplate
  ) where

import Data.Text
import Lucid

imgClockvoid :: Text
imgClockvoid = "https://i.imgur.com/rLs5sRT.png"

blogTemplate :: (FilePath, Html ())
blogTemplate = (blogPath, blogHtml)

blogPath :: FilePath
blogPath = "templates/blog.html"

blogHtml :: Html ()
blogHtml = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "clockvoid - $title$"
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [rel_ "stylesheet", href_ "/css/mystyles.css"]
    body_ [class_ "has-navbar-fixed-top"] $
      div_ [class_ "navbar is-fixed-top has-shadow"] $
        div_ [class_ "container"] $ do
          div_ [class_ "navbar-brand"] $ do
            div_ [class_ "navbar-item"] $
              a_ [href_ "$index-link$"] $
                figure_ [class_ "image"] $
                  img_ [class_ "is-rounded", src_ imgClockvoid, style_ "height: 1.75rem; width: 1.75rem;"]
            div_ [class_ "navbar-burger burger", onclick_ "document.querySelector('.navbar-menu').classList.toggle('is-active');"] $ do
              span_ [] ""
              span_ [] ""
              span_ [] ""
          div_ [class_ "navbar-menu"] $ do
            div_ [class_ "navbar-start"] $ do
              a_ [class_ "navbar-item", href_ "$index-link$"] "Home"
              a_ [class_ "navbar-item", href_ "$about-link$"] "About"
              a_ [class_ "navbar-item", href_ "$blog-link$"] "Blog"
              a_ [class_ "navbar-item", href_ "$works-link$"] "Works"
            div_ [class_ "navbar-end"] $
              div_ [class_ "navbar-item"] $
                a_ [class_ "button is-primary"] $ do
                  span_ [class_ "icon"] $
                    i_ [class_ "fab fa-github"] ""
                  span_ [] "View it in GitHub"
