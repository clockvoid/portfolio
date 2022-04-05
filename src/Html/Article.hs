{-# LANGUAGE OverloadedStrings #-}

module Html.Article
  ( articleTemplate
  ) where

import           Data.Text
import           Lucid

imgClockvoid :: Text
imgClockvoid = "https://i.imgur.com/XUDiIoIm.jpg"

articleTemplate :: (FilePath, Html ())
articleTemplate = (articlePath, articleHtml)

articlePath :: FilePath
articlePath = "templates/article.html"

articleHtml :: Html ()
articleHtml = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "clockvoid - $title$"
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      meta_ [name_ "twitter:card", content_ "summary"]
      meta_ [name_ "og:site", content_ "@clockvoid"]
      meta_ [name_ "og:title", content_ "$title$"]
      meta_ [name_ "og:image", content_ imgClockvoid]
      meta_ [name_ "og:type", content_ "article"]
      link_ [rel_ "shortcut icon", href_ "/favicon.ico", type_ "image/x-icon"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.0.3/styles/dracula.min.css"]
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.10/highlight.min.js"] empty
      script_ "hljs.initHighlightingOnLoad();"
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML" ] empty
      script_ "window.twttr=(function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],t=window.twttr||{};if(d.getElementById(id))returnt;js=d.createElement(s);js.id=id;js.src='https://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);t._e=[];t.ready=function(f){t._e.push(f);};return t;}(document,'script','twitter-wjs'));"
      link_ [rel_ "stylesheet", href_ "/css/mystyles.css"]
    body_ [class_ "has-navbar-fixed-top"] $ do
      div_ [class_ "navbar is-fixed-top has-shadow"] $
        div_ [class_ "container"] $ do
          div_ [class_ "navbar-brand"] $ do
            div_ [class_ "navbar-item"] $
              a_ [href_ "/"] $
                figure_ [class_ "image"] $
                  img_ [class_ "is-rounded", src_ imgClockvoid, style_ "height: 1.75rem; width: 1.75rem;"]
            div_ [class_ "navbar-burger burger", onclick_ "document.querySelector('.navbar-menu').classList.toggle('is-active');"] $ do
              span_ [] ""
              span_ [] ""
              span_ [] ""
          div_ [class_ "navbar-menu"] $ do
            div_ [class_ "navbar-start"] $ do
              a_ [class_ "navbar-item", href_ "/"] "Home"
              a_ [class_ "navbar-item", href_ "/about.html"] "About"
              a_ [class_ "navbar-item", href_ "/blog.html"] "Blog"
              a_ [class_ "navbar-item", href_ "/works.html"] "Works"
            div_ [class_ "navbar-end"] $
              div_ [class_ "navbar-item"] $
                a_ [class_ "button is-primary", href_ "https://github.com/clockvoid/portfolio$if(github-link)$/blob/master/$github-link$ $endif$"] $ do
                  span_ [class_ "icon"] $
                    i_ [class_ "fab fa-github"] ""
                  span_ [] "View it in GitHub"
      div_ [class_ "container"] $
        div_ [class_ "column is-three-fifths is-offset-one-fifth"] "$body$"
      footer_ [] $
        div_ [class_ "column is-three-fifths is-offset-one-fifth footer"] $
          div_ [class_ "level"] $ do
            p_ [class_ "level-item has-text-centered"] "© 2020 clockvoid"
            a_ [href_ "https://twitter.com/clock_void", class_ "level-item has-text-centered"] "Twitter"
            a_ [href_ "https://github.com/clockvoid", class_ "level-item has-text-centered"] "GitHub"
            figure_ [class_ "image level-item has-text-centered"] $
              img_ [class_ "is-rounded", src_ imgClockvoid, style_ "height: 1.75rem; width: 1.75rem;"]
            a_ [href_ "/about.html", class_ "level-item has-text-centered"] "About"
            a_ [href_ "/blog.html", class_ "level-item has-text-centered"] "Blog"
            a_ [href_ "/works.html", class_ "level-item has-text-centered"] "Works"
