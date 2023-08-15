{-# LANGUAGE OverloadedStrings #-}

module Html.Blog
  ( blogTemplate,
  )
where

import Data.Text
import Lucid

blogTemplate :: (FilePath, Html ())
blogTemplate = (blogPath, blogHtml)

blogPath :: FilePath
blogPath = "templates/blog.html"

blogHtml :: Html ()
blogHtml = do
  "$if(empty)$"
  h1_ [style_ "margin: 3rem auto auto 0; text-align: center;", class_ "title"] "投稿はまだありません"
  "$else$"
  ul_ $ do
    "$for(posts)$"
    li_ [style_ "margin-bottom: 0.5rem"] $
      div_ [class_ "box card", href_ "$url$", style_ "padding: 0"] $ do
        div_ [class_ "card-content"] $ do
          p_ [class_ "title"] "$title$"
          p_ [class_ "content"] "$if(summary)$ $summary$ $else$ No info $endif$"
          p_ [class_ "content date"] "Posted on $date$ $if(updated-date)$ , Updated on $updated-date$ $endif$"
        div_ [class_ "card-footer"] $
          a_ [class_ "card-footer-item", href_ "$url$"] "この記事を読む"
    "$endfor$"
    "$endif$"
