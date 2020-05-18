{-# LANGUAGE OverloadedStrings #-}

module Html.BlogPost
  ( blogPostTemplate
  ) where

import Data.Text
import Lucid

blogPostTemplate :: (FilePath, Html ())
blogPostTemplate = (blogPostPath, blogPostHtml)

blogPostPath :: FilePath
blogPostPath = "templates/blog-post.html"

blogPostHtml :: Html ()
blogPostHtml = do
  h1_ [class_ "title is-1", style_ "margin-top: 1rem"] "$title$"
  p_ [class_ "date", style_ "margin-bottom: 1rem"] "Posted on $date$ $if(updated-date)$ , Updated on $updated-date$ $endif$"
  p_ [style_ "margin-bottom: 1rem"] "$if(summary)$ $summary$ $endif$"
  article_ [] "$body$"


  
