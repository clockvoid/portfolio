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
  h1_ [class_ "title"] "$title$"
  p_ [class_ "date"] "Posted on $date$ $if(updated-date)$ , Updated on $updated-date$ $endif$"
  p_ [] "$if(summary)$ $summary$ $endif$"
  article_ [] "$body$"


  
