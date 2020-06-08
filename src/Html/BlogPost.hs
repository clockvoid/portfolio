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
  a_ [class_ "twitter-share-button", href_ "https://twitter.com/intent/tweet?text=$title$&via=clock_void"] "Tweet"
  p_ [class_ "date"] "Posted on $date$ $if(updated-date)$ , Updated on $updated-date$ $endif$"
  p_ [style_ "margin-bottom: 1.5rem"] "$if(summary)$ $summary$ $endif$"
  article_ [class_ "content"] "$body$"
  div_ [id_ "disqus_thread"] ""
  script_ "(function() { var d = document, s = d.createElement('script');s.src = 'https://clockvoid-tk.disqus.com/embed.js';s.setAttribute('data-timestamp', +new Date());(d.head || d.body).appendChild(s);})();"
  noscript_ $
    "Please enable JavaScript to view the"
    --a_ [href_ "http://disqus.com/?ref_noscript"] "comments powered by Disqus."
  
