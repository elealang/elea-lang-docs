--
-- HTML: Page
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.Page (
    pageHTML
  ) where


import Control.Monad (forM_)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (putStrLn)

import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Icon (iconSVGWithName)


-- Page HTML
-------------------------------------------------------------------------------

pageHTML :: Assets -> Html -> Html
pageHTML assets contentHtml = H.docTypeHtml $ do
  H.head $ do
    H.title "Elea Documentation"
    -- Fonts
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "https://cloud.typography.com/6602898/7018412/css/fonts.css"
    -- CSS: Page
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/page.css"
    -- CSS: LessonSidebar
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/lesson-sidebar.css"
    -- CSS: Lesson
    H.link ! A.rel "stylesheet" 
           ! A.type_ "text/css" 
           ! A.href "/static/css/lesson.css"
    -- Alpine
    H.preEscapedString "<script src='https://cdn.jsdelivr.net/gh/alpinejs/alpine@v2.8.0/dist/alpine.min.js' defer></script>"
  H.body $ do
    H.div ! A.id "page" $ do
      pageHeaderHTML assets
      H.div ! A.id "page-content" $ contentHtml



pageHeaderHTML :: Assets -> Html
pageHeaderHTML assets = do 
  H.div ! A.id "page-header" $ do
    H.div ! class_ "page-title" $ do
      H.div ! class_ "page-title-name" $ "ELEA"
      H.div ! class_ "page-title-description" $ "META PROGRAMMING LANGUAGE"
    H.div ! class_ "page-programming-language" $ do
      H.div ! class_ "page-programming-language-label" $ "I am using"
      H.div ! class_ "page-programming-language-name" $ "Python"


contentSidebarHTML :: Assets -> Html
contentSidebarHTML assets = do
  H.div ! A.id "content-sidebar-header" $ do
    H.div ! A.id "content-sidebar-header-title" $ ""
  H.div ! A.id "content-sidebar-nav" $ return ()
  H.div ! A.id "content-sidebar-footer" $ return ()



--pageIndexContentHTML :: Assets -> Html
--pageIndexContentHTML (Assets iconIndex articleIndex) = do
  --H.div ! class_ "page-index" $ do
    ---- last updated
    --H.div ! class_ "page-index-last-updated" $ do
      --H.div ! class_ "page-index-last-updated-text" $ "Last updated Sunday, January 10th, 2021"
    ---- toolbar
    --H.div ! class_ "page-index-toolbar" $ do
      --H.div ! class_ "page-index-toolbar-sort" $ do
        --H.div ! class_ "page-index-toolbar-sort-button" $ "by series"
      --H.div ! class_ "page-index-toolbar-header" $ do
        --H.span ! class_ "page-index-toolbar-normal" $ "showing"
        --H.span ! class_ "page-index-toolbar-count" $ "5"
        --H.span ! class_ "page-index-toolbar-normal" $ "notes, essays, hypotheses, and tutorials"
    ---- article list
    --H.div ! class_ "page-index-article-list" $ return ()
      -- H.ul $ do
        --forM_ articles $ \article -> do
          --H.li $ do
            --H.div ! class_ "page-index-article-sidebar" $ do
              --H.div ! class_ "page-index-article-sidebar-content" $ 
                --toHtml $ articleDatePublished article
            --H.div ! class_ "page-index-article-main" $ do
              --H.span ! class_ "page-index-article-title" $ 
                --toHtml $ articleTitle article
              --H.span ! class_ "page-index-article-summary" $ 
                --toHtml $ articleSummary article
