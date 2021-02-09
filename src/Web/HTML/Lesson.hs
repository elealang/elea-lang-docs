--
-- HTML: Lesson
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.Lesson (
    lessonHtml
  ) where


import CMarkGFM
  ( commonmarkToHtml
  , optUnsafe
  )
import qualified Data.Map as M (lookup)
import Text.Blaze.Html (Html)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Assets (Assets (..))
import Data.Lesson ( Article (..), maybeArticle)
import Web.Types ( Page (..) )



lessonHtml :: Assets -> Page -> Html
lessonHtml assets page = do
  -- TODO bad url
  let mArticle = maybeArticle page.lessonId page.articleId assets.lessonIndex 
  case mArticle of
    Nothing      -> errorPageHtml
    Just article -> lessonContentHtml assets article


lessonContentHtml :: Assets -> Article -> Html
lessonContentHtml (Assets iconIndex _) article = 
  H.div ! class_ "comp-lesson" $ do
    H.div ! class_ "comp-lesson-header" $ do
      H.h1 $ toHtml article.title
    H.div ! class_ "comp-lesson-content" $ do
      H.preEscapedText $ commonmarkToHtml [optUnsafe] [] article.markdown


errorPageHtml :: Html
errorPageHtml = do
  H.div "error"
