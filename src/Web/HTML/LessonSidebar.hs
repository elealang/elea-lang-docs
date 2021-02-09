--
-- HTML: Learn
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.HTML.LessonSidebar (
    sidebarHtml
  ) where


import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T (toUpper, unpack)
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Assets ( Assets (..) )
import Data.Lesson
  ( LessonIndex (..) 
  , Lesson (..)
  , Article (..)
  )
import Web.Types (Page (..))



sidebarHtml :: Assets -> Page -> Html
sidebarHtml (Assets iconIndex lessonIndex) page = do
  H.div ! class_ "comp-lesson-sidebar" $ do
    H.h2 "LEARN"
    H.div ! class_ "comp-lesson-sidebar-summary" $ "Build and deploy a real world application"
    H.div ! class_ "comp-lesson-card-list" $ do
      forM_ lessonIndex.lessons $ lessonCardHtml page

  where
    -- | Lesson Card HTML
    -- TODO why need toValue and toHtml ? 
    lessonCardHtml :: Page -> Lesson -> Html 
    lessonCardHtml page (Lesson id_ title summary articles _) = do 
      let lessonClasses = toValue $ classSelected "comp-lesson-card" 
                                  $ id_ == page.lessonId
      H.div ! class_ lessonClasses $ do
        H.a ! class_ "comp-lesson-card-name" 
            ! href (toValue $ "/lesson/" <> id_) $ toHtml $ T.toUpper title
        H.div ! class_ "comp-lesson-card-summary" $ toHtml summary
        H.div ! class_ "comp-lesson-card-section-list" $ do
          forM_ (zip articles [0..]) $ lessonCardSectionHtml page

    -- | Lesson Artcle Section HTML
    lessonCardSectionHtml :: Page -> (Article, Int) -> Html 
    lessonCardSectionHtml page (Article id_ title _, index) = do 
      let classes = toValue $ classSelected "comp-lesson-card-section" 
                            $ id_ == page.articleId
      H.div ! class_ classes $ do
        H.div ! class_ "comp-lesson-card-section-label" $ 
          ["I","II","III","IV","V"] !! index
        H.div ! class_ "comp-lesson-card-section-name" $ toHtml title


classSelected :: Text -> Bool -> Text
classSelected mainClass isSelected = 
  mainClass <> (if isSelected then " selected" else "")
