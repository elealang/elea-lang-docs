--
-- HTML: Page
--

{-# LANGUAGE OverloadedStrings #-}

module Web.Handler where


import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant (Handler)
import Text.Blaze.Html (Html)

import Data.Assets (Assets (..))
import Web.HTML.LessonSidebar (sidebarHtml)
import Web.HTML.Lesson (lessonHtml)
import Web.HTML.Page (pageHTML)
import Web.Types (Page (..))



lessonPage :: Assets -> Text -> Text -> Handler Html
lessonPage assets lessonId articleId = do
  let page = Page lessonId articleId
  return $ pageHTML assets $ do
    sidebarHtml assets page
    lessonHtml assets page


