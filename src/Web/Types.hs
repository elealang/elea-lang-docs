--
-- WEB: Types
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Types (
    Page (..)
  ) where


import Data.Text (Text)



data Page = Page {
    lessonId  :: Text
  , articleId :: Text
}
