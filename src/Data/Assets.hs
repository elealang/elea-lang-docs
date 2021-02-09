--
-- Assets
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}


module Data.Assets where


import Data.Icon (IconIndex)
import Data.Lesson (LessonIndex)



data Assets = Assets {
    iconIndex   :: IconIndex
  , lessonIndex :: LessonIndex
} deriving (Eq, Show)



