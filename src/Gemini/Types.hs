{-|
Module      : Matching.Types
Description : Data types used by other Matching modules.
Copyright   : (c) Christian Sperandio, 2014
License     : Apache License, version 2.0
Maintainer  : christian.sperandio@gmail.com
-}
module Gemini.Types where

{-|
  The Cursor data is used by the comparison functions. A cursor walks along the
  compared words.
  
  For example, if the cursor is at the first L of the word "HELLO", we have
  previous = "HE" and followers = "LLO".  
-}
data Cursor a
        = Cursor {
                 previous :: ([a], [a]),
                 followers :: ([a], [a])
                 } deriving (Show)
                       
                       
