{-|
Module      : Matching.SimpleComparison
Description : Defines functions to compare words at a cursor position.
Copyright   : (c) Christian Sperandio, 2014
License     : MIT
Maintainer  : christian.sperandio@gmail.com

The functions in this module are the used functions by the module Comparison.

-}
module Gemini.SimpleComparison where

import Gemini.Types (Cursor(..), Equivalence(..))
import Gemini.Equivalence (findEquivalence)

-- |Cursor an invertion between 2 lists at the cursor
findInvertion :: (Eq a) => Cursor a -> Maybe (Cursor a)

findInvertion s@(Cursor (a,b) (x:x':xs, y:y':ys)) = if x /= y && x == y' && x' == y
                                                    then Just s { previous = (a ++ [x, x'], b ++ [y, y']), followers = (xs, ys) }
                                                    else Nothing

findInvertion _ = Nothing


-- |Cursor an insertion between 2 lists at the cursor
findInsertion :: (Eq a) => Cursor a -> Maybe (Cursor a)

findInsertion (Cursor _ ([], [])) = Nothing

findInsertion s@(Cursor (a,b) (x:x':xs, y:ys)) = if x /= y && x' == y
                                                     then Just s { previous = (a ++ [x, x'], b ++ [y]), followers = (xs, ys) }
                                                     else Nothing                                                     

findInsertion s@(Cursor (a,b) (x:xs, [])) = Just s { previous = (a ++ [x], b), followers = (xs, []) }

findInsertion _ = Nothing


-- |Cursor a deletion between 2 lists at the cursor
findDeletion :: (Eq a) => Cursor a -> Maybe (Cursor a)

findDeletion (Cursor _ ([], [])) = Nothing

findDeletion s@(Cursor (a,b) (x:xs, y:y':ys)) = if x /= y && x == y'
                                                  then Just s { previous = (a ++ [x], b ++ [y, y']), followers = (xs, ys) }
                                                  else Nothing

findDeletion s@(Cursor (a,b) ([], y:ys)) = Just s { previous = (a, b ++ [y]), followers = ([],ys) }

findDeletion _ = Nothing

-- |Cursor a same item at the cursor
findEquality :: (Eq a) => Cursor a -> Maybe (Cursor a)

findEquality s@(Cursor (a, b) (x:xs, y:ys)) = if x == y 
                                              then Just s { previous = (a ++ [x], b ++ [y]), followers = (xs, ys) }
                                              else Nothing

findEquality _ = Nothing


-- |Cursor a substitution at the cursor
findSubstitution :: (Eq a) => Cursor a -> Maybe (Cursor a)

findSubstitution s@(Cursor (a, b) (x:xs, y:ys)) = if x /= y
                                                    then Just s { previous = (a ++ [x], b ++ [y]), followers = (xs, ys) }
                                                    else Nothing

findSubstitution _ = Nothing


-- |Cursor a similarity at the cursor
findFirstEquivalence :: (Eq a) => [Equivalence a] -> Cursor a -> Maybe (Cursor a)

findFirstEquivalence [] (Cursor _ _) = Nothing

findFirstEquivalence (x:xs) s@(Cursor _ _) = case findEquivalence x s of
                                             Nothing -> findFirstEquivalence xs s
                                             e@(Just _) -> e


