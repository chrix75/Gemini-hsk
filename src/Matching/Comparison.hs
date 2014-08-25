{-|
Module      : Matching.Comparison
Description : Defines functions to compare words.
Copyright   : (c) Christian Sperandio, 2014
License     : 4-clause BSD license (with advertising clause)
Maintainer  : christian.sperandio@gmail.com

The Comparison module provide main functions which compare 2 words.
A word is a list of any items which responds to class Eq and Ord.

The result of a comparison of 2 words is a score into a Double value from 0 to 1.
1 means the 2 words are the same and 0 means the words are different.

To compute the score, we search all operations needed to transform a word to another.

This module manages these transformations:

Invertion    : CAB vs. CBA
Insertion    : CAAB vs. CAB
Deletion     : CB vs. CAB
Substitution : CAB vs. CXB
Similarity   : TCHO vs. CHO (if we defined the "TCH" <-> "CH" equivalence)

We define all transformation as error except the Similarity one. That explains why
the result of TCHO and CHO gives those words are the same.
 
-}
module Matching.Comparison where

import Matching.Types (Cursor(..), Equivalence)
import qualified Matching.SimpleComparison as C

-- |Defines all managed errors.
data Error a = Invertion { rest :: Cursor a }
             | Insertion { rest :: Cursor a }
             | Deletion { rest :: Cursor a }
             | Substitution { rest :: Cursor a }
             | Similarity { rest :: Cursor a }
             | None { rest :: Cursor a }
           deriving (Show)

-- |Compute the likeness score of 2 words.
likeness :: (Eq a) => [Equivalence a] -> [a] -> [a] -> Double
likeness eqvs a b = likenessFromErrors $ findAllErrors eqvs s
        where s = Cursor ([], []) (a, b)

-- |Compute the likeness score from the found errors while the comparison.
likenessFromErrors :: [Error a] -> Double
likenessFromErrors xs = if l > 0 then 1 - errors / l else 0
        where errors = (fromIntegral . length) $ filter (selectError) xs
              l = fromIntegral $ length xs
              selectError x = case x of
                              None _ -> False
                              Similarity _ -> False
                              _ -> True

findAllErrors :: (Eq a) => [Equivalence a] -> Cursor a -> [Error a]
findAllErrors _ (Cursor _ ([], []))= []
findAllErrors eqvs s = e : findAllErrors eqvs (rest e)
        where e = searchError eqvs s
           
searchError :: (Eq a) => [Equivalence a]-> Cursor a -> Error a
searchError _ s@(Cursor _ ([], [])) = None s
searchError eqvs s = searchSimilarityError eqvs s 
                     >>> searchInsertionError s
                     >>> searchInvertionError s
                     >>> searchDeletionError s
                     >>> searchNoError s
                     >>> searchSubstitutionError s

(>>>) :: Error a -> Error a -> Error a
a >>> b  = if (rest a) >? (rest b) then b else a
        where x >? y = let (x1, x2) = followers x
                           (y1, y2) = followers y
                       in length x1 + length x2 > length y1 + length y2
           

searchInvertionError :: (Eq a) => Cursor a -> Error a
searchInvertionError s = case C.findInvertion s of
                         Nothing -> None s
                         Just e -> Invertion e

searchDeletionError :: (Eq a) => Cursor a -> Error a
searchDeletionError s = case C.findDeletion s of
                         Nothing -> None s
                         Just e -> Deletion e

searchInsertionError :: (Eq a) => Cursor a -> Error a
searchInsertionError s = case C.findInsertion s of
                         Nothing -> None s
                         Just e -> Insertion e

searchSimilarityError :: (Eq a) => [Equivalence a] -> Cursor a -> Error a
searchSimilarityError xs s = case C.findFirstEquivalence xs s of
                             Nothing -> None s
                             Just e -> Similarity e

searchNoError :: (Eq a) => Cursor a -> Error a
searchNoError s = case C.findEquality s of
                  Nothing -> None s
                  Just e -> None e
                
searchSubstitutionError :: (Eq a) => Cursor a -> Error a                
searchSubstitutionError s = case C.findSubstitution s of
                  Nothing -> None s
                  Just e -> Substitution e
