{-|
Module      : Matching.Expression
Description : Searches the likeness between 2 expressions.
Copyright   : (c) Christian Sperandio, 2014
License     : 4-clause BSD license (with advertising clause)
Maintainer  : christian.sperandio@gmail.com

This module compares 2 expressions to provide a likeness score. 
This score is between 0 and 1. When the score value is 1, the 2 expressions are
strictly the same and 0 means the expressions are different.

An expression is any data is compound by an array of items. An item can be a
character but it can be any element of classes Eq and Ord. 
If the item type is character then an expression is a group of words.

In the function description, the term 'word' defines [a].

This module was written to compare fields to find duplicate information.
-}
module Gemini.Expression (likeness, Expr(..)) where

import qualified Gemini.Comparison as C
import qualified Data.Map as M
import Gemini.Types (Equivalence(..))

data Expr a = Expr [[a]]

data Result a = Result {
                       terms :: ([a], [a]),
                       mark :: Double
                } deriving (Show)

{-|
  The likeness function is the main function which provides the likeness score.
  It takes 4 arguments. The first one is a list of weak words. A weak word is a
  word isn't used for the score computation. In other terms, a weak word is an
  optional word may be not present in expressions.
  
  The second argument is a list of equivalences. For more details about equivalences,
  read the Equivalence module documentation.
  
  The 2 last arguments are the compared expressions.
  
  The result of the function is the score saved into a double value.
  
  The comparison made in this function doesn't take care about the words order. 
  Thus, if you compare 2 person data like "christian sperandio" vs. "sperandio christian"
  or "sperandio christian" vs. "christian sperandio", these comparaisons give the same
  result. 
  
  Two examples showing the impact of weaks words below:
  
  >>> likeness [] [] (Expr (words "ibm software corporate")) (Expr (words "ibm software"))
  0.8571428571428572
  
  >>> likeness ["corporate"] [] (Expr (words "ibm software corporate")) (Expr (words "ibm software"))
  1.0
   
-}
likeness :: (Eq a, Ord a) => [[a]] -> [Equivalence a ] -> Expr a -> Expr a -> Double
likeness weaks eqvs a b = (computeAverage e1 + computeAverage e2) / 2
        where e1 = buildBestTree r1
              e2 = buildBestTree r2
              res = compareExpressions weaks eqvs a b
              r1 = resultToMarksList res (fst)
              r2 = resultToMarksList res (snd)
              
-- |The allComparaisons functions returns all possible comparaisons for 2 lists of words.              
allComparisons :: [[a]]-> [[a]] -> [([a], [a])]
allComparisons a b = dispatch a b b
        where dispatch (_:xs) [] b = dispatch xs b b
              dispatch [] _ _ = [] 
              dispatch a@(x:_) (y:ys) b = (x, y) : dispatch a ys b

{-|
  The compareExpressions function compares the expressions words each other (except the weak words).
  The arguments of this function are the same as the likeness function.
  
  This function returns a list of Result data. 
-}
compareExpressions :: (Eq a) => [[a]] -> [Equivalence a ] -> Expr a -> Expr a -> [Result a]
compareExpressions weaks eqvs (Expr x) (Expr y) = map cmp $ filter (\(t, t') -> t `notElem` weaks && t' `notElem` weaks) $ allComparisons x y
        where cmp (t, t') = Result { terms = (t, t'), mark = C.likeness eqvs t t' }

-- | Remove the weak words from the comparisons todo list.
removeWeakWords :: (Eq a) => [([a], [a])] -> [[a]] -> [([a], [a])]
removeWeakWords rs ws = foldl remove [] rs
        where remove acc r@(t, t') = if t `elem` ws || t' `elem` ws
                                     then acc
                                     else acc ++ [r]

-- | Converts a list of Result data to a list will be used to build a tree to compute the final score.        
resultToMarksList :: [Result a] -> (([a], [a]) -> [a]) -> [([a], Double)]
resultToMarksList rs f = map extract rs
        where extract (Result ts m) = (f ts, m)
        
-- | Builds a tree whose each item defines the best word likeness score for every word epxressions.
buildBestTree :: (Ord a) => [([a], Double)] -> M.Map [a] Double

buildBestTree [] = M.empty :: M.Map [a] Double

buildBestTree ((k, v):xs) = insertMarkTree xs (M.singleton k v) 
        where  insertMarkTree [] t = t
               insertMarkTree ((k, v):xs) t = insertMarkTree xs $ M.insertWith (\old new -> max old new) k v t

-- | Computes the average of each item of a tree.
computeAverage :: M.Map [a] Double -> Double
computeAverage m = if l == 0 then 0 else M.fold (\v a -> v + a) 0 m / l
        where l = fromIntegral (length (M.keys m))
        
        