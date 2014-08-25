{-|
Module      : Matching.Equivalence
Description : Manages the equivalence of writings in words.
Copyright   : (c) Christian Sperandio, 2014
License     : 4-clause BSD license (with advertising clause)
Maintainer  : christian.sperandio@gmail.com

The module Matching.Equivalence defines functions to set equivalences and find them in words 
(read the Expression module documentation about word).

You find more explanations about equivalences in the Types modules. 

In the documentation below, you'll find reference to Cursor data. Take a look at Types modules
for more details.

-}
module Matching.Equivalence where

import Matching.Types (Cursor(..), Equivalence(..))

                       
{-|
  The findEauivalence function checks if the current cursor is at the place of
  an equivalence.
  
  The first argument of this function is the tested equivalence.
  The second one is the current cursor.
  
  This function returns either a new cursor if the equivalence was found or
  nothing is the equivalence wasn't.
  
-}
findEquivalence :: (Eq a) => Equivalence a -> Cursor a -> Maybe (Cursor a)
findEquivalence e s = case validateSearch e s of
                      Nothing -> Nothing
                      Just e' -> if correctPrevious e' && correctFollowers e' 
                                 then Just $ jumpEquivalence s e'
                                 else Nothing
    where correctPrevious x = checkPrefixes (previous s) (prefix x)
          correctFollowers x =  checkSuffixes (followers s) (suffixes x)

{- |
   The define equivalence (<->) function builds an Equivalence data.
   
   The two first arguments are the word-parts equivalence. For example, we
   can have the string "TCH" and "CH". Or for French words, we can have those
   equivalence: "O" <-> "EAU", "AU" <-> "EAU", and "O" <-> "AU".
-}
(<->) :: (Eq a) => [a] -> [a] -> Maybe (Equivalence a)
xs@(x:xs') <-> ys@(y:ys') = selectEquivalence (searchNestedEquivalence xs ys []) simpleEquivalence
    where
        searchNestedEquivalence [] _ _ = Nothing
        searchNestedEquivalence _ [] _ = Nothing
        searchNestedEquivalence (x:xs) (y:ys) pre = if x == y
                                                    then searchNestedEquivalence xs ys (pre ++ [x])
                                                    else Just Equivalence { breaker = (x, y), 
                                                                            prefix = pre,
                                                                            suffixes = (xs, ys) 
                                                                          }
        simpleEquivalence = Just Equivalence { breaker = (x, y), prefix = [], suffixes = (xs', ys') }
        
_ <-> _ = Nothing


checkSuffixes :: (Eq a) => ([a],[a]) -> ([a], [a]) -> Bool
checkSuffixes (x,y) (x',y') = startsWith (tail x) x' && startsWith (tail y) y'

checkPrefixes :: (Eq a) => ([a],[a]) -> [a] -> Bool
checkPrefixes (x,y) p = endsWith x p && endsWith y p

jumpEquivalence :: Cursor a -> Equivalence a -> Cursor a
jumpEquivalence s@(Cursor (a,b) (c,d)) (Equivalence _ _ (g,h)) = s { previous = updatedPrevious, followers = updatedFollowers }
    where (e, f) = (head c, head d)
          updatedPrevious = (a ++ e : g, b ++ f : h)
          updatedFollowers = (drop (1 + length g) c, drop (1 + length h) d)

endsWith :: (Eq a) => [a] -> [a] -> Bool
endsWith a t = startsWith (reverse a) (reverse t)

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith a@(x:xs) h@(y:ys) | length h > length a = False
                             | otherwise = if x == y then startsWith xs ys else False
                                 

selectEquivalence :: Maybe (Equivalence a) -> Maybe (Equivalence a) -> Maybe (Equivalence a)
selectEquivalence Nothing a = a
selectEquivalence a _ = a

validateSearch :: (Eq a) => Equivalence a -> Cursor a -> Maybe (Equivalence a)
validateSearch e@(Equivalence (a,b) _ _) (Cursor _ (f,f')) = if validCursor
                                                               then Just $ orderEquivalence (c,d) e
                                                               else Nothing
    where (c,d) = (head f, head f')
          validCursor = a == c && b == d || a == d && b == c

orderEquivalence :: (Eq a) => (a, a) -> Equivalence a -> Equivalence a
orderEquivalence (a,b) eq@(Equivalence (c,d) _ (e,f)) = if a == c && b == d
                                                        then eq
                                                        else eq { breaker = (d,c), suffixes = (f,e) }
