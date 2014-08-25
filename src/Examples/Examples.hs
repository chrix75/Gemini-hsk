{-|
  The module Examples contains a list of functions gives examples of likeness
  function call in different configuration.
  
  For the function with a name is ended by a letter (like compare3b), 
  this function is an extension case of the no-letter function.
  
-}
module Examples.Examples where

import qualified Gemini.Expression as Ex
import qualified Gemini.Equivalence as Eq

-- |This function returns 1 because the 2 expressions are strictly the same.
compare1 :: Double
compare1 = Ex.likeness [] [] e1 e2
        where e1 = Ex.Expr $ words "ibm software corporate"
              e2 = Ex.Expr $ words "ibm software corporate"

-- |This function returns 1 because the 2 expressions are the same even is the words order is not the same.
compare2 :: Double
compare2 = Ex.likeness [] [] e1 e2
        where e1 = Ex.Expr $ words "donald knuth"
              e2 = Ex.Expr $ words "knuth donald"

-- |This function returns 0.8571428571428572 because the term corporate is missing.
compare3 :: Double
compare3 = Ex.likeness [] [] e1 e2
        where e1 = Ex.Expr $ words "ibm software"
              e2 = Ex.Expr $ words "ibm software corporate"

-- |This function returns 1 because here the corporate is defined as a weak word.
compare3b :: Double
compare3b = Ex.likeness ["corporate"] [] e1 e2
        where e1 = Ex.Expr $ words "ibm software"
              e2 = Ex.Expr $ words "ibm software corporate"
              
-- |This function returns 0.9285714285714286 because there is an inversion error.
compare4 :: Double
compare4 = Ex.likeness [] [] e1 e2
        where e1 = Ex.Expr $ words "albert einstein"
              e2 = Ex.Expr $ words "albert einsteni"

-- |This function returns  because of errors in the first name.
compare5 :: Double
compare5 = Ex.likeness [] [] e1 e2
        where e1 = Ex.Expr $ words "jaquie kennedy"
              e2 = Ex.Expr $ words "kennedy jacky"
              
-- |This function returns 1 because 'quie' and 'cky' are set as equivalent.
compare5b :: Double
compare5b = Ex.likeness [] eqvs e1 e2
        where e1 = Ex.Expr $ words "jaquie kennedy"
              e2 = Ex.Expr $ words "kennedy jacky"
              eqvs = Eq.buildEquivalences [("quie", "cky")]

-- |This function returns 0.6642857142857144
compare6a :: Double
compare6a = Ex.likeness [] [] e1 e2
        where e1 = Ex.Expr $ words "the schumea adventure corp sa"
              e2 = Ex.Expr $ words "schumee adventure"
              
-- |This function returns 0.7142857142857143
compare6b :: Double
compare6b = Ex.likeness [] eqvs e1 e2
        where e1 = Ex.Expr $ words "the schumea adventure corp sa"
              e2 = Ex.Expr $ words "schumee adventure"
              eqvs = Eq.buildEquivalences [("sch", "sh"), ("ea", "ee")]

-- |This function returns 0.9285714285714286
compare6c :: Double
compare6c = Ex.likeness ["sa", "corp", "of", "the"] [] e1 e2
        where e1 = Ex.Expr $ words "the schumea adventure corp sa"
              e2 = Ex.Expr $ words "schumee adventure"

-- |This function returns 1 
compare6d :: Double
compare6d = Ex.likeness ["sa", "corp", "of", "the"] eqvs e1 e2
        where eqvs = Eq.buildEquivalences [("sch", "sh"), ("ea", "ee")]
              e1 = Ex.Expr $ words "the schumea adventure corp sa"
              e2 = Ex.Expr $ words "schumee adventure"
              
-- |This function returns 0.10267857142857145 
compare7 :: Double
compare7 = Ex.likeness ["sa", "corp", "corporate", "of", "the"] eqvs e1 e2
        where eqvs = Eq.buildEquivalences [("sch", "sh"), ("ea", "ee")]
              e1 = Ex.Expr $ words "ibm software"
              e2 = Ex.Expr $ words "schumee adventure"
