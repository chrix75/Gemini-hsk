Gemini-hsk
==========

Purpose
-------

The Gemini package provides functions to search the likeness between expressions.
These functions may be used for data matching to mark duplicate values.

History
-------
My work is to write programs which processes data to compare them each other, search duplicates in database or give score about data quality. When I started learning Haskell, I had the idea to develop a library about what I know well :)

The Gemini functions offer you some of my knowledge about data processing. I hope these functions will be useful for you.

Basics
------
###Before running, learn to walk

An expression is a container which contains one or more words. So, compare two expressions is compare their words each other (the order is not important). When two words are compared, we get a score is the representation of the likeness of these words. A score is a value between 0 and 1. 1 for a strong likeness and 0 means the words are different.

The epxressions score is found by taking the best score for each words in the expressions.

Even if this library is often used to compare string expressions, it's not a mandatory. A word is a list of any item whose type implements the Eq and Ord classes.


###Words likeness

To compute the likeness of two words, the library searches the errors between them. The errors are managed by the library are:

+ Invertion of 2 characters
+ Insertion of one character
+ Deletion of one character
+ Substitution of a character by another

The likeness is deduced from the number of found errors in front of the number of comparisons.

There is an another type of "error": the equivalence (or similarity). An equivalence defines many writing ways are similar.
For example, "CH" and "TCH" can be set as equivalent thus, "CHOOPY" and "TCHOOPY" mark an equivalence and not an insertion. The difference of equilence with other errors is an equivalence is not seen like an real error.
  
In other terms, the score for "CHOOPY" vs. "TCHOOPY" is 1, the maximal value.An other example of equivalence is, for French data, "EAU" is equivalent to "O" and "AU". Thus, "CHATEAU" is seen as strict duplicate of "CHATO" or "CHATAU".

####Why not the Levenshtein's distance?

The likeness of 2 words doesn't use the Levenshtein's distance because its result is not sharp enough. And, with the Levenshtein's distance it's not possible to manage the equivalence efficiently.

###Jaccard's distance.

The likeness score is based upon the Jaccard's distance. It's simple :) 

The calculation is: d = 1 - (e / t) <br>
where <br>
e = number of errors<br>
t = number of comparisons

