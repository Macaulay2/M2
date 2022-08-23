-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2019  Juliette Bruce, Daniel Erman, Steve Goldstein, Jay Yang
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- PURPOSE : Data for Veronese embeddings of projective space
--
--
-- PROGRAMMERS : Juliette Bruce, Daniel Erman, Steve Goldstein, Jay yang
--
--
-- UPDATE HISTORY #0 - Program created.
--
--
-- UPDATE HISTORY #1 - April 2019 - Juliette Bruce: Began trying to prepare
-- package for eventual publication. Adding tests, comments, documentation,
-- cleaning up code, etc.
--
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



newPackage("SchurVeronese",
    Version => "1.1",
    Date => "13 May 2019",
    Headline => "Data for Veronese embeddings of projective space",
    Authors => {
        {Name => "Juliette Bruce",           Email => "jebruce2@wisc.edu",       HomePage => "https://juliettebruce.github.io"},
        {Name => "Daniel Erman",             Email => "derman@math.wisc.edu",    HomePage => "http://www.math.wisc.edu/~derman/"},	     
        {Name => "Steve Goldstein",          Email => "sgoldstein@wisc.edu",   HomePage => ""},
	{Name => "Jay Yang",                 Email => "jkyang@umn.edu",   HomePage => "http://www-users.math.umn.edu/~jkyang/"}
	},
   AuxiliaryFiles => true,
   Certification => {
	"journal name" => "The Journal of Software for Algebra and Geometry",
	"journal URI" => "http://j-sag.org/",
	"article title" => "The Schur–Veronese package in Macaulay2",
	"acceptance date" => "5 May 2021",
	"published article URI" => "https://msp.org/jsag/2021/11-1/p09.xhtml",
	"published article DOI" => "10.2140/jsag.2021.11.83",
	"published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x09-SchurVeronese.zip",
	"repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/SchurVeronese.m2",
	"release at publication" => "a76b7f70a983a7a0acaf2b0d84bdecc27ba36157",	    -- git commit number in hex
	"version at publication" => "1.1",
	"volume number" => "11",
	"volume URI" => "https://msp.org/jsag/2021/11-1/"
	}
   )

export {
  "makeBettiTally", 
  "multiBetti",
  "schurBetti", 
  "totalBetti", 
  "totalBettiTally", 
  "dominantWeightsBetti", 
  "lexWeightsBetti", 
  "numDistinctRepsBetti",
  "numRepsBetti", 
  "bsCoeffs" 
  }

--------------------------------------------------------------------
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
--------------------------------------------------------------------


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: file
-----        file should be a filename for a file with betti data
-----
----- OUTPUT: The contents of the file with value applied.
-----
----- DESCRIPTION: This function is used to load values from the
----- appropriate betti data file while caching the result
--------------------------------------------------------------------
--------------------------------------------------------------------
readBettiFile := memoize (file -> (
    value get file
    ))

--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (file,var)
-----        file should be a filename for a file with betti data
-----        var should be a symbol or a string to be evaluated
-----
----- OUTPUT: The desired value
-----
----- DESCRIPTION: This function is used to load values from the
----- appropriate betti data file
--------------------------------------------------------------------
--------------------------------------------------------------------
withBettiFile := (file,var) -> (
    ret := readBettiFile file;
    ret#var
    )



--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: A hash table representing a Betti table 
-----
----- OUTPUT: The BettiTally for the inputted Betti table
-----
----- DESCRIPTION: This function is used to convert a hash table
----- representing a Betti Table into the more familiar BettiTally.
----- The input hash table is assumed to have its keys being pairs
----- (p,q) such that H#(p,q)=>{K_{p,q}(M)}.
--------------------------------------------------------------------
--------------------------------------------------------------------
makeBettiTally = method();
makeBettiTally HashTable := H ->(
    new BettiTally from apply(keys H, h-> (h_0,{h_0+h_1},h_0+h_1)=> H#h)
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: (true,null) or (false, error message)
-----
----- DESCRIPTION: This unexported function is used to check whether 
----- we have  data for a particular case. If we have data for O(b)
----- on P^n embedded by O(d) then it returns (true,null) otherwise it 
----- returns (false, error message). This is primarily used as a
----- way forother functions to quickly produce error messages
--------------------------------------------------------------------
--------------------------------------------------------------------
rangeCheck = method();
rangeCheck (ZZ,ZZ,ZZ) :=(d,n,b) ->(
    message := (true,null);
    if n > 2 or n < 1 then message = (false, error "Dimension of projective space needed to be n = 1 or 2");
    if b >= d then message = (false, error "Degree of auxiliary line bundle required to be strictly smaller than degree of embedding");    
    if b < 0 then message = (false, error "Degree of auxiliary line bundle required to be positive");    
    if n == 1 and d > 10 or d < 2 then message = (
	false, error "If dimension of projective space = 1 then degree of embedding must satisfy 2 <= d <= 10");
    if n == 2 and d > 8 or d < 2 then message = (
	false, error "If dimension of projective space = 2 then degree of embedding must satisfy 2 <= d <= 8");
    message
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: string
-----
----- DESCRIPTION: This non-exported function outputs the path for 
----- the auxiliary file containing the data for O(b) on P^n embedded
----- by O(d) as a string. It is only used internally to ensure that
----- other functions look for the data files in the correct path.
----- It seems curDir needs to be defined outside of the function.
--------------------------------------------------------------------
--------------------------------------------------------------------
curDir := currentFileDirectory;
getFileName := (d,n,b) ->(curDir|"SchurVeronese/bettiP"|toString(n)|"_" | toString(d) | "_"|toString (b)|".m2")


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing the multigraded Betti data for 
----- O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a hashtable containing the 
----- multigraded Betti data for O(b) on P^n embedded by O(d). The
----- keys for this hash table are pairs (p,q) corresponding to the
----- Betti number K_{p,q}(n,b;d). Notice that the multigraded Betti
----- data is stored via a multigraded Hilbert series. See
----- [Sec 1.1, BEGY] for definitions.
--------------------------------------------------------------------
--------------------------------------------------------------------
multiBetti = method();
multiBetti (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;
    withBettiFile(getFileName(d,n,b),"mb")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing the Schur Betti data for 
----- O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a hashtable containing the 
----- Schur Betti data for O(b) on P^n embedded by O(d). The
----- keys for this hash table are pairs (p,q) corresponding to the
----- Betti number K_{p,q}(n,b;d). Notice that the Schur Betti data
----- is stored as a list of pairs ({a,b,c},m) where {a,b,c} is the
----- partition describing the Schur functor and m is the multiplicity
----- with which this Shur functor appears in K_{p,q}(n,b;d). See
----- [Sec 1.2, BEGY] for definitions.
---------------------------------------------------------------------
---------------------------------------------------------------------
schurBetti = method();
schurBetti (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;
    withBettiFile(getFileName(d,n,b),"sb")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing the total graded Betti data for 
----- O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a hash table containing the 
----- total graded Betti data for O(b) on P^n embedded by O(d). The
----- keys for this hash table are pairs (p,q) with the corresponding
----- value being the Betti number dim K_{p,q}(n,b;d). This function
----- is the same as totalBettiTally expect that the output is a hash table
----- instead of a BettiTally. See [Sec 1.1, BEGY] for definitions.
---------------------------------------------------------------------
---------------------------------------------------------------------
totalBetti = method();
totalBetti (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;    
    withBettiFile(getFileName(d,n,b),"tb")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A BettiTally containing the total graded Betti data for 
----- O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a BettiTally containing the 
----- total graded Betti data for O(b) on P^n embedded by O(d). This 
----- function is the same as totalBetti expect that the output is a
----- BettiTally instead of a hash table. See [Sec 1.1, BEGY] for 
----- definitions.
---------------------------------------------------------------------
---------------------------------------------------------------------
totalBettiTally = method();
totalBettiTally (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;
    makeBettiTally totalBetti(d,n,b)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing lists of the dominant Schur 
----- functors appearing in the decomposition of the graded Betti 
----- numbers for O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a hash table whose keys 
----- are pairs (p,q) with the corresponding value being a list
----- of the Schur functors of dominant weight appearing in the 
----- decomposition of K_{p,q}(n,b;d). Note the Schur functors 
----- are recorded via their weights {a,b,c}. See [Sec 1.3, BEGY] 
----- for definitions.
---------------------------------------------------------------------
---------------------------------------------------------------------
dominantWeightsBetti = method();
dominantWeightsBetti (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;
    withBettiFile(getFileName(d,n,b),"dw")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing lists of the lex-leading
----- weights of the Schur functors appearing in the decomposition 
----- of the graded Betti numbers for O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a hash table whose keys 
----- are pairs (p,q) with the corresponding value being a list
----- of the Schur functors of lex-leading weight appearing in the 
----- decomposition of K_{p,q}(n,b;d). Note the Schur functors 
----- are recorded via their weights {a,b,c}. See [Sec 1.3, BEGY] 
----- for definitions.
---------------------------------------------------------------------
---------------------------------------------------------------------
lexWeightsBetti = method();
lexWeightsBetti (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;    
    --A := QQ[t_0,t_1,t_2, MonomialOrder => Lex];
    withBettiFile(getFileName(d,n,b),"lw")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing the number of distinct Schur 
----- functors appearing in the decomposition of the graded Betti 
----- numbers for O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a hash table whose keys 
----- are pairs (p,q) with the corresponding value being the number
----- of distinct Schur functors appearing in the decomposition of
----- K_{p,q}(n,b;d). See [Sec 1.2, BEGY] for definitions.
---------------------------------------------------------------------
---------------------------------------------------------------------
numDistinctRepsBetti = method();
numDistinctRepsBetti  (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;
    withBettiFile(getFileName(d,n,b),"nr")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing the number of Schur functors
----- appearing in the decomposition of the graded Betti numbers 
----- for O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a hash table whose keys 
----- are pairs (p,q) with the corresponding value being the number
----- of Schur functors counted with multiplicity appearing in the 
----- decomposition of K_{p,q}(n,b;d). See [Sec 1.2, BEGY] for
----- definitions.
---------------------------------------------------------------------
---------------------------------------------------------------------
numRepsBetti = method();
numRepsBetti  (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;
    withBettiFile(getFileName(d,n,b),"nrm")
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A hash table containing the errors found when computing
----- the multigraded Betti numbers for O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: As the methods we use to compute multigraded 
----- Betti numbers are numerical in nature there is room for error,
----- and so, we have implemented post processing to catch errors. 
----- This function returns a hash table whose keys are pairs (p,q)
----- with the corresponding value being a multigraded Hilbert
----- series recording the errors encountered when computing the
----- multigraded Betti numbers for K_{p,q}(n,b;d). See [Sec 5.2, BEGY]
----- for a discussion on error processing.
---------------------------------------------------------------------
---------------------------------------------------------------------
errorBetti = method();
errorBetti  (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheck(d,n,b);
    if message_0 == false then return message_1;
    withBettiFile(getFileName(d,n,b),"er")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: (true,null) or (false, error message)
-----
----- DESCRIPTION: This unexported function is used to check whether 
----- we have Boij-Soederberg data for a particular case. If we have data for O(b)
----- on P^n embedded by O(d) then it returns (true,null) otherwise it 
----- returns (false, error message). This is primarily used as a
----- way for other functions to quickly produce error messages
--------------------------------------------------------------------
--------------------------------------------------------------------
rangeCheckBS = method();
rangeCheckBS (ZZ,ZZ,ZZ) :=(d,n,b) ->(
    message := (true,null);
    if n > 2 or n < 1 then message = (false, error "Dimension of projective space needed to be n = 1 or 2");
    if b >= d then message = (false, error "Degree of auxiliary line bundle required to be strictly smaller than degree of embedding");    
    if b < 0 then message = (false, error "Degree of auxiliary line bundle required to be positive");    
    if n == 1 and d > 10 or d < 2 then message = (
	false, error "If dimension of projective space = 1 then degree of embedding must satisfy 2 <= d <= 10");
    if n == 2 and d > 8 or d < 2 then message = (
	false, error "If dimension of projective space = 2 then degree of embedding must satisfy 2 <= d <= 8");
    if n == 2 and (d == 6 and b > 3)  then message = (
	false, error "If dimension of projective space = 2 and the degree of embedding d = 6 then 0 <= b <= 3");
    if n == 2 and (d > 6)  then message = (
	false, error "If dimension of projective space = 2 then degree of embedding must satisfy 2 <= d < 6");
    message
    )



--------------------------------------------------------------------
--------------------------------------------------------------------
----- INPUT: (d,n,b) 
-----
----- OUTPUT: A list of the Boij-Soederberg coefficients for the 
----- decomposition of the Betti table of O(b) on P^n embedded by O(d).
-----
----- DESCRIPTION: This function returns a list of the Boij-Soederberg 
----- coefficients for the  decomposition of the Betti table of O(b) on
----- P^n embedded by O(d). See [Sec 6.3, BEGY] for definitions. 
---------------------------------------------------------------------
---------------------------------------------------------------------
bsCoeffs = method();
bsCoeffs  (ZZ,ZZ,ZZ) := (d,n,b) ->(
    message := rangeCheckBS(d,n,b);
    if message_0 == false then return message_1;    
    withBettiFile(getFileName(d,n,b),"bs")
    )


--------------------------------------------------------------------
--------------------------------------------------------------------
----- Beginning of the tests and the documentation
--------------------------------------------------------------------
--------------------------------------------------------------------

load ("./SchurVeronese/tests.m2")
beginDocumentation()
load ("./SchurVeronese/doc.m2")
