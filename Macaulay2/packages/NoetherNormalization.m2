--=========================================================================--

newPackage(
     "NoetherNormalization",
     Version => "0.1", 
     Date => "Jan 18, 2007",
     Authors => {
	  {Name => "Nathaniel Stapleton", Email => "nstaple2@math.uiuc.edu"},
	  {Name => "Bart Snapp", Email => "snapp@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~snapp/"}
	  },
     Headline => "computes Noether Normalization",
     DebuggingMode => true
     )

--=========================================================================--
     
export{} -- if the new routines which you are adding have new
-- names, then they need to be exported; otherwise they should not be
-- exported
        
--=========================================================================--
-- experiments:

-- need good and extreme examples of NNL. 

clearAll
installPackage "NoetherNormalization"

B = ZZ/7[x,y]/(y^2)
k = coefficientRing B
A = k[flatten entries vars B]
dim(B) -- may be useful in constructing a for loop
V = ideal vars A
V = ideal vars B
G = ideal B -- identify monic or make monic elements
V = eliminate(y,V) -- remove y terms Does it only work for poly rings???


B = ZZ/7[x,y]/(x*y+x+2)
k = coefficientRing B
A = k[flatten entries vars B]
dim(B) -- may be useful in constructing a for loop
V = ideal vars A
G = ideal B -- identify monic or make monic elements
gens G
V = eliminate(y,V) -- r



--=========================================================================--
	
	
--=========================================================================--

--code

--=========================================================================--

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

--docs

--=========================================================================--
