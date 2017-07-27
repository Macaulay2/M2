--*************************************************
--*************************************************
--This file contains the documentation for the 
--Fsing package.
--*************************************************
--*************************************************


document {
    Key => TestIdeals,
    Headline => "a package for calculations of singularities in positive characteristic ",
    EM "TestIdeals", " is a package for basic computations of F-singularities.  It is focused on computing test ideals and related objects. It does this via ", TO "frobeniusRoot", " which computes ", TEX ///$I^{[1/p^e]}$///," as introduced by Blickle-Mustata-Smith (this is equivalent to the image of an ideal under the Cartier operator in a polynomial ring).",
    BR{},BR{},
    "We describe some notable functions below.",    
    BR{},BR{},
    BOLD "Notable functions:",BR{},
    UL {
      {TO "testIdeal", " compute the test ideal of a normal Q-Gorenstein ring or pair."},
      {TO "testModule", " compute the parameter test module of a ring or pair."},      
      {TO "parameterTestIdeal", " compute the parameter test ideal of a Cohen-Macaulay ring."},
      {TO "HSLGModule", " compute the stable image of the trace of Frobenius on the canonical module."},
	  {TO "isFregular", " checks if a normal Q-Gorenstein ring or pair is F-regular."},
	  {TO "isFpure", " checks if a ring is F-pure."},
	  {TO "isFrational", " checks if a  ring is F-rational."},	  
	  {TO "isFinjective", " checks if a  ring is F-injective."},	 
	  {TO "compatibleIdeals", " finds the compatibly F-split ideals with a (near) F-splitting."}, 
	},
	BR{},
	BOLD "Acknowledgements:",BR{},BR{},
	"The authors would like to thank David Eisenbud, Daniel Grayson, Anurag Singh, Greg Smith, and Mike Stillman for useful conversations and comments on the development of this package.",BR{}
}
