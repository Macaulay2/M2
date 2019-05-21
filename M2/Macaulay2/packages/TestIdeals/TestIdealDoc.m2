--*************************************************
--*************************************************
--This file contains the documentation for the
--TestIdeals package.
--*************************************************
--*************************************************


document {
    Key => TestIdeals,
    Headline => "a package for calculations of singularities in positive characteristic ",
    EM "TestIdeals", " is a package for basic computations of ", TEX ///$F$///, "-singularities.
    It is focused on computing test ideals and related objects.
    It does this via ", TO "frobeniusRoot", ", which computes ", TEX ///$I^{[1/p^e]}$,///,"
    as introduced by Blickle-Mustata-Smith (this is equivalent to the image of an ideal under the Cartier
        operator in a polynomial ring).",
    BR{},BR{},
    BOLD "Notable functions:",BR{},
    UL {
      {TO "testIdeal", " computes the test ideal of a normal ", TEX ///\mathbb{Q}///, "-Gorenstein ring or pair."},
      {TO "testModule", " computes the parameter test module of a ring or pair."},
      {TO "parameterTestIdeal", " computes the parameter test ideal of a Cohen-Macaulay ring."},
      {TO "FPureModule", " computes the stable image of the trace of Frobenius on the canonical module."},
	  {TO "isFRegular", " checks if a normal ", TEX ///\mathbb{Q}///, "-Gorenstein ring or pair is ", TEX ///$F$///, "-regular."},
	  {TO "isFPure", " checks if a ring is ", TEX ///$F$///, "-pure."},
	  {TO "isFRational", " checks if a  ring is ", TEX ///$F$///, "-rational."},
	  {TO "isFInjective", " checks if a  ring is ", TEX ///$F$///, "-injective."},
	  {TO "compatibleIdeals", " finds the compatibly ", TEX ///$F$///, "-split ideals with a (near) ", TEX ///$F$///, "-splitting."},
	},
	BR{},"Consider, for instance, the test ideal of the cone over an elliptic curve.",
    EXAMPLE{"R = ZZ/5[x,y,z]/(z*y^2 - x*(x - z)*(x + z));", "testIdeal(R)" },
    BR{}, "The following example was studied by Anurag Singh when showing that ", TEX ///$F$///, "-regularity does not deform.",
    EXAMPLE{"S = ZZ/3[A,B,C,D,T];",
    "M = matrix{{A^2 + T^4, B, D}, {C, A^2, B^3 - D}};",
    "I = ideal(T) + minors(2, M);",
    "isFRegular(S/I)"},
    BR{},BR{},
	BOLD "Acknowledgements:",BR{},BR{},
	"The authors would like to thank David Eisenbud, Daniel Grayson, Anurag Singh, Greg Smith, and Mike Stillman for useful conversations and comments on the development of this package.",BR{}
}
