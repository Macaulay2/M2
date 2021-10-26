

document {
    Key => FrobeniusThresholds,
    Headline => "a package for computing F-pure thresholds and related invariants",
    EM "FrobeniusThresholds", " is a package for computing ", TEX ///$F$///, "-pure thresholds,
    ", TEX ///$F$///, "-jumping exponents, and related numerical measures of singularity in positive characteristic.",
    BR{},BR{},"The Frobenius endomorphism on a ring of prime characteristic ", TEX ///$p > 0$///,", which sends a ring element to its ", TEX ///$p$///,"-th power, is a fundamental tool in positive characteristic commutative algebra.  Kunz showed that regularity is characterized by the behavior of this map, and since then many other properties of the Frobenius map have been used to measure the 'severity' of singularities in commutative algebra. The ", TEX ///$F$///, "-pure threshold is a prominent
    object in this realm.", BR{},BR{},
    "This package relies heavily on the ", TO "TestIdeals", " package. Many special cases (binomials, diagonal polynomials, etc.) are", EM " seamlessly ",
    "handled by using special algorithms found in ", HREF{"https://doi.org/10.1090/S0002-9939-2014-12260-X", "Hernández"}, ", ",HREF{"https://doi.org/10.1090/S0002-9939-2014-11941-1", "Hernández"}, ", and ", HREF{"https://www.sciencedirect.com/science/article/pii/S0747717116300347","Hernández-Teixeira"},
    ".", 
    BR{},BR{},
    BOLD "Notable functions:",BR{},
    UL {
      {TO "fpt", " computes (or estimates) the ", TEX ///$F$///, "-pure threshold."},
      {TO "isFJumpingExponent", " checks whether a given number is an ", TEX ///$F$///, "-jumping exponent."},
      {TO "compareFPT", " determines whether the given number is greater than, less than, or equal to the ", TEX ///$F$///, "-pure threshold."},
      {TO "frobeniusNu", " is a function whose normalized value provides a canonical estimate for the ", TEX ///$F$///, "-pure threshold, ", TEX ///$F$///, "-thresholds, and more."}
	},
	"The following example demonstrates some of the functionality of this package.",
 BR{},BR{},
    EXAMPLE{"p = 131;","R = ZZ/p[x,y];","f = x^13 - y^5;","c = fpt(f)","compareFPT(c - 1/p^2, f)","compareFPT(c, f)","compareFPT(c + 1/p, f)","isFJumpingExponent(36/65, f)"},
    BR{},BR{},
    BOLD "Acknowledgements:",BR{},BR{},
    "The authors would like to thank David Eisenbud, Daniel Grayson, Anurag Singh, Greg Smith, and Mike Stillman for useful conversations and comments on the development of this package.",BR{},BR{},
    BOLD "Contributors", BR{}, BR{},
    "We sincerely thank the following people, who contributed code to this package.",
       UL {
        {"Erin Bela"},
        {HREF{"https://nu.edu.kz/faculty/zhibek-kadyrsizova","Zhibek Kadyrsizova"}},
        {HREF{"http://www.katzman.staff.shef.ac.uk/", "Mordechai Katzman"}},
        {HREF{"https://www.hood.edu/academics/faculty/sara-malec","Sara Malec"}},
        {HREF{"http://www.math.utah.edu/~robinson/", "Marcus Robinson"}}
    }
}
