newPackage("GenericInitialIdeal", 
     Version => "0.1", 
     Headline => "computing generic initial ideals of ideals in a polynomial ring",
     DebuggingMode => true
     )

export (gin, lexgin, AttemptCount, example)

document {"GenericInitialIdeal",
     TT "GenericInitialIdeal", " is a package for computing generic initial ideals of
     ideals in a polynomial ring, that is, the monomial ideal of lead terms after a 
     random change of coordinates.  All of these routines are probabilistic: 
     with high probability, they give the correct answer, but it could be the case that 
     the choice of coordinates is too special."
     }

gin = method(Options => {
	  MonomialOrder => null,
	  AttemptCount => 2
	  }
     )

gin Ideal := opts -> (I) -> (
     -- For now, assume that I is an ideal in a polynomial ring
     I     
     )

lexgin = gin

example = () -> "R = ZZ/101[a..e]
I = ideal(a^2,b^2,c^2)
M = rawMatrixRandom(raw R, 1,5,1.0,0,0)
M = random(R^1, R^5)
"

document {
     Key => (gin,Ideal),
     Headline => "the generic initial ideal",
     Usage => "J = gin I",
     Inputs => {"I" => {"an ", TO Ideal, " in a polynomial ring"}},
     Outputs => {"J" => {"an ", TO Ideal, ", the generic initial ideal of ", TT "I", "."}},
     SeeAlso => "lexgin"
     }
end

restart
load "GenericInitialIdeal.m2"
needsPackage "GenericInitialIdeal"
example()
gin 3
R = ZZ/101[a..e]
I = ideal(a^2,b^2,c^2)
gin I
gin(I, AttemptCount=>3)
gin(I, MonomialOrder => {GRevLex => 5}, AttemptCount=>5)

installPackage GenericInitialIdeal
run("/usr/bin/open tmp/GenericInitialIdeal-0.1/share/doc/Macaulay2/GenericInitialIdeal/html/index.html")
run("/usr/bin/open tmp/GenericInitialIdeal-0.1/share/doc/Macaulay2/GenericInitialIdeal/html/master.html")

---
restart
needsPackage "GenericInitialIdeal"
installPackage GenericInitialIdeal
peek GenericInitialIdeal

