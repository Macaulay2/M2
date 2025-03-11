document {
    Key => {
	newCoordinateSystem,
	(newCoordinateSystem, PolynomialRing, Matrix)
    },
    Headline => "change variables",
    TT "newCoordinateSystem(S,m)", " -- takes a one-rowed matrix ", TT "m", " of
    independent linear forms over a ring ", TT "R", " and returns a pair
    ", TT "(f,g)", ", where ", TT "f", " is a ring map given by some linear change 
    of coordinates from ", TT "R", " to ", TT "S", " which sends the last variables 
    of ", TT"R", " to the forms in ", TT "m", ", and ", TT "g", " is the inverse 
    of ", TT "f", ".",
    PARA{},
    "The ring ", TT "S", " should have the same number of variables as 
    ", TT "S", ".",
    EXAMPLE {
	"R = ZZ/101[a..d]",
	"S = ZZ/101[p..s]",
	"(f,g) = newCoordinateSystem(S,matrix{{a+2*b,3*c-d}});",
	"f",
	"g"
    },
}
