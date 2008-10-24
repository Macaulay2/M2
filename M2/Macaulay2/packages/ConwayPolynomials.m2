newPackage(
	"ConwayPolynomials",
    	Version => "1.0", 
    	Date => "October 23, 2008",
    	Authors => {
	     {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~dan/",
    	Headline => "an optional database of Conway polynomials"
    	)
-- the data comes from http://www.math.rwth-aachen.de:8001/~Frank.Luebeck/data/ConwayPol/
-- the data file is http://www.math.rwth-aachen.de:8001/~Frank.Luebeck/data/ConwayPol/CPimport.txt
-- or http://www.math.rwth-aachen.de:8001/~Frank.Luebeck/data/ConwayPol/CPimport.txt.gz
export conwayPolynomial
fn  := currentFileDirectory | "ConwayPolynomials/ConwayPolynomials.txt"
fix := (p,n,co) -> (
     A := (ZZ/p)(monoid [global a]);
     a := A_0;
     a^n + sum(#co, i -> co#i * a^i))
getCP := memoize(
     () -> (
	  stderr << "--loading file " << fn << endl;
	  hashTable apply( lines get fn,
	       x -> (
	       	    x = value x;
	       	    ((x#0,x#1),drop(x,2))))))
conwayPolynomial = method()
conwayPolynomial(ZZ,ZZ) := (p,n) -> fix(p,n,(getCP())#(p,n))




