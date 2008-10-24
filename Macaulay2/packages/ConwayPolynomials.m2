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
getCP := memoize(
     () -> (
	  stderr << "--loading file " << fn << endl;
	  hashTable apply( lines get fn,
	       x -> (
	       	    x = value x;
	       	    ((x#0,x#1),drop(x,2))))))
Ap := memoize(p -> (ZZ/p)(monoid [global a]))
fix := (p,n,co,a) -> a^n + sum(#co, i -> co#i * a^i)
conwayPolynomial = method()
conwayPolynomial(ZZ,ZZ) := (p,n) -> if (getCP())#?(p,n) then fix(p,n,(getCP())#(p,n),(Ap p)_0)
conwayPolynomial ZZ := q -> (
     factors := factor q;
     if #factors =!= 1 or factors#0#0 === -1
     then error "expected a power of a prime";
     conwayPolynomial(factors#0#0,factors#0#1))
addHook(GaloisField,FindOne,(p,n,a) -> if (getCP())#?(p,n) then break fix(p,n,(getCP())#(p,n),a))
