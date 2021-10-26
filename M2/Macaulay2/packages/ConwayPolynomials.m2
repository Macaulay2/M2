-- -*- coding: utf-8 -*-
newPackage(
	"ConwayPolynomials",
    	Version => "1.0", 
    	Date => "October 23, 2008",
    	Authors => {
	     {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~dan/",
	Keywords => {"Group Theory"},
    	Headline => "a database of Conway polynomials"
    	)
-- the data comes libflint
export "conwayPolynomial"
rawConwayPolynomial := value Core#"private dictionary"#"rawConwayPolynomial"
getCP := (p,n) -> rawConwayPolynomial (p,n,false)
Ap := memoize((p, a) -> (ZZ/p)(monoid [a]))
fix := (p,n,co,a) -> sum(#co, i -> co#i * a^i)
conwayPolynomial = method(Options=>{Variable=>"a"})
conwayPolynomial(ZZ,ZZ) := opts -> (p,n) -> (
     cp := getCP(p,n);
     if cp != {} then fix(p,n,cp,(Ap(p, opts.Variable))_0))
conwayPolynomial ZZ := opts -> q -> (
     factors := factor q;
     if #factors =!= 1 or factors#0#0 === -1
     then error "expected a power of a prime";
     conwayPolynomial(factors#0#0,factors#0#1,opts))
addHook(GaloisField,FindOne,(p,n,a) -> (
     cp := getCP(p,n);
     if cp != {} then break fix(p,n,cp,a)))
isConway := (F) -> (gens ideal ambient F)_(0,0) == sub(conwayPolynomial(F.char,F.degree, Variable=>F_0),ambient ambient F)
map(GaloisField,GaloisField) := RingMap => o -> (K,F) -> (
     p := char F;
     n := K.degree;
     m := F.degree;
     if char K =!= p 
     or n % m != 0
     then error "no map of fields exists";
     if F === K then return map(K,F,vars K);
     if not (isConway F and isConway K) then error "not implemented: maps between non-Conway Galois fields";
     map(K,F,{K_0^((p^n-1)//(p^m-1))}))
beginDocumentation()
document { Key => ConwayPolynomials,
     Headline => "database of Conway polynomials for use with GF",
     PARA {
     	  EM "ConwayPolynomials", " is a package that provides a database of Conway polynomials.
	  A Conway polynomial for a prime p and an exponent n is a particular monic polynomial 
	  whose roots are primitive elements of a finite field with p^n elements.  They can take
	  a very long time to compute, so the package comes with just a publicly available database
	  of Conway polynomials provided by Frank Luebeck."
	  },
     PARA {
	  "After the package is loaded, the function ", TO "GF", " will return Galois
	  fields presented by Conway polynomials, provided they are in the table.  Moreover,
	  ", TO "map", " can be used to produce the canonical maps between Conway Galois fields."
	  },
     PARA {
	  "The package is loaded by default when Macaulay2 starts up.  The database is loaded
	  the first time ", TO "conwayPolynomial", " is called, resulting in a brief pause."
	  },
     PARA {
	  "In this example, we show how ", TO "GF", " and ", TO "map", " behave when the package is present."
	  },
     EXAMPLE lines ///
     GF 125
     ambient oo
     map(GF 125^2, GF 125)
     isWellDefined oo
     ///
     }
document { 
     Key => {conwayPolynomial, (conwayPolynomial,ZZ,ZZ), (conwayPolynomial,ZZ)},
     Headline => "provide a Conway polynomial",
     SYNOPSIS (
     	  Usage => "conwayPolynomial(q,Variable=>a)",
	  Inputs => {
	       "q" => ZZ => {"a power of a prime number"},
	       "a" => Symbol => {"an optional input, the symbol served as variable, default a"}
	       },
	  Outputs => {
	       {"a Conway polynomial whose roots generate a field with q elements"}
	       },
	  EXAMPLE lines ///
	  conwayPolynomial 125
	  ///
	  ),
     SYNOPSIS (
     	  Usage => "conwayPolynomial(p,n,Variable=>a)",
	  Inputs => {
	       "p" => ZZ => {"a prime number"},
	       "n" => ZZ,
	       "a" => Symbol => {"an optional input, the symbol served as variable, default a"}
	       },
	  Outputs => {
	       {"a Conway polynomial whose roots generate a field with ", TEX "p^n", " elements"}
	       },
	  EXAMPLE lines ///
	  conwayPolynomial(2,20,Variable=>"b")
	  ///
	  )
     }

TEST /// -- check map(GaloisField,GaloisField)
K = GF(8,Variable=>a); 
L = GF(64,Variable=>b); 
middleK = GF(8); 
middleL = GF(64);
f1 = map(middleK,K,{middleK_0});
f3 = map(L,middleL,{L_0});
f2 = map(middleL,middleK);
assert(f3 * f2 * f1 === map(L, K));
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages  PACKAGES=ConwayPolynomials RemakePackages=true RerunExamples=true IgnoreExampleErrors=false RemakeAllDocumentation=true"
-- End:
