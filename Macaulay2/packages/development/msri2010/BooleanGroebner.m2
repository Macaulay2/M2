-- -*- coding: utf-8 -*-
newPackage(
	"BooleanGroebner",
    	Version => "0.0", 
    	Date => "January, 2010",
    	Authors => {
	     {Name => "Jane Doe", Email => "doe@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~doe/",
    	Headline => "Boolean Groebner Basis algorithm",
	AuxiliaryFiles => false, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {firstFunction, secondFunction, MyOption}
exportMutable {}

firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> (
	if n == 1
	then "Hello, World!"
	else "D'oh!"	
	)
   
-- A function with an optional argument
secondFunction = method(
     TypicalValue => ZZ,
     Options => {MyOption => 0}
     )
secondFunction(ZZ,ZZ) := o -> (m,n) -> (
     if not instance(o.MyOption,ZZ)
     then error "The optional MyOption argument must be an integer";
     m + n + o.MyOption
     )
secondFunction(ZZ,List) := o -> (m,n) -> (
     if not instance(o.MyOption,ZZ)
     then error "The optional MyOption argument must be an integer";
     m + #n + o.MyOption
     )

beginDocumentation()
document { 
	Key => PackageTemplate,
	Headline => "an example Macaulay2 package",
	EM "PackageTemplate", " is an example package which can
	be used as a template for user packages."
	}
document {
	Key => {firstFunction, (firstFunction,ZZ)},
	Headline => "a silly first function",
	Usage => "firstFunction n",
	Inputs => {
		"n" => ZZ => {}
		},
	Outputs => {
		String => {}
		},
	"This function is provided by the package ", TO PackageTemplate, ".",
	EXAMPLE {
		"firstFunction 1",
		"firstFunction 0"
		}
	}
document {
	Key => secondFunction,
	Headline => "a silly second function",
	"This function is provided by the package ", TO PackageTemplate, "."
	}
document {
	Key => (secondFunction,ZZ,ZZ),
	Headline => "a silly second function",
	Usage => "secondFunction(m,n)",
	Inputs => {
	     "m" => {},
	     "n" => {}
	     },
	Outputs => {
	     {"The sum of ", TT "m", ", and ", TT "n", 
	     ", and "}
	},
	EXAMPLE {
		"secondFunction(1,3)",
		"secondFunction(23213123,445326264, MyOption=>213)"
		}
	}
document {
     Key => MyOption,
     Headline => "optional argument specifying a level",
     TT "MyOption", " -- an optional argument used to specify a level",
     PARA{},
     "This symbol is provided by the package ", TO PackageTemplate, "."
     }
document {
     Key => [secondFunction,MyOption],
     Headline => "add level to result",
     Usage => "secondFunction(...,MyOption=>n)",
     Inputs => {
	  "n" => ZZ => "the level to use"
	  },
     Consequences => {
	  {"The value ", TT "n", " is added to the result"}
	  },
     "Any more description can go ", BOLD "here", ".",
     EXAMPLE {
	  "secondFunction(4,6,MyOption=>3)"
	  },
     SeeAlso => {
	  "firstFunction"
	  }
     }
TEST ///




  assert( booleanGroebner(x*y+x^3, x*y-y, x^2+x, y^2+y) === x+y)
  --  R = ZZ/2[x,y]
  --  I = ideal (x*y+x^3, x*y-y, x^2+x, y^2+y)
  --  J = groebnerBasis I
  -- x+y, y^2+y 
  -- In boolean should be x+y


  --assert(firstFunction 1 === "Hello, World!")
  --assert(secondFunction(1,3) === 4)
  --assert(secondFunction(1,3,MyOption=>5) === 9)
///
  
       
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

installPackage "PackageTemplate"
installPackage("PackageTemplate", RemakeAllDocumentation=>true)
check PackageTemplate

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PackageTemplate pre-install"
-- End:
  
  R = (ZZ/2) [x,y]
  I = ideal (x^2+x, y^2+y)
  R/I
  newIdeal = ideal (x*y+x^3, x*y-y, x^2+x, y^2+y)
  groebnerBasis newIdeal

  R = ZZ/2[x,y,z]/(x^2+x, y^2+y, z^2+z)
  R = ZZ/2[x,y,z]
  I = ideal (x*y+x^3, x*y-y, x^2+x, y^2+y)
  I = ideal (x^2)
  I = ideal (x^2+x, y^2 + y, z^2+z, x*y+z)
  I = ideal (x*y+z)
  gens gb I
  groebnerBasis(I)

  help gens
viewHelp Buchberger

--  Class Brp - binary representation of polynomials in F2 inherits from Polynomial
    -- This class can convert multilinear (later all) polynomials into their
    binary representation. It does addition and multiplication using binary
    representation
      * poly -> brp -> poly 
          input (multilinear poly, number of n), output bitwise rep
      * addition
      * multiplication
      * isDivisble

-- Boolean Gröbner basis: add FPs to ideal, mod out FPs (x^2 +x)

-- S-polynomials: distinguish between (FP,X) and (X,Y). For the case S(FP,X) use
"shortcut" rule, for S(X,Y) use regular lcm/ lt(X)X  +lcm/ lt(Y)Y
-- shortcut rule: S(x^2+x,fx+g) = xg+g


--  Next we implement Buchberger's Algorithm but it's using brp and the above
S-polynomials instead of normal polynomials

-- Use binary representation with bitwise computations  instead of symbolic
representation for speed up (hopefully :) )

-- need computation on bit level - has this been implemented?

"It is well-known that polynomial reduc- tion is the costliest part for any
practical implementation of Buchberger’s algorithm for Groebner basis
computation. We follow Buchberger’s algorithm in a novel way in that extremely
efficient bit operations are used for polynomial reduction."

end

QQ[x,y,z]

Z = new Type of PolynomialRing
Z = new Type of R 
Z+Z := (a,b) -> a;
Z+Z := (a,b) -> b;

ggg = new Z from x+y*z
fff = new Z from y

ggg + fff
ggg*fff

g = x+y

z + fff
z
gg = 2*x + y^2
gg
z = new Z from g




