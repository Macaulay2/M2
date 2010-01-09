-- -*- coding: utf-8 -*-
newPackage(
	"BitwiseRepresentationPolynomials",
    	Version => "0.1", 
    	Date => "April 28, 2005",
    	Authors => {
	     {Name => "Beth, Franzi, Samuel", Email => ""}
	     },
    	HomePage => "http://",
    	Headline => "Computation for polynomials in ZZ/2 using binary
      representation",
	AuxiliaryFiles => false, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {+,*, New}
exportMutable {}

Brp = new Type of List -- this is not quite right yet

-- Convert regular polynomial into its binary representation
convert = method(TypicalValue => Brp)
convert := f -> ( exponents f)

-- Addition: concatenate and eliminate double monomials 
   -- delete doubles, by using tally and check for `odd/even
Brp + Brp := Brp => (a,b) -> new Brp from keys select(tally a + tally b, odd)

-- Multiplication: bitwise OR
Brp * Brp := Brp => (a, m) ->  
  new Brp from apply (#a, i  -> brpOR( a#i, m))

-- bitwise OR for 2 monomials
brpOR (Brp, Brp) := Brp => (a,b) -> 
  apply (#a, i -> max (a#i, b#0#i) )
brpOR (List, Brp) := Brp => (a,b) -> 
  apply (#a, i -> max (a#i, b#0#i) )


end

s = { {aa,ba,ca}, {da,ea,af}}
s#0#0
s#0#1

firstpoly = new Brp from { {1,1,0}, {1,0,0}}
secondpoly = new Brp from {{1,0,0}}
thirdpoly = new Brp from {{1,0,0}, {1,1,1}}

 firstpoly * secondpoly
 firstpoly + secondpoly + thirdpoly
 firstpoly * secondpoly * thirdpoly + myHash1 
 myHash = firstpoly + thirdpoly
 tt = {}
 scanKeys(myHash, i -> append(tt, i))
 tt
 myHash1 = firstpoly + secondpoly
  
  firstpoly + myHash1 
firstpoly 
firstpoly + secondpoly 
myHash1 
 

   


beginDocumentation()
document { 
	Key => BitwiseRepresentationPolynomials,
	Headline => "Binary representation of polynomials in ZZ/2",
	EM "BitwiseRepresentationPolynomials", " is an package."
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
  assert(firstFunction 1 === "Hello, World!")
  assert(secondFunction(1,3) === 4)
  assert(secondFunction(1,3,MyOption=>5) === 9)
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
