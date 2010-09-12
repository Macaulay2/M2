--- status: TODO
--- author(s): 
--- notes: 

undocumented {
     (use,Thing)
     }

document { 
     Key => use,
     Headline => "install or activate object"
     }
document { 
     Key => (use,Package),
     Headline => "activate a previously dismissed package",
     Usage => "use PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => "which was previously loaded, and then dismissed."
	  },
     Consequences => {
	  "activates the package, making all of its exported symbols visible",
	  "runs the function stored under the key ", TT "symbol use", " in the package, if there is one"
	  },     
     "This function is only necessary after dismissing a package.",
     EXAMPLE lines ///
	  loadPackage "FirstPackage"
	  firstFunction
	  dismiss FirstPackage
  	  firstFunction	
	  use FirstPackage
	  firstFunction
	  ///,
     SeeAlso => {loadPackage,(dismiss,Package),"FirstPackage::FirstPackage"}
     }
document { 
     Key => {(use,Ring),(use,Monoid)},
     Headline => "install ring variables and ring operations",
     Usage => "use R",
     Inputs => {
	  "R" => Nothing => {ofClass {Ring, Monoid}}
	  },
     Consequences => {
	  {"All variables of ", TT "R", " are set to global variables.  Additionally, 
	  certain operations creating elements of ", TT "R", " are installed globally."}
	  },     
     "When a ring (or a monoid) is assigned to a global variable, this function is
     automatically called for it.",
     PARA{},
     "It is possible to have several polynomial rings defined, perhaps with a variable
     belonging to several rings.",
     EXAMPLE lines ///
	  R = QQ[a..d]
	  S = QQ[b,c,d,e]
	  b
	  ///,
     "At this point, b is thought to be a variable of S.  If one typed
     ", TT "a+b", ", an error would occur, since Macaulay2 doesn't know how to add elements
     of R and S together.  This is fixed via:",
     EXAMPLE lines ///
	  use R
	  b
	  a+b
	  ///,
     PARA{},
     "There are several functions that create rings for you.  Generally, their variables are not
     globally visible.  However, once you 'use' the ring, the variables are available.",
     "For example, the numerator of the Hilbert function is a polynomial in a ring with a variable T.",
     EXAMPLE lines ///
         T
	 hf = poincare ideal vars S
	 T
	 use ring hf
	 T
	 ///,
     Caveat => {"Any values stored in the variables that have been assigned to are lost,
	  hence this operation should not be used by code in a package."},
     SeeAlso => {GlobalAssignHook}
     }
