document {
     Key => combine,
     Headline => "combine hash tables",
     Usage => "z = combine(x,y,f,g,h) or z=combine(x,y,f,t,g,h)",
     Inputs => {
	  "x" => "a hash table",
	  "y" => {"a hash table of the same class as ", TT "x"},
	  "f" => { "a function of two variables to be used for combining a key
	       of ", TT "x", " with a key of ", TT "y", " to make a new key
	       for ", TT "z", "." },
	  "t" => { "a function combining two keys and returning a value, twisting
	       the value returned by ", TT "g", " based on which keys were used. This argument
	       may be omitted, in which case the ", TT "t(p,q)", " term in the output
	       below is omitted."},
	  "g" => { "a function of two variables to be used for combining a value
	       of ", TT "x", " with a value of ", TT "y", " to make a new value
	       for ", TT "z", "." },
	  "h" => { "a function of two variables to be used for combining two
	       values returned by ", TT "g", " when the corresponding keys
	       returned by ", TT "f", " turn out to be equal.  Its first argument
	       will be the value accumulated so far, and its second argument will
	       be a value just provided by ", TT "g", "."}
	  },
     Outputs => {
	  "z" => {
	       "a new hash table, of the same class as ", TT "x", " and ", TT "y", ",
	       containing the pair ", TT "f(p,q) => g(t(p,q),g(b,c))", "
	       whenever ", TT "x", " contains the pair ", TT "p => b", "
	       and ", TT "y", " contains the pair ", TT "q => c", ",
	       except that ", TT "h", " is used to combine values when two keys
	       coincide.  If ", TT "f", " or ", TT "g", " evaluates ", TO "continue", ", then
	       nothing is contributed to the resulting hash table.  If ", TT "h", " evaluates
	       ", TO "continue", ", then, at that point, the entry stored under the key ", TT "f(p,q)", " 
	       in the hash table under construction is removed."
	       }
	  },
     "The function ", TT "f", " is applied to every pair ", TT "(p,q)", "
     where ", TT "p", " is a key of ", TT "x", " and ", TT "q", " is a
     key of ", TT "y", ".  The number of times ", TT "f", " is evaluated is thus 
     the product of the number of keys in ", TT "x", " and the number of 
     keys in ", TT "y", ".",
     PARA{},
     "The function ", TT "h", " should be an associative function, for otherwise 
     the result may depend on internal details about the implementation of hash 
     tables that affect the order in which entries are encountered.  If ", TT "f", ",
     ", TT "t", " (if present), ", TT "g", ", and ", TT "h", " are commutative functions as well, then the 
     result ", TT "z", " is a commutative function of ", TT "x", " and ", TT "y", ".",
     PARA{},
     "The result is mutable if and only if ", TT "x", " or ", TT "y", " is.",
     PARA{},
     "This function can be used for multiplying polynomials, where it
     can be used in code something like this:", 
     PRE "     combine(x, y, monomialTimes, coeffTimes, coeffPlus)",
     "We illustrate that with a simple-minded implementation of the free ring on the English alphabet, representing words
     as string and polynomials as hash tables that associate coefficients to words.",
     EXAMPLE lines ///
     	  Poly = new Type of HashTable
     	  p = new Poly from { "" => 1, "x" => 2, "y" => 3, "cat" => 5 }
	  Poly * Poly := (p,q) -> combine(p,q,concatenate,times,plus);
	  p*p
     ///,
     "One may also use this function for multiplying divided powers in a similar manner:",
     PRE "     combine(x, y, monomialTimes, divPowCoeff, coeffTimes, coeffPlus)",
     "For example:",
     EXAMPLE lines ///
     	  DivPowPoly = new Type of HashTable
	  divPowCoeff = (i,j) -> binomial(i+j,i)
     	  p = new DivPowPoly from { 0 => 1, 1 => 1 }
	  DivPowPoly * DivPowPoly := (p,q) -> combine(p,q,plus,divPowCoeff,times,plus);
	  p*p
     ///,
     PARA{},
     SeeAlso => {merge}
     }
