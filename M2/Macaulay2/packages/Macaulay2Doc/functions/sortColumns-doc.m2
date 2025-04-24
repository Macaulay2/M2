--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {sortColumns, (sortColumns,Matrix)},
     Headline => "permutation giving sort order",
     Usage => "sortColumns m",
     Inputs => {
	  "m" => "between free modules"
	  },
     Outputs => {
	  List => {"of integers, representing the order
	       which places the columns in order"}
	  },
     PARA{},
     "See ", TO (sort,Matrix), " for the ordering used.  The meanings of the
     optional parameters is also the same.",
     EXAMPLE {
	  "R = ZZ/32003[a..d,MonomialOrder=>Lex];",
	  "m = matrix{{a*d, b^2, b^100, b^50*d^50, c^2*d}}",
	  "p = sortColumns m",
	  "m_p"
	  },
     EXAMPLE {
	  "p = sortColumns(m, DegreeOrder=>null, MonomialOrder=>Descending)",
	  "m_p"
	  },
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "f = matrix{{1,a,a^2,b^2,b,c,c^2,a*b,b*c,a*c}}",
      	  "s = sortColumns f",
      	  "f_s",
      	  "s = sortColumns(f,DegreeOrder => Descending)",
      	  "f_s"
	  },
     SeeAlso => {sort},
     Subnodes => {
	 TO [sortColumns, MonomialOrder],
	 TO [sortColumns, DegreeOrder],
         },
     }
document { 
     Key => [sortColumns, MonomialOrder],
     Headline => "specify Ascending or Descending monomial order",
     Usage => "sortColumns(...,MonomialOrder=>x)",
     Inputs => {
	  "x" => Symbol => { "either ", TO "Ascending", " or ", TO "Descending"}
	  },
     Consequences => {
	  {"After the degree order has been considered, sort the columns of
	  the matrix in Ascending or Descending monomial order"}
	  },     
     Caveat => {"This option only works for sorting columns of a matrix, not
	  the elements of a list"},
     SeeAlso => {}
     }
document { 
     Key => [sortColumns, DegreeOrder],
     Headline => "specify Ascending, Descending, or null",
     Usage => "sortColumns(...,DegreeOrder=>x)",
     Inputs => {
	  "x" => Symbol => { "either ", TO "Ascending", ", ", TO "Descending", ", or ", TO null}
	  },
     Consequences => {
	  {"If ", TT "x", " is not null, first order the columns of the matrix
	  in ascending or descending degree (depending on ", TT "x", ").  If ",
	  TT "x", " is null, then only use the monomial order in the sort"}
	  },     
     Caveat => {"This option only works for sorting columns of a matrix, not
	  the elements of a list"},
     SeeAlso => {}
     }
