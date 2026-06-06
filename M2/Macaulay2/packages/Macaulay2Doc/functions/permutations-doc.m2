document {
    Key => {
	permutations,
       (permutations, ZZ),
       (permutations, VisibleList)
    },
    Headline => "produce all permutations of a list",
    Usage => "permutations x",
    Inputs => { "x" => { ofClass {VisibleList, ZZ} } },
    Outputs => { { "a list of all the permutations of the visible list ", TT "x",
	    ", or, if ", TT "x", " is an integer, of the list of integers from 0 through ", TT "n-1" } },
    EXAMPLE {
	"permutations {a,b,c,d}",
	"permutations 3"
    }
}

document {
    Key => {
	uniquePermutations,
       (uniquePermutations, ZZ),
       (uniquePermutations, VisibleList)
    },
    Headline => "produce all unique permutations of a list",
    Usage => "uniquePermutations x",
    Inputs => { "x" => { ofClass {VisibleList, ZZ} } },
    Outputs => { { "a list of all distinct permutations of the visible list ", TT "x",
	    ", or, if ", TT "x", " is an integer, of the list of integers from 0 through ", TT "n-1" } },
    EXAMPLE {
	"uniquePermutations {a,b,a,b}",
    }
}

document {
    Key => {
	inversePermutation,
       (inversePermutation, VisibleList)
    },
    Headline => "inverse permutation",
    Usage => "y = inversePermutation x",
    Inputs => {
	"x" => List => {"a list of length ", TT "n", " whose elements are the numbers 0, 1, 2, ..., ", TT "n-1", ", in some order,
	    representing the permutation defined by sending ", TT "i", " to ", TT "x#i"
	}
    },
    Outputs => {
	"y" => List => {"the list representing the inverse permutation of ", TT "x" }
    },
    EXAMPLE lines ///
      x = {1,2,3,4,5,0}
      y = inversePermutation x
      all(#x, i -> x#(y#i) == i)
      all(#x, i -> y#(x#i) == i)
    ///,
    PARA { "We compose permutations with ", TT "_", "; see ", TO (symbol_, VisibleList, List), "." },
    EXAMPLE lines ///
      x_x_x
      x_x_x_x_x_x
      x_y
      y_x
    ///
}
