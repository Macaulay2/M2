-*
   Copyright 2020, Luigi Ferraro, Federico Galetto,
   Francesca Gandini, Hang Huang, Matthew Mastroeni, Xianglong Ni.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-


document { 
	Key => InvariantRing,
	Headline => "invariants of group actions",
	EM "InvariantRing", " is a package implementing algorithms
	to compute invariants of linearly reductive groups.",
	PARA {
	    "Current algorithms include:"
	    },
	UL { 
	    {"An elimination theory algorithm that computes the Hilbert ideal for any linearly reductive group: ",
		"Derksen, H. & Kemper, G. (2015). ",
		HREF{"https://link.springer.com/book/10.1007%2F978-3-662-48422-7","Computational Invariant Theory"}, 
	   	". Heidelberg: Springer. Algorithm 4.1.9, pp 159-164"
		},
	    {"A simple and efficient algorithm for invariants of tori based on: ",
		"Derksen, H. & Kemper, G. (2015). ",
		HREF{"https://link.springer.com/book/10.1007%2F978-3-662-48422-7","Computational Invariant Theory"}, 
	   	". Heidelberg: Springer. Algorithm 4.3.1 pp 174-177"
		},
	    {"An adaptation of the tori algorithm for invariants of finite abelian groups based on: ",
		"Gandini, F. ",
		HREF{"https://deepblue.lib.umich.edu/handle/2027.42/151589","Ideals of Subspace Arrangements"}, 
	   	". Thesis (Ph.D.)-University of Michigan. 2019. ISBN: 978-1392-76291-2. pp 29-34."
		},
	    {"King's algorithm and the linear algebra method for invariants of finite groups: ",
		"Derksen, H. & Kemper, G. (2015). ",
		HREF{"https://link.springer.com/book/10.1007%2F978-3-662-48422-7","Computational Invariant Theory"}, 
	   	". Heidelberg: Springer. Algorithm 3.8.2, pp 107-109; pp 72-74"
		},
	    {"The algorithms for primary and secondary invariants, and Molien series of finite groups implemented
		in version 1.1.0 of this package by: ",
		"Hawes, T. ",
		HREF{"https://msp.org/jsag/2013/5-1/p03.xhtml","Computing the invariant ring of a finite group"}, 
	   	". JSAG, Vol. 5 (2013). pp 15-19. DOI: 10.2140/jsag.2013.5.15"
		}
            },
	PARA {
	    "Version history:"
	    },
	UL { 
	    {BOLD "1.1.0: ", "the first version of this package was
		developed by Thomas Hawes. It focused on
		computing primary and secondary invariants of
		finite groups. For more information, see: ",
		"Hawes, T. ",
		HREF{"https://msp.org/jsag/2013/5-1/p03.xhtml","Computing the invariant ring of a finite group"}, 
	   	". JSAG, Vol. 5 (2013). pp 15-19. DOI: 10.2140/jsag.2013.5.15"},
	    {BOLD "2.0: ", "this version was developed by L. Ferraro,
		F. Galetto, F. Gandini, H. Huang, M. Mastroeni, and
		X. Ni. It introduces types for different group
		actions as well as rings of invariants.
		It also contains new functionality for invariants
		of finite groups, diagonal actions (tori/abelian
		    groups), and linearly reductive groups.
		The code from version 1.1.0 is preserved in the
		auxiliary file Hawes.m2 (with documentation
		    in the file HawesDoc.m2) and has been updated
		to work with the new types."
		}
	    }
	}
    
document {
	Key => {(dim, GroupAction)},
	
	Headline => "dimension of the polynomial ring being acted upon",
	
	Usage => "dim G",
	
	Inputs => {
	    	"G" => GroupAction => {"a group action on a polynomial ring"},
		},
	
	Outputs => {
		ZZ => {"the dimension of the polynomial ring being acted upon"}
		},
	
	PARA {"This function is provided by the package ", 
	    TO InvariantRing,"."},
	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"T = diagonalAction(matrix {{0,1,-1,1},{1,0,-1,-1}}, R)",
		"dim T == dim R"
		},
	    }

document {
	Key => {GroupAction},
	
	Headline => "the class of all group actions",
	
	"This class is provided by the package ", TO InvariantRing,".",
	
	PARA {
	    	TT "GroupAction", " is the class of all group actions
		on polynomial rings for the purpose of computing
		invariants. This is not typically used directly,
		delegating creation to the various constructor
		functions for different kinds of group actions:"
	    },
	UL {
	    {TO "FiniteGroupAction", ", the class of 
	    a finite matrix group action, is created with ",
	    TO "finiteAction"},
	    {TO "DiagonalAction", ", the class of the diagonal
	    action of a product of a torus and a finite abelian group, 
	    is created with ",
	    TO "diagonalAction"},
	    {TO "LinearlyReductiveAction", ", the class of a
	    linearly reductive matrix group action,
	    is created with ",
	    TO "linearlyReductiveAction"}
	    },
	
	PARA {
	    "Each class implements different algorithms to
	    compute invariants. Although mathematically speaking
	    all the above group actions are linearly reductive
	    (at least in the non modular case), the class ",
	    TO "LinearlyReductiveAction", " should be used only
	    when none of the other classes apply because it has fewer
	    and possibly less efficient methods."
	    },
	
	PARA {
	    	"The class ", TT "GroupAction ", "is implemented as
		a ", TT "HashTable", ". When created it stores
		information such as the action (in a format
		dependent upon the group) and the polynomial ring
	    	being acted upon."
	    },
	}

document {
	Key => {(ring, GroupAction)},
	
	Headline => "the polynomial ring being acted upon",
	
	Usage => "ring G",
	
	Inputs => {
	    	"G" => GroupAction => {"a group action on a polynomial ring"},
		},
	
	Outputs => {
		Ring => {"the polynomial ring being acted upon"}
		},
	
	PARA {"This function is provided by the package ",
	    TO InvariantRing,"."},
	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"T = diagonalAction(matrix {{0,1,-1,1},{1,0,-1,-1}}, R)",
		"ring T === R"
		},
	    }

document {
	Key => {
	    (net, RingOfInvariants),
	    (net, DiagonalAction),
	    (net, FiniteGroupAction),
	    (net, LinearlyReductiveAction)
	    },
	Headline => "format for printing, as a net",
	PARA {"Format objects of the package ",
	    TO InvariantRing, " for printing. See ",
	    TO net," for more information."},
	    }
