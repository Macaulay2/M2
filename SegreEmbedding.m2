newPackage(
    "SegreEmbedding",
    Version => "0.1",
    Date => "July 19, 2017",
    Authors => {{
	    Name => "Juliette Bruce", 
	    Email => "juliette.bruce@math.wisc.edu", 
	    HomePage => "http://www.math.wisc.edu/~juliettebruce/"}},
    Headline => "computing Segre embedings of products of projective spaces",
    DebuggingMode => true
  )

needsPackage "SimpleDoc"

export {
    "segreIdeal"
    }

-- given a list of positive integers {a1,a2,a3,..an} computes the defining ideal of
-- the Segre embedding PPa1 x PP a2 x .. x PPan into P^n for approriate N.
-- (Note: the returned answer will have been as if the ak are increasing.
segreIdeal=method()
segreIdeal (List) := (D) ->(
    x := getSymbol "x";
    z := getSymbol "z";
    --
    D = sort D;
    D' := apply(D,i->i+1);
    N := product(D')-1;  
    --
    ID := id_(ZZ^(#D));
    L := flatten apply(#D,i->apply(D_i+1,j->(entries ID_i)));
    V := flatten apply(#D,i->apply(D_i+1,j->z_(i,j))); 
    --
    R := QQ(monoid[x_{0}..x_{N}]);
    S := QQ(monoid[V,Degrees=>L]);   
    --
    B := basis(apply(#D,i->1),S);
    kernel map(S, R, B)
)

end

--------------------------------------------------------------------------------
-- DOCUMENTATION
--------------------------------------------------------------------------------
beginDocumentation()
doc ///
    Key
    	SegreEmbedding
    Headline
    	computing Segre embedings of products of projective spaces
    Description
    	Text
	    This package has a function for computing the defining ideal of a Segre emedding 
    Caveat
        This package is underdevelopment.
///

doc ///
    Key
    	segreIdeal
	(segreIdeal,List)
    Headline
    	computes the defining ideal of a Segre embedding
    Usage
    	K = segreIdeal(D)
    Inputs
    	D:List
	    containing the dimensions of the projective spaces
    Outputs
    	K:Ideal
	    defining the imge of the Segre embedding
    Description
    	Text
	    Given a list {a1,a2,a3,..an} of positive integers
	    this computes the defining ideal of the Segre embedding 
	    of PPa1 x PP a2 x .. x PPan.
	    
	    Note: The returned answer will have been as if the ak's are increasing.
	Example
	    segreIdeal({1,1})
	    segreIdeal({1,2})
    	
///

TEST ///
    R = QQ[x_{0},x_{1},x_{2},x_{3}]
    I = ideal(x_{1}*x_{2}-x_{0}*x_{3}
    assert ( sub(segreIdeal({1,1}),R) == I)
///
end
  
    	
    
    	