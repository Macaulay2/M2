
doc ///
	Key
		"First LieAlgebra Tutorial"
	Headline 
		A tutorial of the package GradedLieAlgebras
	Description
		Text
	       	        In this elementary tutorial, we give a brief introduction 
			on how to use the package 
			@TO GradedLieAlgebras@.	
					    	    	
		Text
		    	The most common way to construct a Lie algebra 
			is by means of the constructor
			@TO lieAlgebra@.
			
		Example
		        L1 = lieAlgebra({a,b}, {})
			computeLie 5
			
		Text
		        The above list is the dimensions of the 
			free Lie algebra on two generators
			in degrees 1 to 5. To get an explicit basis in a certain degree,
			use @TO basisLie@.
			
		Example
			basisLie 2
			basisLie 3
			
		Text
		        The basis elements in degree 3 above should be interpreted as 
			[a,[b,a]] and [b,[b,a]]. To multiply two elements, use @TO multLie@.
			
		Example
			prod=multLie([a,b],[a,a,b])
				
		Text
		        The output above should be interpreted as 
			-[a,[b,[a,[b,a]]]] + [b,[a,[a,[b,a]]]], that is, 
			we use the right associative
			convention for Lie monomials, see @TO monomialLie@. 
			The output from @TO multLie@ is a linear 
			combination of the basis elements
			of degree 5.
			
		Example 
		    	basisLie 5
			
		Text				
		        An expression like prod above may be used as 
			a relation in a Lie algebra.
				
		Example
		        L2=lieAlgebra({a,b},{prod})	
		        computeLie 5
			
		Text
		        As expected, the dimension in degree 5 of L2 is one less than 
			that of L1.
		      	Each relation in the second argument to the constructor 
			@TO lieAlgebra@ is a general 
			Lie expression, @TO generalExpressionLie@, see 
			@TO "How to write Lie elements"@.
						
	SeeAlso		    		        
		 "Second LieAlgebra Tutorial"
		 "Differential Lie algebras Tutorial"
		 "How to write Lie elements"  
		 "Constructing Lie algebras"
		 "Symmetries"
				       	 
///
end			
	
	
	