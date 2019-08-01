
doc ///
	Key
		"Second LieAlgebra Tutorial"
	Headline 
		Second tutorial of the package GradedLieAlgebras
	Description
		Text
	       	        In this second tutorial, we continue the introduction on how to use 
			the package 
			GradedLieAlgebras.	
			
	        Text	
			By default, the scalars are assumed to be rational numbers,
			but it is also possible to define the coefficient field with
			the option @TO field@, which is one out of four options that can
			be used in the constructor @TO lieAlgebra@.
			
			
			
		Example
		        F = frac(ZZ/7[x])
		    	L = lieAlgebra({a_1,a_2,a_3},field=> F)/ 
			     {3 a_1 a_2 a_3+(5*x) a_2 a_1 a_3,
			      a_3 a_2 a_3 a_1}	
			
    	    	Text	
			The example above specifies the Lie algebra over the fraction
			field of ZZ/7[x] on 
			three generators a_1,a_2,a_3, modulo the ideal generated
			by 3[a_1,[a_2,a_3]] + 5x[a_2,[a_1,a_3]] and [a_3,[a_2,[a_3,a_1]]].
			As input coefficients one may always use 
			an integer or a quotient of integers 
			with non-zero denominator in the field.
			
		
	    
			
		Text
 			To specify weights of the generators, 
			use the option @TO genWeights@.
		        The weights are lists of integers of the same
			length, where the first degree is positive, also just called
			the degree, see @TO degLie@. The degree is input for the 
			function @TO dimsLie@. 
			The last degree is the homological degree, which is non-negative
			and less than the first degree.
			If the user does not specify a differential, the weights may also 
			be given as just positive integers;
			the program defines the homological degree to be zero and adds a last 
                        degree 0 to the existing degrees.		       
			Use @TO weightLie@ to compute the weight of an arbitrary 
			homogeneous Lie expression.
			
			 
		Example
		        L1 = lieAlgebra({a,b,c},genWeights=>{{1,-1},{1,4},{2,3}})
			weightLie b
			ex = a b+(1/3) c
			weightLie ex
			degLie ex		      
			L11 = lieAlgebra({a,b,c},genWeights=>{1,1,2})
			L11.genWeights
						
		Text
		        To specify signs of the generators, use the option @TO genSigns@.
			The sign of a generator is either 0 (even) or 1 (odd). By default,
			the sign of all generators are 0. Writing the option as
			"genSigns => 1"
			defines the sign of all the generators to be 1.  
			The signs affect the axioms of a Lie superalgebra, 
			see @TO axiomsLie@. 
			Use @TO signLie@ to 
			compute the sign of an arbitrary homogeneous Lie expression.
		
			  
		Example	
		        L2 = lieAlgebra({a,b,c},genWeights=>{{1,-1},{1,4},{2,3}},
			      genSigns=>{0,1,1})
			ex2 = a b+(1/3) c	       
			signLie ex2			
			          
		Text	
			The forth option @TO diffl@ is false by default and is set to
			be true if a differential is defined, 
			see @TO "Differential LieAlgebra Tutorial"@.
			For each Lie algebra L, ambient(L) is a free Lie algebra M such
			that L (without differential) is a quotient of M. Moreover, the relations belong to M, 
			even if L is defined as a quotient in two steps from M. This is 
		        somewhat different from 
			the situation for Rings, where ambient(R)
			might not be a polynomial ring.
			

                Example
		        M = lieAlgebra({a,b,c})
                        L = M/{a b}
                        a c
                        Q = L/{a c}
                        Q.relsLie
                        oo_1 
			ambient Q
		       
		Text 	
		        The relations may contain a linear part. 
			Use @TO minPresLie@ 
			to get a minimal presentation.	
			Use @TO peekLie@ to get
			information about the Lie algebra. This contains the value of
			the four options and also the keys 
		        @TO gensLie@, @TO relsLie@, @TO genDiffs@ and @TO compdeg@. If @TO minmodelLie@ has been
			computed (e.g., through @TO minPresLie@ or @TO extTableLie@) then also the key @TO minmodel@
			is given by peekLie.		       
		        
			
		Example		       
			L3 = lieAlgebra({a,b,c},genWeights=>{1,1,2},
			      genSigns=>{1,0,0})/{-c+a a,(a b) a}
			dimsLie 4
			peekLie L3
			M = minPresLie 4
			peekLie L3
			peekLie M  
			       			     	     	       	    	
		Text
		       In L.cache there
		       is more information about L collected during a computation, which
		       is not visible for the user, except 
		       @TO lieRing@ and @TO mbRing@.
		       L.cache.lieRing is the internal polynomial ring representation of 
    		       Lie elements, which cannot be used
    		       by the user but can be looked upon
    		       by writing "L.cache.lieRing". The Lie monomials are represented as
    		       commutative monomials in this ring. The number of generators in lieRing
    		       is the number of generators in the Lie algebra times the internal counter
    		       "L.cache.maxDeg" which initially is set to 5 and is changed to n+5 if dimsLie n
    		       is performed with n>maxdeg. 
		       
		Example
		       L4 = lieAlgebra({a,b})/{a a a b,b b b a}
		       dimsLie 4
		       peek L4.cache	      
		       dimsLie 6
		       peek L4.cache
		       
		Text
		       The ring L.cache.mbRing is used to get an output of
		       LieElements with indexed basis elements,
		       which sometimes is better to use than the 
		       iterated Lie products of generators, especially in 
		       high degrees. Use @TO indexFormLie@ to get the 
		       output in mbRing and @TO defLie@ to get back 
		       the standard output. The ring L.cache.mbRing is very
		       large, it has as many generators as the total 
		       dimension of the computed Lie algebra, so to avoid
		       a large output you should give the ring a name.
		       
		       
		Example
		       L = lieAlgebra{a,b,c}
		       x = (a b c a) (b c) 
		       Q = L.cache.mbRing
		       indexFormLie x
		       defLie oo
		        
		        
		       
		
		
	        Text
		       The interested reader may continue to read about the 
		       functions (among other) 
	               @TO idealTableLie@, @TO subalgTableLie@,
		       @TO annLie@, 
		       and @TO centerLie@.   
						   		        				
        SeeAlso		    		        
		        "First LieAlgebra Tutorial"
		        "Differential LieAlgebra Tutorial"		        
		        "Constructing Lie algebras"
		        "Holonomy Lie algebras and Symmetries"
		   			       
		      

			
 ///
 end	
	    	

