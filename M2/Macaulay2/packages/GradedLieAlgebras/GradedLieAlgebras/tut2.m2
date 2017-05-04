
doc ///
	Key
		"Second LieAlgebra Tutorial"
	Headline 
		Second tutorial of the package GradedLieAlgebras
	Description
		Text
	       	        In this second tutorial, we continue the introduction on how to use 
			the package 
			@TO GradedLieAlgebras@.	
			
	        Text	
			By default, the scalars are assumed to be rational numbers,
			but it is also possible to define the coefficient field with
			the option @TO field@, which is one out of four options that can
			be used in the constructor @TO lieAlgebra@.
			
		Example
		        F=frac(ZZ/7[x])
		    	L = lieAlgebra({a_1,a_2,a_3}, 
			    {{{3,5*x},{[a_1,a_2,a_3],[a_2,a_1,a_3]}},
				[a_3,a_2,a_3,a_1]}, field => F)	
			
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
			function @TO computeLie@. 
			The last degree is the homological degree, which is non-negative
			and less than the first degree.
			If the user does not specify a differential, 
			the program defines the homological degree to be zero and adds a last 
                        degree 0 to the existing degrees.
			Use @TO weightLie@ to compute the weight of an arbitrary expression,
			see @TO "How to write Lie elements"@.
			 
		Example
		        L1 = lieAlgebra({a,b,c},{},genWeights=>{{1,-1},{1,4},{2,3}})
			weightLie b
			ex={{1,1/3},{[a,b],[c]}}
			weightLie ex
			degLie ex
		
			
		Text
		        To specify signs of the generators, use the option @TO genSigns@.
			The sign of a generator is either 0 (even) or 1 (odd). By default,
			the sign of all generators are 0. Writing the option as
			"genSigns => 1"
			defines the sign of all the generators to be 1.  
			The signs affect the axioms of a Lie superalgebra, 
			see @TO axiomsLie@. Use @TO signLie@ to 
			compute the sign of an arbitrary expression,
			see @TO "How to write Lie elements"@ for the use of
			 @TO mbRing@, @TO indexFormLie@ and @TO defLie@.
			  
		Example	
		        L2 = lieAlgebra({a,b,c},{},genWeights=>{{1,-1},{1,4},{2,3}},
			    genSigns=>{0,1,1})	       
			signLie ex
			computeLie 3
			S=L2.cache.mbRing			
			indexFormLie ex
			signLie mb_{3,1}			   
			
			          
		Text	
			The relations may contain a linear part. 
			Use @TO minPresLie@ to get a minimal presentation.	
			In order to use a
			relation which contains Lie products, which are 
			not of the form @TO monomialLie@, 
			use @TO toMonomialLie@. Use @TO peek@ to get
			information about the Lie algebra. This contains the value of
			the four options and also the keys 
			@TO compdeg@, @TO deglength@,  @TO gensLie@, 
			@TO numGen@ and @TO relsLie@. In L.cache there
			is more information about L collected during a computation, which
			is not visible for the user, except @TO maxDeg@, 
			@TO lieRing@ and @TO mbRing@.
					
		Example
		        rel=toMonomialLie([[a,b],a],{a,b},{1,0})
			L3=lieAlgebra({a,b,c},{{{-1,1},{[c],[a,a]}},rel},
			    genWeights=>{1,1,2},genSigns=>{1,0,0})
			computeLie 4
			peek L3
			M=minPresLie 4
			peek M  
			
		Text
		        Observe that the output of @TO toMonomialLie@ may not be a 
			@TO basicExpressionLie@ in the free Lie algebra. In fact, we may
			use @TO normalFormLie@, see @TO "How to write Lie elements"@, to 
			reduce the expression rel as follows.
			
		Example
		        L=lieAlgebra({a,b},{},genSigns=>{1,0})
			normalFormLie rel
			
		Text
		        This explains why the relation in the minimal presentation M
			above has
			changed from rel to [b,a,a].
			     	     	   
    	    	Text
		        The differential of the generators is defined by the option
			@TO genDiffs@. The expressions used should be 
			@TO generalExpressionLie@, see @TO "How to write Lie elements"@.
			The differential preserves all weights except the last homological
			degree which is lowered by 1
			and it changes the sign. 
			Also the square of the differential
			is zero. All this is checked when lieAlgebra is executed. 
			Use @TO evalDiffLie@ to compute the value of the differential of 
			an arbitrary expression.  
			The 
			differential is a derivation and may also be constructed using the 
			constructor @TO derLie@, see @TO "Constructing Lie algebras"@.     
			
		Example
		        L4 = lieAlgebra({a,b,c},{},genWeights=>{{1,0},{2,1},{3,2}},
			    genSigns=>{1,1,1},genDiffs=>{[],[],[a,b]})
			peek L4
			{{1,1/3},{[a,b,b],[a,a,c]}}		
			evalDiffLie oo
		
			
		Text
		       The key @TO maxDeg@ in L.cache is by default 5.
		       If computeLie n is executed 
		       for n>L.cache.maxDeg, then the program changes the key to n+5. 
		       The value of maxDeg defines the internal representation of 
		       Lie elements in
		       the polynomial ring "L.cache.lieRing", which cannot be used
		       by the user but can be looked upon
		       by writing "L.cache.lieRing". The Lie monomials are represented as
		       commutative monomials in this ring. 
		       
		Example
		       L5=lieAlgebra({a,b},{[a,a,a,b],[b,b,b,a]})
		       computeLie 4
		       peek L5.cache
		       L5.cache.lieRing		      
		       computeLie 6
		       L5.cache.maxDeg
		       L5.cache.lieRing
		
	        Text
		    	The interested reader may continue to read about the 
			functions @TO idealLie@, @TO subalgLie@,
			@TO divisorLie@, @TO invImageLie@, @TO intersectionLie@, @TO annLie@, 
			and @TO centreLie@.  
						   		        				
        SeeAlso		    		        
		        "First LieAlgebra Tutorial"
		        "Differential Lie algebras Tutorial"
		        "How to write Lie elements"
		        "Constructing Lie algebras"
		        "Symmetries"
		   			       
		      

			
 ///
 end	
	    	

