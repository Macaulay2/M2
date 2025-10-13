
doc ///
	Key
		"Second Lie algebra tutorial"

	Description
		Text
	       	        In this second tutorial, we continue the introduction on how to use 
			the package 
			GradedLieAlgebras.	
			
	        Text	
			By default, the scalars are assumed to be rational numbers,
			but it is also possible to define the coefficient field with
			the option @TO Field@, which is one out of four options that can
			be used in the constructor @TO lieAlgebra@.
			
			
			
		Example
		        F = frac(ZZ/7[x])
		    	L = lieAlgebra({a_1,a_2,a_3},Field => F)/ 
			     {3 a_1 a_2 a_3+(5*x) a_2 a_1 a_3,
			      a_3 a_2 a_3 a_1}	
			
    	    	Text	
			The example above specifies the Lie algebra over the fraction
			field of $\mathbb Z/7$ [ $x$ ] on 
			three generators $a_1, a_2, a_3$, modulo the ideal generated
			by $3$ [$a_1$, [ $a_2$, $a_3$ ]] + $5x$ [ $a_2$, [ $a_1$, $a_3$ ]] and 
			[$a_3$, [ $a_2$, [ $a_3$, $a_1$ ]]].
			As input coefficients one may always use 
			an integer or a quotient of integers 
			with non-zero denominator in the field.
			
		
	    
			
		Text
 			To specify weights of the generators, 
			use the option @TO [lieAlgebra,Weights]@.
		        The weights are lists of integers of the same
			length, where the first degree is positive, 
			also just called
			the degree, see @TO "firstDegree(LieElement)"@. 
			The degree is the two first inputs for the 
			function @TO "dims(ZZ,ZZ,LieAlgebra)"@. 
			If the option @TO [lieAlgebra,LastWeightHomological]@ 
			is {\tt true}, 
			then
			the last degree is the homological degree, 
			which is non-negative
			and less than the first degree.
			If the option @TO [lieAlgebra,LastWeightHomological]@ 
			is {\tt false}, 
			which
			is the default value, then 	
			the program defines the homological degree to be zero, and adds a last 
                        component 0 to the existing degrees. In this case the weights
			can also be given as a list of positive integers that will be the
			degrees. See @TO "Differential Lie algebra tutorial"@ 
			how to define a differential.				       
			Use @TO weight@ to compute the weight of an arbitrary 
			homogeneous Lie expression.
			
			 
		Example
		        L = lieAlgebra({a,b,c},Weights => {{1,-1},{1,4},{2,3}})
			weight b
			ex = a b+(1/3) c
			weight ex
			firstDegree ex		      
			L1 = lieAlgebra({a,b,c},Weights => {1,1,2})
			describe L1
						
		Text
		        To specify signs of the generators, use the option 
			@TO [lieAlgebra,Signs]@.
			The sign of a generator is either 0 (even) or 1 (odd). By default,
			the sign of all generators are 0. Writing the option as
			"Signs => 1"
			defines the signs of all the generators to be 1.  
			The signs affect the axioms of a Lie superalgebra, 
			see @TO LieAlgebra@. 
			Use @TO (sign, LieElement)@ to 
			compute the sign of an arbitrary homogeneous Lie expression.
		
			  
		Example	
		        L = lieAlgebra({a,b,c},Weights => {{1,-1},{1,4},{2,3}},
			      Signs=>{0,1,1})
			ex = a b+(1/3) c	       
			sign ex			
			          
		Text	
			For each finitely presented
			Lie algebra $L$, {\tt ambient(L)} 
			represents a free Lie algebra $M$ such
			that $L$ (without differential) is a quotient of $M$, 
			see also
			@TO "Quotient Lie algebras and subspaces"@. 
			Moreover, the relations belong to $M$, 
			even if $L$ is defined as a quotient 
			in two or more steps from $M$. This is 
		        somewhat different from 
			the situation for rings.
			

                Example
		        M = lieAlgebra{a,b,c}
                        L = M/{a b}
                        a c
                        Q = L/{a c}
                        ideal(Q)
                        oo_1 
			ambient Q
		       
		Text 	
		        The relations may have a non-zero linear part. 
			Use @TO "minimalPresentation(ZZ,LieAlgebra)"@ 
			to get a minimal presentation.	
			Use @TO "describe(LieAlgebra)"@ to get
			information about the Lie algebra. This contains the value of
			the three options @TO [lieAlgebra,Weights]@, @TO [lieAlgebra,Signs]@,
			@TO [lieAlgebra,Field]@, and also the values 
		        @TO "generators(LieAlgebra)"@, @TO "ideal(LieAlgebra)"@, 
			@TO "ambient(LieAlgebra)"@, @TO "diff(LieAlgebra)"@ and 
			@TO "computedDegree(LieAlgebra)"@  
			
		Example		       
			F = lieAlgebra({a,b,c},Weights => {1,1,2},
			      Signs=>{1,0,0})
			L = F/{-c+a a,(a b) a}
			dims(1,4,L)
			describe L
			M = minimalPresentation(4,L)
			describe M
			  
			       			     	     	       	    	
		Text
		       In {\tt L#cache} there
		       is more information about $L$ collected 
		       during a computation. Some parts
		       of the information are visible to the user, e.g., 
		       @TO lieRing@ and @TO mbRing@.
		       The ring {\tt L#cache.lieRing} is the internal polynomial ring 
		       representation of 
    		       Lie elements. The Lie monomials are represented as
    		       commutative monomials in this ring. 
		       The program produces internally an 
		       ideal of relations and a basis, 
		       which may be seen as {\tt L#cache.gb} and
		       {\tt L#cache.basis}. 
		       The number of generators in @TO lieRing@
    		       is the number of generators in the 
		       Lie algebra times the internal counter
    		       {\tt L#cache.max}, which initially 
		       is set to $5$, and is changed to $n+5$ if the computation of $L$
    		       is performed up to degree $n$ with $n\ >$ {\tt L#cache.max}. 
		       
		Example
		       L = lieAlgebra{a,b}/{a a a b,b b b a}
		       dims(1,4,L)
		       peek L#cache
		       L#cache.basis#4
		       basis(4,L)
		       L#cache.gb#4
		       a b b a	      
	 	       dims(1,6,L)
		       peek L#cache
		       
		Text
		       The ring {\tt L#cache.mbRing} is used to get an output of
		       Lie elements with indexed basis elements,
		       which sometimes is better to use than the 
		       iterated Lie products of generators, especially in 
		       high degrees. Use @TO indexForm@ to get the 
		       output in @TO mbRing@ and 
		       @TO "standardForm(RingElement,LieAlgebra)"@ to get back 
		       the standard output. The ring @TO mbRing@ is very
		       large; it has as many generators as the total 
		       dimension of the computed Lie algebra. 
		       For this reason, you should give the ring a name
		       to avoid
		       a large output.
		       
		       
		Example
		       L = lieAlgebra{a,b,c}
		       x = (a b c a) (b c) 
		       Q = L#cache.mbRing
		       numgens Q
		       indexForm x
		       standardForm(oo,L)
		        
		        
		       
		
		
	        
						   		        				
        SeeAlso		    		        
		        "First Lie algebra tutorial"
		        "Differential Lie algebra tutorial"		        
		        "Holonomy Lie algebras and symmetries"
		   			       
		      

			
 ///
 end	
	    	

