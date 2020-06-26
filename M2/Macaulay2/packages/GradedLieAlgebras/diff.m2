doc ///

     	Key
	    	"Differential Lie algebra tutorial"
	
	SeeAlso
	         
		 "First Lie algebra tutorial"
		 "Second Lie algebra tutorial"
		 "Holonomy Lie algebras and symmetries"
		 "Minimal models, Ext-algebras and Koszul duals"	
	
	Description
		Text
		        A differential Lie algebra is defined by first 
			using the constructor @TO lieAlgebra@ with the 
			option @TO [lieAlgebra,LastWeightHomological]@ set to {\tt true} 
			to define a free Lie algebra $F$. Hereby, 
			the last weight is 
			the homological degree, and it must be non-negative and
			less than the first degree. Next define the differential 
			Lie algebra $D$ using  
			@TO differentialLieAlgebra@
			with input the list of differentials of the generators with 
			values in $F$.
			The differential should preserve 
			all weights except the homological
			degree, which is lowered by 1,
			and it also changes the sign. 
			All this is checked to be true when
			@TO differentialLieAlgebra@ 
			is executed.
			The value zero for a generator is 
			given as $0_F$, which has any
			weight and sign (see however @TO weight@ and @TO sign@).
			The program adds (non-normalized) 
			relations to the Lie algebra to get
			the square of the differential
			to be 0.  		
			
		Example
		        F1 = lieAlgebra({a,b,c},Weights => {{1,0},{2,1},{3,2}},
			     Signs => {1,1,1},LastWeightHomological => true)
			D1=differentialLieAlgebra{0_F1,a a,a b}
			describe D1
			F2 = lieAlgebra({a,b,c2,c3,c4},Signs => {0,0,1,0,1},
                             Weights => {{1,0},{1,0},{2,1},{3,2},{5,3}},
		             LastWeightHomological => true)
	                D2=differentialLieAlgebra{0_F2,0_F2,a b,a c2,a b c3}
		        describe D2
			
					       
		Text
		        There is a unique extension to a derivation $d$ on the free Lie
			algebra $F$ given the values of $d$ on the generators. This map
			induces a derivation with square zero on the differential 
			Lie algebra $D$ (which might have some relations).  
			The differential is obtained using 
			@TO differential@ applied to $D$. 
			The value of the differential $d$ applied to 
			an arbitrary Lie element $x$ in $D$ is obtained as $d(x)$.       
						
			
    	    	Example		 
		        d2 = differential D2
		        x = a c3 + b c3 + (1/2) c2 c2		
		        d2 x
			
		Text	      
		  It is possible to define quotients of a differential Lie
		  algebra in the same way as for ordinary Lie algebras. The program 
		  adds (non-normalized) relations to obtain that the ideal is
		  invariant under the differential.
		  
		Example
		  F3 = lieAlgebra({a,b,c},Signs => 1,
		         Weights => {{1,0},{1,0},{2,1}},
		         LastWeightHomological => true)
		  D3 = differentialLieAlgebra{0_F3,0_F3,a b}
		  L3 = D3/{b c,c c}
		  describe L3
		  		 
		Text
		  The homology as a vector space can be obtained using 
		  @TO lieHomology@. Bases and dimensions in different degrees
		  are obtained using @TO "basis(ZZ,ZZ,VectorSpace)"@ and 
		  @TO "dims(ZZ,VectorSpace)"@. The output of the latter is
		  a matrix consisting of dimensions of the vector space 
		  for different first degrees and last
		  degrees. The basis elements for the homology are represented
		  as cycles in the Lie algebra. 
		  The set of boundaries and the set of cycles
		  are subalgebras of the Lie algebra, and they are obtained using
		  @TO boundaries@ and @TO cycles@, and bases and dimensions 
		  of them are 
		  obtained in the same way as for homology.
		  
		  
		  
		Example
		  use D3
		  L4 = D3/{a a,b b}	          	 
		  H4 = lieHomology L4
		  B4 = boundaries L4
		  C4 = cycles L4
		  dims(5,H4)
		  basis(4,1,H4)
		  basis(4,1,B4)

		Text
		  It follows from the result above that a basis for the 
		  cycles of weight (4,1) is
		  \{{\tt b a c, a b c}\}. 
		  
		Example
		  basis(4,1,C4)
		  
		Text
		  The product of a cycle and a boundary is a boundary: 
		  
		Example
		  (b a c) (b a c + (a b c))
		  member(oo,B4)
		  
		Text
		  In weight (3,1) there are 
		  two independent cycles and no boundaries:
		  
		Example		  
		  basis(3,1,H4)
		  basis(3,1,B4) 

		Text		
	          In weight (5,1) all elements are boundaries, so the 
		  homology is 0, which is seen in the table above. 
		  In weight (5,2) there are no cycles.
		  
		Example		  
		  basis(5,1,B4)
		  basis(5,1,L4)
		  d4 = differential L4
		  b52 = basis(5,2,L4)
		  d4\b52		
		  basis(5,2,C4)
		  
		
///
end		   


		   
		  
		   
