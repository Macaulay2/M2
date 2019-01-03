doc ///

     	Key
	    	"Differential LieAlgebra Tutorial"
	Headline
	        A tutorial for differential Lie algebras
	SeeAlso
	         
		 "First LieAlgebra Tutorial"
		 "Second LieAlgebra Tutorial"
		 "Holonomy Lie algebras and Symmetries"
		 "Constructing Lie algebras"	
	
	Description
		Text
		        A differential Lie algebra is defined by changing the option
		        	@TO diffl@  
			to true, diffl=>true, in the constructor @TO lieAlgebra@ and using  
			@TO diffLieAlgebra@
			with input the differential of the generators and output the new
			differential Lie algebra. The last weight is 
			the homological degree and it must be non-negative and
			less than the first degree. 
			The differential preserves all weights except the homological
			degree which is lowered by 1
			and it also changes the sign. All this is checked when
			@TO diffLieAlgebra@ 
			is executed.
			The value zero for a generator is given as L.zz, which has any
			weight and sign (see however @TO weightLie@ and @TO signLie@). 		
			The differential is a derivation and is obtained by 
			@TO diffLie@ 
			as
			d=diffLie(). The value of the differential d applied to 
			an arbitrary Lie expression x is obtained by d(x).       
			
		Example
		        F1 = lieAlgebra({a,b,c},genWeights=>{{1,0},{2,1},{3,2}},
			     genSigns=>{1,1,1},diffl=>true)
			L1=diffLieAlgebra{F1.zz,a a,a b}
			peekLie L1
			d = diffLie()
			x = a b b + (1/2) a a c		
			d x
					       
		Text
		        The program adds relations to the Lie algebra to get
			the square of the differential
			to be zero and a warning is printed if this is done. 
			The current Lie algebra must be free 
			when @TO diffLieAlgebra@ is used.  It is also
			possible to define the differential using
			L.genDiffs (see L3 below), but then no checking is made.
			
			
    	    	Example		 
		  F2 = lieAlgebra({a,b,c2,c3,c4},genSigns=>{0,0,1,0,1},
                       genWeights=>{{1,0},{1,0},{2,1},{3,2},{5,3}},diffl=>true)
	          L2=diffLieAlgebra{F2.zz,F2.zz,a b,a c2,a b c3}
		  peekLie L2	      
		  		 
		Text
		  The dimensions of the homology can be obtained using 
		  @TO homologyTableLie@, which gives a  
		  table of dimensions for the first and the last degrees.
		  
		  A basis for the homology, boundaries and cycles 
		  in a certain first and homological degree is 
		  obtained by @TO homologyBasisLie@,  
		  @TO cyclesBasisLie@ and @TO boundariesBasisLie@.
		  
		Example	          	 
		  L3 = lieAlgebra({a,b,c},genSigns=>1,
		      genWeights=>{{1,0},{1,0},{2,1}},diffl=>true)/{a a,b b}
		  L3.genDiffs = {L3.zz,L3.zz,a b}
		  homologyTableLie 5
		  homologyBasisLie(4,1)
		  boundariesBasisLie(4,1)

		Text
		  It follows from above that a basis for the cycles of weight (4,1) is
		  \{b a c, a b c\}. 
		  
		Example
		  cyclesBasisLie(4,1)
		  
		Text
		  In degree (3,1) there are 
		  two independent cycles and no boundaries:
		  
		Example		  
		  homologyBasisLie(3,1)
		  boundariesBasisLie(3,1) 

		Text		
	          In degree (5,1) all elements are boundaries, so the 
		  homology is zero, which is seen in the table above. In degree (5,2) there are no cycles.
		  
		Example		  
		  boundariesBasisLie(5,1)
		  basisLie(5,1)
		  d = diffLie()
		  basisLie(5,2)
		  d(a c c)
		  d(b c c)		
		  cyclesBasisLie(5,2)
		  
		Text
		  Below is an example with zero homology and hence also its minimal model, 
		  see 
		  @TO minmodelLie@,
		  is the zero Lie algebra.
		  
		Example
		  L4 = lieAlgebra({a,b},genSigns=>{1,0},genWeights=>{{2,0},{2,1}},diffl=>true)
		  L4.genDiffs = {L4.zz,a}		  
		  homologyTableLie 12
		  M4 = minmodelLie 12
		  peekLie oo
		  whichLie()
		  useLie M4
		  homologyTableLie 12
		  peekLie M4.modelmap
		  
		Text
		  The above result is true in characteristic zero and false in 
		  positive characteristic.
		  
		Example
		  L5 = lieAlgebra({a,b},genSigns=>{1,0},genWeights=>{{2,0},{2,1}},field=>ZZ/3,diffl=>true)
		  L5.genDiffs = {L5.zz,a}		 
		  homologyTableLie 12
		  homologyBasisLie(6,1)
		  homologyBasisLie(6,2)
		  		  
	        Text
		  In characteristic different from two, the differential of
		  (b b a) is 3/2(b a a) and (b a a) is a cycle, which explains the result
		  above (in characteristic 2, (b a) is a cycle which is not a boundary).
		  
	        Example
		  useLie L4
		  d = diffLie()
		  d b b a
		  
		Text  
		  Below is a differential Lie algebra which is non-free and where the 
		  differential has a linear part. 
 
		  
	        Example
		  F6 = lieAlgebra({a,b,c,r3,r4,r42},	             
	             genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},	             
	             genSigns=>{0,0,0,1,1,0},diffl=>true)
		  L6=diffLieAlgebra{F6.zz,F6.zz,F6.zz,a c,a a c,r4 - a r3}/{b c - a c,a b,b r4 - a r4}		  
	          homologyTableLie 5
		  
		Text
		  The homology in homological degree zero is concentrated in degree
		  1 and 2. The function @TO minPresLie@ gives a minimal presentation
		  of the Lie algebra H_0 (and it also changes the current Lie algebra).
		  
		Example  
		   peekLie (P= minPresLie 3)
		   		   
		Text
		  We now compute the minimal model of L6 and check that its homology is 
		  the same as for L6.
		  
		Example
		   whichLie()
		   useLie L6 
		   M6 = minmodelLie 5
		   useLie M6
		   homologyTableLie 5
		   peekLie M6
		   
		Text
		  The quasi-isomorphism is obtained as f=M.modelmap. 
		  If L has no differential, then		   
		  f is surjective, but in general this is
		  not true as is shown by L4 above and also by this example. 
		  
		Example
		   f = M6.modelmap
		   peekLie f 
		   useLie L6
		   dimTableLie 5
		   imageTableLie(5,f)
		   
		Text
		  We check below that H(f) is iso in degree (5,1).
		  
		Example
		   homologyBasisLie(5,1)
		   useLie M6
		   homologyBasisLie(5,1)
		   f oo
		   
		Text
		  The generators for the minimmal model M of L yield basis elements 
		  for the cohomology $Ext_{UL}(k,k)$ (with a shift in the homological
		      degree, k=L.field). 
		  Its dimensions may be obtained using
		  @TO extTableLie@ (observe that the first row gives 
		      $Ext_{UL}(k,k)$ in degree 1 to 5 and homological degree 1 ).
		  
		Example
		   useLie L6
		   extTableLie 5
		   M6.genWeights
///
end		   


		   
		  
		   
