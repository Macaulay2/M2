doc ///
	Key
		"Differential Lie algebras Tutorial"
	Headline 
		A tutorial for differential Lie algebras
	SeeAlso
                 "First LieAlgebra Tutorial"
		 "Second LieAlgebra Tutorial"		 
		 "Constructing Lie algebras"
		 "How to write Lie elements" 
		 "Symmetries"   
	Description
		Text
		  See @TO "Second LieAlgebra Tutorial"@ on 
		  how to use the option @TO genDiffs@.
		  
    	    	Example		 
		  L=lieAlgebra({a,b,c},{[a,a],[b,b]},genSigns=>1,
		      genWeights=>{{1,0},{1,0},{2,1}},genDiffs=>{[],[],[a,b]})
		  		 
		Text
		  The dimensions of the homology can be obtained using @TO homologyLie@. A
		  basis for the homology and the boundaries 
		  in a certain first and homological degree is 
		  obtained by @TO homologyBasisLie@ and @TO boundariesBasisLie@.
		  
		Example
		  homologyLie 5
		  boundariesBasisLie(4,1)		  
		  invImageBasisLie(4,1,diffLie(),oo)
		  evalDiffLie[c,c]
		  homologyBasisLie(4,1)
		  
		Text
		  It follows from above that a basis for the cycles of weight (4,1) is
		  \{[b,a,c],[a,b,c]\}. In degree (3,1) there are 
		  two independent cycles and no boundaries:
		  
		Example		  
		  homologyBasisLie(3,1)
		  boundariesBasisLie(3,1)
		  
		Text		
	          In degree (5,1) all elements are boundaries, so 
		  homologyLie(5,1)=0 (which was obtained above). In degree (5,2) there are no cycles.
		  
		Example		  
		  boundariesBasisLie(5,1)
		  basisLie(5,1)
		  homologyLie(5,1)
		  basisLie(5,2)
		  evalDiffLie([c,a,c])
		  evalDiffLie([c,b,c])
		  homologyLie(5,2)
		  boundariesBasisLie(5,2)
		  
		Text
		  Here is an example with zero homology and hence also its minimal model, 
		  see @TO minmodelLie@,
		  is the zero Lie algebra.
		  
		Example
		  L1=lieAlgebra({a,b},{},genSigns=>{0,1},genWeights=>{{2,0},{2,1}},
		      genDiffs=>{[],[a]})		  
		  homologyLie 12
		  M1=minmodelLie 12
		  peek oo
		  whichLie()
		  useLie M1
		  homologyLie 12
		  peek  M1.modelmap
		  
		Text
		  The above result is true in characteristic zero and false in 
		  positive characteristic.
		  
		Example
		  L3=lieAlgebra({a,b},{},genSigns=>{1,0},genWeights=>{{2,0},{2,1}},
		      genDiffs=>{[],[a]},field=>ZZ/3)		 
		  homologyLie 12
		  homologyBasisLie(6,1)
		  homologyBasisLie(6,2)
		  
	        Text
		  In characteristic different from two, the differential of
		  [b,b,a] is 3/2[b,a,a] and [b,a,a] is a cycle, which explains the result
		  above (in characteristic 2, [b,a] is a cycle which is not a boundary).
		  
		Text  
		  Below is a differential Lie algebra which is non-free and where the 
		  differential has a linear part.  
		  
	        Example
		  L=lieAlgebra({a,b,c,r3,r4,r42},
	             {{{1,-1},{[b,c],[a,c]}},[a,b],{{1,-1},{[b,r4],[a,r4]}}},
	             genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
	             genDiffs=>{[],[],[],[a,c],[a,a,c],{{1,-1},{[r4],[a,r3]}}},
	             genSigns=>{0,0,0,1,1,0}) 
	          homologyLie 5
		  
		Text
		  The homology in homological degree zero is concentrated in degree
		  1 and 2. The function @TO minPresLie@ gives a minimal presentation
		  of the Lie algebra H_0 (and L is still the current Lie algebra). 
		  
		Example  
		   minPresLie 3
		   peek oo
		   whichLie()
		   
		Text
		  We now compute the minimal model of L and check that its homology is 
		  the same as for L.
		  
		Example 
		   M=minmodelLie 5
		   useLie M
		   homologyLie 5
		   peek M
		   
		Text
		  The quasi-isomorphism is obtained as f=M.modelmap. In homological
		  degree zero, 
		  f is surjective, but in general in higher homological degrees, it is 
		  not so as in this example, see below. 
		  
		Example
		   f=M.modelmap 
		   peek f
		   useLie L
		   dimTableLie 5
		   imageLie(5,f)
		   
		Text
		  We check below that H(f) is iso in degree (5,1).
		  
		Example
		   homologyBasisLie(5,1)
		   useLie M
		   homologyBasisLie(5,1)
		   evalMapLie(f,oo_0)
		   
		Text
		  The generators of M yield basis elements 
		  for the cohomology Ext_{UL}(k,k) (with a shift in the homological
		      degree , k=L.field). 
		  Its dimensions may be obtained using
		  @TO extAlgLie@ (observe that the first row gives 
		      Ext_{UL}(k,k) in degree 1 to 5 and homological degree 1 ).
		  
		Example
		   extAlgLie 5
		   M.genWeights
		   
		  
		   
		   
		 
		  
		  
///
end

		 