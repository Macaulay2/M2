doc ///
	Key
		"Symmetries"
	Headline 
		Operating by permutations of the generators
	SeeAlso
	         "First LieAlgebra Tutorial"
		 "Second LieAlgebra Tutorial"
		 "Differential Lie algebras Tutorial"
		 "Constructing Lie algebras"
		 "How to write Lie elements"
		 
		 
	Description
	     Text
	          The function @TO holonomyLie@ constructs the holonomy Lie algebra
		  of an arrangement or matroid given by the set of 2-flats. 
		  The input may be any set of 
		  subsets of a finite set, such that all subsets have at most one element 
		  in common and are of length at least three. Indeed, for any such set
		  of subsets there is a unique simple matroid of rank at most three with the
		  given set as the set of 2-flats of size at least three, and @TO holonomyLie@
		  gives the holonomy Lie algebra of this matroid.
		  
	     Example
		  L = holonomyLie({{1,2,3},{1,4,5},{2,4,6}})
		  L.relsLie
		  decompidealLie 3
		  
	     Text
	          The kernel of the map
		  from L to the direct sum of the local Lie algebras, see @TO localLie@,
		  is @TO decompidealLie@. Since it is the empty set, 
		  L is the direct sum of the
		  local Lie algebras. They may be obtained using @TO localLie@ 
		  (observe that the numbering of the flats begins with 0).
		  
	     Example
	          localLie(2,4)
		  
	     Text
	          The kernel of the map from L to one of the  local Lie algebras, in 
		  degree at least two, may be obtained as follows (i0 is the kernel
		  of the map from L to the first local Lie algebra, etc).
		  
	     Example
	          i0=idealBasisLie(4,{[4,5],[4,6]})
		  i1=idealBasisLie(4,{[2,3],[4,6]})
		  i2=idealBasisLie(4,{[2,3],[4,5]})
		  
	     Text
	          The intersection of these ideals is @TO decompidealLie@.
		  
	     Example
	          intersectionLie(4,{i0,i1,i2})
		  
	     Text
		  Here is the "quadrangel" arrangement, i.e., the graphical
		  arrangement of the complete graph on four vertices. Its
		  holonomy Lie algebra is not decomposable.
		  
	     Example
	          L = holonomyLie({{1,2,3},{1,4,5},{2,4,6},{3,5,6}})
		  decompidealLie 3
		  
	     Text
	          Here is a way to obtain @TO decompidealLie@ (which is not used
		  in its program). The direct
		  sum of the local Lie algebras may be obtained as follows
		  
	     Example
	          M=holonomyLie({{1,2,3},{a1,4,5},{a2,a4,6},{a3,a5,a6}})
		  
	     Text
	          and the map from L to M is given as
		  
	     Example
	          f=mapLie(M,L,{{{1,1},{[1],[a1]}},{{1,1},{[2],[a2]}},{{1,1},{[3],[a3]}},
			  {{1,1},{[4],[a4]}},{{1,1},{[5],[a5]}},{{1,1},{[6],[a6]}}})
		  
	     Text
	          and hence the ideal may be obtained as the kernel of f
		  
	     Example
	         kernelBasisLie(3,f)
	         
		 	    
	     Text
	          The symmetric group S_4 operates on the vertices of K_4
		  and this induces an action of S_4 on the six edges, which
		  in turn induces an action of S_4 on L as automorphisms. One
		  such permutation of the edges is (123)(465) but not (123)(456).
		  It is possible to check, using @TO symmCyclePermLie@, 
		  if a permutation of the generators, 
		  written as a product
		  of cycles, defines an automorphism of the Lie algebra.
		   		   
	     Example
	          useLie L
		  symmCyclePermLie({{1,2,3},{4,5,6}})
		  symmCyclePermLie({{1,2,3},{4,6,5}})
		  peek oo
		  
	     Text
	          The ideal decompidealLie is invariant under all automorphisms
		  of L. Wee may use @TO characterLie@ and a character table
		  for S_4 to determine its irreducible
		  representation constituents. There are four conjugacy classes 
		  (except id). Representatives for them as permutation of the six 
		  generators are (23)(45), (123)(465), (16)(2354) and (16)(25)  
		  corresponding to one 2-cycle, one 3-cycle, one 4-cycle and a product
		  of two 2-cycles.
		  
	     Example
	          dec4=decompidealLie 4
		  characterLie(4,{{2,3},{4,5}},dec4)
		  characterLie(4,{{1,2,3},{4,6,5}},dec4)
		  characterLie(4,{{1,6},{2,3,5,4}},dec4)
		  characterLie(4,{{1,6},{2,5}},dec4)
		  
	     Text
	          Making calculations with the character table, we see that
		  decompidealLie 4 is the sum of the irreducible representations
		  except the trivial representation. 
		  
		  
		  
		  
	     	  
    	    	  
///
end
