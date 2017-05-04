doc ///
	Key
	
	        "Constructing Lie algebras"
	Headline 
		An overview of ways to construct Lie algebras and maps
	SeeAlso
                lieAlgebra
		holonomyLie
		koszulDualLie
		minmodelLie
		minPresLie
		mapLie
		derLie	
	        "First LieAlgebra Tutorial"
		"Second LieAlgebra Tutorial"
		"Differential Lie algebras Tutorial"		
		"How to write Lie elements" 
		"Symmetries" 
		 
	Description
		Text
	       	  There are three new Types in this Package, @TO LieAlgebra@, @TO MapLie@ 
		  and @TO DerLie@. All of them have as immediate ancestor 
		  @TO MutableHashTable@. To each Type, there is a general constructor, 
		  @TO lieAlgebra@, @TO mapLie@ and @TO derLie@.
		  
		Text
		  In the example below, we first define a Lie algebra L
		  by the constructor @TO lieAlgebra@. Then @TO minmodelLie@ is used
		  to construct the Lie algebra M, together with the quasi-isomorphism
		  f:M->L, which is obtained by use of the key @TO modelmap@. Also,
		  M itself can be obtained as L.minmodel, if @TO minmodelLie@ 
		  has been executed. Observe that the current Lie algebra is not changed to M. 		 
		  The differential in M is obtained by use of the function @TO diffLie@. 
		  Now peek is used to look at the different hashTables and finally 
		  @TO extAlgLie@ gives the dimensions of Ext_{UL}(QQ,QQ), an 
		  expected result, since the KoszulDual of UL is QQ[x].
		  
		  
		Example
		  L = lieAlgebra({a},{[a,a]},genSigns=>1)
		  M = minmodelLie 5
		  L.minmodel		  
		  f = M.modelmap
		  useLie M
		  d = diffLie()
		  peek L
		  peek M
		  peek f
		  peek d
		  useLie L
		  extAlgLie 5
         	  L1=koszulDualLie(QQ[x])
		  peek L1
		  
		Text
		  We see that L1 is the same Lie algebra as L, except that the name of 
		  the generator is different. Also L.compdeg=5 while L1.compdeg=0 which 
		  means that L has been computed up to degree 5 while L1 is not computed. 
		  
		Text
		  The differential d in M may also be constructed using @TO derLie@.
		  
		Example
		  useLie M
		  d1=derLie({[],[fr_0,fr_0],[fr_0,fr_1],
			  {{1,4},{[fr_1,fr_1],[fr_0,fr_2]}},
		          {{2,1},{[fr_1,fr_2],[fr_0,fr_3]}}})
		  peek d1
		  peek d1.maplie
		  
		Text
		  Here is the first found example of a non-Koszul algebra, due to
		  Christer Lech. It is the polynomial algebra in four variables
		  modulo five general quadratic forms, which may be specialized as follows.
		  
		Example
		  R=QQ[x,y,z,u]
                  I={x^2,y^2,z^2,u^2,x*y+z*u}
                  S=R/ideal I
		  hilbertSeries(S,Order=>4)
                  L=koszulDualLie(S)
		  extAlgLie 4
		
		Text
		  The following example shows a way to determine the derivations 
		  of the Lie algebra L, which was found by David Anick 
		  and may be seen as the positive part of the twisted loop
		  algebra on sl_2. 
		  
		Example
                   L=lieAlgebra({a,b},{[a,a,a,b],[b,b,b,a]})
                   computeLie 20
		   
		Text
		  The derivations of degree 0 is 2-dimensional and contains the Euler
		  derivation, which is the identity in degree one.
		  
		Example
		   deuler=derLie({[a],[b]})
		   evalDerLie(deuler,[b,a,b,a,b,a,b,a])
		   
		Text
		  The linear maps from degree 1 to degree 7 is 4-dimensional. Not all
		  of them define derivations.
		  
		Example 		  
		   basisLie 7
		   da61=derLie({[a,b,a,b,a,b,a],[]})
		   db61=derLie({[],[a,b,a,b,a,b,a]})
		   da62=derLie({[b,b,a,b,a,b,a],[]})
		   db62=derLie({[],[b,b,a,b,a,b,a]})
		   
		Text
		   Hence, da61 and db62 are derivations. To determine if a linear
		   combination of db61 and da62 is a derivation (i.e., maps the
		   relations in L to zero), we consider
		   derivations from the free Lie algebra on a,b to L.
		   
		Example
		   M=lieAlgebra({a,b},{})
		   f=mapLie(L,M,{[a],[b]})		  
		   dMb61=derLie(f,{[],[a,b,a,b,a,b,a]})
		   dMa62=derLie(f,{[b,b,a,b,a,b,a],[]})
		   evalDerLie(dMb61,[a,a,a,b])
		   evalDerLie(dMa62,[a,a,a,b])
		   
		Text
		   It follows that the only linear combination of dMb61 and dMa62 
		   which is zero on [a,a,a,b] is dMb61, but we have seen that db61 is
		   not a derivation on L. Hence the derivations of degree 6 is 2-dimensional.
		   Also, da61 + db62 is the inner derivation corresponding to
		   right multiplication with the basis element of degree 6, [b,a,b,a,b,a].
		   
		Example		   
		   useLie L		   
		   da7=derLie({[b,a,b,a,b,a,b,a],[]})
		   db7=derLie({[],[b,a,b,a,b,a,b,a]})
		   multListLie({[a],[b]},{(basisLie 7)_0}) 
		   multListLie({[a],[b]},{(basisLie 7)_1})
		   
		Text
		   From the above, it follows that the derivations of degree 7 is 
		   also 2-dimensional, but all are inner derivations. The conclusion
		   is that the derivations of L of positive degree modulo the inner 
		   derivations is 1-dimensional in all even
		   degrees, and 0 in all odd degrees. 
		   We may also use @TO multDerLie@ to examine the  
		   structure of this quotient Lie algebra. 
		   
		Example
		   d2=derLie({[a,b,a],[]})
		   d4=derLie({[a,b,a,b,a],[]})		   
		   d=multDerLie(d2,d4)
		   evalDerLie(d,[a])
		   evalDerLie(d,[b])
		   
	        Text
		   Define dn (n>=2) as the derivation which maps a to [a,b,a,b,...,a] of
		   length n+1 and b to []. It follows from above that [d2,d4]=d6.
		   
		Example
		   d6=derLie({[a,b,a,b,a,b,a],[]})
		   d=multDerLie(d2,d6)
		   evalDerLie(d,[a])
		   evalDerLie(d,[b])
		   d16=derLie({[a,b,a,b,a,b,a,b,a,b,a,b,a,b,a,b,a],[]})
		   d=multDerLie(d2,d16)
		   evalDerLie(d,[a])
		   evalDerLie(d,[b])
		   
		Text	   
		  
		   It follows that [d2,d6]=2d8 and [d2,d16]=7d18.		   
		   In fact, this Lie algebra is the infinite 
		   dimensional filiform Lie algebra, which is  
		   the Witt algebra in positive degrees (with a degree doubling).
		   
		   
		  
		   
		  
		  
		Text  
		  See @TO "Symmetries"@ for some examples 
		  on how to use the constructor @TO holonomyLie@. 
		  
///
end



	        