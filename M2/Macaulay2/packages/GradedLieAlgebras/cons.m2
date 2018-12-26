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
		"Differential LieAlgebra Tutorial"				
		"Holonomy Lie algebras and Symmetries" 
		 
	Description
		Text
	       	  There are three new Types in this package, @TO LieAlgebra@, @TO MapLie@ 
		  and @TO DerLie@. All of them have  
		  @TO MutableHashTable@ as parent. To each Type, there is a general constructor, 
		  @TO lieAlgebra@, @TO mapLie@ and @TO derLie@, where the first returns a free
		  Lie algebra. 
		  Also each element of type @TO LieAlgebra@
		  is a Type with parent @TO LieElement@. 
		  There are two general constructions of a new Lie algebra
		  given two Lie algebras, namely the free product, 
		  @TO  (symbol *,LieAlgebra,LieAlgebra)@, 
		  which corresponds to the free product of enveloping algebras, 
		  and the direct sum (product),
		  @TO  (symbol **,LieAlgebra,LieAlgebra)@,
		  which corresponds to the tensor product of enveloping algebras. Other 
		  constructions of new Lie algebras are
		Text
		  @TO diffLieAlgebra@, a differential Lie algebra		  
		Text
		  @TO (symbol /,LieAlgebra,List)@, a quotient Lie algebra
		Text
		  @TO ambient@, applied to L it gives a free Lie algebra of which L (without differential) is a quotient 		  
		Text
		  @TO minmodelLie@, the minimal model		  
		Text
		  @TO koszulDualLie@, the Lie algebra whose enveloping algebra 
		  is the Koszul dual of a quadratic algebra
		Text
		  @TO holonomyLie@, the holonomy Lie algebra of a hyperplane arrangement (or matroid)
		Text
		  @TO minPresLie@, a minimal presentation
		   
		  
		  
		Text
		  In the example below, we first define a Lie algebra L
		  by the constructor @TO lieAlgebra@. Then @TO minmodelLie@ is used
		  to construct the Lie algebra M, together with the quasi-isomorphism
		  f:M->L, which is obtained by use of the key @TO modelmap@. Also,
		  M itself can be obtained as L.minmodel, if @TO minmodelLie@ 
		  has been executed. 
		  Observe that the current Lie algebra is not changed to M. 		 
		  The differential in M is obtained by use of the function @TO diffLie@. 
		  Now @TO peekLie@ is used to look at the different hashTables and finally 
		  @TO extTableLie@ gives the dimensions of Ext_{UL}(QQ,QQ), an 
		  expected result, since the KoszulDual of UL is QQ[x].
		  
		  
		Example
		  L = lieAlgebra({a},genSigns=>1)/{a a}
		  M = minmodelLie 5
		  f = M.modelmap
		  L.minmodel		  
		  useLie M
		  d = diffLie()
		  d fr_1
		  peekLie L
		  peekLie M
		  peekLie f
		  peekLie d		  
		  useLie L
		  extTableLie 5
         	  L1 = koszulDualLie(QQ[x])
		  peekLie L1
		  
		Text
		  We see that L1 is the same Lie algebra as L, except that the name of 
		  the generator is different. Also L.compdeg=5 while L1.compdeg=1 which 
		  means that L has been computed up to degree 5 while L1 has been computed
		  up to degree 1. 
		  
	       
		  
		Text
		  Here is the first found example of a non-Koszul algebra, due to
		  Christer Lech. It is the polynomial algebra in four variables
		  modulo five general quadratic forms, which may be specialized as follows.
		  
		Example
		  R = QQ[x,y,z,u]
                  I = {x^2,y^2,z^2,u^2,x*y+z*u}
                  S = R/ideal I
		  hilbertSeries(S,Order=>4)
                  L = koszulDualLie(S)
		  extTableLie 4
	        Text
		  Below is an example of the use of "formal" Lie multiplication to compute
		  the Lie algebra of strictly upper triangular 5x5-matrices by means
		  of its multiplication table. The relation (e14 e15) is of degree 7
		  and in order to compute this, the free algebra F below must be 
		  computed in degree 7, where it is of dimension 7596. To avoid this, the 
		  relations are not normalized and in this way F need not be computed. The dimensions
		  of the quotient are then easy to compute and also a minimal presentation. 
		  The "at sign"
		  is used as formal Lie multiplication and formal 
		  multiplication by scalars, ++ is used as
		  formal addition and / is used as formal subtraction. 
		  Observe that "at sign" like SPACE  is right associative, 
		  while / is left associative, so a/b/c means a-b-c and not a-b+c.
		  Here is an example of a formal 
		  expression, whose normal form is zero. The normal form may be obtained by copying and pasting
		  the expression or applying normalFormLie.
		  
		Example
		   L=lieAlgebra{a,b,c}
		   a@b@c/3@a@b@c/2@a@b@c++4@a@b@c
		   normalFormLie oo
		   
		Text
		   Here is the matrix example. Observe that the quotient algebra is defined by just 
		   giving the value of F.relsLie. In this way there is no checking done of the correctness 
		   of the relations. Writing the quotient in the usual way as F/(F.relsLie) 
		   has however the effect that F.relsLie is computed.
		   
		Example
		   F=lieAlgebra({e12,e23,e34,e45,e13,e24,e35,e14,e25,e15},
		       genWeights=>{1,1,1,1,2,2,2,3,3,4})
		   F.relsLie={e12@e34,e12@e45,e23@e45,e12@e13,e12@e35,e12@e14,
	                     e12@e15,e23@e45,e23@e13,e23@e24,e23@e14,e23@e25,
	                     e23@e15,e34@e24,e34@e35,e34@e14,e34@e25,e34@e15,
                 	     e45@e13,e45@e35,e45@e25,e45@e15,e13@e24,e13@e14,
	                     e13@e25,e13@e15,e24@e35,e24@e14,e24@e25,e24@e15,
	                     e35@e14,e35@e25,e35@e15,e14@e25,e14@e15,e25@e15,
                             e12@e23/e13, e12@e24/e14,
                             e12@e25/e15, e13@e34/e14,
                             e13@e35/e15, e14@e45/e15,
                             e23@e34/e24, e23@e35/e25,
                             e24@e45/e25, e34@e45/e35}
		   dimsLie 5
		   peekLie minPresLie 4
		   
		
		Text
		  The following example shows a way to determine the derivations 
		  of a Lie algebra studied by David Anick, 
		  which may be seen as the positive part of the twisted loop
		  algebra on sl_2. 

		Example
                   L = lieAlgebra({a,b})/{a a a b,b b b a}
                   dimsLie 20
		   
		Text
		  The derivations of degree 0 is 2-dimensional and contains the Euler
		  derivation, which is the identity in degree one.
		  
		Example
		   deuler = derLie({a,b})
		   deuler b a b a b a b a
		   
		Text
		  The linear maps from degree 1 to degree 7 is 4-dimensional. Not all
		  of them define derivations.
		  
		Example 		  
		   basisLie 7
		   da61 = derLie{a b a b a b a,L.zz}
		   db61 = derLie{L.zz,a b a b a b a}
		   da62 = derLie{b b a b a b a,L.zz}
		   db62 = derLie{L.zz,b b a b a b a}
		   
		Text
		   Hence, da61 and db62 are derivations. To determine if a linear
		   combination of db61 and da62 is a derivation (i.e., maps the
		   relations in L to zero), we consider
		   derivations from the free Lie algebra on a,b to L.
		   
		Example
		   M = lieAlgebra({a,b})
		   f = imapLie(L,M)
		   useLie L		  
		   dMb61 = derLie(f,{L.zz,a b a b a b a})
		   dMa62 = derLie(f,{b b a b a b a,L.zz})
		   useLie M
		   dMb61 a a a b
		   dMa62 a a a b
		   
		Text
		   It follows that the only linear combination of dMb61 and dMa62 
		   which is zero on (a a a b) is dMb61, but we have seen that db61 is
		   not a derivation on L. Hence the derivations of degree 6 is 2-dimensional.
		   Also, da61 + db62 is the inner derivation corresponding to
		   right multiplication with the basis element of degree 6, (b a b a b a). 
		   This is seen by using @TO multListLie@ (one could also have used SPACE as
		   multiplication operator on the lists).
		   
		Example
		   peekLie(da61+db62)
		   multListLie({a,b},{b a b a b a})
		   
		Text
		   Since the dimension of the Lie algebra in degree 8 is one, the dimension
		   of the derivations of degree 7 is at most 2.
		Example		   
		   useLie L		   
		   peekLie derLie({b a b a b a b a,L.zz})
		   peekLie derLie({L.zz,b a b a b a b a})
		   {a,b} {(basisLie 7)_1}
		   {a,b} {(basisLie 7)_0}
		   
		Text
		   From the above, it follows that the derivations of degree 7 is 
		   also 2-dimensional, but all are inner derivations. The conclusion
		   is that the derivations of L of positive degree modulo the inner 
		   derivations is 1-dimensional in all even
		   degrees, and 0 in all odd degrees. 
		   We may also use @TO (symbol SPACE,DerLie,DerLie)@
		   to examine the  
		   structure of this quotient Lie algebra. 
		   
		Example
		   d2 = derLie({a b a,L.zz})
		   d4 = derLie({a b a b a,L.zz})		   
		   peekLie d2 d4
		   		   
	        Text
		   Define dn (n>=2, n even) as the derivation which maps a 
		   to (a b a b ... a] of
		   length n+1 and b to 0. It follows from above that [d2,d4]=d6.
		   
		Example
		   d6 = derLie({a b a b a b a,L.zz})
		   peekLie d2 d6
		   d16 = derLie({a b a b a b a b a b a b a b a b a,L.zz})
		   peekLie d2 d16
		   		   
		Text	   
		  
		   It follows that [d2,d6]=2d8 and [d2,d16]=7d18.		   
		   In fact, this Lie algebra is the infinite 
		   dimensional filiform Lie algebra, which is  
		   the Witt algebra in positive degrees (with a degree doubling).
		 
		   
		  
		  
		Text  
		  See @TO "Holonomy Lie algebras and Symmetries"@ for some examples 
		  on how to use the constructor @TO holonomyLie@. 
		  
///
end
