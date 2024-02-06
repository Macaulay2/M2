doc ///
	Key
	
	        "Homomorphisms and derivations"

	SeeAlso
                "map(LieAlgebra,LieAlgebra,List)"
		lieDerivation
		"isWellDefined(ZZ,LieAlgebraMap)"
	        "isWellDefined(ZZ,LieDerivation)"
		"Differential Lie algebra tutorial"				
		
		 
	Description
		Text
	       	  A Lie algebra homomorphism $M \ \to\ L$ is defined
		  using @TO "map(LieAlgebra,LieAlgebra,List)"@ by giving the
		  values in $L$ of the generators of $M$. A homomorphism 
		  preserves weight and sign, and {\tt M#Field} must be the same as 
		  {\tt L#Field}.
		Example
		  M = lieAlgebra({x,y},Weights => {2,2}) 
		  L = lieAlgebra({a,b},Signs => 1)
		  f1 = map(L,M,{a a,b b})
		  describe f1
		Text
		  Like the situation for ring maps, the meaning of {\tt map(L,M)}
		  is that a generator in $M$ is sent to the generator in $L$ with 
		  the same name, weight and sign if there is such a generator, 
		  otherwise it is sent to zero.
		Example
		  M = lieAlgebra{a,b,c}
		  L = lieAlgebra({a,b,d},Weights => {2,1,1}) 
		  f2 = map(L,M)
		  describe f2
		Text
		  Another similarity with ring maps is that a map $M \ \to\  L$ 
		  need not 
		  be well defined, in the sense that the relations in $M$
		  need not be sent to zero in $L$. 
		  It may also happen that the map
		  does not commute with the differentials in $M$ and $L$. All this can 
		  be checked up to a certain degree using 
		  @TO "isWellDefined(ZZ,LieAlgebraMap)"@. If $M$ is finitely presented,
		  see @TO "Quotient Lie algebras and subspaces"@, then it is possible 
		  to get the information that the map is well defined and
		  commutes with the differentials for all degrees, if the 
		  first input $n$ in {\tt isWellDefined(n,f)} is big
		  enough.
		Example
		  F=lieAlgebra({a,b},Weights => {{1,0},{2,1}},Signs => 1,
		      LastWeightHomological => true)
		  D=differentialLieAlgebra{0_F,a a}
		  f=map(D,F)
		  isWellDefined(2,f)
		  use F
		  Q=F/{a a}
		  g=map(Q,D)
		  isWellDefined(2,g)
		Text
		  Surjectivity for a Lie algebra map may be tested using 
		  @TO "isSurjective(LieAlgebraMap)"@. The input map might not be well
		  defined. The method function @TO "isIsomorphism(LieAlgebraMap)"@ 
		  may be used to test if a Lie algebra map $f: M \ \to\  L$ 
		  is an isomorphism.
		  Here $M$ and $L$ must be equal, but not necessarily identical. Also,
		  $M$ must be finitely presented. It is 
		  tested that the map is well defined, commutes with the differentials 
		  and is surjective. Injectivity follows from this by dimension 
		  reasons. See @TO "Holonomy Lie algebras and symmetries"@ for 
		  applications where the map is a permutation of the variables.
		Example
		  isSurjective f
		  use F
		  Q1=F/{a a}
		  Q1===Q
		  Q1==Q
		  h=map(Q1,Q)
		  isIsomorphism h
		Text
		  A derivation $d: M \ \to\  L$ is defined using @TO lieDerivation@
		  by giving a Lie algebra map
		  $f: M \ \to\  L$ and a list of elements in $L$ 
		  that are the values of $d$
		  on the generators of $M$. 
		  One may use @TO "isWellDefined(ZZ,LieDerivation)"@
		  to test if a derivation is well defined, which means that
		  the relations in $M$ are sent to zero (the derivation need not 
		  commute with the differentials). 		 
		Example
		  use Q
		  d=lieDerivation(g,{a b,b b})
		  isWellDefined(2,d) 
		  use D
		  f=map(D,F)
		  d=lieDerivation(f,{a b,b b})
		  isWellDefined(2,d) 
		Text
		  Omitting the first input in @TO lieDerivation@ gives derivations
		  $d: L \ \to\  L$ with the identity map on $L$ as the defining map.
		Text
		  The following example shows a way to determine the derivations 
		  of a Lie algebra studied by David Anick, 
		  which may be seen as the positive part of the twisted loop
		  algebra on sl_2. This also explains the periodic behaviour 
		  of the Lie algebra. 

		Example
                   L = lieAlgebra{a,b}/{a a a b,b b b a}
                   dims(1,20,L)
		   
		Text
		  The space of derivations of degree 0 is 2-dimensional, 
		  and contains the Euler
		  derivation, see @TO "euler(LieAlgebra)"@, 
		  which is the identity in degree 1.
		  
		Example
		   deuler = euler L
		   deuler b a b a b a b a
		   
		Text
		  We will now prove that the space of derivations
		  of degree 6 is 2-dimensional.
		  The space of linear maps from degree 1 to degree 7 
		  is 4-dimensional. 
		  Not all
		  of them define derivations.
		  
		Example 		  
		   basis(7,L)
		   da61 = lieDerivation{a b a b a b a,0_L}
		   isWellDefined(4,da61)
		   db61 = lieDerivation{0_L,a b a b a b a}
		   isWellDefined(4,db61)
		   da62 = lieDerivation{b b a b a b a,0_L}
		   isWellDefined(4,da62)
		   db62 = lieDerivation{0_L,b b a b a b a}
		   isWellDefined(4,db62)
		   
		Text
		   The output displayed above shows that
		   {\tt da61} and {\tt db62} are derivations. 
		   To determine whether a linear
		   combination of {\tt db61} and {\tt da62} is well defined 
		   (i.e., maps the
		   relations in $L$ to zero), we consider
		   derivations from the free Lie algebra $M$ on $a,b$ to $L$.
		   
		Example
		   M = lieAlgebra{a,b}
		   f = map(L,M)
		   use L		  
		   dMb61 = lieDerivation(f,{0_L,a b a b a b a})
		   dMa62 = lieDerivation(f,{b b a b a b a,0_L})
		   use M
		   dMb61 a a a b
		   dMa62 a a a b
		   
		Text
		   It follows from the output displayed above
		   that the only linear combination of {\tt dMb61} 
		   and {\tt dMa62}  
		   that is zero on {\tt (a a a b)} is a multiple of {\tt dMb61}, 
		   but we have seen that {\tt dMb61} is
		   not a derivation on $L$. 
		   Hence, the space of derivations of degree 6 is 2-dimensional.
		   Also, {\tt da61 + db62} is the inner derivation corresponding to
		   right multiplication with the basis element of 
		   degree 6, {\tt (b a b a b a)}. 
		   This is seen by using @TO innerDerivation@.
		   
		Example
		   use L
		   da61+db62===innerDerivation(b a b a b a)
		  
		Text
		   Since the dimension of the Lie algebra in degree 8 is 1, 
		   the dimension
		   of the space of derivations of degree 7 is at most 2.
		Example		   		   
		   da7=lieDerivation({b a b a b a b a,0_L})
		   isWellDefined(4,da7)
		   db7=lieDerivation({0_L,b a b a b a b a})
		   isWellDefined(4,db7)
		   da7===innerDerivation(b b a b a b a)
		   db7===innerDerivation(a b a b a b a)
		   
		Text
		   It follows from the output displayed above that the space 
		   of derivations of degree 7 is 
		   also 2-dimensional, but consists only of inner derivations. 
		   The conclusion
		   is that the space of derivations of $L$ of positive degree 
		   modulo the inner 
		   derivations is 1-dimensional in all even
		   degrees, and 0 in all odd degrees. 
		   We may also use @TO (symbol SPACE,LieDerivation,LieDerivation)@
		   to examine the  
		   structure of this quotient Lie algebra. 
		   
		Example
		   d2 = lieDerivation({a b a,0_L})
		   d4 = lieDerivation({a b a b a,0_L})		   
		   describe d2 d4
		   		   
	        Text
		   Define $dn$ ($n\ \ge\ 2$, $n$ even) as the derivation which maps $a$ 
		   to {\tt  (a b a b ... a)} of
		   length $n+1$ and $b$ to 0. 
		   It follows from the output displayed above that [ $d2$, $d4$ ] = $d6$.
		   
		Example
		   d6 = lieDerivation({a b a b a b a,0_L})
		   describe d2 d6
		   d16 = lieDerivation({a b a b a b a b a b a b a b a b a,0_L})
		   describe d2 d16
		   		   
		Text	   
		  
		   It follows from the output displayed above 
		   that [ $d2$, $d6$ ] = $2d8$ and [ $d2$, $d16$ ] = $7d18$.		   
		   In fact, this Lie algebra is the infinite 
		   dimensional filiform Lie algebra, which is  
		   the Witt algebra in positive degrees (with a degree doubling).
		 
		   
		   
		   
		  
		  
		  
		 
		
///
end
