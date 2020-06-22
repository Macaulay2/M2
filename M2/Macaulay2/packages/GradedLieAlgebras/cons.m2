doc ///
	Key
	
	        "Minimal models, Ext-algebras and Koszul duals"
	
	SeeAlso 
		koszulDual 
		minimalModel
		"minimalPresentation(ZZ,LieAlgebra)"
		extAlgebra
		"Differential Lie algebra tutorial"				
		"Holonomy Lie algebras and symmetries"
	Description
	      Text
	        The Koszul dual of the polynomial ring $\mathbb Q$ [ $x$ ] is the exterior 
		algebra on one odd generator. This is the enveloping algebra of
		the free Lie algebra on one odd generator $a$ modulo [$a$,$a$].
	      Example
	        R=QQ[x]
		L=koszulDual R
		describe L
	      Text
	        The Ext-algebra of $L$ is $Ext_{UL}(k,k)$, where $k$ is the 
		coefficient field of $L$.  
		It may be obtained using @TO extAlgebra@. 
		A vector space basis for the 
		Ext-algebra in positive degrees is obtained using
		@TO "generators(ExtAlgebra)"@. This basis originates from
		the Lie generators in the minimal
		model, @TO minimalModel@, 
		for which the homological degree have been raised by 1 and
		the signs changed.
	      Example
	        M=minimalModel(4,L)
		describe M
		E=extAlgebra(4,L)
		gE=gens E
		weight\gE
		sign\gE
	      Text
	        The product in the Ext-algebra, 
		@TO (symbol SPACE,ExtElement,ExtElement)@, 
		is derived by the program from the quadratic part 
		of the differential in the minimal model.
		The Ext-algebra is a skew-commutative algebra. 
		In case $L$ is the Koszul dual
		of a skew-commutative Koszul algebra $R$, 
		the Ext-algebra of $L$ is equal to $R$.
	      Example
		dims(4,E)
		ext_0 ext_0 ext_0 ext_0
	      Text
	          Observe that the first row of the matrix {\tt dims(4,E)} gives
		  the dimensions of $E$ in degree 1 to 5 and homological degree 1.
	      Text
		  Here is the first known example of a non-Koszul algebra, due to
		  Christer Lech. It is the polynomial algebra in four variables
		  modulo five general quadratic forms, 
		  which may be specialized as follows.		  
	      Example
		  R = QQ[x,y,z,u]
                  I = {x^2,y^2,z^2,u^2,x*y+z*u}
                  S = R/I
		  hilbertSeries(S,Order=>4)
                  L = koszulDual(S)
		  E=extAlgebra(4,L)
	          dims(4,E)
	      Text
	          The minimal model may also be used to 
		  compute a minimal presentation of a Lie algebra, 
		  see @TO "minimalPresentation(ZZ,LieAlgebra)"@.
		  Below is an example of computing a minimal presentation of
		  the Lie algebra of strictly upper triangular 5x5-matrices. The
		  Lie algebra is presented  by means of the multiplication table of
		  the natural basis \{$ekn;\ 1\ \le\ k\ <\ n \le\ 5$\}. 
		  The degree of $ekn$ is $n-k$. 
		  The relation [ $e14$, $e15$ ] is of degree 7 
		  in the free Lie algebra $F$ on the 
		  basis, and the dimension of $F$ in degree 7 is 7596. 
		  To avoid a computation
		  of the normal form of [ $e14$, $e15$ ] one uses "formal" operators.
		  The symbol $\@$
		  is used as formal Lie multiplication and formal 
		  multiplication by scalars, ++ is used as
		  formal addition, and / is used as formal subtraction. 
		  Observe that $\@$, like SPACE, is right associative, 
		  while / is left associative, so $a/b/c$ means $a-b-c$ and not $a-b+c$.
		  Here is an example of a formal 
		  expression, whose normal form is 0. 
		  The normal form may be obtained by applying @TO normalForm@.		  
	      Example
		  L=lieAlgebra{a,b,c}
		  a@b@c++3@a@c@b++2@c@b@a/2@b@c@a
		  normalForm oo

	      Text
		   Here is the computation of the matrix example.
	      Example
		   F=lieAlgebra({e12,e23,e34,e45,e13,e24,e35,e14,e25,e15},
		       Weights => {1,1,1,1,2,2,2,3,3,4})
		   I={e12@e34,e12@e45,e23@e45,e12@e13,e12@e35,e12@e14,
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
		   L=F/I
		   dims(1,5,L)
		   M=minimalPresentation(4,L)
		   describe M
		   
	      Text  
		  Below is a differential Lie algebra, which 
		  is non-free, and where the 
		   linear part of the differential is non-zero. 
 
		  
	      Example
		  F = lieAlgebra({a,b,c,r3,r4,r42},	             
	             Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},	             
	             Signs => {0,0,0,1,1,0},
		     LastWeightHomological => true)
		  D = differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
		  L = D/{b c - a c,a b,b r4 - a r4}
		  M = minimalModel(5,L)
		  describe M		  
	         
		  
	      Text
		  The homology in homological degree 0 is concentrated in first degree
		  1 and 2. In the general case, for a differential Lie algebra $L$,
		  the function @TO "minimalPresentation(ZZ,LieAlgebra)"@ 
		  gives a minimal presentation of the Lie algebra $H_0(L)$.
		  
	      Example
	          HL = lieHomology L
		  dims(5,HL)  
		  describe minimalPresentation(3,L)
		  
		   		   
	      Text
		  We now 
		  check that the  homology of the minimal model $M$ is 
		  the same as for $L$.
		  
	      Example 		   
		  HM = lieHomology M
		  dims(5,HM)
		   
		   
	      Text
		  The quasi-isomorphism \ $f:\ M\ \to\ L$ from the 
		  minimal model $M$ of $L$
		  to $L$ is obtained as {\tt map(M)}. 
		  If $L$ has no differential, then \ 		   
		  $f$ \ is surjective, but in general this is
		  not true as is shown by the example below. 
		  Another
		  example is obtained letting $L$ 
		  be a non-zero Lie algebra
		  with zero homology, 
		  see @TO "Differential Lie algebra tutorial"@. 
		  
	      Example
		  f = map M 
		  dims(5,L)
		  image f
		  dims(5,oo)
		   
	      Text
		  We check below that $H(f)$ is iso in degree (5,1).
		  
              Example
		  basis(5,1,HL)
		  basis(5,1,HM)
		  f\oo
		    
	      
///
end		   

		
		  
