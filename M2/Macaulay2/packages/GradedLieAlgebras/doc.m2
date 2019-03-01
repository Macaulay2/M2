
doc ///
	Key 
		GradedLieAlgebras
		
	Headline
		A package for doing computations in graded Lie algebras
	Description
		Text
			This package provides routines for doing 
			computations in graded Lie algebras.
			
			The package relies on algorithmic theory 
			on graded Lie algebras developed by 
			Clas Löfwall and Jan-Erik Roos in 
			{\it A Nonnilpotent 1-2-Presented Graded Hopf 
			Algebra Whose Hilbert Series Converges 
			in the Unit Circle}, 
			Adv. Math. 130 (1997), no. 2, 161–200.
			
			See also the earlier implementation in Mathematica by
			C. Löfwall,  
			{\it Liedim, a Mathematica program for Lie-calculations
			    (2001-2016)}, 
		    	available at http://www2.math.su.se/liedim/ 

			See @TO "First LieAlgebra Tutorial"@,
			@TO "Second LieAlgebra Tutorial"@, 
			@TO "Differential LieAlgebra Tutorial"@,
			@TO "Constructing Lie algebras"@,
			@TO "Holonomy Lie algebras and Symmetries"@  
			for some illustrations of ways to use this package.  	
	Caveat
	        Computations with squares in characteristic two is not supported 
		in the current version.    		    	    	      
///    
doc ///
	Key
		LieAlgebra
	Headline 
		a Type for Lie algebras
	Description
		Text
			This Type represents graded Lie algebras.
			Each Lie algebra L is also a type and elements 
			in L belong also to the type LieElement, which
			is the parent of L.
		Example			
			L = lieAlgebra{a,b}
			a
			class L
			parent L			 
	SeeAlso
		lieAlgebra
		LieElement
		"First LieAlgebra Tutorial"  
///
doc ///
	Key
		LieElement
	Headline 
		a Type for elements in Lie algebras
	Description
		Text
			This Type represents elements in Lie algebras.
			Each Lie algebra L is also a type and elements 
			in L belong also to the type LieElement, which
			is the parent of L. Internally a LieElement is 
			a BasicList of two basic lists, the first is a list 
			of coefficients and the second is a list of basic 
			lists of numbered generators which correspond to iterated 
			Lie products of generators.
		      
		Example			
			L = lieAlgebra{a,b,c}
			showStructure L			
			x = a a b - 3 c b a
			x#0
			x#1			 
	SeeAlso
		lieAlgebra
		(symbol SPACE,LieElement,LieElement)
		"First LieAlgebra Tutorial"  
///
doc ///
	Key
		DerLie
	Headline 
		a Type for derivations in Lie algebras
	Description
		Text
			This Type represents derivations d from M to L,
			where M and L are Lie algebras. There is also
			a homomorphism f from M to L defining L as an
			M-module (f is the identity for the case
			of ordinary derivations from L to L). 
		        The derivation law reads \break
			d[x,y]=[dx,fy]+/- [fx,dy]\break
			where the sign is determined by the sign of
			interchanging d and x. 
		     
		       
		      
		Example			
			L = lieAlgebra{a,b}
			M = lieAlgebra{a,b,c}
			f = mapLie(L,M)
			useLie L
			der = derLie(f,{a a b,b b a,a a b+b b a})
			peekLie der
			useLie M
			der a c			 
	SeeAlso
		derLie		
		"Constructing Lie algebras"  
///
doc ///
	Key
		MapLie
	Headline 
		a Type for homomorphisms of Lie algebras
	Description
		Text
			This Type represents homomorphisms f from M to L,
			where M and L are Lie algebras. Use the constructor
			@TO mapLie@ to define homomorphisms.
			A homomorphism is given
			by the value of the generators and it should
			preserve weight and sign. Observe that the zero
			element is considered to have any weight (and sign). 
			The zero element in L is denoted L.zz.
		     
		       
		      
		Example			
			M = lieAlgebra({a,b,c},genWeights=>{2,1,1})
			L = lieAlgebra{a,b}
			f = mapLie(L,M,{a b,a,L.zz})
			peekLie f			
			useLie M
			f (a c - a b)			 
	SeeAlso
		mapLie		
		"Constructing Lie algebras"  
///
doc ///
Key
     (symbol SPACE,LieElement,LieElement)
Headline
     The Lie multiplication
Usage
     u=x y
Inputs
     x: LieElement
        x is of type L, where L is of type LieAlgebra
     y: LieElement
        y is of type L
Outputs
     u: LieElement
        u is of type L, the Lie multiplication of x and y  
Description
  Text
      SPACE is used as infix notation for the Lie
      multiplication. It is also possible to use 
      the prefix notation @TO multLie@, but SPACE is 
      easier to use, it is right associative and hence 
      \break
      b b b a is the 
      same as multLie(b,multLie(b,multLie(b,a))), which is
      written as (b b b a) in output, or a normal form equivalent.
  Example 
      L = lieAlgebra{a,b,c}
      b b b a 
      (a b+b c) (a c) 
      
///
doc ///
Key
   (symbol SPACE,List,List)
Headline
  Lie multiplication of lists or multiplication in the Ext-algebra of lists
Usage
  l=l1 l2  
SeeAlso
  multListLie
  extMultLie
  (symbol SPACE,LieElement,LieElement)
  (symbol SPACE,RingElement,RingElement)
  extRepRing
Inputs
  l1: List
      of LieElement or RingElement
  l2: List
      of LieElement  or RingElement  
Outputs
  l: List

Description
  Text
    It is also possible to use the prefix operator @TO multListLie@ 
    to multiply two lists of @TO LieElement@. In the case of lists of RingElement, 
    the elements should belong to the polynomial ring L.cache.extRepRing, see @TO extRepRing@.
    In this case it is also possible to use the prefix operator @TO extMultLie@ 
  Example
    L = lieAlgebra( {a,b},genWeights => {{1,1},{1,2}},
	genSigns=>{1,0})/{a a a b}
    b2 = basisLie 2
    b3 = basisLie 3
    b2 b3
    indexFormLie oo
    M=lieAlgebra({a,b},genSigns=>1)/{a a,a b}
    extTableLie 3
    {ext_0,ext_1,ext_2} {ext_3}   
///
doc ///
Key
     (symbol +,LieElement,LieElement)
Headline
     Addition of LieElements
Usage
   u=x + y
Inputs
   x: LieElement
     x is of type L, where L is of type LieAlgebra
   y: LieElement
     y is of type L
Outputs
   u: LieElement
     u is of type L, the addition of x and y  
Description
  Text
      The symbol + is used as infix notation for the Lie
      addition. 
  Example 
      L = lieAlgebra{a,b}
      b b b a + a a a b       
///
doc ///
Key
     (symbol -,LieElement,LieElement)
Headline
     Subtraction of LieElements
Usage
   u=x - y
Inputs
   x: LieElement
     x is of type L, where L is of type LieAlgebra
   y: LieElement
     y is of type L
Outputs
   u: LieElement
     u is of type L, the subtraction of x and y  
Description
  Text
      The symbol - is used as infix notation for the Lie
      subtraction. 
  Example 
      L = lieAlgebra{a,b}
      b b b a - a a a b       
///
doc ///
Key
     (symbol -,LieElement)
Headline
     Unary negation of LieElements
Usage
   u = - x
Inputs
   x: LieElement
     x is of type L, where L is of type LieAlgebra
   
Outputs
   u: LieElement
     u is of type L, the negation of x   
Description
  Text
      The symbol - is used as notation for the Lie
      negation. 
  Example 
      L = lieAlgebra{a,b}
      - 2 b b b a        
///
doc ///
Key
     (symbol SPACE,Number, LieElement)
Headline
     Multiplication of a Number and a LieElement
Usage
   u = a x
Inputs
   a: Number
   x: LieElement
     x is of type L, where L is of type LieAlgebra
   
Outputs
   u: LieElement
     u is of type L, the product a*x   
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars are of type Number if the field is QQ,
      otherwise the scalars are of type RingElement.
      If the LieElement is surrounded by a parenthesis,
      you don't need to write the SPACE.
  Example 
      L = lieAlgebra{a,b}
      - 3(b b a + 2 a a b) 
      
SeeAlso
       (symbol SPACE,RingElement,LieElement)
                
///
doc ///
Key
     (symbol SPACE,RingElement,LieElement)
Headline
     Multiplication of a field element and a LieElement
Usage
   y = a x
Inputs
   a: RingElement
      a is an element in L.field, where L is the Lie algebra
   x: LieElement
      x is of type L  
Outputs
   y: LieElement
      y is of type L, the product a*x
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars belong to L.field. 
      If the field is not QQ, then the 
      scalars are of type RingElement. If the field is QQ, then 
      the scalars are of type Number. If the field is not a 
      prime field, then sometimes it is necessary to define the field
      outside the constructor lieAlgebra. Observe also that it 
      is necessary to use the function toField when F is defined 
      as an algebraic extension of a prime field.
  Example 
      F = toField(ZZ/7[x]/ideal{x^2+1})
      L = lieAlgebra({a,b},field=>F)
      (3*x+2) a b + (2*x+3) b a
SeeAlso
       (symbol SPACE,RingElement,MapLie)
       (symbol SPACE,RingElement,DerLie)   
///
doc ///
Key
     (symbol @,LieElement,LieElement)
Headline
     Formal multiplication of LieElements
SeeAlso
   (symbol SPACE,LieElement,LieElement)
   (symbol ++,LieElement,LieElement)
   (symbol /,LieElement,LieElement)
   (symbol @,Number,LieElement)
   (symbol @,RingElement,LieElement)
Usage
   u=x (symbol @) y
Inputs
   x: LieElement
     x is of type L, where L is of type LieAlgebra
   y: LieElement
     y is of type L
Outputs
   u: LieElement
     u is of type L, the formal Lie multiplication of x and y  

Description
  Text
      The "at sign"  is used as infix notation for a "formal" Lie
      multiplication (and also formal multiplication by scalars) where no simplifications are performed.
      (The formal addition is written as ++ and / is used as formal subtraction.)
      In this sense, it is different from the use of SPACE
      as multiplication operator which always gives an object of normal form as output. The formal
      operations are useful when relations are introduced in a big free Lie algebra, since then 
      it might be too hard to compute the normal form of the relations, which is not needed in order to
      define a quotient Lie algebra. 
      For an example, see @TO "Constructing Lie algebras"@.   
  Example 
      L = lieAlgebra{a,b}
      (b@b)@a/3@b@a@b++2@a@b@b
      (b b) a - 3 b a b + 2 a b b       
///
doc ///
Key
     (symbol ++,LieElement,LieElement)
Headline
     Formal addition of LieElements
SeeAlso
   (symbol +,LieElement,LieElement)
   (symbol @,LieElement,LieElement)
   (symbol /,LieElement,LieElement)
   (symbol @,Number,LieElement)
   (symbol @,RingElement,LieElement)
Description
  Text
     See the documentation of the formal Lie multiplication for more information.
///
doc ///
Key
     (symbol /,LieElement,LieElement)
Headline
     Formal subtraction of LieElements
SeeAlso
   (symbol -,LieElement,LieElement)
   (symbol @,LieElement,LieElement)
   (symbol ++,LieElement,LieElement)
   (symbol @,Number,LieElement)
   (symbol @,RingElement,LieElement)
Description
  Text
     See the documentation of the formal Lie multiplication for more information.
///
doc ///
Key
     (symbol @,Number,LieElement)
Headline
     Formal multiplication of a number and a LieElement
SeeAlso
   (symbol SPACE,Number,LieElement)
   (symbol @,LieElement,LieElement)
   (symbol /,LieElement,LieElement)
   (symbol ++,LieElement,LieElement)
   (symbol @,RingElement,LieElement)
Description
  Text
     See the documentation of the formal Lie multiplication for more information.
///
doc ///
Key
     (symbol @,RingElement,LieElement)
Headline
     Formal multiplication of a RingElement and a LieElement
SeeAlso
   (symbol SPACE,RingElement,LieElement)
   (symbol @,LieElement,LieElement)
   (symbol /,LieElement,LieElement)
   (symbol @,Number,LieElement)
   (symbol ++,LieElement,LieElement)
Description
  Text
     See the documentation of the formal Lie multiplication for more information.
///
doc ///
Key
     (symbol +,MapLie,MapLie)
Headline
     Addition of Lie homomorphisms
Usage
   u = f + g
Inputs
   f: MapLie
     
   g: MapLie
     
Outputs
   u: MapLie
      
Description
  Text
      The symbol + is used as infix notation for the 
      addition of Lie homomorphisms. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = mapLie(L,M,{a,b})
      g = mapLie(L,M,{b,a}) 
      peekLie(f+g)      
///
doc ///
Key
     (symbol -,MapLie,MapLie)
Headline
     Subtraction of Lie homomorphisms
Usage
   u = f - g
Inputs
   f: MapLie
    
   g: MapLie
     
Outputs
   u: MapLie
      
Description
  Text
      The symbol - is used as infix notation for the 
      subtraction of Lie homomorphisms. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = mapLie(L,M,{a,b})
      g = mapLie(L,M,{b,a}) 
      peekLie(f-g)            
///
doc ///
Key
     (symbol -,MapLie)
Headline
     Unary negation of Lie homomorphisms
Usage
   u = - f
Inputs
   f: MapLie
     
Outputs
   u: MapLie
        
Description
  Text
      The symbol - is used as notation for the 
      negation of Lie homomorphisms. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = mapLie(L,M)
      peekLie(-f)      
///
doc ///
Key
     (symbol SPACE,Number, MapLie)
Headline
     Multiplication of a Number and a homomorphism
Usage
   g = a f
Inputs
   a: Number
   f: MapLie
     
   
Outputs
   g: MapLie
     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars are of type Number if the field is QQ,
      otherwise the scalars are of type RingElement.
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = mapLie(L,M,{a,b})
      peekLie(-2 f) 
SeeAlso
      (symbol SPACE,RingElement,MapLie)         
///
doc ///
Key
     (symbol SPACE,RingElement,MapLie)
Headline
     Multiplication of a field element and a homomorphism
Usage
   g = a f
Inputs
   a: RingElement
      is an element in L.field, where L=f.targetLie
   f: MapLie
        
Outputs
   g: MapLie
     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars belong to L.field which must be
      the same as M.field, where f: M->L. If the field is not QQ, then the 
      scalars are of type RingElement. If the field is QQ, then 
      the scalars are of type Number. 
  Example 
      F = toField(ZZ/7[x]/ideal{x^2+1})
      M = lieAlgebra({a,b},field=>F)
      L = lieAlgebra({a,b},field=>F)
      f = mapLie(L,M,{x a,3 b})
      peekLie((3*x) f)
SeeAlso
       (symbol SPACE,Number,MapLie)
       (symbol SPACE,RingElement,LieElement)
       (symbol SPACE,RingElement,DerLie)           
///
doc ///
Key
     (symbol +,DerLie,DerLie)
Headline
     Addition of Lie derivations
Usage
   u = d + e
Inputs
   d: DerLie
     
   e: DerLie
     
Outputs
   u: DerLie
      
Description
  Text
      The symbol + is used as infix notation for the 
      addition of Lie derivations M->L with the same 
      defining map f: M->L. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a1,b1}
      f = mapLie(L,M,{b1,a1})
      d = derLie(f,{a1,b1})
      e = derLie(f,{2 b1,2 a1})
      u = d+e
      peekLie u
      u a b         
///
doc ///
Key
     (symbol -,DerLie,DerLie)
Headline
     Subtraction of Lie derivations
Usage
   u = d - e
Inputs
   d: DerLie
    
   e: DerLie
     
Outputs
   u: DerLie
      
Description
  Text
      The symbol - is used as infix notation for the 
      subtraction of Lie derivations M->L with the same 
      defining map f: M->L. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a1,b1}
      f = mapLie(L,M,{b1,a1})
      d = derLie(f,{a1,b1})
      e = derLie(f,{2 b1,2 a1})
      u = d-e
      peekLie u
      u a b              
///
doc ///
Key
     (symbol -,DerLie)
Headline
     Unary negation of Lie derivations
Usage
   u = - d
Inputs
   d: DerLie
     
Outputs
   u: DerLie
        
Description
  Text
      The symbol - is used as notation for the 
      negation of Lie derivations. 
  Example 
      L = lieAlgebra{a,b}
      d = derLie{a a b,b b a}
      peekLie(-d)      
///

doc ///
Key
     (symbol SPACE,Number,DerLie)
Headline
     Multiplication of a Number and a Derivation
Usage
   e = a d
Inputs
   a: Number
   d: DerLie
        
Outputs
   e: DerLie
     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars.
      The scalars are of type Number if the field is QQ,
      otherwise the scalars are of type RingElement.
       
  Example 
      L = lieAlgebra{a,b}
      d = derLie{a a b,b b a}
      peekLie(-2 d) 
SeeAlso
       (symbol SPACE,RingElement,DerLie)
       (symbol SPACE,Number,LieElement)
       (symbol SPACE,Number,MapLie)               
///
doc ///
Key
     (symbol SPACE,RingElement,DerLie)
Headline
     Multiplication of a field element and a derivation
Usage
   e = a d
Inputs
   a: RingElement
      is an element in L.field, where L=d.targetLie
   d: DerLie
        
Outputs
   e: DerLie
     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars belong to L.field which must be
      the same as M.field, where d: M->L. If the field is not QQ, then the 
      scalars are of type RingElement. If the field is QQ, then 
      the scalars are of type Number, see 
      @TO (symbol SPACE,Number,DerLie)@. 
  Example 
      F = toField(ZZ/7[x]/ideal{x^2+1})
      M = lieAlgebra({a,b},field=>F)
      L = lieAlgebra({a,b},field=>F)
      f = mapLie(L,M,{x a,3 b})
      d = derLie(f,{-x b,-2 a})
      peekLie (3*x) d 
SeeAlso
       (symbol SPACE,RingElement,LieElement)
       (symbol SPACE,RingElement,MapLie)
       (symbol SPACE,Number,DerLie)   
               
///
doc ///
Key
     (symbol *,MapLie,MapLie)
Headline
     composition of homomorphisms
Usage
   h = f*g
Inputs
   f: MapLie
      a homomorphism from M to L
   g: MapLie
      a homomorphism from N to M
Outputs
   h: MapLie
      the composition from N to L
Description   
  Example 
      L = lieAlgebra({a,b})
      f = mapLie(L,L,{b,-a})
      i = idMapLie()
      peekLie(f*f+i)          
///
doc ///
Key
     (symbol *,DerLie,MapLie)
Headline
     operation of maps to the right of a derivation
Usage
   h = d*g
Inputs
   d: DerLie
      a derivation from M to L
   g: MapLie
      a homomorphism from N to M
Outputs
   h: DerLie
      a derivation from N to L
Description
 Text
      The composition of maps d*g is a derivation N->L, with 
      the composition f*g defining the module structure of L over N,
      where f: M->L defines the module structure of L over M.
      
         
 Example			
      L = lieAlgebra{a,b}
      M = lieAlgebra{a,b,c}
      N = lieAlgebra{a1,b1}
      f = mapLie(L,M)
      useLie M
      g = mapLie(M,N,{b,a})
      useLie L
      d = derLie(f,{a a b,b b a,a a b+b b a})
      peekLie d
      peekLie(f*g)
      h = d*g
      peekLie h
      
      	        
///
doc ///
Key
     (symbol *,MapLie,DerLie)
Headline
     operation of maps to the left of a derivation
Usage
   h = g*d
Inputs
   g: MapLie
      a homomorphism from L to N
   d: DerLie
      a derivation from M to L
Outputs
   h: DerLie
      a derivation from M to N
Description
 Text
      The composition of maps g*d is a derivation M->N, with 
      the composition g*f defining the module structure of N over M,
      where f: M->L defines the module structure of L over M.
          
 Example			
      L = lieAlgebra{a,b}
      M = L
      d = derLie{a a b,b b a}
      peekLie d
      N = lieAlgebra{a1,b1}
      g = mapLie(N,L,{b1,a1})
      h = g*d
      peekLie h
      	        
///

doc///
Key
  (symbol /,LieAlgebra,List)
Headline
   A quotient Lie algebra
Usage
   Q=L/I
Inputs
   L: LieAlgebra
   I: List
      a list of LieElement of type L
Outputs
   Q: LieAlgebra
      the quotient of L by the ideal generated by the list I
Description
   Text
     The list I is extended
     by the action on I of the differential in L.
     The program converts the elements in the list, so that they
     will have type M=ambient(L) instead.
     The list of these
     converted elements may be looked upon by writing
     Q.relsLie. 
   Example
     M = lieAlgebra({a,b,c})
     L = M/{a b}
     Q = L/{a c}
     peekLie Q
     ambient Q
     class (Q.relsLie)_0     
   Text
     The Lie algebra Q below is M modulo the ideal 
     in M generated by the elements in Q.relsLie and with differential
     defined by the induced differential on L. 
   Example
     L = lieAlgebra({a,b,c2,c3},genWeights=>{{1,0},{1,0},{2,1},{3,2}},
          genSigns=>{1,1,1,1},diffl=>true)
     L = diffLieAlgebra{L.zz,L.zz,a a,b c2}/{a c2}
     Q = L/{b c3}
     peekLie Q
     M = ambient Q
     peekLie M
      
SeeAlso
     (symbol /,LieAlgebra,MapLie)
     
///
doc///
Key
  (symbol /,LieAlgebra,MapLie)
Headline
   A quotient Lie algebra by the image of a map
Usage
   Q=L/f
Inputs
   L: LieAlgebra
   f: MapLie
      a map from N to L
Outputs
   Q: LieAlgebra
      the quotient of L by the ideal generated by the image of the map f
Description
   Text
     The program converts the elements f(x), where x is a generator in N,
     so that they
     will have type M=ambient(L) instead. The list of these
     converted elements may be looked upon by writing
     Q.relsLie. The Lie algebra Q is M modulo the ideal 
     in M generated by the elements in Q.relsLie together
     with the induced differential on L.
   Example
     M = lieAlgebra({a,b,c})
     L = M/{a b}
     N = lieAlgebra({d}, genWeights=>{2})
     f = mapLie(L,N,{a c})
     Q = L/f
     Q.relsLie
     Q1 = M/Q.relsLie
     peekLie Q1
     peekLie Q
SeeAlso
     (symbol /,LieAlgebra,List)
///
doc///
Key
  (symbol SPACE,DerLie,DerLie)
Headline
   Lie multiplication of ordinary derivations
Usage
   e = d1 d2
Inputs
   d1: DerLie
   d2: DerLie
      
Outputs
   e: DerLie
      the Lie multiplication of d1 and d2
Description
   Text
      The vector space of graded derivations from L to L 
      with the identity map as defining map,
      see @TO DerLie@, is a graded Lie algebra. If L has a differential \delta, 
      then DerLie is a differential graded Lie algebra with differential d->[\delta,d].
      This Lie algebra is however not of type
      LieAlgebra, unless a positively graded 
      finite presentation can be given.
   Example
      L = lieAlgebra({a,b})/{a a a b,b b b a}
      d0 = derLie{a,b}
      d2 = derLie{a b a,L.zz}
      d4 = derLie{a b a b a,L.zz}
      peekLie d2 d4
      peekLie d0 d4
SeeAlso
      multLie
      derLie
///
doc///
Key
  (symbol SPACE,DerLie,LieElement)
Headline
   Application of a derivation to a LieElement
Usage
   y = d x 
Inputs
   d: DerLie
   x: LieElement
      x is of type d.sourceLie
      
Outputs
   y: LieElement
      y is of type d.targetLie
Description
   
   Example
      L = lieAlgebra({a,b})
      d = derLie{a,b}
      d a a a b a
SeeAlso
     (symbol SPACE,MapLie,LieElement)
///
doc///
Key
  (symbol SPACE,DerLie,List)
Headline
   Application of a derivation to every element in a list
Usage
   y = d x 
Inputs
   d: DerLie
   x: List
      the elements in x are of type d.sourceLie
      
Outputs
   y: List
      the elements in y are of type d.targetLie
Description
   
   Example
      L = lieAlgebra({a,b})
      d = derLie{a,b}
      indexFormLie d basisLie 5
///
doc///
Key
  (symbol SPACE,MapLie,LieElement)
Headline
   Application of a Lie map to a LieElement
Usage
   y = f x 
Inputs
   f: MapLie
   x: LieElement
      x is of type f.sourceLie
      
Outputs
   y: LieElement
      y is of type f.targetLie
Description
   
   Example
      L = lieAlgebra{a,b}
      M = L/{a a b}
      f = mapLie(M,L)
      useLie L
      f b a a b
///
doc///
Key
  (symbol SPACE,MapLie,List)
Headline
   Application of a Lie map to every element in a list
Usage
   y = f x 
Inputs
   f: MapLie
   x: List
      the elements in x are of type f.sourceLie
      
Outputs
   y: List
      the elements in y are of type f.targetLie
Description
   
   Example
      L = lieAlgebra{a,b}
      M = L/{a a b}
      f = mapLie(M,L)
      useLie L
      basisLie 4
      f oo
      apply(oo,class)
///
doc///
Key
  (symbol SPACE,RingElement,RingElement)
Headline
   Multiplication in the Ext-algebra
Usage
   z = x y 
Inputs
   x: RingElement
      
   y: RingElement
SeeAlso
     extMultLie
     extBasisLie
     minmodelLie   
      
Outputs
   z: RingElement
      
Description
   Text
     The elements x and y are linear polynomials in L.cache.extRepRing and z is 
     the product in the skewcommutative algebra Ext_{UL}(k,k) where k is L.field.
   Example
      L = lieAlgebra({a,b,c},genSigns=>1)/{a a,b c,a b+c c}
      extBasisLie 4
      extTableLie 4
      ext_0 ext_5
      weightExtLie ext_6
///     
doc ///
Key
     (symbol *,LieAlgebra,LieAlgebra)
Headline
     Free product of Lie algebras
Usage
   M = L1*L2
Inputs
   L1: LieAlgebra
   L2: LieAlgebra
Outputs
   M: LieAlgebra
      the free product of L1 and L2
Description
   
  Example 
      F1 = lieAlgebra({a,b},genSigns=>{0,1},genWeights=>{{2,0},{2,1}},
	  diffl=>true)
      L1 = diffLieAlgebra{F1.zz,a}
      F2 = lieAlgebra({a,b,c},genWeights=>{{1,0},{2,1},{3,2}},
          genSigns=>{1,1,1},diffl=>true)
      L2 = diffLieAlgebra{F2.zz,a a,a b}/{b b+4 a c}
      M = L1*L2
      peekLie(M)
      d = diffLie()
      d (pr_1 pr_3)
      
      
///
doc ///
Key
     (symbol **,LieAlgebra,LieAlgebra)
Headline
     Direct sum of Lie algebras
Usage
   S = L**M
Inputs
   L: LieAlgebra
   M: LieAlgebra
Outputs
   S: LieAlgebra
      the direct sum of L and M
Description
   
  Example 
      L1 = lieAlgebra({a,b},genSigns=>{0,1},genWeights=>{{2,0},{2,1}},
	   diffl=>true)
      L1 = diffLieAlgebra{L1.zz,a}
      L2 = lieAlgebra({a,b,c},genWeights=>{{1,0},{2,1},{3,2}},
          genSigns=>{1,1,1},diffl=>true)
      L2 = diffLieAlgebra{L2.zz,a a,a b}/{b b+4 a c}
      S = L1**L2
      peekLie(S)
      
///
doc///
Key
  (ambient,LieAlgebra)
Headline
  the free underlying Lie algebra
Usage
  M = ambient L
Inputs
  L: LieAlgebra
Outputs
  M: LieAlgebra
     a free Lie algebra of which L (without differential) is a quotient
Description
  Text
    Relations in L are elements
    in ambient(L). When a quotient Lie algebra Q=L/I is constructed,
    then the elements in the list I must be of type L, but the program
    converts the relations so that they have type ambient(L) instead. 
    This may be seen by looking at Q.relsLie.
  Example
     M = lieAlgebra({a,b,c})
     L = M/{a b}
     a c
     Q = L/{a c}
     Q.relsLie
     oo_1
  
///
doc///
Key
  (baseName,LieElement)
Description
  Text
    This is for internal use. baseName(x) gives back the symbol
    corresponding to the generator x as a LieElement. This is used 
    when several Lie algebras are constructed with common names
    of generators.
///									
end

Inputs
   x: LieElement
     x is of type L, where L is of type LieAlgebra
   y: LieElement
     y is of type L
Outputs
   u: LieElement
     u is of type L, the formal Lie multiplication of x and y  