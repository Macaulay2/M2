
doc ///
	Key 
		GradedLieAlgebras
		
	Headline
		a package for doing computations in graded Lie algebras
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


			See
		Code	
			UL {
			TO  "First Lie algebra tutorial",
			TO "Second Lie algebra tutorial", 
		 	TO "Differential Lie algebra tutorial",
			TO "Homomorphisms and derivations",
			TO "Quotient Lie algebras and subspaces",
			TO "Minimal models, Ext-algebras and Koszul duals",
			TO "Holonomy Lie algebras and symmetries",
			}  
		Text
			for some illustrations of ways to use this package.
			
	      					
	Caveat
	        Computations with squares in characteristic 2 is not supported 
		in the current version.
/// 

doc ///
	Key
		LieAlgebra
	Headline 
		the class of all Lie algebras
	Description
		Text
			This type represents graded Lie algebras.
			More precisely, the Lie algebras under consideration are 
			graded over 
			${\mathbb Z}^{+}\times 
			{\mathbb Z}^n\times {\mathbb Z}/2{\mathbb Z}$, 
			where the first 
			component is called the @TO degree@, and the last component
			is called the @TO sign@, which is 0 or 1 and have effect 
			on the axioms, see below. The list of components of the
			grading except the sign 
			is called the @TO weight@, and the last component of the 
			weight is called the homological degree. If no differential
			is defined, then the homological degree will be zero for
			all elements.
			Each object of type {\tt LieAlgebra} 
			is also a type {\tt L},
			and elements 
			in {\tt L} belong also to the type @TO LieElement@, which
			is the parent of {\tt L}. Lie multiplication is given by 
     			@TO (symbol SPACE,LieElement,LieElement)@. 
			
			The axioms for Lie algebras depend on the 
			signs of the generators, 
   			which are specified by @TO [lieAlgebra,Signs]@. 
   			The sign of a homogeneous element can be 
			obtained by the function 
   			@TO sign@. In the axioms
   			below the sign of an element $a$ is written sign($a$).
		       
		       
   		       Anticommutativity:
      		       [$a$, $b$] =  -(-1)^{sign($a$) * sign($b$)} [$b$, $a$]
    	    	    	
    		       Jacobi identity:
      		       [$a$, [$b$, $c$]] = [[$a$, $b$], $c$] + 
		       (-1)^{sign($a$) * sign($b$)} [$b$, [$a$, $c$]]
      	      	       
      		       Also, in characteristic 2 and 3, there are in 
   		       addition the following axioms:
       	       	        
   		       Characteristic 2:
      		       [$a$, $a$] = 0
    	    	    	  
   		       Characteristic 3:
      		       [$a$, [$a$, $a$]] = 0
		              
	        Example			
			L = lieAlgebra{a,b}
			a
			class L
			parent L
			a b
			weight oo			 
	SeeAlso
		lieAlgebra
		LieElement
		(symbol SPACE,LieElement,LieElement)
		(symbol /,LieAlgebra,List)
		(symbol /,LieAlgebra,LieIdeal)
		"First Lie algebra tutorial"  
///

doc ///
	Key
		LieElement
	Headline 
		the class of all Lie algebra elements
	Description
		Text
			This type represents elements in Lie algebras.
			Each object of type {\tt LieAlgebra} 
			is itself a type {\tt L}, and elements 
			in {\tt L} belong also to the type {\tt LieElement}, which
			is the parent of {\tt L}. Internally an element of
			type {\tt LieElement} is of type
			{\tt BasicList} consisting of two basic lists. 
			The first is a list 
			of coefficients and the second is a list of basic 
			lists of numbered generators that correspond to 
			iterated Lie products of generators.
		      
		Example			
			L = lieAlgebra{a,b,c}
			showStructure L			
			x = a a b - 3 c b a
			x#0
			x#1			 
	SeeAlso
		lieAlgebra
		(symbol SPACE,LieElement,LieElement)
		"First Lie algebra tutorial"  
///
doc ///
	Key
		ExtAlgebra
	Headline 
		the class of all Ext-algebras
	Description
		Text
			This type represents the Ext-algebra of a graded
			differential Lie algebra $L$,
			$E=Ext_{UL}(F,F)$, where $F$ is the 
			field of $L$ and $UL$ is the enveloping algebra of $L$.
			Each object of type {\tt ExtAlgebra} 
			is itself a type {\tt E}, and 
			homogeneous elements 
			in {\tt E} belong also to the type @TO ExtElement@, which
			is the parent of {\tt E}. The generators of {\tt E}, 
			see @TO "generators(ExtAlgebra)"@, 
			represents a
			basis for $E$ as a vector space and correspond to the 
			Lie algebra generators for the minimal model $M$ of $L$; 
			however, the 
			homological degree of a generator in $E$ is 1 more than
			the homological degree for the 
			corresponding generator in $M$ 
			(and also the sign is switched).
	        Example			
			L = lieAlgebra{a,b}/{a a b,b b b a}
			E = extAlgebra(5,L)
			describe E
			parent E
			ext_0 ext_1
			M = minimalModel(5,L)
			describe M
			gens E
						 
	SeeAlso
		ExtElement
		extAlgebra		
		minimalModel 
///
  
doc ///
	Key
		ExtElement
	Headline 
		the class of all Ext-algebra elements
	Description
		Text
			This type represents elements in Ext-algebras.
			Each object of type {\tt ExtAlgebra} 
			is itself a type {\tt E}, and 
			homogeneous elements 
			in {\tt E} belong also to the type {\tt ExtElement}, which
			is the parent of {\tt E}. Internally an element of
			type {\tt ExtElement} is of type {\tt BasicList} 
			consisting of two basic lists, 
			the first is a list 
			of coefficients and the second is a list of 
			indices for the generators occurring in the element.
		      
		Example			
			L = lieAlgebra{a,b}/{a a b,b b a}
			E = extAlgebra(5,L)		
			x = 4 ext_2 - ext_3
			x#0
			x#1			 
	SeeAlso
		ExtAlgebra
		ExtElement
		(symbol SPACE,ExtElement,ExtElement)
  
///
doc ///
	Key
		LieDerivation
	Headline 
	        the class of all Lie algebra derivations
	Description
		Text
			Given a homomorphism of Lie algebras $f: M \ \to\  L$, 
			one has the 
			notion of a derivation $d: M \ \to\  L$ over $f$, and
			{\tt LieDerivation} is the type representing 
			such pairs $(d,\,f)$ 
			($f$ is the identity for the case
			of ordinary derivations from $L$ to $L$). 
		        The derivation law reads \break
			$d$ [x, y] = [$d$ x, $f$ y] ± [$f$ x, $d$ y],
			\break
			where the sign is determined by the sign of
			interchanging $d$ and $x$, i.e., the sign is plus if
			sign$(d)$=0 or sign$(x)$=0 and minus otherwise. 
			An object of type {\tt LieDerivation} need not be 
			well defined as a map. 
			Use @TO "isWellDefined(ZZ,LieDerivation)"@
			to check if the derivation is well defined.
		     
		       
		      
		Example			
			L = lieAlgebra{a,b}
			M = lieAlgebra{a,b,c}
			f = map(L,M)
			use L
			der = lieDerivation(f,{a a b,b b a,a a b+b b a})
			describe der
			use M
			der a c			 
	SeeAlso
		lieDerivation
		"isWellDefined(ZZ,LieDerivation)"
		sign		
		"Homomorphisms and derivations"  
///

doc ///
	Key
		LieAlgebraMap
	Headline 
		the class of all Lie algebra homomorphisms 
	Description
		Text
			This type represents homomorphisms from $M$ to $L$,
			where $M$ and $L$ are Lie algebras 
			defined over the same field.		      			
			Use the constructor
			@TO "map(LieAlgebra,LieAlgebra,List)"@ to 
			define homomorphisms.
			A homomorphism is given
			by the value of the generators and it should
			preserve weight and sign. Observe that the zero
			element is considered to have any weight 
			and sign. 
			The zero element in {\tt L} is denoted by {\tt 0_L}.
			An object of type {\tt LieAlgebraMap} need not be 
			well defined as a map and need not commute
			with the differentials. 
			Use @TO "isWellDefined(ZZ,LieAlgebraMap)"@
			to check if this is true.
		     
		       
		      
		Example			
			M = lieAlgebra({a,b,c},Weights=>{2,1,1})
			L = lieAlgebra{a,b}
			f = map(L,M,{a b,a,0_L})
			isWellDefined(3,f)
			describe f			
			use M
			f (a c - a b)			 
	SeeAlso
		"map(LieAlgebra,LieAlgebra,List)"
		"isWellDefined(ZZ,LieAlgebraMap)"
		(symbol SPACE,LieAlgebraMap,LieElement)		
		"Homomorphisms and derivations"  
///
doc ///
	Key
		VectorSpace
	Headline 
		the class of all vector spaces
	Description
		Text
			This type represents e.g., homology of Lie algebras, 
			and it
			has the subtypes {\tt LieSubSpace,
			LieSubAlgebra, LieIdeal, FGLieIdeal, FGLieSubAlgebra}. 
		
			 
		Example			
			L = lieAlgebra{a,b}			
			I=lieIdeal{a}
			showStructure class I
			H=lieHomology L
						 
	SeeAlso
		LieSubAlgebra
		lieSubAlgebra
		lieIdeal
		lieHomology
///
doc ///
	Key
		LieSubSpace
	Headline 
		the class of all Lie subspaces
	Description
		Text
			This type represents  Lie subspaces, and has 
			the subtypes
			{\tt LieIdeal, FGLieIdeal, 
			LieSubAlgebra, FGLieSubAlgebra}.
		    	A Lie subspace need not be invariant under the 
			differential. 
		
			 
		Example			
			L = lieAlgebra{a,b}
			S=lieSubSpace{a b}
			I=lieIdeal{a b}
			instance(S,LieSubAlgebra)
			dims(1,3,S)
			dims(1,3,I)
			
			
						 
	SeeAlso
		LieIdeal
		LieSubAlgebra
		
///
doc ///
	Key
		LieSubAlgebra
	Headline 
		the class of all Lie subalgebras
	Description
		Text
			This type represents Lie subalgebras 
			(which are invariant under the differential), and has 
			the subtypes
			{\tt LieIdeal, FGLieIdeal, FGLieSubAlgebra}. 
		
			 
		Example			
			L = lieAlgebra{a,b}
			S=lieSubAlgebra{a}
			I=lieIdeal{a}
			instance(I,LieSubAlgebra)
			instance(I,FGLieSubAlgebra)
						 
	SeeAlso
		LieIdeal
		FGLieSubAlgebra
		
///
doc ///
	Key
		FGLieSubAlgebra
	Headline 
		the class of all finitely generated Lie subalgebras
	Description
		Text
			This type represents  finitely generated
			Lie subalgebras, which is a subtype 
			of {\tt LieSubAlgebra}.
		
			 
		Example			
			L = lieAlgebra{a,b}
			S=lieSubAlgebra{a}
			
			
						 
	SeeAlso
		LieIdeal
	        LieSubAlgebra
		
///
doc ///
	Key
		FGLieIdeal
	Headline 
		the class of all finitely generated Lie ideals
	Description
		Text
			This type represents  finitely generated
			Lie ideals. It is a subtype of {\tt LieIdeal},
			but not of {\tt FGSubAlgebra}.
		
			 
		Example			
			L = lieAlgebra{a,b}
			S=lieIdeal{a}
			
			
						 
	SeeAlso
		LieIdeal
	        FGLieSubAlgebra
		
///
doc ///
	Key
		LieIdeal
	Headline 
		the class of all Lie ideals
	Description
		Text
			This type represents 
			Lie ideals. It is a subtype of {\tt LieSubAlgebra} and
			it has {\tt FGLieIdeal} as a subtype.
		
			 
		Example			
			L = lieAlgebra{a,b}
			I=lieIdeal{a a b}
			Q=L/I
			f=map(Q,L)
			J=kernel f
			I===J
			describe I
		Text
		        The kernel of $f$ is defined as the inverse image
			under $f$
			of the zero ideal.
	        Example		        
			describe J 
			J#inverse_1===zeroIdeal Q
			
			
						 
	SeeAlso
		FGLieIdeal
	        FGLieSubAlgebra
		
///
doc ///
Key
     (symbol ==,LieAlgebra, LieAlgebra)
Headline
     whether two Lie algebras are defined in the same way
Usage
   L==M
Inputs
   L: LieAlgebra
   M: LieAlgebra     
Outputs
   b: Boolean
        
Description
  Text
      Two objects {\tt L} and {\tt M} of type {\tt LieAlgebra} satisfies 
      {\tt L==M} 
      if they have the same 
      generators, weights, signs, field, defining ideal, and differential.
     
  Example
       F = lieAlgebra{a,b}
       I = lieIdeal{a a b} 
       L=F/{a a b} 
       M=F/I 
       L==M
       L===M
      
SeeAlso
      
       LieAlgebra
                
///
doc ///
Key
     (symbol ==,LieAlgebraMap, LieAlgebraMap)
Headline
     whether two Lie algebra homomorphisms are defined in the same way
Usage
   f==g
Inputs
   f: LieAlgebraMap
   g: LieAlgebraMap     
Outputs
   b: Boolean
        
Description
  Text
      Two objects {\tt f} and {\tt g} of type {\tt LieAlgebraMap} 
      satisfies {\tt f==g} if 
      {\tt source f==source g, target f==target g}, and the values
      of the generators of {\tt source f} under {\tt f} considered as 
      elements in {\tt target g} are the same as the values of the 
      generators of {\tt source g} under {\tt g}. 
      
     
  Example
       F = lieAlgebra{a,b}
       I = lieIdeal{a a b} 
       f = map(F/I,F)
       G = lieAlgebra{a,b}       
       g=map(G/{a a b},G) 
       f==g 
       f===g
      
SeeAlso
      
       LieAlgebraMap
                
///
doc ///
Key
  extAlgebra
     (extAlgebra, ZZ, LieAlgebra)
Headline
  compute the Ext-algebra of a Lie algebra 
Usage
  E=extAlgebra(n,L)

Inputs
  n: ZZ
     the degree of computation
  L: LieAlgebra
Outputs
  E: ExtAlgebra
SeeAlso
   ExtElement
   ExtAlgebra
   "Differential Lie algebra tutorial"
   (symbol SPACE,ExtElement,ExtElement)
   koszulDual
 
Description
  Text
    The Ext-algebra of the Lie algebra $L$ is $Ext_{UL}(F,F)$, where $F$ is the 
    field of $L$ and $UL$ is the enveloping algebra of $L$. 
    It is computed using the minimal model of $L$, 
    see @TO ExtAlgebra@. If $R$ is a quadratic (skew)commutative Koszul algebra,
     and {\tt L} is the value of @TO koszulDual@($R$) then 
     {\tt extAlgebra(n,L)} represents the ring $R$
     up to degree $n$. A basis for $Ext_{UL}(F,F)$ as a vector space  
     is represented by 
     @TO "generators(ExtAlgebra)"@.  The symbol SPACE is used as multiplication 
     of elements in the Ext-algebra and for multiplication by scalars.
  Example
    F = lieAlgebra({a,b,c},Weights=>{{1,0},{1,0},{2,1}},
	   Signs=>{1,1,1},LastWeightHomological=>true)
    D = differentialLieAlgebra{0_F,0_F,a a + b b}
    L=D/{a b,a c}
    E=extAlgebra(3,L)
    describe E
    (ext_0 - 2 ext_1) ext_2
    R=QQ[a,b,c]/{a*a,b*b,c*c}
    L=koszulDual(R)
    E=extAlgebra(4,L)
    describe E
    ext_0 ext_1 ext_2
    
    
///
 
doc ///
Key
  lieAlgebra
     (lieAlgebra, List)
Headline
  make a free Lie algebra 
Usage
  L=lieAlgebra(liegens)

Inputs
  liegens: List
            of elements of class  {\tt Symbol} or {\tt IndexedVariable}  
Outputs
  L: LieAlgebra
SeeAlso
   LieElement
  "First Lie algebra tutorial"
  "Second Lie algebra tutorial"
  "Differential Lie algebra tutorial"
  (symbol /,LieAlgebra,List)
  (symbol /,LieAlgebra,LieIdeal)
  differentialLieAlgebra
 
Description
  Text
    A generator may be of class {\tt Symbol} or {\tt IndexedVariable}. 
    The same name for
    a generator can be used in several Lie algebras and also as name 
    for a variable
    in a polynomial ring. If a symbol $a$ has been used 
    as name for some output, then 
    you must write {\tt a = symbol a} to be able to use 
    the symbol as a generator instead.
    Relations are introduced 
    by the operator /, see @TO (symbol /,LieAlgebra,List)@.
    It is also possible to define a Lie algebra modulo an ideal.
    See @TO (symbol /,LieAlgebra,LieIdeal)@. 
    A differential Lie algebra is defined by giving 
    the value of the differential on the generators, 
    see @TO differentialLieAlgebra@. If relations are introduced 
    as a list, then the program adds relations 
    to make the ideal of relations invariant under the 
    differential. 
    These non-normalized relations are obtained 
    using @TO "ideal(LieAlgebra)"@ and
    can also be seen using @TO "describe(LieAlgebra)"@, see {\tt L2} below.
    The zero Lie algebra (over {\tt QQ}) is defined as
    {\tt lieAlgebra\{\}}.
    
  Example
    F1 = lieAlgebra{a,b}
    L1=F1/{a a b - b b a, a a a a b}
    dims(1,6,L1)
    describe L1
    F2 = lieAlgebra({a,b,c},Weights=>{{1,0},{1,0},{2,1}},
	   Signs=>{1,1,1},LastWeightHomological=>true)
    D2 = differentialLieAlgebra{0_F2,0_F2,a a + b b}
    L2=D2/{a b,a c}
    describe L2
    dims(5,L2)
    describe lieAlgebra{}
    
/// 

doc ///
Key
  [lieAlgebra, Weights]
 
Headline
  optional argument for lieAlgebra 
Usage
  L = lieAlgebra(gen,Weights => w)
  
Inputs
    gen: List
    w: List
        of lists of weights or a list of positive integers
    w: ZZ
      1, the same weight (1,0) is assigned to each generator    
Outputs
  L: LieAlgebra 
SeeAlso
   lieAlgebra
   weight
   "firstDegree(LieElement)"
   "Second Lie algebra tutorial"
Description
  Text
      This is an option to tell @TO lieAlgebra@ to assign 
      the given weights to the generators.
      A weight
      is a list of integers of a length 
      that is the same for all generators, see @TO "degreeLength(LieAlgebra)"@. 
      The first component (also just called the degree) is positive, 
      see @TO "firstDegree(LieElement)"@. 
      If the option @TO LastWeightHomological@ is {\tt true}, 
      then the last component in a weight is the homological degree, 
      which is non-negative
      and less than the first degree. If the 
      option @TO LastWeightHomological@ is {\tt false}, then
      the program adds a last 
      component 0 to the existing degrees. When the option 
      is given as a list of integers $n1,n2,\ldots$,
      which is not possible when the option 
      @TO LastWeightHomological@ is {\tt true},
      then the program
      defines the weights for the generators to be 
      $\{n1,0\},\ \{n2,0\},\ldots$. 
      The default value is 1, which has the effect 
      that all generators have weight $\{1,0\}$. 
  Example
    describe lieAlgebra({a,b},Weights=>{{1,2},{2,3}})
    describe lieAlgebra({a,b},Weights=>{{2,1},{3,2}},
	LastWeightHomological=>true)
    describe lieAlgebra{a,b}
    describe lieAlgebra({a,b},Weights=>{{1,0},{1,0}})
    describe lieAlgebra({a,b},Weights=>{{1,0},{1,0}},
	LastWeightHomological=>true)
    describe lieAlgebra({a,b},Weights=>{1,2})    
///

doc ///
Key
  [lieAlgebra, Signs]
 
Headline
  optional argument for lieAlgebra 
Usage
  L = lieAlgebra(gen,Signs => s)
  
Inputs
   gen: List
       the list of generators
   s: List
       of signs (0 or 1), s has the same length as {\tt gen}
   s: ZZ
      0 or 1, the same sign is assigned to each generator
Outputs
  L: LieAlgebra 
SeeAlso
   lieAlgebra
   sign
   "Second Lie algebra tutorial"
Description
  Text
    This is an option to tell @TO lieAlgebra@ to assign 
    the given "signs" to the generators, which are either
    0 (even) or 1 (odd). If the option is given the value 1,
    then all generators will be odd. The default value is that
    all generators are even. The signs affect the axioms of a Lie 
    superalgebra, see @TO LieAlgebra@. 
    Use @TO sign@ to 
    compute the sign of an arbitrary homogeneous Lie expression. 
    
  Example
    describe lieAlgebra{a,b}
    describe lieAlgebra({a,b},Signs=>{1,0})
    describe lieAlgebra({a,b},Signs=>1) 
      
///
doc ///
Key
  Signs
Headline
  name for an optional argument for lieAlgebra
SeeAlso
  lieAlgebra
  sign
  "Second Lie algebra tutorial"
Description
  Text
    This is an option to tell @TO lieAlgebra@ to assign 
    the given "signs" to the generators, which are either
    0 (even) or 1 (odd).
///

doc ///
Key
  [lieAlgebra,LastWeightHomological]
Headline
  optional argument for lieAlgebra
Usage
  L = lieAlgebra(gen,LastWeightHomological => true)
Inputs
  gen: List
Outputs
  L: LieAlgebra 
SeeAlso
  lieAlgebra
  differentialLieAlgebra
  weight
  "Differential Lie algebra tutorial"
Description
  Text
    This is an option, which tells @TO lieAlgebra@ to consider the 
    last weight of a generator as the homological degree, if the 
    option is given the value {\tt true}. Default value is {\tt false}.
  Example
    describe lieAlgebra({a,b},Weights=>{{2,1},{3,2}})
    describe lieAlgebra({a,b},Weights=>{{2,1},{3,2}},
	        LastWeightHomological=>true)
///
doc ///
Key
  LastWeightHomological 
Headline
  name for an optional argument for lieAlgebra
SeeAlso
  lieAlgebra
  differentialLieAlgebra
  weight
  "Differential Lie algebra tutorial"
Description
  Text
    This is an option to tell @TO lieAlgebra@ to consider the 
    last weight of a generator as the homological degree.
    
///
doc ///
Key
  [lieAlgebra,Field]
 
Headline
  optional argument for lieAlgebra 
Usage
  L = lieAlgebra(gen,Field => F)
   
Inputs
    gen: List        
    F:   Ring
        the field of coefficients    
Outputs
  L: LieAlgebra
SeeAlso
  lieAlgebra
  holonomy
  "Second Lie algebra tutorial"
  
Description
  Text
    This is an option for @TO lieAlgebra@ 
    and @TO holonomy@ to define the coefficient field, 
    which is {\tt QQ} by default. You may use any "exact" field 
    (not the real numbers or the complex numbers), 
    such as a prime field or an algebraic extension, e.g.,
    {\tt toField(ZZ/7[x]/ideal\{x^2+1\})} or a fraction field, e.g., 
    {\tt frac(QQ[x])}. Observe that it 
    is necessary to use the function {\tt toField} when $F$ is defined 
    as an algebraic extension of a prime field.
  Example
      F = toField(ZZ/7[x]/ideal{x^2+1})
      L = lieAlgebra({a,b},Field=>F)
      (3*x+2) a b + (2*x+3) b a
///
doc ///
Key
  Field
Headline
  name for an optional argument for lieAlgebra and holonomy
SeeAlso
  lieAlgebra
  holonomy
  "Second Lie algebra tutorial" 
  
Description
  Text
    This is an option for @TO lieAlgebra@ and 
    @TO holonomy@ to define the coefficient field, 
    which is {\tt QQ} by default.
///
doc ///
Key
 
     (map, LieAlgebra, LieAlgebra, List)
     
Headline
  make a Lie algebra homomorphism 
Usage
  f=map(L,M,homdefs)

Inputs
  L: LieAlgebra
  M: LieAlgebra
  homdefs: List
            of elements in L
Outputs
  f: LieAlgebraMap
     a map from $M$ to $L$
SeeAlso
  "map(LieAlgebra,LieAlgebra)"
  "map(LieAlgebra)"
  "map(LieDerivation)"
  "source(LieAlgebraMap)"
  "target(LieAlgebraMap)"
  (symbol SPACE,LieAlgebraMap,LieElement)
  "isWellDefined(ZZ,LieAlgebraMap)"
  "Homomorphisms and derivations"
  
  
Description
  Text
   The optional inputs given above are not relevant for Lie algebras.
   The generators of {\tt M} are mapped to the Lie elements 
   in the last argument {\tt homdefs}. 
   The output map {\tt f} might not be well defined 
   and not commute with the differentials. It
   can be checked whether this is true by using 
   @TO "isWellDefined(ZZ,LieAlgebraMap)"@.
   
  Example
      L1=lieAlgebra({a,b},Signs=>1,LastWeightHomological=>true,
	  Weights=>{{1,0},{2,1}})/{a a}
      F2=lieAlgebra({a,b,c},
	  Weights=>{{1,0},{2,1},{5,2}},Signs=>1,LastWeightHomological=>true)
      D2=differentialLieAlgebra{0_F2,a a,a a a b}
      L2=D2/{a a a a b,a b a b + a c}
      use L1
      f=map(L1,L2,{a,0_L1,a b b})
      isWellDefined(6,f)
      describe f
      use L2
      f c c
     
/// 
doc ///
Key
     (map, LieAlgebra, LieAlgebra)
Headline
  make a natural Lie algebra homomorphism 
Usage
  f=map(L,M)

Inputs
  L: LieAlgebra
  M: LieAlgebra 
Outputs
  f: LieAlgebraMap
     a map from $M$ to $L$
SeeAlso
  "map(LieAlgebra,LieAlgebra,List)"
  "map(LieAlgebra)"
  "map(LieDerivation)"
  "source(LieAlgebraMap)"
  "target(LieAlgebraMap)"
  (symbol SPACE,LieAlgebraMap,LieElement)
  "isWellDefined(ZZ,LieAlgebraMap)"
  "Homomorphisms and derivations"
  
  
Description
  Text
   The optional inputs given above are not relevant for Lie algebras.
   The effect of {\tt map(L,M)} 
   is that the "common" generators are mapped to 
   themselves and the other generators are mapped to 0 (cf map for rings). 
   Two generators are "common" if they have the same 
   name and the same weight and sign. 
   The output map {\tt f} might not be well defined 
   and not commute with the differentials. It
   can be checked whether this is true by using 
   @TO "isWellDefined(ZZ,LieAlgebraMap)"@.
   
  Example
       M = lieAlgebra{a,b,c}
       L = lieAlgebra({a,b,d},Weights=>{2,1,1}) 
       f = map(L,M)
       describe f
     
///
doc ///
Key

  (map,LieAlgebra)

Headline
 get the map of a minimal model
SeeAlso
  "map(LieAlgebra,LieAlgebra,List)"
  "map(LieAlgebra,LieAlgebra)"
  "map(LieDerivation)"
   minimalModel
  "Differential Lie algebra tutorial"
   
Usage
  f = map(M)
  
Inputs
  M: LieAlgebra
      the minimal model         
Outputs
  f: LieAlgebraMap  
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
    If $M$ is the minimal model of a Lie algebra $L$, 
    then there is defined a Lie algebra map $f: M \ \to\  L$, and this map is 
    represented by {\tt map(M)}.
  Example
   L=lieAlgebra{a,b,c}/{a b,a b c}
   M=minimalModel(3,L)
   describe map(M)
       
/// 
doc ///
Key
  (map,LieDerivation)

Headline
 get the map in the definition of a Lie derivation
SeeAlso
  "map(LieAlgebra,LieAlgebra,List)"
  "map(LieAlgebra,LieAlgebra)"
  "map(LieAlgebra)"
  lieDerivation
  "Homomorphisms and derivations"
   
Usage
  f = map(d)
  
Inputs
  d: LieDerivation              
Outputs
  f: LieAlgebraMap  
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
    If $d$ is a derivation $M \ \to\  L$, then there is 
    defined a Lie algebra map $f: M \ \to\  L$,
    which determines the $M$-module-structure on $L$, and this map
    is represented by {\tt map(d)}.
  Example
      L=lieAlgebra({x,y},Signs=>1)	
      M=lieAlgebra({a,b},Signs=>0,Weights=>{2,2})
      f = map(L,M,{x x,x y})
      d = lieDerivation(f,{2 x,-y})
      describe d
      d a b
///

doc ///
Key
  lieSubAlgebra
      (lieSubAlgebra,List) 
Headline
  make a Lie subalgebra
Usage
  S=lieSubAlgebra(gens)

Inputs
  gens: List
         a list of elements in a Lie algebra $L$
Outputs
  S: FGLieSubAlgebra
      the subalgebra of $L$ generated by the list {\tt gens} 
SeeAlso
  lieAlgebra
  lieIdeal
  
Description
  Text
    The input should be a list of Lie elements in a Lie
    algebra $L$. The program adds generators for the subalgebra 
    to make it
    invariant under the differential. 
  
  Example
      F=lieAlgebra({a,b,c,r3,r4,r42},
         Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         Signs=>{0,0,0,1,1,0},LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
      S=lieSubAlgebra{b c - a c,a b,b r4 - a r4}
      describe S
      basis(5,S)
      d=differential D
      d\S#gens
      (b c-a c) a b   
 ///
document {
     Key => {
	   lieIdeal,
	   (lieIdeal,List),
     	   (lieIdeal,LieSubSpace)
	  },
     Headline => "make a Lie ideal", 
      TEX "The input should be a list $g$ of Lie elements 
    in a Lie algebra $L$ or a subspace $S$ 
    of $L$.
    The program adds generators to the input list or the subspace 
    to make the ideal 
    invariant under the differential. 
    In the case when the input is a list, these extra non-normalized 
    generators may be seen using ", TT "gens(I).",
    SeeAlso => {"lieAlgebra","lieSubSpace",(symbol /,LieAlgebra,LieIdeal)},
     SYNOPSIS {
	  Usage => "I=lieIdeal(g)",
     	  Inputs => {
	       "g" => List
	       },
	  Outputs => {
	       "I" => LieIdeal => TEX "the ideal generated by the list $g$"
	       }
	  },
      
     EXAMPLE{
      "F = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)",
      "D = differentialLieAlgebra({0_F,a a,a b})",    
      "I = lieIdeal{a b,c}",
      "gens I"
      },
     
     SYNOPSIS {
	  Usage => "I=lieIdeal(S)",
     	  Inputs => {
	       "S" => LieSubSpace
	       },
	  Outputs => {
	       "I" => LieIdeal  => TEX "the ideal generated by the subspace $S$"
	       }
	  },
      
    EXAMPLE{
      "C = cycles D",
      "basis(4,C)",
      "I = lieIdeal C",
      "basis(4,I)"
	}
     
     }

  
 doc ///
Key
  lieSubSpace
     (lieSubSpace,List)
Headline
  make a Lie subspace
Usage
  S=lieSubSpace(gens)

Inputs
  gens: List
         a list of homogeneous elements in a Lie algebra $L$
Outputs
  S: LieSubSpace
      the subspace of $L$ generated by the list {\tt gens} 
SeeAlso
  lieAlgebra
  lieSubAlgebra
  lieIdeal
  
Description
  Text
    The input should be a list of homogeneous Lie elements in a Lie
    algebra $L$.
    The output need not in general be 
    invariant under the differential. 
  
  Example
      F=lieAlgebra({a,b,c,r3,r4,r42},
         Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         Signs=>{0,0,0,1,1,0},LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
      S=lieSubSpace{b c - a c,a b,b r4 - a r4}
      describe S
      d=differential D
      basis(5,S)
      d\oo
      
 ///

 doc ///
Key
  
     (image,LieAlgebraMap,LieSubSpace)
     
Headline
  make the image of a Lie subspace under a Lie algebra map
Usage
  I=image(f,S)

Inputs
  f: LieAlgebraMap
  S: LieSubSpace
       an instance of type {\tt LieSubSpace},  
       a Lie subspace of the source of $f$         
Outputs
  I : LieSubSpace
      	an instance of {\tt LieSubSpace},
        the image $f(S)$, a Lie subspace of the target of $f$        
SeeAlso
  "image(LieDerivation,LieSubSpace)"
  "image(LieAlgebraMap)"
  "image(LieDerivation)"
  "kernel(LieAlgebraMap)"
  "inverse(LieAlgebraMap,LieSubSpace)"
  
Description
  Text
    If $S$ is of type {\tt FGLieSubAlgebra}, then 
    {\tt image(f,S)} is of type {\tt FGLieSubAlgebra}. If $S$ is 
    an instance of {\tt LieSubAlgebra}, but not of {\tt FGLieSubAlgebra}, then  
    {\tt image(f,S)} is of type {\tt LieSubAlgebra}.  
    Otherwise, {\tt image(f,S)} is of type {\tt LieSubSpace}.
  Example
      F=lieAlgebra({a,b,c,r3,r4,r42},
        Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},Signs=>{0,0,0,1,1,0},
	LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
      I=lieIdeal{b c - a c,a b,b r4 - a r4}
      S=lieIdeal{a c}
      Q=D/I
      f=map(Q,D)
      T=image(f,S)
      basis(5,T)
 /// 
 doc ///
Key
  
     (image,LieAlgebraMap)
     
Headline
  make the image of a Lie algebra map
Usage
  I=image(f)

Inputs
  f: LieAlgebraMap         
Outputs
  I : FGLieSubAlgebra
      the image of $f$, a finitely generated Lie subalgebra
      of the target of $f$
      	      
SeeAlso
  "image(LieAlgebraMap,LieSubSpace)"
  "image(LieDerivation,LieSubSpace)"
  "image(LieDerivation)"
  "kernel(LieAlgebraMap)"
  "inverse(LieAlgebraMap,LieSubSpace)"
  "isSurjective(LieAlgebraMap)"
  
Description
  
  Example
      F=lieAlgebra({a,b,c,r3,r4,r42},
        Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},Signs=>{0,0,0,1,1,0},
	LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
      I=lieIdeal{b c - a c,a b,b r4 - a r4}
      Q=D/I
      f=map(Q,D)
      T=image(f)
      isSurjective f
 /// 
  doc ///
Key
  
     (image,LieDerivation,LieSubSpace)
     
Headline
  make the image of a Lie subspace under a Lie derivation
Usage
  I=image(d,S)

Inputs
  d: LieDerivation
  S: LieSubSpace
       an instance of type {\tt LieSubSpace},  
       a Lie subspace of the source of $d$         
Outputs
  I : LieSubSpace
      	an instance of {\tt LieSubSpace},
        the image $d(S)$, a Lie subspace of the target of $d$        
SeeAlso
  "image(LieAlgebraMap,LieSubSpace)"
  "image(LieAlgebraMap)"
  "image(LieDerivation)"
  "kernel(LieDerivation)"
  "inverse(LieDerivation,LieSubSpace)"
  
Description
  Text
    If $d$ is a differential on a Lie algebra $L$ and $S$ is 
    an ideal in $L$,  
    then {\tt image(d,S)} is of type {\tt LieSubAlgebra}. 
    Otherwise, {\tt image(d,S)} is of type {\tt LieSubSpace}.
  Example
      F=lieAlgebra({a,b,c,r3,r4,r42},
        Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},Signs=>{0,0,0,1,1,0},
	LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
      S=lieIdeal{a r3}
      d=differential D
      T=image(d,S)
      basis(5,T)
     
 /// 
doc ///
Key
  
     (image,LieDerivation)
     
Headline
  make the image of a Lie derivation
Usage
  I=image(d)

Inputs
  d: LieDerivation         
Outputs
  I : LieSubSpace
      an instance of type {\tt LieSubSpace}, the image of $d$ 
      	      
SeeAlso
  "image(LieAlgebraMap,LieSubSpace)"
  "image(LieDerivation,LieSubSpace)"
  "image(LieAlgebraMap)"
  "kernel(LieDerivation)"
  "inverse(LieDerivation,LieSubSpace)"
   boundaries
  
  
Description
  
  Text
    If $d$ is a differential on a Lie algebra $L$,
    then {\tt image(d)} (which is the same as the boundaries of $L$) 
    is of type {\tt LieSubAlgebra}. Otherwise, {\tt image(d)} is of type
    {\tt LieSubSpace}.
  Example
      F=lieAlgebra({a,b,c,r3,r4,r42},
        Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},Signs=>{0,0,0,1,1,0},
	LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
      d=differential D
      J=image(d)
      dims(6,J)
      dims(6,boundaries D)
 /// 
 doc ///
Key
  
     (inverse,LieAlgebraMap,LieSubSpace)
     
Headline
  make the inverse image of a Lie subspace under a Lie algebra map
Usage
  I=inverse(f,S)

Inputs
  f: LieAlgebraMap
  S: LieSubSpace
       an instance of type {\tt LieSubSpace},  
       a Lie subspace of the target of $f$         
Outputs
  I : LieSubSpace
      	an instance of {\tt LieSubSpace},
        a Lie subspace of the source of $f$,
        the inverse image of $S$ under $f$,
	
SeeAlso
  "inverse(LieDerivation,LieSubSpace)"
  "image(LieAlgebraMap,LieSubSpace)"
  "kernel(LieAlgebraMap)"

  
Description
  Text
    If $S$ is an instance of {\tt LieIdeal}, then $I$ is of type {\tt LieIdeal}.     
    If $S$ is an instance of  {\tt LieSubAlgebra} but not of {\tt LieIdeal},  
    then $I$ is of type {\tt LieSubAlgebra}. 
    Otherwise, $I$ is of type {\tt LieSubSpace}.
  
  Example
      F=lieAlgebra{a,b,c}        
      I=lieIdeal{b c - a c}
      Q=F/I
      f=map(Q,F)
      J=lieIdeal{a b}
      K=inverse(f,J)
      dims(1,6,F/K)
      dims(1,6,Q/J)
 /// 
 doc ///
Key
     (inverse,LieDerivation,LieSubSpace)
Headline
  make the inverse image of a Lie subspace under a Lie derivation
Usage
  I=inverse(d,S)

Inputs
  d: LieDerivation
  S: LieSubSpace
       an instance of type {\tt LieSubSpace}, 
       a Lie subspace of the target of $d$        
Outputs
  I : LieSubSpace
        a Lie subspace of the source of $d$,
        the inverse image of $S$ under $d$,
	an instance of {\tt LieSubSpace}
SeeAlso
  "inverse(LieAlgebraMap,LieSubSpace)"
  "image(LieAlgebraMap,LieSubSpace)"
  "kernel(LieAlgebraMap)"
  
  
Description
  Text
    If $S$ is an instance of {\tt LieIdeal}, then $I$ is of type {\tt LieSubAlgebra}.     
    Otherwise, $I$ is of type {\tt LieSubSpace}.
  
  Example
      F = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
      D = differentialLieAlgebra({0_F,a a,a b})
      d = differential D
      B = boundaries D
      x = (a a b a c) + (a a a b c)
      member(x,B) 
      S = inverse(d,lieIdeal{x})
      weight x
      basis(8,4,S)
      d\oo
      
 /// 
  doc ///
Key
     (quotient,LieIdeal,FGLieSubAlgebra)
Headline
  make the quotient of a Lie ideal by a finitely generated Lie subalgebra
Usage
  T=quotient(I,S)

Inputs
  I: LieIdeal
  S: FGLieSubAlgebra         
Outputs
  T : LieSubAlgebra
        the Lie elements that multiply
	all of $S$ into $I$
SeeAlso
  "annihilator(FGLieSubAlgebra)"
  "inverse(LieAlgebraMap,LieSubSpace)"
  
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
    A Lie element $x$ is in $T$ if $x$ multiplies all the 
    generators of $S$ into $I$. However,
    $T$ is not in general finitely generated.
  
  Example
      L=lieAlgebra{a,b,c}/{a a b-c c b,b b a-b b c}
      I=lieIdeal{a}
      S=lieSubAlgebra{b,c}
      K=quotient(I,S)      
      basis(2,K)
      
 /// 
 
 doc ///
Key
     (kernel,LieAlgebraMap)
Headline
  make the kernel of a map
Usage
  I=kernel(f)

Inputs
  f: LieAlgebraMap           
Outputs
  I : LieIdeal
      the kernel of $f$
         
SeeAlso
  "kernel(LieDerivation)"
  "image(LieAlgebraMap)"
  "inverse(LieAlgebraMap,LieSubSpace)"
  
Description
  Text
    The optional input given above is not relevant for Lie algebras. 
    
  Example
      L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
             Signs=>{1,1,1},LastWeightHomological=>true)
      D= differentialLieAlgebra({0_L,a a,a b})     
      Q=D/{b b+4 a c}
      R=Q/{a b b}
      f=map(R,Q)
      I=kernel f
      basis(6,I)
      
 /// 
 doc ///
Key
     (kernel,LieDerivation)
Headline
  make the kernel of a map
Usage
  I=kernel(d)

Inputs
  d: LieDerivation           
Outputs
  I : LieSubSpace
      an instance of {\tt LieSubSpace},
      the kernel of $d$
         
SeeAlso
  "kernel(LieAlgebraMap)"
  "image(LieDerivation)"
  "inverse(LieDerivation,LieSubSpace)"
  
Description
  Text
    The optional input given above is not relevant for Lie algebras. 
    If $d$ commutes with the 
    differentials in the 
    source and target of $d$, then the output is of type {\tt LieSubAlgebra}.  
    Otherwise, the output is of type {\tt LieSubSpace}.
    
  Example
      L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
             Signs=>{1,1,1},LastWeightHomological=>true)
      D= differentialLieAlgebra({0_L,a a,a b})     
      Q=D/{b b+4 a c}
      d=differential Q
      Z=kernel d
      C=cycles Q
      dims(8,Z)
      dims(8,C)
      
      
 /// 

 doc ///
Key
     (member,LieElement,LieSubSpace)
Headline
  whether a Lie element belongs to a Lie subspace
Usage
  b=member(x,S)

Inputs
  x: LieElement
  S: LieSubSpace
      an instance of type {\tt LieSubSpace}         
Outputs
  b : Boolean
         {\tt true} if $x$ belongs to $S$ and {\tt false} otherwise
SeeAlso
   LieSubSpace
  "inverse(LieAlgebraMap,LieSubSpace)"
  
Description
  
  Example
      L=lieAlgebra{a,b,c}/{a a b-c c b,b b a-b b c}
      I=lieIdeal{a}
      S=lieSubAlgebra{b,c}
      member(b b c,I)      
      member(a b b c,S)
      
 /// 

  

  doc ///
Key
  
     (symbol +,LieSubSpace,LieSubSpace)
Headline
  make the sum of two Lie subspaces
Usage
  S = A + B

Inputs
  A: LieSubSpace  
       an instance of type {\tt LieSubSpace}
  B: LieSubSpace  
       an instance of type {\tt LieSubSpace}       
Outputs
  S : LieSubSpace
        an instance of type {\tt LieSubSpace},
        the sum of $A$ and $B$
SeeAlso
  (symbol @,LieSubSpace,LieSubSpace)
  
  
Description
  Text
    If both $A$ and $B$ are instances of {\tt FGLieIdeal}, 
    then $S$ is of type {\tt FGLieIdeal}. 
    Otherwise, if both $A$ and $B$ are instances of {\tt LieIdeal}, 
    then $S$ is of type {\tt LieIdeal}. If exactly 
    one of $A$ and $B$
    is an instance of {\tt LieIdeal}, 
    and the other is an instance of {\tt LieSubAlgebra}, 
    then $S$ is of type {\tt LieSubAlgebra}. 
    Otherwise, $S$ is of type {\tt LieSubSpace}.
  
  Example
      F=lieAlgebra({a,b,c,r3,r4,r42},
         Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         Signs=>{0,0,0,1,1,0},LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
      I=lieIdeal{b c - a c,a b,b r4 - a r4}
      Q=D/I
      f=map(Q,D)
      J=lieIdeal{a c}
      K=inverse(f,J)
      use D
      I+lieIdeal{a c}
      dims(6,oo)
      dims(6,K)
 /// 

doc ///
Key
  
     (symbol @, LieSubSpace, LieSubSpace)    
Headline
  make the intersection of two Lie subspaces 
Usage
  S = A @ B  
Inputs
  A: LieSubSpace  
       an instance of {\tt LieSubSpace}
  B: LieSubSpace  
       an instance of {\tt LieSubSpace}     
Outputs
  S: LieSubSpace  
      an instance of {\tt LieSubSpace}, the intersection of $A$ and $B$
SeeAlso
  (symbol +,LieSubSpace,LieSubSpace)
  (symbol ++,LieAlgebra,LieAlgebra)
  
       
Description
 Text
   If both $A$ and $B$ are instances of {\tt LieIdeal}, 
   then $S$ is of tyoe  {\tt LieIdeal}.
   If both $A$ and $B$ are instances of {\tt LieSubAlgebra} 
   but not both of {\tt LieIdeal}, 
   then 
   $S$ is of tyoe  {\tt LieSubAlgebra}. 
   Otherwise, $S$ is of tyoe  {\tt LieSubSpace}. 
 Example
  L = lieAlgebra{a,b,c}
  A=lieIdeal{a}
  B=lieIdeal{b}
  S=A@B
  basis(3,S)
  T=A+B
  dims(1,3,L/T)
  dims(1,5,L/A@B)
  dims(1,5,L/A++L/B)
///                      
doc///
Key
  (symbol /,LieAlgebra,List)
Headline
   make a quotient Lie algebra
Usage
   Q=L/x
Inputs
   L: LieAlgebra
   x: List
      a list of elements of type {\tt L}
Outputs
   Q: LieAlgebra
      the quotient of $L$ by the ideal generated by the list $x$
Description
   Text
     Consider first the case where $L$ has zero differential, and where $L$
     is finitely presented as a quotient of a free Lie algebra $F$. In this
     case, the output $Q$ is also finitely presented as a quotient of $F$.
     
   Example
     F = lieAlgebra{a,b,c}
     L = F/{a b}
     Q = L/{a c}
     describe Q
     class\Q#ideal
     F/Q#ideal==Q    
   Text
     In case $L$ has a non-zero differential, 
     the program adds relations depending on the fact that 
     the ideal should be invariant under the differential. 
     These extra (non-normalized) relations may be looked 
     upon using  @TO "describe(LieAlgebra)"@. 
     Observe that $D$ is not free in this example,
     see @TO differentialLieAlgebra@.
   Example
     F = lieAlgebra({a,b,c2,c3},Weights=>{{1,0},{1,0},{2,1},{3,2}},
          Signs=>{1,1,1,1},LastWeightHomological=>true)
     D = differentialLieAlgebra{0_F,0_F,a a,b c2}
     L = D/{a c2}
     Q = L/{b c3}
     describe D
     describe Q
     class\ideal(Q)
     class\diff(Q)
    
   Text
     If the input Lie algebra $L$ is given as a finitely presented 
     Lie algebra $M$ modulo an ideal $J$
     that is not (known to be) finitely generated 
     (e.g., the kernel of a homomorphism ),
     then the output Lie algebra $Q$ is 
     presented as a quotient of a finitely presented Lie algebra
     $N$ by an ideal $I$, where 
     $N$ is given as $M$ modulo a lifting of the input list 
     $x$ to $M$, and $I$ is the image 
     of the natural map from $M$ to $N$ applied to $J$,
     see @TO "image(LieAlgebraMap,LieSubSpace)"@.   
   Example
     F = lieAlgebra{a,b,c}
     M = F/{a b}
     f=map(M,M,{0_M,b,c})
     J=kernel f
     L = M/J
     Q=L/{b c}          
     N=ambient Q
     describe Q
     use M
     N==M/{b c}
     ideal(Q)===new LieIdeal from image(map(N,M),J)
     
     
     
     
      
SeeAlso
     (symbol /,LieAlgebra,LieAlgebraMap)
     (symbol /,LieAlgebra,LieIdeal)
     differentialLieAlgebra
     "image(LieAlgebraMap,LieSubSpace)"
     "inverse(LieAlgebraMap,LieSubSpace)"
     zeroIdeal
     "Quotient Lie algebras and subspaces"
     
///
doc///
Key
  (symbol /,LieAlgebra,LieAlgebraMap)
Headline
   make a quotient Lie algebra 
Usage
   Q=L/f
Inputs
   L: LieAlgebra
   f: LieAlgebraMap
      a map with target $L$
Outputs
   Q: LieAlgebra
      the quotient of $L$ by the ideal generated by the image of the map $f$
Description
   Text
     This is the same as $L$ modulo the set of values of $f$ applied to the 
     generators of the source of $f$.
   Example
     M = lieAlgebra{a,b,c}
     L = M/{a b}
     N = lieAlgebra({d}, Weights=>{2})
     f = map(L,N,{a c})
     Q = L/f
     describe Q
SeeAlso
     (symbol /,LieAlgebra,List)
     (symbol /,LieAlgebra,LieIdeal)
///
doc ///
Key
  (symbol /,LieAlgebra,LieIdeal)
Headline
   make a quotient Lie algebra 
Usage
   Q=L/I
Inputs
   L: LieAlgebra
   I: LieIdeal
      an ideal of $L$
Outputs
   Q: LieAlgebra
      the quotient of $L$ by the ideal $I$
Description
   Text
     Any object of type {\tt LieAlgebra} is a finitely presented 
     (differential) Lie algebra modulo an ideal, which 
     is an object of type {\tt LieIdeal} (and which might be zero). 
     If the input Lie algebra $L$ is finitely presented, 
     then the output Lie algebra $Q$ is 
     simply presented as a quotient of $L$ 
     by the input ideal $I$.
     (Observe that each time {\tt L/I} is executed, 
     a new different copy of {\tt L/I} is produced.)
   Example
     F = lieAlgebra{a,b,c}
     L = F/{a b}
     f=map(L,L,{0_L,b,c})
     I=kernel f
     Q = L/I
     describe Q
     Q===L/I
     Q==L/I
   Text
     If the input Lie algebra $L$ is given as a 
     finitely presented Lie algebra $M$ modulo an ideal $J$ 
     that is not (known to be) finitely generated 
     (e.g., the kernel of a homomorphism ),
     then the output Lie algebra $Q$ is presented as $M$ modulo the 
     ideal that is the inverse image of the natural map
     from $M$ to $L$ applied to the input ideal $I$.
   
   Example
     F = lieAlgebra{a,b,c}
     M = F/{a b}
     f=map(M,M,{0_M,b,c})
     J=kernel f
     L = M/J
     X=lieAlgebra{x}
     g=map(X,L,{0_X,x,x})
     I=kernel g
     Q=L/I
     ambient Q===M
     ideal(Q)===inverse(map(L,M),I)
     
     
     
     
      
SeeAlso
     (symbol /,LieAlgebra,LieAlgebraMap)
     (symbol /,LieAlgebra,List)
     differentialLieAlgebra
     lieIdeal
     "kernel(LieAlgebraMap)"
     "inverse(LieAlgebraMap,LieSubSpace)"
     "Quotient Lie algebras and subspaces"
     
///

doc ///
Key
  differentialLieAlgebra
     (differentialLieAlgebra,List)
Headline
  make a differential Lie algebra
Usage
  L=differentialLieAlgebra(defs)
Inputs
  defs: List
        of Lie elements in a free Lie algebra
Outputs
  L: LieAlgebra
SeeAlso
  lieAlgebra
  differential
  
   
Description
  Text
    The input should be a list of Lie elements in a free Lie
    algebra $F$ and this list consists of the differentials of 
    the generators, where $0_F$ is used for the zero element. 
    The option @TO LastWeightHomological@ for $F$
    must have the value {\tt true}.  The program adds relations 
    to the Lie algebra to get
    the square of the differential to be 0. Use @TO "ideal(LieAlgebra)"@
    to get these non-normalized relations or @TO "describe(LieAlgebra)"@
    to just look at them.
    


     
     
  Example
      F1=lieAlgebra({a,b,c,r3,r4,r42},
         Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         Signs=>{0,0,0,1,1,0},LastWeightHomological=>true)
      D1=differentialLieAlgebra{0_F1,0_F1,0_F1,a c,a a c,r4 - a r3}
      ideal D1   
      F2=lieAlgebra({a,b,c2,c3,c5},Signs=>{0,0,1,0,1},
         Weights=>{{1,0},{1,0},{2,1},{3,2},{5,3}},
	 LastWeightHomological=>true)
      D2=differentialLieAlgebra{0_F2,0_F2,a b,a c2,a b c3}
      describe D2
      
      
      
         
///
doc ///
Key
     (symbol SPACE,LieElement,LieElement)
Headline
     multiplication of Lie elements
Usage
     u=x y
Inputs
     x: LieElement
        $x$ is of type $L$, where $L$ is of type {\tt LieAlgebra}
     y: LieElement
        $y$ is of type $L$
Outputs
     u: LieElement
        $u$ is of type $L$, the Lie product of $x$ and $y$  
Description
  Text
      SPACE is used as infix notation for the Lie
      multiplication. It is right associative and hence 
      \break
      {\tt b b b a} is the 
      same as {\tt (b (b (b a)))}, which in output is
      written as {\tt (b b b a)} or a normal form equivalent.
  Example 
      L = lieAlgebra{a,b,c}
      b b b a 
      (a b+b c) (a c) 
      
///
 
doc ///
Key
     (symbol +,LieElement,LieElement)
     
Headline
     addition of Lie elements
Usage
   u=x + y
Inputs
   x: LieElement
     $x$ is of type $L$, where $L$ is of type {\tt LieAlgebra}
   y: LieElement
     $y$ is of type $L$
Outputs
   u: LieElement
     $u$ is of type $L$, the sum of $x$ and $y$  
Description
  Text
      The symbol + is used as infix notation for the Lie
      addition. The elements $x$ and $y$ must have the same
      weight and sign.  
  Example 
      L = lieAlgebra{a,b}
      b b b a + a a a b       
///
doc ///
Key
     (symbol -,LieElement,LieElement)
Headline
     subtraction of Lie elements
Usage
   u=x - y
Inputs
   x: LieElement
     $x$ is of type $L$, where $L$ is of type {\tt LieAlgebra}
   y: LieElement
     $y$ is of type $L$
Outputs
   u: LieElement
     $u$ is of type $L$, the difference of $x$ and $y$  
Description
  Text
      The symbol - is used as infix notation for the Lie
      subtraction. The elements $x$ and $y$ must hade the same
      weight and sign.
  Example 
      L = lieAlgebra{a,b}
      b b b a - a a a b       
///
doc ///
Key
     (symbol -,LieElement)
Headline
     unary negation 
Usage
   u = - x
Inputs
   x: LieElement
     $x$ is of type $L$, where $L$ is of type {\tt LieAlgebra}   
Outputs
   u: LieElement
     $u$ is of type $L$, the negation of $x$   
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
     multiplication of a number and a Lie element
Usage
   u = a x
Inputs
   a: Number
   x: LieElement
     $x$ is of type $L$, where $L$ is of type {\tt LieAlgebra}  
Outputs
   u: LieElement
     $u$ is of type $L$, the product $a*x$   
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars are of type {\tt Number} if the field is {\tt QQ},
      otherwise the scalars are of type {\tt RingElement}.
      If the Lie element is surrounded by parentheses,
      the SPACE may be omitted.
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
     multiplication of a field element and a Lie element
Usage
   y = a x
Inputs
   a: RingElement
      $a$ is an element in {\tt L#Field}, where $L$ is of type {\tt Lielgebra}
   x: LieElement
      $x$ is of type $L$  
Outputs
   y: LieElement
      $y$ is of type $L$, the product $a*x$
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars belong to {\tt L#Field}. 
      If the field is not {\tt QQ}, then the 
      scalars are of type {\tt RingElement}. If the field is {\tt QQ}, then 
      the scalars are of type {\tt Number}. If the field is not a 
      prime field, then sometimes it is necessary to define the field
      outside the constructor lieAlgebra. Observe also that it 
      is necessary to use the function {\tt toField} when $F$ is defined 
      as an algebraic extension of a prime field.
  Example 
      F = toField(ZZ/7[x]/{x^2+1})
      L = lieAlgebra({a,b},Field=>F)
      (3*x+2) a b + (2*x+3) b a
SeeAlso
       (symbol SPACE,RingElement,LieAlgebraMap)
       (symbol SPACE,RingElement,LieDerivation) 
       (symbol SPACE,Number, LieElement)  
///
doc ///
Key
     (symbol @,LieElement,LieElement)
Headline
     formal multiplication of Lie elements
SeeAlso
   (symbol ++,LieElement,LieElement) 
   (symbol /,LieElement,LieElement)
   (symbol SPACE,LieElement,LieElement)  
Usage
   u=x (symbol @) y
Inputs
   x: LieElement
     $x$ is of type $L$, where $L$ is of type LieAlgebra
   y: LieElement
     $y$ is of type $L$
Outputs
   u: LieElement
     u is of type $L$, the formal Lie product of $x$ and $y$  
Description
  Text
      The "at sign" $\@$ is used as infix notation for a "formal" Lie
      multiplication (and also formal multiplication by scalars) 
      where no simplifications are performed.
      (The formal addition is written as ++ and / is used 
      as formal subtraction.)
      In this sense, it is different from the use of SPACE
      as multiplication operator, which always gives an object 
      of normal form as output. The formal
      operations are useful when relations are introduced 
      in a big free Lie algebra, since then 
      it might be too hard to compute the normal form of 
      the relations, which is not needed in order to
      define a quotient Lie algebra. 
      For an example, see  
      @TO "Minimal models, Ext-algebras and Koszul duals"@.   
  Example 
      L = lieAlgebra{a,b}
      (b@b)@a/3@b@a@b++2@a@b@b
      (b b) a - 3 b a b + 2 a b b       
///
doc ///
Key
     (symbol ++,LieElement,LieElement)
Headline
     formal addition of Lie elements
SeeAlso
   (symbol @,LieElement,LieElement) 
   (symbol /,LieElement,LieElement)
   (symbol +,LieElement,LieElement)  
Description
  Text
     Formal addition of Lie elements.
     See the documentation of the formal Lie multiplication 
     for more information.
  Example
    L = lieAlgebra{a,b}
    a@b++b@a
///
doc ///
Key
     (symbol /,LieElement,LieElement)
Headline
     formal subtraction of Lie elements
SeeAlso
   (symbol @,LieElement,LieElement) 
   (symbol ++,LieElement,LieElement)
   (symbol -,LieElement,LieElement)
Description
  Text
     Formal subtraction of Lie elements.
     See the documentation of the formal Lie multiplication 
     for more information.
  Example
    L = lieAlgebra{a,b}
    a/a
///
doc ///
Key
     (symbol @,Number,LieElement)
Headline
     formal multiplication of a number and a Lie element
SeeAlso
   (symbol SPACE,Number,LieElement)
   (symbol @,LieElement,LieElement)
Description
  Text
     Formal multiplication by scalars.
     See the documentation of the formal Lie multiplication 
     for more information.
  Example
     L = lieAlgebra{a,b}
     (2@a)++(2@a)
///
doc ///
Key
     (symbol @,RingElement,LieElement)
Headline
     formal multiplication of a ring element and a Lie element
SeeAlso
   (symbol SPACE,RingElement,LieElement)
   (symbol @,LieElement,LieElement)
Description
  Text
     Formal multiplication by scalars.
     See the documentation of the formal Lie multiplication 
     for more information.
  Example
     L = lieAlgebra({a,b},Field=>ZZ/7)
     5@a++2@a
///
doc ///
Key
     (symbol SPACE,ExtElement,ExtElement)
Headline
     multiplication of Ext-algebra elements
Usage
     u=x y
Inputs
     x: ExtElement
        $x$ is of type $E$, where $E$ is of type {\tt ExtAlgebra}
     y: ExtElement
        $y$ is of type $E$
Outputs
     u: ExtElement
        $u$ is of type $E$, the Ext-algebra product of $x$ and $y$  
Description
  Text
      SPACE is used as infix notation for the Ext-algebra
      multiplication, which is graded commutative.
  Example 
      L = lieAlgebra{a,b,c}/{a b,b c}
      E = extAlgebra(3,L) 
      (ext_1+2 ext_0) ext_2 
SeeAlso
      ExtAlgebra
      sign
      weight
      
///
 
doc ///
Key
     (symbol +,ExtElement,ExtElement)
     
Headline
     addition of Ext-algebra elements
Usage
   u=x + y
Inputs
   x: ExtElement
     $x$ is of type $E$, where $E$ is of type {\tt ExtAlgebra}
   y: ExtElement
     y is of type E
Outputs
   u: ExtElement
     $u$ is of type $E$, the sum of $x$ and $y$  
Description
  Text
      The symbol + is used as infix notation for the Ext-algebra
      addition. The elements $x$ and $y$ must have the same
      weight and sign.  
  Example 
      L = lieAlgebra{a,b,c}/{a b,b c}
      E = extAlgebra(3,L) 
      basis(2,E)
      ext_4+2 ext_3 
SeeAlso
      ExtAlgebra 
///
doc ///
Key
     (symbol -,ExtElement,ExtElement)
Headline
     subtraction of Ext-algebra elements
Usage
   u=x - y
Inputs
   x: ExtElement
     $x$ is of type $E$, where $E$ is of type {\tt ExtAlgebra}
   y: ExtElement
     $y$ is of type $E$
Outputs
   u: ExtElement
     $u$ is of type $E$, the difference of $x$ and $y$  
Description
  Text
      The symbol - is used as infix notation for the Ext-algebra
      subtraction. The elements $x$ and $y$ must hade the same
      weight and sign.
  Example 
      L = lieAlgebra{a,b,c}/{a b,b c}
      E = extAlgebra(3,L) 
      basis(2,E)
      ext_4-2 ext_3 
SeeAlso
      ExtAlgebra      
///
doc ///
Key
     (symbol -,ExtElement)
Headline
     unary negation 
Usage
   u = - x
Inputs
   x: ExtElement
     $x$ is of type $E$, where $E$ is of type {\tt ExtAlgebra}  
Outputs
   u: ExtElement
     $u$ is of type $E$, the negation of $x$   
Description
  Text
      The symbol - is used as notation for the Ext-algebra
      negation. 
  Example 
     L = lieAlgebra{a,b,c}/{a b,b c}
     E = extAlgebra(3,L) 
     -(2 ext_3-ext_4)
SeeAlso
      ExtAlgebra           
///
doc ///
Key
     (symbol SPACE,Number, ExtElement)
Headline
     multiplication of a number and an Ext-algebra element
Usage
   u = a x
Inputs
   a: Number
   x: ExtElement
     $x$ is of type $E$, where $E$ is of type {\tt ExtAlgebra}   
Outputs
   u: ExtElement
     $u$ is of type $E$, the product $a*x$   
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars are of type {\tt Number} 
      if the field is {\tt QQ},
      otherwise the scalars are of type {\tt RingElement}.
     
  Example
       L = lieAlgebra{a,b,c}/{a b,b c}
       E = extAlgebra(3,L)  
      (-3) (2 ext_3-ext_4)  
      
SeeAlso
       (symbol SPACE,RingElement,ExtElement)
       ExtAlgebra
                
///
doc ///
Key
     (symbol SPACE,RingElement,ExtElement)
Headline
     multiplication of a field element and a Ext-algebra element
Usage
   y = a x
Inputs
   a: RingElement
      $a$ is an element in the field of $E$, where $E$ is of type {\tt ExtAlgebra}
   x: ExtElement
      $x$ is of type $E$  
Outputs
   y: ExtElement
      $y$ is of type $E$, the product $a*x$
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars belong to the field of $E$. 
      If the field is not {\tt QQ}, then the 
      scalars are of type {\tt RingElement}. If the field is {\tt QQ}, then 
      the scalars are of type {\tt Number}. Observe that it 
      is necessary to use the function {\tt toField} when $F$ is defined 
      as an algebraic extension of a prime field.
  Example 
      F = toField(ZZ/7[x]/{x^2+1})
      L = lieAlgebra({a,b,c},Field=>F)/{a b,b c}
      E = extAlgebra(3,L)
      (3*x+1) (ext_1 ext_2)+(2*x+3) (ext_2 ext_1)
SeeAlso
       (symbol SPACE,Number,ExtElement)
       ExtAlgebra  
///
doc ///
Key
     (symbol +,LieAlgebraMap,LieAlgebraMap)
Headline
     addition of Lie homomorphisms
Usage
   u = f + g
Inputs
   f: LieAlgebraMap
     
   g: LieAlgebraMap     
Outputs
   u: LieAlgebraMap
      the sum of $f$ and $g$      
Description
  Text
      The symbol + is used as infix notation for the 
      addition of Lie homomorphisms. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = map(L,M,{a,b})
      g = map(L,M,{b,a}) 
      describe(f+g)      
///
doc ///
Key
     (symbol -,LieAlgebraMap,LieAlgebraMap)
Headline
     subtraction of Lie homomorphisms
Usage
   u = f - g
Inputs
   f: LieAlgebraMap
    
   g: LieAlgebraMap     
Outputs
   u: LieAlgebraMap
      the difference of $f$ and $g$      
Description
  Text
      The symbol - is used as infix notation for the 
      subtraction of Lie homomorphisms. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = map(L,M,{a,b})
      g = map(L,M,{b,a}) 
      describe(f-g)            
///
doc ///
Key
     (symbol -,LieAlgebraMap)
Headline
     unary negation 
Usage
   u = - f
Inputs
   f: LieAlgebraMap     
Outputs
   u: LieAlgebraMap        
Description
  Text
      The symbol - is used as notation for the 
      negation of Lie homomorphisms. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = map(L,M)
      describe(-f)      
///
doc ///
Key
     (symbol SPACE,Number, LieAlgebraMap)
Headline
     multiplication of a number and a homomorphism
Usage
   g = a f
Inputs
   a: Number
   f: LieAlgebraMap   
Outputs
   g: LieAlgebraMap     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars are of type {\tt Number} 
      if the field is {\tt QQ},
      otherwise the scalars are of type {\tt RingElement}.
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a,b}
      f = map(L,M,{a,b})
      describe(-2 f) 
SeeAlso
      (symbol SPACE,RingElement,LieAlgebraMap)         
///
doc ///
Key
     (symbol SPACE,RingElement,LieAlgebraMap)
Headline
     multiplication of a field element and a homomorphism
Usage
   g = a f
Inputs
   a: RingElement
      $a$ is an element in {\tt L#Field}, where $L$ is the target of $f$
   f: LieAlgebraMap        
Outputs
   g: LieAlgebraMap     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars belong to {\tt L#Field}, which must be
      the same as {\tt M#Field}, where \ $f: M\ \to\ L$. 
      If the field is not {\tt QQ}, then the 
      scalars are of type {\tt RingElement}. If the field is {\tt QQ}, then 
      the scalars are of type {\tt Number}. 
  Example 
      F = toField(ZZ/7[x]/{x^2+1})
      M = lieAlgebra({a,b},Field=>F)
      L = lieAlgebra({a,b},Field=>F)
      f = map(L,M,{x a,3 b})
      describe((3*x) f)
SeeAlso
       (symbol SPACE,Number,LieAlgebraMap)
       (symbol SPACE,RingElement,LieElement)
       (symbol SPACE,RingElement,LieDerivation)           
///
doc ///
Key
     (symbol +,LieDerivation,LieDerivation)
Headline
     addition of Lie derivations
Usage
   u = d + e
Inputs
   d: LieDerivation
     
   e: LieDerivation     
Outputs
   u: LieDerivation
      the sum of $d$ and $e$      
Description
  Text
      The symbol + is used as infix notation for the 
      addition of Lie derivations $M\ \to\ L$ with the same 
      defining map $f: M\ \to\ L$. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a1,b1}
      f = map(L,M,{b1,a1})
      d = lieDerivation(f,{a1,b1})
      e = lieDerivation(f,{2 b1,2 a1})
      u = d+e
      describe u
      u a b         
///
doc ///
Key
     (symbol -,LieDerivation,LieDerivation)
Headline
     subtraction of Lie derivations
Usage
   u = d - e
Inputs
   d: LieDerivation
    
   e: LieDerivation     
Outputs
   u: LieDerivation 
      the difference of $d$ and $e$        
Description
  Text
      The symbol - is used as infix notation for the 
      subtraction of Lie derivations $M\ \to\ L$ with the same 
      defining map $f: M\ \to\ L$. 
  Example 
      M = lieAlgebra{a,b}
      L = lieAlgebra{a1,b1}
      f = map(L,M,{b1,a1})
      d = lieDerivation(f,{a1,b1})
      e = lieDerivation(f,{2 b1,2 a1})
      u = d-e
      describe u
      u a b              
///
doc ///
Key
     (symbol -,LieDerivation)
Headline
     unary negation 
Usage
   u = - d
Inputs
   d: LieDerivation    
Outputs
   u: LieDerivation        
Description
  Text
      The symbol - is used as notation for the 
      negation of Lie derivations. 
  Example 
      L = lieAlgebra{a,b}
      d = lieDerivation{a a b,b b a}
      describe(-d)      
///

doc ///
Key
     (symbol SPACE,Number,LieDerivation)
Headline
     multiplication of a number and a derivation
Usage
   e = a d
Inputs
   a: Number
   d: LieDerivation        
Outputs
   e: LieDerivation     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars.
      The scalars are of type {\tt Number} if the field is {\tt QQ},
      otherwise the scalars are of type {\tt RingElement}.
       
  Example 
      L = lieAlgebra{a,b}
      d = lieDerivation{a a b,b b a}
      describe(-2 d) 
SeeAlso
       (symbol SPACE,RingElement,LieDerivation)
       (symbol SPACE,Number,LieElement)
       (symbol SPACE,Number,LieAlgebraMap)               
///
doc ///
Key
     (symbol SPACE,RingElement,LieDerivation)
Headline
     multiplication of a field element and a derivation
Usage
   e = a d
Inputs
   a: RingElement
      $a$ is an element in {\tt L#Field}, where $L$ is the target of $d$
   d: LieDerivation        
Outputs
   e: LieDerivation     
Description
  Text
      The symbol SPACE is used as notation for multiplication
      by scalars. The scalars belong to {\tt L#Field}, which must be
      the same as {\tt M#Field}, where $d: M\ \to\ L$. 
      If the field is not {\tt QQ}, then the 
      scalars are of type {\tt RingElement}. If the field is {\tt QQ}, then 
      the scalars are of type {\tt Number}, see 
      @TO (symbol SPACE,Number,LieDerivation)@. 
  Example 
      F = toField(ZZ/7[x]/{x^2+1})
      M = lieAlgebra({a,b},Field=>F)
      L = lieAlgebra({a,b},Field=>F)
      f = map(L,M,{x a,3 b})
      d = lieDerivation(f,{-x b,-2 a})
      describe (3*x) d 
SeeAlso
       (symbol SPACE,RingElement,LieElement)
       (symbol SPACE,RingElement,LieAlgebraMap)
       (symbol SPACE,Number,LieDerivation)   
               
///
doc ///
Key
     (symbol *,LieAlgebraMap,LieAlgebraMap)
Headline
     composition of homomorphisms
Usage
   h = f*g
Inputs
   f: LieAlgebraMap
      a homomorphism from $M$ to $L$
   g: LieAlgebraMap
      a homomorphism from $N$ to $M$
Outputs
   h: LieAlgebraMap
      the composition from $N$ to $L$
Description 
  Text
      If $f$ or $g$ is not well defined, 
      then it may happen that $f*g$ is well defined and 
      not equal to the map $x \ \to\  f(g(x))$ 
      but equal to the map induced by the
      values $f(g(gen))$ where $gen$ is a generator for $M$. Here is
      an example of this fact for rings.
  Example
      R = QQ[x]
      S = R/(x*x)
      f = map(R,S)
      g = map(S,R)
      h = f*g
      isWellDefined f
      isWellDefined h
      use R
      h(x*x)
      f(g(x*x))
         
  Example 
      L = lieAlgebra{a,b}
      f = map(L,L,{b,-a})
      describe(f*f+id_L)          
///
doc ///
Key
     (symbol *,LieDerivation,LieAlgebraMap)
Headline
     composition of a derivation and a homomorphism
Usage
   h = d*g
Inputs
   d: LieDerivation
      a derivation from $M$ to $L$
   g: LieAlgebraMap
      a homomorphism from $N$ to $M$
Outputs
   h: LieDerivation
      a derivation from $N$ to $L$
Description
 Text
      The composition of maps $d*g$ is a derivation $N\ \to\ L$, with 
      the composition $f*g$ defining the module structure of $L$ over $N$,
      where $f: M\ \to\ L$ defines the module structure of $L$ over $M$.
      
         
 Example			
      L = lieAlgebra{a,b}
      M = lieAlgebra{a,b,c}
      N = lieAlgebra{a1,b1}
      f = map(L,M)
      use M
      g = map(M,N,{b,a})
      use L
      d = lieDerivation(f,{a a b,b b a,a a b+b b a})
      describe d
      describe(f*g)
      describe(d*g)
      
      
      	        
///
doc ///
Key
     (symbol *,LieAlgebraMap,LieDerivation)
Headline
     composition of a homomorphism and a derivation
Usage
   h = g*d
Inputs
   g: LieAlgebraMap
      a homomorphism from $L$ to $N$
   d: LieDerivation
      a derivation from $M$ to $L$
Outputs
   h: LieDerivation
      a derivation from $M$ to $N$
Description
 Text
      The composition of maps $g*d$ is a derivation $M\ \to\ N$, with 
      the composition $g*f$ defining the module structure of $N$ over $M$,
      where $f: M\ \to\ L$ defines the module structure of $L$ over $M$.
          
 Example			
      L = lieAlgebra{a,b}
      d = lieDerivation{a a b,b b a}
      describe d
      N = lieAlgebra{a1,b1}
      g = map(N,L,{b1,a1})
      h = g*d
      describe h
      	        
///


doc///
Key
  (symbol SPACE,LieDerivation,LieDerivation)
Headline
   Lie multiplication of ordinary derivations
Usage
   e = d1 d2
Inputs
   d1: LieDerivation
   d2: LieDerivation      
Outputs
   e: LieDerivation
      the Lie product of $d1$ and $d2$
Description
   Text
      The vector space $D$ of graded derivations from $L$ to $L$ 
      with the identity map as defining map,
      see @TO LieDerivation@, is a graded Lie algebra. 
      If $L$ has a differential $del$, 
      then $D$ is a differential graded Lie algebra 
      with differential $d$\ \to\ [$del$,$d$].
      
   Example
      L = lieAlgebra{a,b}/{a a a b,b b b a}
      d0 = lieDerivation{a,b}
      d2 = lieDerivation{a b a,0_L}
      d4 = lieDerivation{a b a b a,0_L}
      describe d2 d4
      describe d0 d4
SeeAlso 
      lieDerivation
///
doc///
Key
  (symbol SPACE,LieDerivation,LieElement)
Headline
   apply a derivation
Usage
   y = d x 
Inputs
   d: LieDerivation
   x: LieElement
      $x$ is an element in the source of $d$      
Outputs
   y: LieElement
      $y$ is the result of applying $d$ to the element $x$   
Description
   
   Example
      L = lieAlgebra{a,b}
      d = lieDerivation{a,b}
      d a a a b a
SeeAlso
     (symbol SPACE,LieAlgebraMap,LieElement)
///
doc///
Key
  (symbol \,LieDerivation,List)
Headline
   apply a derivation to every element in a list
Usage
   y = d\x 
Inputs
   d: LieDerivation 
   x: List
      the elements in $x$ belong to the source of $d$      
Outputs
   y: List
      the result of applying $d$ to every element in $x$   
Description
   
   Example
      L = lieAlgebra{a,b}
      d = euler L
      d\basis(5,L)
///
doc///
Key
  (symbol SPACE,LieAlgebraMap,LieElement)
Headline
   apply a Lie homomorphism
Usage
   y = f x 
Inputs
   f: LieAlgebraMap
   x: LieElement
      $x$ is an element in the source of $f$      
Outputs
   y: LieElement
      $y$ is the result of applying $f$ to the element $x$    
Description
   
   Example
      L = lieAlgebra{a,b}
      M = L/{a a b}
      f = map(M,L)
      use L
      f a b b a
///
doc///
Key
  (symbol \,LieAlgebraMap,List)
Headline
   apply a Lie homomorphism to every element in a list
Usage
   y = f\x 
Inputs
   f: LieAlgebraMap
   x: List
      the elements in $x$ belong to the source of $f$       
Outputs
   y: List
     the result of applying $f$ to every element in $x$   
Description
   
   Example
      L = lieAlgebra{a,b}
      M = L/{a a b}
      f = map(M,L)
      basis(4,L)
      f\oo
      
///
doc///
Key
  (symbol @,LieDerivation,LieElement)
Headline
   formal application of a derivation to a Lie element
Usage
   y = d@x 
Inputs
   d: LieDerivation
   x: LieElement
      $x$ is an element in the source of $d$   
Outputs
   y: LieElement
      $y$ is the formal application of $d$ to the Lie element $x$
Description
   
   Example
      L = lieAlgebra{a,b}
      d = euler L
      d@a a a b a
SeeAlso
     (symbol SPACE,LieDerivation,LieElement)
///

doc///
Key
  (symbol \\,LieDerivation,List)
Headline
   formal application of a derivation to every element in a list
Usage
   y = d\\x 
Inputs
   d: LieDerivation 
   x: List
      the elements in $x$ belong to the source of $d$       
Outputs
   y: List
      the elements in $y$ are the formal application of $d$ to 
      the elements in $x$
SeeAlso
    (symbol \,LieDerivation,List)  
Description
   
   Example
      L = lieAlgebra{a,b}/{a a a b,b b b a}
      d = euler L
      d\\basis(5,L)
      d\basis(5,L)

///
doc///
Key
  (symbol @,LieAlgebraMap,LieElement)
Headline
   formal application of a Lie map to a Lie element
Usage
   y = f@x 
Inputs
   f: LieAlgebraMap
   x: LieElement
      $x$ is an element in the source of $f$       
Outputs
   y: LieElement
      $y$ is the formal application of $f$ to the Lie element $x$ 
Description
   
   Example
      L = lieAlgebra{a,b}
      M = L/{a a b}
      f = map(M,L)
      use L
      f@a b b a
      normalForm oo
SeeAlso
     (symbol SPACE,LieAlgebraMap,LieElement)   
///
doc///
Key
  (symbol \\,LieAlgebraMap,List)
Headline
   formal application of a Lie map to every element in a list
Usage
   y = f\\x 
Inputs
   f: LieAlgebraMap
   x: List
       the elements in $x$ belong to the source of $f$        
Outputs
   y: List
      the elements in $y$ are the formal application of 
      $f$ to the elements in $x$
SeeAlso
   (symbol \,LieAlgebraMap,List)
Description
   
   Example
      L = lieAlgebra{a,b}
      M = L/{a a b}
      f = map(M,L)
      b = basis(4,L)
      f\\b
      f\b
 
///

doc ///
Key
     (symbol *,LieAlgebra,LieAlgebra)
Headline
     free product of Lie algebras
Usage
   M = L1*L2
Inputs
   L1: LieAlgebra
   L2: LieAlgebra
Outputs
   M: LieAlgebra
      the free product of $L1$ and $L2$
Description
   
  Example 
      F1 = lieAlgebra({a,b},Signs=>{0,1},Weights=>{{2,0},{2,1}},
	  LastWeightHomological=>true)
      L1 = differentialLieAlgebra{0_F1,a}
      F2 = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
      L2 = differentialLieAlgebra{0_F2,a a,a b}/{b b+4 a c}
      M = L1*L2
      describe(M)
      normalForm\ideal(M)
      d = differential M
      d (pr_1 pr_3)
      
      
///
doc ///
Key
     (symbol ++,LieAlgebra,LieAlgebra)
Headline
     direct sum of Lie algebras
Usage
   S = L++M
Inputs
   L: LieAlgebra
   M: LieAlgebra
Outputs
   S: LieAlgebra
      the direct sum of $L$ and $M$
Description
   
  Example 
      F1 = lieAlgebra({a,b},Signs=>{0,1},Weights=>{{2,0},{2,1}},
	   LastWeightHomological=>true)
      L1 = differentialLieAlgebra{0_F1,a}
      F2 = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
      L2 = differentialLieAlgebra{0_F2,a a,a b}/{b b+4 a c}
      T=L1++L2
      describe(T)
      normalForm\ideal(T)
      
///
document {
     Key => {
	          (symbol _,ZZ,LieAlgebra)
                 
	  },
     Headline => "get the zero element", 
     SYNOPSIS {
	  Usage => "0_L",
     	  Inputs => {
	       ZZ => "0",
	       "L" => LieAlgebra
	       },
	  Outputs => {
	       LieElement 
	       }
	  },
     EXAMPLE{
         "L = lieAlgebra{a,b}/{a a b,b b a}",
         "0_L"
      }
  }
document {
     Key => {
	          
                  (symbol _,ZZ,ExtAlgebra)

	  },
     Headline => "get the zero element", 
     
     SYNOPSIS {
	  Usage => "0_E",
     	  Inputs => {
	       "0" => ZZ,
	       "E" => ExtAlgebra
	       },
	  Outputs => {
	       ExtElement 
	       }
	  }, 
      EXAMPLE{
         "L = lieAlgebra{a,b}/{a a b,b b a}",
         "E = extAlgebra(4,L)",
         "0_E"     
	},
     
     }


doc ///
Key
     (symbol _,ScriptedFunctor,LieAlgebra)
     
Headline
     get the identity homomorphism
Usage
    f=id_L 
Inputs
   id: ScriptedFunctor
   L: LieAlgebra
Outputs
   f: LieAlgebraMap
      the identity homomorphism from $L$ to $L$

Description
   
  Example 
      L = lieAlgebra{a,b}
      id_L
      peek oo
      
///
doc///
Key
  (ambient,LieAlgebra)
Headline
  get the ambient Lie algebra
Usage
  M = ambient L
Inputs
  L: LieAlgebra
Outputs
  M: LieAlgebra
     a Lie algebra of which $L$ is a quotient
Description
  Text
    Relations in $L$ are elements
    in {\tt ambient(L)}. When a quotient Lie algebra {\tt Q=L/I} is constructed,
    where $I$ is a list,
    then the elements in $I$ must be of type $L$, but the program
    converts the relations so that they have type {\tt ambient(L)} instead. 
    This may be seen by looking at {\tt ideal(Q)}. If $I$ is of type
    {\tt LieIdeal}, then the value of the expression {\tt ambient(Q)} is 
    {\tt L}, 
    and the value of the expression {\tt ideal(Q)} is {\tt I}.
  Example
     M = lieAlgebra{a,b,c}
     L = M/{a b}
     a c
     Q = L/{a c}
     ideal(Q)
     class\oo
     f=map(Q,L)
     I = kernel f
     R = L/I
     ambient R
     ideal R
  
///
 document {
     Key => {
	   lieDerivation,
           (lieDerivation, LieAlgebraMap, List),
	   (lieDerivation, List)
	  },
     Headline => "make a graded derivation", 
     TEX "Let $f: M ->  L$ be a map of Lie algebras.
     Let $F$ be
     a free Lie algebra together with a surjective 
     homomorphism $p: F  ->  M$.
     Define $g: F ->  L$ as the composition $g=f*p$. 
     A derivation $dF:F  ->   L$ over $g$ 
     is defined by defining $dF$ on the generators of $F$ 
     and then extending $dF$ to 
     all of $F$ by the derivation rule  	
     $dF$ [x, y] = [$dF$ x, $g$ y] ± [$g$ x, $dF$ y], 
     where  the sign is plus if sign$(d)=0$ or sign$(x)=0$ and minus otherwise.
     The output ", TT "d", TEX " represents the induced map $M  ->   L$, 
     which might not be well defined. 
     That the derivation is indeed well defined 
     may be checked (up to a certain degree) using ", TO "isWellDefined(ZZ,LieDerivation)", 
     TEX ". When no $f$ of class ", TT "LieAlgebraMap", 
     TEX " is given as input, the
     derivation $d$ maps $L$ to $L$ (and $f$ is the
     identity map). In this case, 
     the set $D$ of elements of class ", TT "LieDerivation", TEX " is a 
     graded Lie algebra with 
     Lie multiplication using SPACE. 
     If $L$ has differential $del$, 
     then $D$ is a differential Lie algebra with differential 
     $d ->  $[$del$,$d$].
     If $e$ is the Euler derivation on $L$, 
     then $d ->  $[$e$,$d$] is the Euler derivation
     on $D$.",
    SeeAlso => {"map(LieAlgebra,LieAlgebra,List)","sign","weight",
	"LieDerivation","differentialLieAlgebra"},
     SYNOPSIS {
	  Usage => "d=lieDerivation(f,defs)",
     	  Inputs => {
	       "f" => LieAlgebraMap,
	       "defs" => List => "the values of the generators"
	       },
	  Outputs => {
	       "d" => LieDerivation 
	       }
	  },
      
     EXAMPLE{
      "L=lieAlgebra({x,y},Signs=>1)",	
      "M=lieAlgebra({a,b},Weights=>{2,2})/{b a b}",
      "f = map(L,M,{x x,0_L})",
      "d = lieDerivation(f,{x,y})",
      "isWellDefined(6,d)",
      "describe d",
      "d a b"
	},
    
     SYNOPSIS {
	  Usage => "d=lieDerivation(defs)",
     	  Inputs => {
	       "defs" => List => "the values of the generators"
	       },
	  Outputs => {
	       "d" => LieDerivation 
	       }
	  },
      
     EXAMPLE{
      "L=lieAlgebra({x,y},Signs=>1)",
      "e = euler L",
      "d1 = lieDerivation{x y,0_L}",
      "d3 = lieDerivation{x x x y,0_L}",
      "describe d3",
      "e d1",
      "e d3",
      "oo===3 d3"
	}
         
     } 

end

 
