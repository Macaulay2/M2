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
			@TO "Differential Lie algebras Tutorial"@, 
			@TO "How to write Lie elements"@,
			@TO "Constructing Lie algebras"@ and
			@TO "Symmetries"@ 
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
		Example			
			L = lieAlgebra({a,b}, {[a,a,b]})			 
	SeeAlso
		lieAlgebra
		"First LieAlgebra Tutorial"
///

doc ///
	Key
		MapLie
	Headline 
		a Type for Lie algebra homomorphisms
	Description
		Text
			This Type represents homomorphisms of 
			graded (differential) Lie algebras.
		Example			
			L=lieAlgebra({x},{})
			M=lieAlgebra({x,y},{})
			f = mapLie(L,M,{[x],[]})
			peek f			 
	SeeAlso
		mapLie
		"Constructing Lie algebras"
///

doc ///
	Key
		DerLie
	Headline 
		a Type for Lie algebra derivations
	Description
		Text
			This Type represents graded derivations d:M->L where
			M,L are graded (differential) Lie algebras 
			and L is an M-module
			via f:M->L. If M=L and f is the identity, 
			the set of elements of class DerLie is a Lie algebra with 
			Lie multiplication @TO multDerLie@.
			However it is not of class  @TO LieAlgebra@, 
			if we do not have a finite presentation.
			
		Example			
		        L=lieAlgebra({x,y},{},genSigns=>1)
			M=lieAlgebra({a,b},{},genSigns=>0,genWeights=>{2,2})
			f = mapLie(L,M,{[x,x],[]})
			d = derLie(f,{[x,x],[x,y]})
			peek f
			peek d
			evalDerLie(d,[a,b])
			
			
						 
	SeeAlso
                derLie
	 	mapLie
		"Constructing Lie algebras"
///

doc ///
Key
  mapLie
     (mapLie, LieAlgebra, LieAlgebra, List)
Headline
  constructing a Lie algebra homomorphism 
Usage
  f=mapLie(L,M,homdefs)

Inputs
  L: LieAlgebra
  M: LieAlgebra
  homdefs: List
Outputs
  f: MapLie
SeeAlso
  sourceLie
  targetLie
  evalMapLie
  
Description
  Text
   The generators of M are mapped to the elements in the last argument homdefs 
   and they should be given as @TO generalExpressionLie@. 
   It is checked by the program that f maps
   the relations in M to zero and commutes with the 
   differential and that f preserves
   the weight and sign.
   
  Example
      L1=lieAlgebra({a,b},{[a,a]},genSigns=>1,genDiffs=>{[],[]},
	  genWeights=>{{1,0},{2,1}})
      L2=lieAlgebra({a,b,c},{[a,a,a,a,b],{{1,1},{[a,b,a,b],[a,c]}}},
	  genWeights=>{{1,0},{2,1},{5,2}},genSigns=>1,genDiffs=>{[],[a,a],[a,a,a,b]})
      f=mapLie(L1,L2,{[a],[],[a,b,b]})
      peek f
     
///  

doc ///
Key
  derLie
     (derLie, MapLie, List)
     (derLie, List)
Headline
  constructing a graded derivation
Usage
  d=derLie(f,defs)
  d=derLie(defs)

Inputs
  f: MapLie
  defs: List
Outputs
  d: DerLie
SeeAlso
  maplie
  signDer
  weightDer
  sourceLie
  targetLie
  diffLie
  evalDerLie
  multDerLie
  
  
Description
  Text
  
     The generators of M=f.sourceLie are mapped to the elements 
     in the last argument defs 
     and they should be given as @TO generalExpressionLie@ in L=f.targetLie. 
     If no f of class 
     MapLie is given,
     then the current Lie algebra L is used and the 
     derivation d maps L to L (and f is the
     identity map). The set of elements of class DerLie is a Lie algebra with 
     Lie multiplication @TO multDerLie@, 
     however it does not belong to @TO LieAlgebra@ if we do not have a finite 
     presentation.  It is checked by the program that d maps
     the relations in d.sourceLie to zero.
     
  Example
      L=lieAlgebra({x,y},{},genSigns=>1)	
      M=lieAlgebra({a,b},{[b,a,b]},genSigns=>0,genWeights=>{2,2})
      f = mapLie(L,M,{[x,x],[]})
      d1 = derLie(f,{[x,x],[x,y]})
      peek d1
      evalDerLie(d1,[a,a,b])
      useLie L
      d2 = derLie({[x,x,y],[x,x,y]})
      peek d2
      peek d2.maplie
      evalDerLie(d2,[x,x,y])
 
     
      
///      



doc ///
Key
  lieAlgebra
     (lieAlgebra, List, List)
Headline
  constructing a Lie algebra from its presentation
Usage
  L=lieAlgebra(liegens,lierels)

Inputs
  liegens: List
      the elements may be of class  Symbol, IndexedVariable or Integer
  lierels: List
      of elements of the form @TO generalExpressionLie@ 
Outputs
  L: LieAlgebra
SeeAlso
  compdeg
  deglength
  gensLie
  relsLie
  numGen
  "First LieAlgebra Tutorial"
  "Second LieAlgebra Tutorial"
Description
  Text
    A generator may be of class Symbol, IndexedVariable or Integer, a relation
    or a value for the differential
    should be a @TO generalExpressionLie@, see @TO "How to write Lie elements"@.
    
  Example
    L1 = lieAlgebra({a,b}, {{{1,-1},{[a,a,b],[b,b,a]}},[a,a,a,a,b]})
    computeLie 6
    peek L1
    L2=lieAlgebra({a,b,c},{[a,b],[a,c]},genWeights=>{{1,0},{1,0},{2,1}},
		   genSigns=>{1,1,1},
		   genDiffs=>{[],[],{{1,1},{[a,a],[b,b]}}})
    peek L2
    computeLie 5
    
///    


doc ///
Key
  genWeights
Headline
  optional argument for lieAlgebra and randomLie
SeeAlso
  lieAlgebra
  randomLie
  degLie
  weightLie
  "Second LieAlgebra Tutorial"
Description
 Text
      This is an option to tell @TO lieAlgebra@ to assign 
      the given weights to the generators.
       A weight
      is a list of integers of the same length, 
      the first degree (also just called the degree)  is positive and, 
      if the Lie algebra has a 
      differential, the last degree is the homological degree 
      which is non-negative
      and less than the first degree. If the
      Lie algebra has no differential, the program defines a 
      differential to be 0 and adds a last 
      degree 0 to the existing degrees. When the option 
      is given an integer value n, 
      which is not possible when a non-zero differential is given,
      then the program
      defines the weight for each generator to be \{n,0\}. 
 
///

doc ///
Key
  [lieAlgebra, genWeights]
  [randomLie, genWeights]
Headline
  optional argument for lieAlgebra and randomLie
Usage
  L = lieAlgebra(liegens,lierels,genWeights => w)
  l = randomLie(d, g, genWeights => w)
Inputs
  w: List
     of weights
  w: ZZ
     a weight is assigned to each generator
Outputs
  L: LieAlgebra
  l: List
SeeAlso
   lieAlgebra
   randomLie
   weightLie
   degLie
   "Second LieAlgebra Tutorial"
Description
 Text
      This is an option to tell @TO lieAlgebra@ or @TO randomLie@ to assign 
      the given weights to the generators.
      A weight
      is a list of integers of the same length, 
      the first degree (also just called the degree)  is positive and, 
      if the Lie algebra has a 
      differential, the last degree is the homological degree 
      which is non-negative
      and less than the first degree. If the
      Lie algebra has no differential, the program defines a 
      differential to be 0 and adds a last 
      degree 0 to the existing degrees. When the option 
      is given an integer value n, 
      which is not possible when a non-zero differential is given,
      then the program
      defines the weight for each generator to be \{n,0\}. 
///

doc ///
Key
  genSigns
Headline
  optional argument for lieAlgebra and randomLie
SeeAlso
   lieAlgebra
   randomLie
   signLie
   "Second LieAlgebra Tutorial"
   
Description
 Text
      This is an option to tell @TO lieAlgebra@ or @TO randomLie@ to assign 
      the given signs to the generators.
      When the option is given as an integer s, which must be 0 or 1, 
      then the program assigns
      the sign s to each generator.
///

doc ///
Key
  [lieAlgebra, genSigns]
  [randomLie, genSigns]
Headline
  optional argument for lieAlgebra and randomLie
SeeAlso
   lieAlgebra
   randomLie
   signLie
   "Second LieAlgebra Tutorial"
   
Usage
  L = lieAlgebra(liegens,lierels,genSigns => s)
  l = randomLie(d, g, genSigns => s)
Inputs
  s: List
     of signs, 0 (even) or 1 (odd)
  s: ZZ
     a sign which is 0 (even) or 1 (odd), assigned to each generator

Description
 Text
      This is an option to tell @TO lieAlgebra@ or @TO randomLie@ to assign 
      the given signs to the generators.
      When the option is given as an integer s, which must be 0 or 1, 
      then the program assigns
      the sign s to each generator.
///


doc ///
Key
  field
Headline
  optional argument for lieAlgebra, holonomyLie and randomLie
SeeAlso
    lieAlgebra
    holonomyLie
    randomLie
    "Second LieAlgebra Tutorial"
Description
 Text
      This is an option to tell @TO lieAlgebra@, @TO holonomyLie@ or 
      @TO randomLie@ 
      which the underlying field is. You may use any "exact" field (not the real numbers or the
	  complex numbers), such as a prime field or an algebraic extension, e.g.,
      toField(ZZ/7[x]/ideal\{x^2+1\}) or a fraction field, e.g., frac(QQ[x])
///

doc ///
Key
  [lieAlgebra, field]
  [holonomyLie, field]
  [randomLie, field]
Headline
  optional argument for lieAlgebra, holonomyLie and randomLie
Usage
  L = lieAlgebra(liegens,lierels,field => r)
  L = holonomyLie(twoflats,field => r)
  l = randomLie(d,liegens,field => r)
Inputs
  liegens: List
  lierels: List
  twoflats: List
  r: Ring
     the field
  d: ZZ
     the degree
     
Description
 Text
      This is an option to tell @TO lieAlgebra@, @TO holonomyLie@ or
      @TO randomLie@  
      which the underlying field is.
///


doc ///
Key
  genDiffs
Headline
  optional argument for lieAlgebra
SeeAlso
  lieAlgebra
  evalDiffLie 
  "Second LieAlgebra Tutorial"
Description
 Text
       This is an option to tell @TO lieAlgebra@ to assign differential
       values to the generators. These values should be general Lie expressions,
       @TO generalExpressionLie@, of homological degree one less than
       the homological degree of the generator and all other weights equal and
       its sign opposite to the sign of the generator.  

///

doc ///
Key
  [lieAlgebra, genDiffs]
Headline
  optional argument for lieAlgebra
Usage
  L = lieAlgebra(liegens,lierels,genDiffs => dl)
Inputs
  dl: List
     differential values, which are of the form  @TO generalExpressionLie@
    
Description
  Text
      This is an option to tell @TO lieAlgebra@ to 
      assign differential values to the generators.
      These values should be general Lie expressions,
      @TO generalExpressionLie@, of homological degree one less than
      the homological degree of the generator and all other weights equal and
      its sign opposite to the sign of the generator.
      
  Example
      L=lieAlgebra({a,b,c},{[a,b],[a,c]},genWeights=>{{1,0},{1,0},{2,1}},
		   genSigns=>{1,1,1},
		   genDiffs=>{[],[],{{1,1},{[a,a],[b,b]}}})
      peek L

 Text
       Observe that L.compdeg = 3, hence computeLie 3 has been executed for L. 
       This is because the program checks that the differential is well-defined
       and has square zero.
    
///


doc ///
Key
  weightLie
     (weightLie, Symbol)
     (weightLie, ZZ)
     (weightLie, IndexedVariable)
     (weightLie, Array)
     (weightLie, List)
Headline
  gives the multi-degree of a graded element in a Lie algebra 
SeeAlso
 degLie
 genWeights
Usage 
  dl = weightLie(g)
  
Inputs
  g: Symbol
      a generator of L
  g: ZZ
      a generator of L
  g: IndexedVariable
      a generator of L
  g: Array       
      a @TO monomialLie@ in L
  g: List
      a @TO generalExpressionLie@ element or a list of 
      @TO generalExpressionLie@ elements
     
Outputs
  dl: List
     the multi-degree of the element
Description
  Text
    If the Lie algebra has no differential, the program adds
    an extra last homological degree zero to each generator. The weight of [] is defined
    to be a list of zeroes of the same length as the weight of the generators. However,
    the weight of [] should be thought of as arbitrary since, in the example below, 
    the element [b,b] has weight {2,2,0} and is equal to [] in L. 
    		       		
  Example
    L=lieAlgebra({a,b,c},{},genWeights => {{1,1},{1,1},{2,2}})
    weightLie(a)
    weightLie([a,a,b,a])
    g={{1,-1},{[a,c],[b,c]}}
    weightLie g
    m=indexFormLie g
    degree m
    weightLie{[],[b,b],{{1,2},{[c],[a,b]}}} 
    
  Text
      It is possible to use weightLie also in the case when the generators 
      are indexedVariables or integers. 
      
  Example   
    L2=lieAlgebra({a_2,b,1},{}, genWeights => {1,2,3})
    weightLie(1)     
///   


doc ///
Key
  degLie
     (degLie, Symbol)
     (degLie, ZZ)
     (degLie, IndexedVariable)
     (degLie, Array)
     (degLie, List)
Headline
  the first degree of a graded element in the LieAlgebra 
SeeAlso
  weightLie
  genWeights  
Usage 
  dl = degLie(g)
  
  
Inputs
  g: Symbol
      a generator of the Lie algebra
  g: ZZ
      a generator of the Lie algebra
  g: IndexedVariable
      a generator of the Lie algebra
  g: Array       
      a @TO monomialLie@
  g: List
      a  @TO generalExpressionLie@ element or a list of @TO generalExpressionLie@ elements
Outputs
  dl: ZZ
     the first degree of the element or a list of first degrees
Description
  Text
    The degLie of [] is defined
    to be 0. However,
    the degree of [] should be thought of as arbitrary since, in the example below, 
    the element [b,b] has degLie 2 and is equal to [] in L.
     
  Example
    L=lieAlgebra({a,b,c},{},genWeights => {{1,1},{1,1},{2,2}})
    degLie(a)
    degLie([a,a,c])
    g={{1,-1},{[a,c],[b,c]}}
    degLie g
    m=indexFormLie g
    (degree m)_0
    degLie {[],[b,b],{{1,2},{[c],[a,b]}}}
   
    
///  


doc ///
Key
  dimsLie
     (dimsLie, ZZ) 
Headline
  the dimensions of a Lie algebra
SeeAlso
  (dimtotLie)
  (dimLie)
  dimTableLie
  computeLie
Usage
  l = dimsLie(d)
Inputs
  d: ZZ
Outputs
  l: List
     the list of dimensions of the Lie algebra in degree i, 
     where i ranges from 1 to d.
Description
  Example
    L=lieAlgebra({a,b,c},{[a,a,b]})
    dimsLie 3
///



doc ///
Key
  dimLie
     (dimLie, ZZ)
     (dimLie, ZZ, ZZ)
     (dimLie, List) 
Headline
  the dimension of a Lie algebra
SeeAlso
  (dimtotLie)
  (dimsLie)
  dimTableLie
Usage
  a = dimLie(d)
  a = dimLie(d1, d2)
  a = dimLie(dl)
Inputs
  d: ZZ
      the degree
  d1: ZZ
      the first degree
  d2: ZZ
      the last degree
  dl: List
      the multidegree
Outputs
  a: ZZ
     the dimension in the specified degree.
Description
  Example
    L=lieAlgebra({a,b,c},{[c,c,b]},genWeights=>{{2,1},{1,0},{1,0}},
	genDiffs=>{[b,c],[],[]},genSigns=>{1,0,0})
    dimTableLie 4
    dimLie 4
    dimLie(4,0)
    dimLie(4,1)
    dimLie({4,2})
    
///


doc ///
Key
  dimtotLie
     (dimtotLie, ZZ)
Headline
  the total dimension up to degree d
SeeAlso
  (dimLie)
  (dimsLie)
  dimTableLie
Usage
  dl = dimtotLie(d)
Inputs
  d: ZZ
Outputs
  dl: ZZ
      the total dimension up to degree d
Description
  Example
    L=lieAlgebra({a,b,c,r3,r4,r42},{{{1,-1},{[b,c],[a,c]}},[a,b],
	    {{1,-1},{[b,r4],[a,r4]}}},
	genWeights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
	genDiffs=>{[],[],[],{{-1},{[a,c]}},
	    [a,a,c],{{1,1},{[r4],[a,r3]}}},genSigns=>{0,0,0,1,1,0})
    dimTableLie 6
    dimtotLie 6
///



doc ///
Key
  basisLie
     (basisLie, ZZ)
     (basisLie, ZZ, ZZ)
     (basisLie, List) 
Headline
   a basis of Lie monomials in a given (multi-)degree
SeeAlso
  (dimLie)
  (basisExtLie)
  indexFormLie
  basicMonomialLie
Usage
  b = basisLie(d)
  b = basisLie(d1, d2)
  b = basisLie(dl)
Inputs
  d:  ZZ
      the degree
  d1: ZZ
      the first degree
  d2: ZZ
      the last degree = the homological degree
  dl: List
      the multidegree
Outputs
  b:  List
     the basis, a list of Lie monomials 
Description
  Example
    L=lieAlgebra({x,y},{},genSigns=>{1,0},genWeights=>{{2,0},{2,1}},
	genDiffs=>{[],[x]},field=>ZZ/3)
    dimTableLie 4
    basisLie 4
    basisLie{4,1}
    basisLie(4,0)
///


doc ///

Key
  basisExtLie
     (basisExtLie, ZZ)
     (basisExtLie, ZZ, ZZ)
     (basisExtLie, List) 
Headline
  a basis in a given degree of the Ext-algebra
SeeAlso
  signExtLie
  extAlgRing
  extAlgLie
Usage
  b = basisExtLie d
  b = basisExtLie(d1, d2)
  b = basisExtLie(dl)
Inputs
  d: ZZ
      the degree
  d1: ZZ
      the first degree
  d2: ZZ
      the last degree = the homological degree
  dl: List
      the multidegree
Outputs
  b: List
     the list of basis elements
Description
   Text
     This gives a basis in the specified degree of Ext_{UL}(k,k) where k=L.field.
     
   Example
     L=lieAlgebra({a,b},{[a,a,b],[b,b,a]})
     basisExtLie(3,2)
     extAlgLie 3
     L.cache.extAlgRing
   
///


doc ///
Key
  computeLie
     (computeLie,ZZ)
Headline
  computes everything that is needed for a Lie algebra up to a given degree
Usage
  l = computeLie(d)
Inputs
  d: ZZ
      the degree
Outputs
  l: List
     the list of dimensions of the graded pieces of 
     the Lie Algebra in degree i, 
     where i ranges from 1 to d.
SeeAlso
  useLie
  dimsLie
  compdeg
Description
  Text
    Most functions need the execution of computeLie, 
    but this is done already in each program.
    
  Example
    L=lieAlgebra({a,b,c},{[a,a,b],[a,c]}, genWeights=>{1,1,2})
    computeLie 3
    basisLie 3
    L.compdeg
    basisLie 4
    L.compdeg

///


doc ///
Key
  multListLie
     (multListLie, List, List)
Headline
  Lie multiplication of lists of general Lie expressions
Usage
  l=multLie(l1,l2)
  l=multLie(l1,l2,multOnly=>f)
SeeAlso
  generalExpressionLie
  multLie
Inputs
  l1: List
      of @TO generalExpressionLie@ elements
  l2: List
      of @TO generalExpressionLie@ elements
  f: Function
     a boolean function on l1 x l2     
Outputs
  l: List

Description
  Example
    L = lieAlgebra( {a,b}, {[a,a,a,b]},genWeights => {{1,1},{1,2}},
	genSigns=>{1,0})
    b2 = basisLie 2
    b3 = basisLie 3
    multListLie(b2,b3)
    indexFormLie oo
   
   
 Text
      There is an option multOnly which
      only multiplies those pairs
      (x,y) for which multOnly(x,y) is true.
      
 Example
   apply(b2,weightLie)
   apply(b3,weightLie)
   multListLie(b2,b3,multOnly=>(x,y)->
       (weightLie x)_1 === 3 and (weightLie y)_1 === 5) 
   indexFormLie oo
   
///  
doc ///
Key
  multLie
     (multLie,Thing,Thing)
     
Headline
  Lie multiplication of two general Lie expression elements 
Usage
  l=multLie(l1,l2)

 
SeeAlso
  generalExpressionLie
  multListLie
  indexFormLie

Inputs
  l1: Thing
        a @TO generalExpressionLie@ element 
  l2: Thing
       a @TO generalExpressionLie@ element 
 
Outputs
  l: Thing
      a @TO generalExpressionLie@ element
  
Description
  Example
    L = lieAlgebra( {a,b}, {[a,a,a,b]},genWeights => {{1,1},{1,2}},
	genSigns=>{1,0})
    b2 = basisLie 2
    b3 = basisLie 3
    b4 = basisLie 4
    m1=multLie([a],b4_1)
    m2=multLie([b],m1)
    m3=multLie(m1,m2)
    S=L.cache.mbRing
    i1=indexFormLie m1
    i2=indexFormLie m2
    i3=indexFormLie m3    
  
    
///    

  


doc ///
Key
  multOnly
Headline
  optional argument for multListLie
SeeAlso
  multListLie
Description
 Text
      This is an option to tell @TO multListLie@ to only 
      consider certain pairs (x,y)
      when performing outer multiplication wrt two lists. 
///

doc ///
Key
  [multListLie, multOnly]
Headline
  optional argument for  multListLie
Usage
  l = multListLie(l1,l2,multOnly=>f)
Description
 Text
      This is an option to tell @TO multListLie@ to only 
      consider certain pairs (x,y)
      when performing outer multiplication wrt two lists; 
      here f is a boolean function on l1 x l2 
///


doc ///
Key
  defLie
     (defLie, RingElement)
     (defLie, List)
Headline
  returns a general Lie expression corresponding to input
Usage
  a=defLie(r)  
  l=defLie(r)
  ll=defLie(rl)
SeeAlso
  indexFormLie
  mbRing

Inputs
  r: RingElement
     element in mbRing
  rl: List
     of elements in mbRing
Outputs
  a: Array
     if r is a variable, then the @TO monomialLie@ corresponding to r is returned
  l: List
     if r is not a variable, but a linear combination of variables, then a 
     @TO generalExpressionLie@ is returned
  ll: List
     the list of all values of defLie on the elements in rl is returned   
Description
 Example
    L = lieAlgebra( {a,b}, {})
    b3 = basisLie 3
    L.cache.mbRing
    c3 = indexFormLie b3
    defLie oo
    defLie mb_{3,0}
    defLie (mb_{3,0}+2*mb_{3,1}) 
    indexFormLie oo  
    
///



doc ///
Key
  indexFormLie
     (indexFormLie, ZZ)
     (indexFormLie, IndexedVariable)
     (indexFormLie, Symbol)
     (indexFormLie, Array)
     (indexFormLie, List)
Headline
  returns an element in the ring representation corresponding to the input
Usage
  r=indexFormLie(a)
Inputs
  a: ZZ
      a generator
  a: IndexedVariable
      a generator
  a: Symbol
      a generator
  a: Array
     a @TO monomialLie@  
  a: List
     a @TO generalExpressionLie@ or a list of such elements         
Outputs
  r: RingElement
     the element in L.cache.mbRing corresponding to the input.  
SeeAlso
   defLie
   basicMonomialLie
   basicExpressionLie
   normalFormLie 
Description
 Text
     The output of indexFormLie uses basis elements for the Lie algebra which are indexed
     variables with two indices, the first degree and a number, see @TO mbRing@ and
     @TO "How to write Lie elements"@.
     
 Example
    L = lieAlgebra( {a,b}, {})
    b3=basisLie 3
    indexFormLie b3_0 
    defLie oo
    indexFormLie([b,a,b])
    defLie oo
    indexFormLie{oo,[a],[a,b]}
///




doc ///
Key
  signLie
     (signLie, Symbol)
     (signLie, IndexedVariable)
     (signLie, ZZ)
     (signLie, Array)
     (signLie, List)
     (signLie, RingElement)
     
Headline
  returns the sign of a graded Lie element.
Usage
  s=signLie(s)  
  s=signLie(v)
  s=signLie(i)
  s=signLie(a)
  s=signLie(gl)
  s=signLie(r)
Inputs
  s: Symbol
     a generator of the Lie algebra
  v: IndexedVariable
     a generator of the Lie algebra
  i: ZZ
     a generator of the Lie algebra
  a: Array
     a @TO monomialLie@ in the Lie algebra
  gl: List
     a @TO generalExpressionLie@ or a list of @TO generalExpressionLie@  elements
  r: RingElement
     an element in mbRing
Outputs
  s: ZZ
     the sign of the input element   
Description
 Text
    The signs of the generators, see @TO genSigns@ define the sign of an 
    arbitrary expression. The sign of [] is defined
    to be 0. However,
    the sign of [] should be thought of as arbitrary since, in the example below, 
    the element [a_1,a_1,a_1] has sign 1 and is still equal to [] in L. 
    
 Example
    L = lieAlgebra( {a_1,a_2}, {}, genSigns => {1,0})
    prod=multLie([a_1,a_1],[a_2])
    signLie oo
    signLie{[a_2],[a_1,a_1,a_1],{{1,2},{[a_1,a_2,a_2],[a_2,a_2,a_1]}}}
    indexFormLie prod
    signLie oo
    
///



doc ///

Key
  signExtLie
     (signExtLie, RingElement)
Headline
  returns the sign of a generator in the Ext-algebra
SeeAlso
  extAlgLie
  extAlgRing
Usage
  s=signExtLie(r)  
Inputs
  r: RingElement
     a generator in @TO extAlgRing@        
Outputs
  s: ZZ
     the sign of the input element 
Description
   Text
     To get the multi-degree of the element, use @TO degree@.
   Example
      L=lieAlgebra({a,b},{[a,a,b],[b,b,a]},genWeights=>{{1,1},{1,1}})
      extAlgLie 3
      L.cache.extAlgRing
      signExtLie(ext_2)
      degree ext_2
///




doc ///

Key
  idealBasisLie
     (idealBasisLie, ZZ, List)
     (idealBasisLie, List, List)    
Headline
  computes a basis of a Lie ideal in a given degree or multidegree
SeeAlso
  idealLie
Usage
  l=idealBasisLie(d,genlist)  
  l=idealBasisLie(md,genlist)
Inputs
  d: ZZ
     the degree 

  md: List 
     the multidegree 

  genlist: List 
     the set of generators of the ideal as @TO generalExpressionLie@ elements, which may
     be of different degrees     
Outputs
  l: List
     a list of basis elements in the given (multi-)degree 
Description
  Text
   A basis is given in the specified degree or multidegree. 
   Observe that if the Lie algebra has no differential, then 
    an extra homological degree=0 is added to the given weights of the generators. 
    
 Example
   L = lieAlgebra({a,b,c},{[c,a]},genSigns=>{1,0,1},genWeights=>{{1,0},{1,0},{1,2}})
   computeLie 4
   d=defLie (mb_{4,5}+2*mb_{4,6})
   i=idealBasisLie(5,{[a,a],d})       
   length oo
   idealLie(5,{[a,a],d})
   weightLie i
   idealBasisLie({5,4,0},{[a,a],d}) 
   indexFormLie oo
   indexFormLie i 
   
   
///

doc ///
Key
  idealLie
     (idealLie, ZZ, List)
     (idealLie, List, List)
Headline
  computes the dimensions of a Lie ideal
SeeAlso
   subalgLie
   subalgBasisLie
   idealBasisLie
Usage
  l=idealLie(d,genlist)  
  n=idealLie(md,genlist)
Inputs
  d: ZZ
     the maximal degree 

  genlist: List 
     the set of generators of the ideal as @TO generalExpressionLie@ elements, which may
     be of different degrees     
  md: List
     the multidegree  
Outputs
  l: List
     the dimension of the ideal from degree 1 up to degree d.
  n: ZZ
    the dimension of the ideal in the specified multidegree.  
Description
  Text
    When the first argument is an integer, 
    then the dimensions up to that degree
    are given. When the first argument is a list, 
    then the dimension in that specific
    multidegree is given. Observe that if the Lie algebra 
    has no differential, then 
    an extra homological degree=0 is added to the given weights of the generators. 
    
 Example
   L = lieAlgebra({a,b,c},{[c,a]},genSigns=>{1,0,1},genWeights=>{{1,0},{1,0},{1,2}})
   computeLie 5
   d=defLie(mb_{4,5}+2*mb_{4,6})
   idealLie(5,{[a,a],d}) 
   idealLie({5,4,0},{[a,a],d}) 
   
 Text
   Below is shown a way to construct the quotient Lie algebra Q=L/I, where I
   is the ideal generated by [a,a] and d defined above. 
   
 Example
   Q=lieAlgebra({a,b,c},{[c,a],[a,a],d},genSigns=>{1,0,1},genWeights=>{{1,0},{1,0},{1,2}})
   computeLie 5
   
    
///



doc ///
Key
  subalgBasisLie
     (subalgBasisLie, ZZ, List)
     (subalgBasisLie, List, List)    
Headline
  computes a basis of a Lie subalgebra in a given degree or multidegree
SeeAlso
   subalgLie
  (idealLie)
  (idealBasisLie)
Usage
  l=subalgLie(d,genlist)  
  l=subalgLie(md,genlist)
Inputs
  d: ZZ
     the degree 
  md: List 
     the multidegree 
  genlist: List 
     the set of generators of the subalgebra as @TO generalExpressionLie@ elements, which may
     be of different degrees     
Outputs
  l: List
     a list of basis elements in the given (multi-)degree 
Description
  Text
     A basis is given in the specified degree or multidegree. 
     Observe that if the Lie algebra has no differential, then 
     an extra homological degree=0 is added to the given weights of the generators. 
     The function may be used to get a basis for the span of
     a given set of elements of the same degree, by choosing
     the degree in input as the degree of the elements. 
    
 Example
   L = lieAlgebra({a,b,c},{},genSigns=>{1,0,1},genWeights=>{{1,0},{1,2},{1,0}})
   subalgBasisLie(4,{[a],[b,c]})
   indexFormLie oo 
   subalgBasisLie({4,4,0},{[a],[b,c]}) 
   indexFormLie oo 
   subalgBasisLie(3,{[a,b,c],[a,c,b],[b,a,c],[b,c,a],[c,b,a],[c,a,b]})    
///


doc ///
Key
  subalgLie
     (subalgLie, ZZ, List)
     (subalgLie, List, List)
Headline
  computes the dimensions of a Lie subalgebra up to a specified degree
SeeAlso
  (subalgBasisLie)
  (idealLie)
  (idealBasisLie)
Usage
  l=subalgLie(d,genlist)  
  n=subalgLie(md,genlist)
Inputs
   d: ZZ
     the degree 
   md: List 
     the multidegree
   genlist: List 
     the set of generators of the subalgebra 
     as @TO generalExpressionLie@ elements, which may
     be of different degrees       
Outputs
  l: List
     the dimensions of the subalgebra up to the given degree 
  n: ZZ
     the dimension in the specified multidegree
Description
  Text
    When the first argument is an integer, 
    then the dimensions up to that degree
    are given. When the first argument is a list, 
    then the dimension in that specific
    multidegree is given. Observe that if the Lie algebra has no differential, 
    then 
    an extra homological degree=0 is added to the given weights of the generators. 
    
 Example
   L = lieAlgebra({a,b,c},{},genSigns=>{1,0,1},genWeights=>{{1,0},{1,2},{1,0}})
   subalgLie(4,{[a],[b,c]})
   subalgLie({4,4,0},{[a],[b,c]}) 
   
///



doc ///

Key
  divisorLie
     (divisorLie, ZZ,ZZ,List, List)    
Headline
  computes a basis for the divisor subspace
Usage
  b=divisorLie(t,s,m,p)  
Inputs
  t: ZZ
  s: ZZ
  m: List 
     of elements of degree t
  p: List 
     of elements of degree s
Outputs
  b: List
     a basis for the set X of elements x of degree t-s such that multLie(x,y) 
     is in the span of m for all y in p
SeeAlso
  annLie
  centreLie
Description
 Example
   L = lieAlgebra({a,b,c},{})
   computeLie 3
   basisLie 2
   basisLie 3
   g=defLie(mb_{3,0}+mb_{3,1})
   divisorLie(3,2,{g,[c,a,b], [a,a,c]},{[a,b]})
   indexFormLie oo
///



doc ///

Key
  annLie
     (annLie, ZZ,ZZ, List)    
Headline
  computes a basis for the annihilator in a given degree
Usage
  l=annLie(s,d,p)  
Inputs
  s: ZZ
     the degree of the elements in p 
  d: ZZ 
     the degree
  p: List 
     of elements of degree s that we want to annihilate
Outputs
  l: List
     a basis for the set X of elements x of degree d such that 
     multLie(x,y)=0 for all y in p.
SeeAlso
  divisorLie
Description
 Example
    L = lieAlgebra({a,b,c},{[a,a,c],[b,a,c]})
    basisLie 3
    annLie(2,3,{[a,c]})
    
   
///


doc ///
Key
  intersectionLie
     (intersectionLie, ZZ, List)    
Headline
  computes a basis for the intersection of subspaces of a given degree
Usage
  l=intersectionLie(d,m)  
Inputs
  d: ZZ 
     the degree
  m: List 
     of lists of @TO generalExpressionLie@ elements of degree d  
Outputs
  l: List
     a basis for the intersection of the subspaces generated by the lists in m
Description
 Example
  L = lieAlgebra({a,b,c},{})
  computeLie 4
  basisLie 4
  d=defLie(mb_{4,2}+mb_{4,1})
  intersectionLie(4,{{[b, a, b, a], [c, a, b, a]},{[c,b,b,a],d}}) 
  indexFormLie oo
///



doc ///

Key
  centreLie
     (centreLie, ZZ)
     (centreLie, LieAlgebra)  
Headline
  computes the central elements
Usage
  c=centreLie(d)  
  cAll=centreLie(L)  
Inputs
  d: ZZ 
     the degree
  L: LieAlgebra
Outputs
  c: List
     the central elements of degree d
  cAll: List
     the central elements of degree less than or equal to 
     L.compdeg - (maximal degree of generators) 
SeeAlso
  annLie
  divisorLie   
Description
 Example
  L = lieAlgebra({a,b,c},{[a,b],[a,c], [c,b,c],[b,b,c,b]})
  centreLie 1
  centreLie 2
  centreLie L
  L.compdeg
  computeLie 4
  centreLie L 
///



doc ///
Key
  randomLie
     (randomLie, ZZ, List)  
     (randomLie, List)  
Headline
  gives a random element of a lie algebra
Usage
  ri=randomLie(d,g)  
  ra=randomLie(md)  
Inputs
  d: ZZ 
     the degree  
  g: List 
     the generators of the lie algebra 
  md: List
     the multidegree    
Outputs
  ri: List
     a random @TO generalExpressionLie@ element of the specified degree 
     in the free Lie algebra on generators g with optional values of weights,
     signs and field
  ra: List
     a random  @TO generalExpressionLie@ element of the 
     specified (multi-)degree in the current Lie algebra
Description
 Text
  Below is an example of a periodic Lie algebra (a periodization of sl_3) with
  random quadratic relations.
 Example
  rels=apply(7,i->randomLie(2,{a,b,c,d,e}, field => ZZ/7))
  L = lieAlgebra({a,b,c,d,e}, rels, field=>ZZ/7)
  computeLie 8
  randomLie({4,0})  
///

doc ///
Key
  monomialLie
    
Headline
  checks if an array is a correct iterated Lie product
Usage
  b=monomialLie(a)  
  
Inputs
  a: Thing
       
Outputs
  b: Boolean
     
SeeAlso
  generalExpressionLie
  basicExpressionLie
  basicMonomialLie
  normalFormLie
  defLie
  indexFormLie 
  toMonomialLie  
Description
  Text
    An expression is a "Lie monomial" if it is an array of generators from the
    Lie algebra. Its interpretation is an iterated Lie product of the generators
    in the array starting from the right, i.e. [a,b,c]=[a,[b,c]]. The function
    @TO toMonomialLie@ may be used to transform general Lie products to 
    linear combinations of @TO monomialLie@, see  @TO "How to write Lie elements"@.
    
 Example
  L = lieAlgebra({a,b,c},{},genSigns=>{1,1,0})
  monomialLie[a,a,b]
  toMonomialLie[a,[a,b]]
  
///

doc ///

Key
  generalExpressionLie
     
Headline
  checks if an expression is of right input form for e.g. relations
Usage
  b=generalExpressionLie(a)  
 
Inputs
  a: Thing
       
 
       
Outputs
  b: Boolean
     
SeeAlso
  basicExpressionLie
  monomialLie
  basicMonomialLie
  normalFormLie
  defLie
  indexFormLie
Description
 Text
  An expression is a general Lie expression if it is either a @TO monomialLie@ or
  of the form 
  \{\{coefs\},\{liemons\}\}, where coefs belong to L.field and liemons
  are @TO monomialLie@, not [], of the same weight and sign. As coefficients one
  may always use 
  an integer or a quotient of integers with non-zero denominator in the field. See
  @TO "How to write Lie elements"@. 
   
 Example
  L = lieAlgebra({a,b,c},{},genSigns=>{1,1,0},field=>frac(ZZ/5[x]))
  generalExpressionLie{{1,2},{[a,b],[a,c]}}
  generalExpressionLie{{1,2},{[a,b],[a,b,c]}}
  generalExpressionLie{{1/3,2*x-1},{[a,a,b],[a,c,c]}}
  
///

doc ///

Key
  basicExpressionLie
      
Headline
  checks if a general Lie expression is of normal form
Usage
  b=basicExpressionLie(a)  
  
Inputs
  a: Thing
       
Outputs
  b: Boolean
     
SeeAlso
  generalExpressionLie
  monomialLie
  basicMonomialLie
  normalFormLie
  defLie
  indexFormLie   
Description
 Text
  An expression is a "basic Lie expression" if it is @TO generalExpressionLie@
  and is equal to its @TO normalFormLie@, which means that either it is [] or
  a @TO basicMonomialLie@ or it is of the form 
  \{\{coefs\},\{liemons\}\},  where coefs
  are non-zero elements in L.field (and not just a single 1) and 
  liemons are @TO basicMonomialLie@,
  which means that they are basis vectors for the Lie algebra
  chosen by the program, see @TO "How to write Lie elements"@. 
  
 Example
  L = lieAlgebra({a,b,c},{},genSigns=>{1,1,0})
  basicExpressionLie{{1,2},{[a,a,b],[a,c,c]}}
  normalFormLie{{1,2},{[a,a,b],[a,c,c]}}
  basicExpressionLie oo
  basicExpressionLie []
  basicExpressionLie {{1},{[b,a,a]}}
  normalFormLie {{1},{[b,a,a]}} 
  
///



doc ///
Key
  basicMonomialLie
     
Headline
  checks if an array is a basis element for the Lie algebra
Usage
  b=basicMonomialLie(a)  
  
Inputs
  a: Thing
       
Outputs
  b: Boolean
     
SeeAlso
  generalExpressionLie
  basicExpressionLie
  monomialLie
  normalFormLie
  defLie
  indexFormLie 

Description
 Text
  An expression is a "basic Lie monomial" if it is a @TO monomialLie@ which 
  is a basis element for the Lie algebra chosen by the program, see
   @TO "How to write Lie elements"@.
  
 Example
  L = lieAlgebra({a,b,c},{},genSigns=>{1,1,0})
  basicMonomialLie[a,a,b]
  basicMonomialLie[b,a,a]
  
  
///

doc ///
Key
  normalFormLie
     
Headline
  returns a basic Lie expression for the Lie algebra equal to the input
Usage
  b=normalFormLie(a)
      
Inputs
  a: Thing
    
Outputs
  b: List
  b: Array 
    
SeeAlso
  generalExpressionLie
  basicExpressionLie
  monomialLie
  basicMonomialLie
  defLie
  indexFormLie 

Description
 Text
  An input of the form @TO generalExpressionLie@ is transformed to a 
  @TO basicExpressionLie@ which 
  is equal to the input in the Lie algebra, see  @TO "How to write Lie elements"@.
  
 Example
  L = lieAlgebra({a,b,c},{},genSigns=>{1,1,0})
  normalFormLie[a,a,b]
  normalFormLie{{0,1},{[b,a,a],[b,c,c]}}
   
///
doc ///
Key
  axiomsLie
     
Headline
  the axioms for Lie algebras 
Description
 Text
   The axioms depend on the sign of the generators, which are specified by @TO genSigns@. 
   The sign of an element can be obtained by the function @TO signLie@, in the axioms
   below the sign of an element a is written sign(a).
   
   Anticommutativity:
      [a,b] =  -(-1)^{sign(a) * sign(b)} [b,a]
    
   Jacobi identity:
      [a,[b,c]] = [[a,b],c] + (-1)^{sign(a) * sign(b)} [b,[a,c]]
      
   Also, in characteristic 2 and 3, there are in addition the following axioms:
    
   Characteristic 2:
      [a,a] = 0
      
   Characteristic 3:
      [a,[a,a]] = 0  
    
   
///




doc ///
Key
  maxDeg
Headline
  determines the number of variables in the internal ring of representation, lieRing
Usage
  L.cache.maxDeg
Description
 Text
   The key @TO maxDeg@ is by default 5.
   If computeLie n is executed 
   for n>L.cache.maxDeg, then the program changes the key to n+5. 
   The value of maxDeg defines the internal representation of 
   Lie elements in the polynomial ring "L.cache.lieRing", which cannot be used
   by the user but can be looked upon
   by writing "L.cache.lieRing". The Lie monomials are represented as
   commutative monomials in this ring.
    
 Example
   L=lieAlgebra({a,b},{[a,a,a,b],[b,b,b,a]})
   computeLie 4
   peek L.cache
   L.cache.lieRing		      
   computeLie 6
   L.cache.maxDeg
   L.cache.lieRing
///
end

