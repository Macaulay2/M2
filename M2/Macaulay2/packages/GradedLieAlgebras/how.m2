doc ///
	Key
		"How to write Lie elements"
	Headline 
		An overview of the different representations of elements of a graded Lie Algebra
	SeeAlso
	         "First LieAlgebra Tutorial"
		 "Second LieAlgebra Tutorial"
		 "Differential Lie algebras Tutorial"
		 "Constructing Lie algebras"
		 "Symmetries"
		 
		 
	Description
	     Text
	       An array of generators, [a,b,c], is interpreted as [a,[b,c]], i.e., 
	       as an iterated Lie product of the generators in the array starting
	       from the right. The empty array, [], is the zero element in the Lie
	       algebra and just one generator, a, is written [a]. A generator may be 
	       of class Symbol,	IndexedVariable or Integer.
	       It follows from the axioms, see @TO axiomsLie@ that
	       any Lie element is a linear combination of such iterated products, see
	       @TO monomialLie@. When executing computeLie n,
	       the program chooses some of the Lie monomials
	       as a basis for the Lie algebra up to (first) degree n, 
	       see @TO basicMonomialLie@.
	       
	     Example
	       L=lieAlgebra({a,b},{[a,a,a,b],[b,b,b,a]})
	       computeLie 6
	       basisLie 5
	       monomialLie[a,b,a,a,b]
	       basicMonomialLie[a,b,a,a,b]
	       
	     Text
	       A general Lie expression, see @TO generalExpressionLie@, is either a
	       @TO monomialLie@ or of 
	       the form \{\{coefs\},\{liemons\}\}, where coefs belong to L.field, 
	       specified in the option @TO field@, and liemons are Lie
	       monomials, @TO monomialLie@, not [], of the same weight and sign. 
	       These expressions should be used
	       when specifying the relations in @TO lieAlgebra@ and optionally
	       also the values
	       of the generators for a differential, see @TO genDiffs@. The output
	       of many functions in the package is a (list of) basic Lie expressions,
	       @TO basicExpressionLie@, 
	       which are general Lie expressions containing only non-zero coefficients and  
	       basic Lie monomials, @TO basicMonomialLie@. 
	       
	     Example
	       L1=lieAlgebra({a,b,c},{{{1,-1},{[a,b],[a,c]}}})	     
	       multLie([a,b],[b,c])
	       basicExpressionLie oo
	       L2=lieAlgebra({a,b,c},{[a,b],[a,c]},genWeights=>{{1,0},{1,0},{2,1}},
		   genSigns=>{1,1,1},
		   genDiffs=>{[],[],{{1,1},{[a,a],[b,b]}}})
	       peek L2
	       
	     Text
	       Any general Lie expression is equal in the Lie algebra to a basic Lie expression,
	       which is unique up to the order of the basic Lie monomials. 
	       One such expression can be obtained 
	       using @TO normalFormLie@. Two general
	       Lie expressions are equal in the Lie algebra 
	       iff their normal forms are equal. 
	       
	     Example
	       L=lieAlgebra({a,b},{})
	       basisLie 6
	       normalFormLie[a,a,a,b,a,b]
	       
	     Text
	       The notation with separate lists for coefficients
	       and Lie monomials might feel unintuitive. Also, in high degrees
	       the Lie monomials will be long and hard to identify. For that reason we 
	       have another representation of Lie elements, namely as linear polynomials
	       in a ring with variables constituting a basis for the Lie algebra. This ring
	       can be accessed as L.cache.mbRing, see @TO mbRing@. 
	       It is a good idea to give a 
	       name for L.cache.mbRing, since this ring 
	       is rather big and if a name is given only this name is displayed. 
	       The variable in L.cache.mbRing corresponding to the
	       i:th basis element of degree d in L is denoted  mb_{\{d,i\}} (beginning with i=0).
	       
	     Example
	       L.cache.mbRing  
	       S=oo		 
	       mb_{2,0}
	       
	     Text
	       Use @TO indexFormLie@ to translate a general Lie expression or a generator
	       or a list of such elements to the ring L.cache.mbRing
	       and use @TO defLie@ in the other direction, i.e., 
	       to translate a (list of) linear 
	       polynomial(s) in L.cache.mbRing to a (list of) @TO basicExpressionLie@.
	       There is no direct way to build linear combinations of general Lie
	       expressions, but this can be done using the functions  @TO indexFormLie@
	       and @TO defLie@.
	       
	     Example
	       indexFormLie[a,a,a,b,a,b]  
	       de=defLie oo  
	       indexFormLie basisLie 6
	       defLie oo
	       indexFormLie{[a],[b],[a,b],de}
	       defLie(2*indexFormLie{{1,2},{[a,a,b],[b,b,a]}}-
		   3*indexFormLie{{2},{[a,a,b]}})
	       
	     Text
	       When there are several Lie algebras, the variables mb_{\{d,i\}} may 
	       belong to different rings. To get the basis element mb_{\{2,0\}}
	       in S=L.cache.mbRing, one can just write useLie L, see @TO useLie@.
	     
	    
	     Example
	       L1=lieAlgebra({a,b},{})
	       computeLie 3
	       S1=L1.cache.mbRing
	       L2=lieAlgebra({c,d,e},{})
	       computeLie 3
	       S2=L2.cache.mbRing
	       mb_{2,0}
	       defLie oo
	       useLie L1
	       mb_{2,0} 
	       defLie oo
	      
	     
	       
	        	       		   		       	 
///
end
