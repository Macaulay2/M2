--------------------------------------------------------------------------------
-- Copyright 2014  Federico Galetto
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

doc ///
     Key
     	  HighestWeights
     Headline
     	  decompose free resolutions and graded modules with a semisimple Lie group action
     Description
     	  Text
     	       This package provides tools to study the representation
	       theoretic structure of equivariant free resolutions and graded 
	       modules with the action of a semisimple Lie group. The methods
	       of this package allow one to consider the free modules in an
	       equivariant resolution, or the graded components of a module,
	       as representations of a semisimple Lie group by means of their
	       weights and to obtain their decomposition into highest weight
	       representations.
	       
	       This package implements an algorithm introduced in 
	       @HREF("http://dx.doi.org/10.1016/j.jsc.2015.05.004","Galetto - Propagating 
		weights of tori along free resolutions")@.
	       The methods of this package are meant to be used in 
	       characteristic zero.
	       
	       The following links contain some sample computations carried out
	       using this package. 
	       The first and second example are discussed in more detail, so we
	       recommend reading through them first.
	  Code
	      UL {
	      TO "Example 1",
	      TO "Example 2",
	      TO "Example 3",
	      TO "Example 4",
	      TO "Example 5",
	      TO "Example 6",
	      TO "Example 7",
	      }
///

doc ///
     Key
     	  LieWeights
     Headline
     	  stores the (Lie theoretic) weights of the variables of a ring
     Description
     	  Text
	       A key in the hash table of a ring which is used to store the
	       (Lie theoretic) weights of the variables of the ring. Set this
	       using the @TO "setWeights"@ command.
     SeeAlso
     	  setWeights
///

doc ///
     Key
     	  GroupActing
     Headline
     	  stores the Dynkin type of the group acting on a ring
     Description
     	  Text
	       A key in the hash table of a ring which is used to store the
	       Dynkin type of the group acting on the ring. Set this
	       using the @TO "setWeights"@ command.
     SeeAlso
     	  setWeights
///

doc ///
     Key
     	  setWeights
     Headline
     	  attach (Lie theoretic) weights to the variables of a ring
     Description
     	  Text
	       Use this method to assign weights to the variables of a 
	       polynomial ring.
     SeeAlso
     	  getWeights
///

doc ///
     Key
	  (setWeights,PolynomialRing,DynkinType,List)
     Headline
     	  attach (Lie theoretic) weights to the variables of a ring
     Usage
     	  T = setWeights(R,D,L)
     Inputs
     	  R:PolynomialRing
	  D:DynkinType
	  L:List
	       a table of weights given as lists of integers
     Outputs
     	  T:Tally
	       the highest weights decomposition of @TT "L"@
     Consequences
     	  Item
	       Two keys are created in the hashTable of @TT "R"@:
	       @TT "LieWeights"@, with value a matrix whose rows are the weights
	       in the list @TT "L"@;
	       @TT "GroupActing"@, with value the DynkinType @TT "D"@.
     Description
     	  Text
	       Let $G$ be a semisimple Lie group of Dynkin type @TT "D"@ which 
	       acts on @TT "R"@ compatibly with the grading. Let $T\subset G$ 
	       be a maximal torus and assume the variables in @TT "R"@ are 
	       weight vectors for the action of $T$. This function is used to 
	       assign a weight to each  variable of @TT "R"@. Knowing these 
	       weights allows Macaulay2 to return the weight of monomials of 
	       @TT "R"@ upon request.
	       
	       For more information on inputting the Dynkin type of the group 
	       the user should consult the documentation of the 
	       @TT "WeylGroups"@ package.
	       
	       Weights are expressed with respect to the basis of 
	       fundamental weights in the weight lattice associated to the 
	       root system of the given type @TT "D"@. In this package, each 
	       weight $w$ is represented by a list of integers, namely the 
	       coefficients of $w$ in the basis of fundamental weights.
	       
	       In the following example, the polynomial ring @TT "R"@ is the
	       symmetric algebra over $V$, where $V=\mathbb{C}^4$ and is acted
	       upon by the group $SL_4 (\mathbb{C})$.

     	  Example
	       R=QQ[x_1..x_4]
	       D=dynkinType{{"A",3}}
	       W={{1,0,0},{-1,1,0},{0,-1,1},{0,0,-1}}
	       setWeights(R,D,W)
///

doc ///
     Key
     	  getWeights
     Headline
     	  retrieve the (Lie theoretic) weight of a monomial
     Description
     	  Text
	       Suppose an algebraic torus acts on a polynomial ring compatibly 
	       with the grading. If each variable is a weight vector, then each 
	       monomial is also a weight vector. Use this method to obtain the 
	       weight of a monomial.
     SeeAlso
     	  setWeights
///

doc ///
     Key
     	  (getWeights,RingElement)
     Headline
     	  retrieve the (Lie theoretic) weight of a monomial
     Usage
     	  w = getWeights(r)
     Inputs
     	  r:RingElement
     Outputs
     	  w:List
     Description
     	  Text
	       If @TT "r"@ is a monomial, this will return the weight of 
	       @TT "r"@ which is simply the sum of the weights of the variables 
	       in the support of @TT "r"@ each counted with multiplicity 
	       equal to its exponent. If @TT "r"@ is a polynomial, then 
	       the weight of the leading monomial is returned (which may differ 
	       from the weights of the other monomials of @TT "r"@).

	       In the following example, the polynomial ring @TT "R"@ is the
	       symmetric algebra over $V$, where $V=\mathbb{C}^4$ and is acted
	       upon by the group $SL_4 (\mathbb{C})$.

     	  Example
	       R=QQ[x_1..x_4];
	       D=dynkinType{{"A",3}};
	       W={{1,0,0},{-1,1,0},{0,-1,1},{0,0,-1}};
	       setWeights(R,D,W);
	       getWeights(x_1^4*x_2*x_4^6)

     Caveat
     	  The weight of 0 is undefined, so an error is returned if @TT "r"@ is 
	  0.
     SeeAlso
     	  setWeights
///

doc ///
     Key
     	  Forward
     Headline
     	  propagate weights from domain to codomain
     Description
     	  Text
	       Set this argument to @TT "true"@ to ensure weights are propagated
	       forward along a map of graded free modules, i.e., from domain to 
	       the codomain of the map. The default setting is @TT "false"@
	       meaning that weights are propagated backwards, i.e., from codomain
	       to domain.
     SeeAlso
     	  propagateWeights
///

doc ///
     Key
     	  [propagateWeights,Forward]
     Headline
     	  propagate weights from domain to codomain
     Description
     	  Text
	       Set this argument to @TT "true"@ to ensure weights are propagated
	       forward along a map of graded free modules, i.e., from domain to 
	       the codomain of the map. The default setting is @TT "false"@
	       meaning that weights are propagated backwards, i.e., from codomain
	       to domain.
     SeeAlso
     	  propagateWeights
///

doc ///
     Key
     	  MinimalityTest
     Headline
     	  check that the input map is minimal
     Description
     	  Text
	       The method propagateWeights tests whether the input is a minimal
	       map (i.e., a minimal presentation of its cokernel). Set this
	       argument to @TT "false"@ to bypass this test.
     SeeAlso
     	  propagateWeights
///

doc ///
     Key
     	  [propagateWeights,MinimalityTest]
     Headline
     	  check that the input map is minimal
     Description
     	  Text
	       The method propagateWeights tests whether the input is a minimal
	       map (i.e., a minimal presentation of its cokernel). Set this
	       argument to @TT "false"@ to bypass this test.
     SeeAlso
     	  propagateWeights
///

doc ///
     Key
     	  LeadingTermTest
     Headline
     	  check the columns of the input matrix for repeated leading terms
     Description
     	  Text
	       The method propagateWeights tests whether the input map is 
	       represented by a matrix whose columns have all different leading
	       terms. Set this argument to @TT "false"@ to bypass this test.
     SeeAlso
     	  propagateWeights
///

doc ///
     Key
     	  [propagateWeights,LeadingTermTest]
     Headline
     	  check the columns of the input matrix for repeated leading terms
     Description
     	  Text
	       The method propagateWeights tests whether the input map is 
	       represented by a matrix whose columns have all different leading
	       terms. Set this argument to @TT "false"@ to bypass this test.
     SeeAlso
     	  propagateWeights
///

doc ///
     Key
     	  propagateWeights
     Headline
     	  propagate (Lie theoretic) weights along equivariant maps
     Description
     	  Text
	       Let $T$ be a torus which acts on a polynomial ring compatibly
	       with the grading. Assume the variables in the ring are 
	       weight vectors for the action of $T$.
	       
	       Use this method on a $T$-equivariant map of graded free modules,
	       to obtain the weights of the domain from the weights of the
	       codomain (or viceversa).

	       The weights of the variables in the ring must be set a priori,
	       using the method @TO "setWeights"@.
	       
	       This method is called by other methods in this package. The only
	       reason for using this method directly is that it returns a 
	       complete list of weights for the domain (resp. codomain) of an 
	       equivariant map, instead of the highest weights decomposition.

	       This method implements an algorithm introduced in 
	       @HREF("http://dx.doi.org/10.1016/j.jsc.2015.05.004","Galetto - Propagating 
		weights of tori along free resolutions")@.
     SeeAlso
     	  setWeights
///

doc ///
     Key
     	  (propagateWeights,Matrix,List)
     Headline
     	  propagate (Lie theoretic) weights along an equivariant map of graded free modules
     Usage
     	  (V,C) = propagateWeights(M,W)
     Inputs
     	  M:Matrix
	       an equivariant minimal homogeneous map of graded free modules
	  W:List
	       a table of weights for the codomain (resp. domain) of @TT "M"@
	  Forward => Boolean
	       propagate weights to domain (when false) or codomain (when true)
	  MinimalityTest => Boolean
	       check whether @TT "M"@ is a minimal map
	  LeadingTermTest => Boolean
	       check whether the leading terms of the columns of @TT "M"@ are
	       all distinct
     Outputs
     	  V:List
	       a table of weights for the domain (resp. codomain) of @TT "M"@
	  C:Matrix
	       a change of basis so that the columns of @TT "M*inverse(C)"@ have
	       different leading terms
     Description
     	  Text
	       Let $\phi:E\rightarrow F$ be a homogeneous map of graded free 
	       modules. Fix a homogeneous basis $\{e_i\}$ of $E$ and assume 
	       $\phi$ is minimal, in the sense that $\{\phi (e_i)\}$ is a 
	       minimal set of generators of the image of $\phi$.
	       Moreover, suppose $\phi$ is equivariant for the action 
	       of an algebraic torus and fix a homogeneous basis of weight
	       vectors $\{f_j\}$ of $F$.
	       Use this function to recover the weights of the domain $E$ 
	       from the weights of the codomain $F$ (or viceversa, setting the
	       optional input @TO "Forward"@ to @TT "true"@.).
	       
	       The input consists of @TT "M"@, the matrix of $\phi$ with 
	       respect to the bases $\{e_i\}$ and $\{f_j\}$, together with
	       @TT "W"@, a list of weights $\{w_j\}$ such that $w_j$ is the 
	       weight of $f_j$.
	       
	       If the optional argument @TO "LeadingTermTest"@ is @TT "true"@
	       (and it is by default),
	       a new homogeneous basis $\{e_i'\}$ for $E$ is computed.
	       The change of basis from $\{e_i\}$ to $\{e_i'\}$, given by the 
	       matrix @TT "C"@, is chosen so that the columns of the matrix 
	       @TT "M*inverse(C)"@ have all different leading terms.
	       This is a sufficient condition for the algorithm to work.
	       The matrix @TT "C"@ is part of the output.
	       The user can choose to avoid this change of basis by 
	       setting this optional argument to @TT "false"@;
	       then @TT "C"@ is taken to be the identity matrix.
	       
	       The output @TT "V"@ is a list of weights $\{v_i\}$ such that 
	       there is a basis of homogeneous weight vectors $\{e_i''\}$ 
	       of $E$, with $v_i$ the weight of $e_i''$.
	       Notice that the change of basis from $\{e_i\}$ to $\{e_i''\}$ is
	       not returned.
	       
	       This method implements an algorithm introduced in 
	       @HREF("http://dx.doi.org/10.1016/j.jsc.2015.05.004","Galetto - Propagating 
		weights of tori along free resolutions")@.
	       
	       In the following example, the polynomial ring @TT "R"@ is the
	       symmetric algebra over $\mathbb{C}^2 \otimes \mathbb{C}^4$, 
	       with the natural action of $SL_2 (\mathbb{C}) \times SL_4 
	       (\mathbb{C})$.
	       The map $\phi$ is the unique (up to scalars) equivariant map
	       $\mathbb{C}^4 \otimes R(-1) \rightarrow (\mathbb{C}^2)^* \otimes
	        R$.
	       If $\{f_1,f_2\}$ and $\{e_1,e_2,e_3,e_4\}$ are the coordinate
	       bases of $\mathbb{C}^2$ and $\mathbb{C}^4$ respectively, then
	       @TT "M"@ is the matrix of $\phi$ with respect to the bases 
	       $\{f_1^*,f_2^*\}$ and $\{e_1,e_1+e_2,e_3,e_4\}$.

     	  Example
	       R=QQ[x_(1,1)..x_(2,4)];
	       D=dynkinType{{"A",1},{"A",3}};
	       U={{1,1,0,0},{1,-1,1,0},{1,0,-1,1},{1,0,0,-1},{-1,1,0,0},{-1,-1,1,0},{-1,0,-1,1},{-1,0,0,-1}};
	       setWeights(R,D,U);
	       M=map(R^2,R^{4:-1},{{x_(1,1),x_(1,1)+x_(1,2),x_(1,3),x_(1,4)},{x_(2,1),x_(2,2)+x_(2,1),x_(2,3),x_(2,4)}})
	       (V,C)=propagateWeights(M,{{-1,0,0,0},{1,0,0,0}});
	       V
	       C
	       M*inverse(C)

     	  Text
     	       While the first and second columns of @TT "M"@ have the same 
	       leading terms, all the columns of @TT "M*inverse(C)"@
	       have different leading terms.
	       The weights in @TT "V"@ are in order the weights of $e_4$, $e_3$,
	        $e_2$ and $e_1$.
	       
	       A list of weights obtained using this method can be decomposed
	       into highest weights using the function @TO "decomposeWeightsList"@.
     Caveat
     	  This function does not check that @TT "M"@ is an equivariant map.
	  When used on a map that is not equivariant, this function will 
	  produce meaningless results.
     SeeAlso
     	 decomposeWeightsList
///

doc ///
     Key
     	  Range
     Headline
     	  decompose only part of a complex
     Description
     	  Text
	       Restrict the range of a complex to be decomposed into highest
	       weight modules. Set this argument to @TT "{lo,hi}"@ to restrict
	       to homological dimensions from @TT "lo"@ to @TT "hi"@.
     Caveat
          This argument is not used when decomposing rings, ideals or modules
	  but only with complexes.
     SeeAlso
     	  highestWeightsDecomposition
///

doc ///
     Key
     	  [highestWeightsDecomposition,Range]
     Headline
     	  decompose only part of a complex
     Description
     	  Text
	       Restrict the range of a complex to be decomposed into highest
	       weight modules. Set this argument to @TT "{lo,hi}"@ to restrict
	       to homological dimensions from @TT "lo"@ to @TT "hi"@.
     Caveat
          This argument is not used when decomposing rings, ideals or modules
	  but only with complexes.
     SeeAlso
     	  propagateWeights
///

doc ///
     Key
     	  highestWeightsDecomposition
     Headline
     	  irreducible decomposition of a complex, ring, ideal or module
     Description
     	  Text
	       Let $G$ be a semisimple algebraic group which acts on a 
	       polynomial ring $R$ compatibly with the grading. 
	       Let $T\subseteq G$ be a maximal torus and assume the variables 
	       in $R$ are weight vectors for the action of $T$.
	       
	       This method can be used to obtain the decomposition into
	       highest weight representations of various objects over $R$ that
	       carry a compatible $G$-action. These objects can be ideals in $R$,
	       quotients of $R$ by an ideal, $R$-modules or the terms in a 
	       complex of free $R$-modules.

	       The weights of the variables in the ring $R$  must be set a 
	       priori, using the method @TO "setWeights"@.
	       
	       The decomposition of a representation is described by means of a
	       @TT "Tally"@, with keys equal to some highest weights 
	       and values equal to the multiplicity of the corresponding
	       irreducible representations. Where more than one degree (resp. 
	       homological dimension) is decomposed, the result is a hash table
	       with keys equal to the degrees (resp. homological dimensions) and
	       values equal to the corresponding decomposition.

	       All weights are expressed with respect to the basis of 
	       fundamental weights in the associated weight lattice. 
	       Each weight $w$ is represented by a list of integers, namely the 
	       coefficients of $w$ in the basis of fundamental weights.
     SeeAlso
     	  setWeights
///

doc ///
     Key
     	  (highestWeightsDecomposition,ChainComplex,ZZ,List)
	  (highestWeightsDecomposition,ChainComplex)
     Headline
     	  decompose an equivariant complex of graded free modules
     Usage
     	  highestWeightsDecomposition(C,i,W)
     Inputs
     	  C:ChainComplex
	       an equivariant minimal complex of graded free modules
	  i:ZZ
	       the homological dimension of a module of @TT "C"@
	  W:List
	       a table of weights for @TT "C_i"@
	  Range => List
	       decompose only part of @TT "C"@
     Outputs
     	  H:HashTable
	       the decomposition of the modules of @TT "C"@
     Description
     	  Text
	       Let $G$ be a semisimple algebraic group which acts on a 
	       polynomial ring $R$ compatibly with the grading. 
	       Let $T\subseteq G$ be a maximal torus and assume the variables 
	       in $R$ are weight vectors for the action of $T$.
	       
	       Suppose @TT "C"@ is a minimal $G$-equivariant complex of graded 
	       free $R$-modules. Suppose the coordinate basis for the module
	       @TT "C_i"@ is a homogeneous basis of weight vectors and the
	       weight of the $j$-th basis vector is $w_j$.
	       
	       Use this function to obtain the decomposition of the modules in
	       @TT "C"@ into highest weight representations. The input is the 
	       complex @TT "C"@, the index @TT "i"@ and the list of weights
	       $\{w_j\}$.
	       
	       The output is a hash table with keys equal to the indices of
	       the non zero modules of @TT "C"@ (within the selected range).
	       The values are themselves hash tables with keys equal to the
	       degrees of the generators of the corresponding free module.
	       Finally the keys of the nested hash tables are tallies whose
	       keys are the highest weights of certain irreducible 
	       representations and 
	       those representations.
	       
	       In the following example, the polynomial ring @TT "R"@ is the
	       symmetric algebra over $\mathbb{C}^3 \otimes \mathbb{C}^4$, 
	       with the natural action of $G = SL_3 (\mathbb{C}) \times SL_4 
	       (\mathbb{C})$.
	       The complex is the minimal free resolution of the ideal of
	       $2\times 2$ minors of a generic $3\times 4$ matrix.
	       We choose to provide the weights for the module in homological
	       dimension zero, so we input 0 for the second argument.
	       The module in homological dimension zero has rank one and $G$ 
	       acts trivially on it hence it has only one weight, the zero
	       weight.

     	  Example
	       R=QQ[x_(1,1)..x_(3,4)];
	       G=genericMatrix(R,4,3)
	       I=minors(2,G);
	       resI=res I
	       betti resI
	       D=dynkinType{{"A",2},{"A",3}};
	       U={{1,0,1,0,0},{1,0,-1,1,0},{1,0,0,-1,1},{1,0,0,0,-1},{-1,1,1,0,0},{-1,1,-1,1,0},{-1,1,0,-1,1},{-1,1,0,0,-1},{0,-1,1,0,0},{0,-1,-1,1,0},{0,-1,0,-1,1},{0,-1,0,0,-1}};
	       setWeights(R,D,U)
	       highestWeightsDecomposition(resI,0,{{0,0,0,0,0}})
	  
	  Text
	       This shows, for example, that the third module in the complex
	       decomposes as $\mathbb{C}^3\otimes S_{2,1,1} \mathbb{C}^4 \oplus
	       S_{3,1}\mathbb{C}^3$ generated in degree 4. Similarly, the fourth
	       module decomposes as $S_2 \mathbb{C}^3 \otimes \mathbb{C}^4$ in
	       degree 5 and $S_{2,2,2} \mathbb{C}^4$ in degree 6.
	       Here $S_\lambda$ denotes the Schur functor corresponding to the
	       partition $\lambda$.
	       
	       To obtain only a partial decomposition, we may use the optional
	       argument @TO "Range"@.

     	  Example
	       highestWeightsDecomposition(resI,0,{{0,0,0,0,0}},Range=>{0,2})

	  Text
	       When the first free module in the complex has rank one,
	       as when resolving a quotient of @TT "R"@, 
	       then the group $G$ acts trivially on it.
	       Then we may use a simplified version of the command with 
	       the complex as the only argument.

     	  Example
	       highestWeightsDecomposition(resI)

///

doc ///
     Key
     	  (highestWeightsDecomposition,Ideal,List)
	  (highestWeightsDecomposition,Ideal,ZZ)
	  (highestWeightsDecomposition,Ideal,ZZ,ZZ)
     Headline
     	  decompose an ideal with a semisimple Lie group action
     Usage
     	  highestWeightsDecomposition(I,L)
     	  highestWeightsDecomposition(I,deg)
     	  highestWeightsDecomposition(I,lo,hi)
     Inputs
     	  I:Ideal
	       in a polynomial ring
	  L:List
	       a multidegree
     Outputs
     	  T:Tally
	       the decomposition of @TT "I"@ in degree @TT "L"@
     Description
     	  Text
	       Let $G$ be a semisimple algebraic group which acts on a 
	       polynomial ring $R$ compatibly with the grading. 
	       Let $T\subseteq G$ be a maximal torus and assume the variables 
	       in $R$ are weight vectors for the action of $T$.
	       
	       Suppose @TT "I"@ is an ideal which is stable under the action of
	       $G$.
	       
	       Use this function to obtain the decomposition of a graded 
	       component of @TT "I"@.
	       The input is the ideal @TT "I"@ and the (multi)degree of the
	       graded component.
	       
	       The output is a tally whose keys are the highest weights of 
	       certain irreducible representations and whose values are 
	       the multiplicities of those representations.
	       
	       In the following example, the polynomial ring @TT "R"@ is the
	       symmetric algebra over $\mathbb{C}^3 \otimes \mathbb{C}^4$, 
	       with the natural action of $G = SL_3 (\mathbb{C}) \times SL_4 
	       (\mathbb{C})$.
	       The ideal is generated by the $2\times 2$ minors of a generic 
	       $3\times 4$ matrix.

     	  Example
	       R=QQ[x_(1,1)..x_(3,4)];
	       G=genericMatrix(R,4,3)
	       I=minors(2,G);
	       D=dynkinType{{"A",2},{"A",3}};
	       U={{1,0,1,0,0},{1,0,-1,1,0},{1,0,0,-1,1},{1,0,0,0,-1},{-1,1,1,0,0},{-1,1,-1,1,0},{-1,1,0,-1,1},{-1,1,0,0,-1},{0,-1,1,0,0},{0,-1,-1,1,0},{0,-1,0,-1,1},{0,-1,0,0,-1}};
	       setWeights(R,D,U)
	       highestWeightsDecomposition(I,{2})
	  
	  Text
	       This shows that the component of @TT "I"@ of degree 2 is
	       the representation $S_{1,1} \mathbb{C}^3 \otimes 
	       S_{1,1} \mathbb{C}^4$.
	       Here $S_\lambda$ denotes the Schur functor corresponding to the
	       partition $\lambda$.
	       
	       When the polynomial ring is $\mathbb{Z}$-graded the degree can
	       be given as an integer instead of a list.
	       Moreover, in the $\mathbb{Z}$-graded case, one can decompose
	       a range of degrees all at once as illustrated below.

     	  Example
	       highestWeightsDecomposition(I,2)
	       highestWeightsDecomposition(I,0,4)
///

doc ///
     Key
     	  (highestWeightsDecomposition,Ring,List)
	  (highestWeightsDecomposition,Ring,ZZ)
	  (highestWeightsDecomposition,Ring,ZZ,ZZ)
     Headline
     	  decompose a ring with a semisimple Lie group action
     Usage
     	  highestWeightsDecomposition(Q,L)
	  highestWeightsDecomposition(Q,deg)
	  highestWeightsDecomposition(Q,lo,hi)
     Inputs
     	  Q:Ring
	       a (quotient of a) polynomial ring
	  L:List
	       a multidegree
     Outputs
     	  T:Tally
	       the decomposition of @TT "Q"@ in degree @TT "L"@
     Description
     	  Text
	       Let $G$ be a semisimple algebraic group which acts on a 
	       polynomial ring $R$ compatibly with the grading. 
	       Let $T\subseteq G$ be a maximal torus and assume the variables 
	       in $R$ are weight vectors for the action of $T$.
	       
	       Suppose @TT "Q"@ is a quotient of $R$ by an ideal which is 
	       stable under the action of $G$.
	       
	       Use this function to obtain the decomposition of a graded 
	       component of @TT "Q"@.
	       The input is the ring @TT "Q"@ and the (multi)degree of the
	       graded component.
	       
	       The output is a tally whose keys are the highest weights of 
	       certain irreducible representations and whose values are 
	       the multiplicities of those representations.
	       
	       In the following example, the polynomial ring $R$ is the
	       symmetric algebra over $\mathbb{C}^3 \otimes \mathbb{C}^4$, 
	       with the natural action of $G = SL_3 (\mathbb{C}) \times SL_4 
	       (\mathbb{C})$.
	       The ring is the quotient of $R$ by the ideal generated by the 
	       $2\times 2$ minors of a generic $3\times 4$ matrix.

     	  Example
	       R=QQ[x_(1,1)..x_(3,4)];
	       G=genericMatrix(R,4,3)
	       I=minors(2,G);
	       Q=R/I;
	       D=dynkinType{{"A",2},{"A",3}};
	       U={{1,0,1,0,0},{1,0,-1,1,0},{1,0,0,-1,1},{1,0,0,0,-1},{-1,1,1,0,0},{-1,1,-1,1,0},{-1,1,0,-1,1},{-1,1,0,0,-1},{0,-1,1,0,0},{0,-1,-1,1,0},{0,-1,0,-1,1},{0,-1,0,0,-1}};
	       setWeights(R,D,U)
	       highestWeightsDecomposition(Q,{2})
	  
	  Text
	       This shows that the component of @TT "Q"@ of degree 2 is
	       the representation $S_2 \mathbb{C}^3 \otimes 
	       S_2 \mathbb{C}^4$.
	       Here $S_\lambda$ denotes the Schur functor corresponding to the
	       partition $\lambda$.
	       
	       When the ring @TT "Q"@ is $\mathbb{Z}$-graded the degree can
	       be given as an integer instead of a list.
	       Moreover, in the $\mathbb{Z}$-graded case, one can decompose
	       a range of degrees all at once as illustrated below.

     	  Example
	       highestWeightsDecomposition(Q,2)
	       highestWeightsDecomposition(Q,0,4)
///

doc ///
     Key
     	  (highestWeightsDecomposition,Module,List,List)
	  (highestWeightsDecomposition,Module,ZZ,List)
	  (highestWeightsDecomposition,Module,ZZ,ZZ,List)
     Headline
     	  decompose a module with a semisimple Lie group action
     Usage
     	  highestWeightsDecomposition(M,L,W)
	  highestWeightsDecomposition(M,deg,W)
	  highestWeightsDecomposition(M,lo,hi,W)
     Inputs
     	  M:Module
	       over a polynomial ring
	  L:List
	       a multidegree
	  W:List
	       a table of weights for the generators of @TT "M"@
     Outputs
     	  T:Tally
	       the decomposition of @TT "M"@ in degree @TT "L"@
     Description
     	  Text
	       Let $G$ be a semisimple algebraic group which acts on a 
	       polynomial ring $R$ compatibly with the grading. 
	       Let $T\subseteq G$ be a maximal torus and assume the variables 
	       in $R$ are weight vectors for the action of $T$.
	       
	       Suppose @TT "M"@ is a finitely generated module over $R$
	       which is stable under the action of $G$.
	       We assume that @TT "M"@ was input in Macaulay2 using a
	       specific presentation $\phi : E\rightarrow F$, where 
	       $E$ and $F$ are graded free $R$-modules of finite rank.
	       Moreover assume the coordinate basis of $F$ is a 
	       homogeneous basis of weight vectors for the action of $T$
	       and the weight of the $j$-th basis vector is $w_j$.
	       
	       Use this function to obtain the decomposition of a graded 
	       component of @TT "M"@.
	       The input consists of three parameter: the module @TT "M"@,
	       the (multi)degree of the graded component, and the list of
	       weights $\{w_j\}$.
	       
	       The output is a tally whose keys are the highest weights of 
	       certain irreducible representations and whose values are 
	       the multiplicities of those representations.
	       
	       In the following example, the polynomial ring @TT "R"@ is the
	       symmetric algebra over $\mathbb{C}^2 \otimes \mathbb{C}^4$, 
	       with the natural action of $G=SL_2 (\mathbb{C}) \times SL_4 
	       (\mathbb{C})$.
	       The map $\phi$ is the unique (up to scalars) equivariant map
	       $\mathbb{C}^4 \otimes R(-1) \rightarrow (\mathbb{C}^2)^* \otimes
	        R$.
	       If $\{f_1,f_2\}$ and $\{e_1,e_2,e_3,e_4\}$ are the coordinate
	       bases of $\mathbb{C}^2$ and $\mathbb{C}^4$ respectively, then
	       the matrix of $\phi$ with respect to the bases 
	       $\{f_1^*,f_2^*\}$ and $\{e_1,e_2,e_3,e_4\}$ is a generic
	       matrix of indeterminates.

     	  Example
	       R=QQ[x_(1,1)..x_(4,2)];
	       D=dynkinType{{"A",1},{"A",3}};
	       U={{1,1,0,0},{-1,1,0,0},{1,-1,1,0},{-1,-1,1,0},{1,0,-1,1},{-1,0,-1,1},{1,0,0,-1},{-1,0,0,-1}};
	       setWeights(R,D,U);
	       G=genericMatrix(R,2,4);
	       M=coker G
	       highestWeightsDecomposition(M,{2},{{-1,0,0,0},{1,0,0,0}})
	  
	  Text
	       This shows that the component of @TT "M"@ of degree 2 is
	       the representation $S_3 \mathbb{C}^3 \otimes 
	       S_2 \mathbb{C}^4$.
	       Here $S_\lambda$ denotes the Schur functor corresponding to the
	       partition $\lambda$.
	       
	       When the polynomial ring is $\mathbb{Z}$-graded the degree can
	       be given as an integer instead of a list.
	       Moreover, in the $\mathbb{Z}$-graded case, one can decompose
	       a range of degrees all at once as illustrated below.

     	  Example
	       highestWeightsDecomposition(M,2,{{-1,0,0,0},{1,0,0,0}})
	       highestWeightsDecomposition(M,0,4,{{-1,0,0,0},{1,0,0,0}})
///

doc ///
     Key
     	  decomposeWeightsList
     Headline
     	  decompose a list of weights into highest weights
     Usage
     	  decomposeWeightsList(D,L)
     Inputs
     	  D:DynkinType
	  L:List
     Outputs
     	  T:Tally
     Description
     	  Text
	       This function takes in the Dynkin type of a root system
	       along with the complete list of weights of a representation
	       and returns the decomposition into highest weight 
	       representations. It implements a modified version of
	       Freudenthal's multiplicity formula based on the algorithm
	       discussed in chapter 8.9 of W. A. de Graaf, {\it Lie algebras: 
	       theory and algorithms}, North-Holland Publishing, and the
	       article @HREF("http://dx.doi.org/10.1090/S0273-0979-1982-15021-2",
	       "R. V. Moody and J. Patera - Fast recursion formula for 
	       weight multiplicities")@.
	       
	       The only application of this function is to decompose the
	       list of weights obtained with @TO "propagateWeights"@.

     	  Example
	       D=dynkinType{{"A",3}};
	       L={{1,0,0},{-1,1,0},{0,-1,1},{0,0,-1}};
	       decomposeWeightsList(D,L)

     Caveat
     	  To speed up the decomposition of the list of weights in the input,
	  this function discards all non
	  dominant weights, then decomposes the dominant character obtained
	  from the remaining dominant weights. If the dominant weights in 
	  the list do not form the dominant character of a representation
	  an error is returned. This is typically caused by assigning the 
	  wrong weights to the variables of the ring or to the presentation
	  of a module. However, if the non dominant weights in @TT "L"@ do
	  not belong to the character of a representation, this will go
	  undetected by this function.
	  
     SeeAlso
     	  propagateWeights
///
