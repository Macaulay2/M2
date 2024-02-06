-- -*- coding: utf-8 -*-
-*
  Copyright 2016 Brian Pike

  You may redistribute this file under the terms of the GNU General Public
  License as published by the Free Software Foundation, either version 2 of
  the License, or any later version.
*-

newPackage(
		"VectorFields",
		Version => "1.80", 
		Date => "April 7, 2016",
		Authors => {
			{Name => "Brian Pike", 
			Email => "bapike@gmail.com",
			HomePage => "http://www.brianpike.info/"}},
		Headline => "vector fields",
		Keywords => {"Commutative Algebra"},
		PackageImports => {"PrimaryDecomposition"},		     
		DebuggingMode => false 
	)

-*
Changelog:

v1.80: April 7, 2016
  o This is a large rewrite of VectorFields, and is not compatible with v1.0.
    The most notable change is that, rather than using Matrices and Modules interchangeably,
    many functions' behavior now changes dramatically depending on which type you provide.
    (See, e.g., bracket.)  A Matrix represents a finite collection of vector fields,
    while a Module represents an actual module of vector fields.  In a later release,
    this may change so that a Matrix represents a module over the coefficientRing (in most
    cases, a finite-dimensional vector space).
      Users of this package should carefully audit their code when upgrading. 
  o Fixed bugs:
    o der(Ideal I,Ideal J) has really been computing der(gens of I,J), which is
      correct when I is a subset of J (the most common use case) but is not
      what was expected.  Implement the general case and add a der(List,Ideal)
      method with the original behavior.
  o Renamed functions:
    package v1.0                     | package v1.80                | why?
    ---------------------------------+------------------------------+---------------------------------
    isModuleOfVectorFields           | isVectorField                | does not only apply to modules, shorten
    applyVF                          | applyVectorField             | style guidelines (abbreviations should be avoided)
    isHomogeneousModuleOfVectorFields| isHomogeneousVectorField     | does not only apply to modules, shorten
    computeModuleBracket             | bracket(Module,Module)       | merge these functions 
    computeCommutator                | commutator(Module)           | better name
    computeDerivedSeries             | derivedSeries(ZZ,Module)     | better name
    computeLowerCentralSeries        | lowerCentralSeries(ZZ,Module)| better name
    derlogV                          | derlog                       | V is arbitrary
  o Changes (N=New method or function, C=changed, D=deleted)
    (N) applyVectorField(Matrix,List)
    (N) applyVectorField(Vector,RingElement or List)
    (C) applyVectorField(Module,RingElement) now returns an Ideal, and the Module can have >1 generator.
    (N) applyVectorField(Module,Ideal)
    (N) homogeneousVectorFieldDegree was added
    (C) isHomogeneousVectorField uses an incompatible, but more correct method to compute the 'degrees'
    (C) bracket(Module,Module) now computes what computeModuleBracket used to compute
    (N) bracket(Vector,Vector)
    (N) commutator(Matrix)
    (D) isLieAlgebra(Matrix), as this did not do exactly what one might expect
    (N) derivedSeries(ZZ,Matrix)
    (N) lowerCentralSeries(ZZ,Matrix)
    (C) der(Ideal,Ideal) was sometimes producing incorrect results (see above)
    (N) der(List,Ideal) is new, but its output should match the (incorrect) output of the old der(Ideal,Ideal)
    (C) derlog now returns a module
    (D) derlogH(Ideal), as it was not computing the vector fields annihilating the ideal.  Instead use derlogH(List)
    (N) derlogH(List) is new, but its output should match the output of the old derlogH(Ideal)
    (C) derlogH now returns a module
    (N) isLogarithmic checks whether vector field(s) are in derlog(I)
    (C) stratifyByRank now returns a StratificationByRank, a type of HashTable
    (N) isFiniteStratification encapsulates the key step from both isHolonomic and isHHolonomic
    (D) getDim, because dim(...) exists and I don't remember why I wrote my own
  o change asserts to errors with meaningful error messages.
  o rewrite internals of bracket to be faster
  o rewrite internal degree-finding function
  o Write tests for all functions
  o Fix a comment that had an erroneous calc of [fX,gY]
  o symbols should be quoted for M2 v1.8
  o Add license, changelog, updated contact information
v1.0: April 9, 2013
  o initial public release

Maybe TODO someday:
 - isLieAlgebra(Matrix)
 - allow override of \partial_{...} degrees?
 - the other version of Saito's criterion? (eta_i\in derlog(f), is det()=(f)?)
   It could be isFreeDivisor(Matrix/Module,f)
 - homogeneousVectorFieldDegree(Vector)?
 - triple check all proofs...
 - Display a 'multiplication table' for a Lie algebra of vfs?
*-

export {
	-- utility functions
	"isVectorField",
	"applyVectorField",
	-- homogeneity
	"homogeneousVectorFieldDegree",
	"isHomogeneousVectorField",
	-- bracketing
	"bracket",
	"commutator",
	"isLieAlgebra",
	"derivedSeries",
	"lowerCentralSeries",
	-- logarithmic vector fields
	"der",
	"derlog",
	"derlogH",
	"isLogarithmic",
	-- free divisors
	"isFreeDivisor",
	-- stratifications
	"StratificationByRank", --(a type)
	"stratifyByRank",
	"isFiniteStratification",
	"isHolonomic",
	"isHHolonomic"
};

mydoc:="";

mydoc=concatenate(mydoc,///
Node
	Key
		VectorFields
	Headline
		a package for manipulating polynomial vector fields
	Description
		Text
			VectorFields provides functions
			to study polynomial vector fields on affine space, and
			modules of such vector fields.
			This package may be of interest to those studying 
			representations of Lie groups, foliations, or logarithmic
			vector fields.

			We represent a vector field on affine $n$-space as an element of
			the module $R^n$, where $R$ is a polynomial ring in $n$ variables.
			Concretely, the ordering of variables given by {\tt vars(R)} is used to
			identify each vector field with a $n\times 1$ @TO Matrix@,
			and vice-versa,
			with each entry in the matrix storing the coefficient of a derivative
			with respect to a particular variable.
			For instance, on a 3-dimensional space the vector field
			$M=2y \partial_y + z \partial_z$ is entered this way:
		Example 
			R=QQ[x,y,z];
			vars(R)
			M=matrix {{0},{2*y},{z}}
		Text

			Many functions in this package are designed to accept
			either a finite collection of vector fields (in the form
			of a @TO Matrix@ where each column represents a vector field),
			or a module of vector fields (in the form of a submodule of $R^n$).
			These different versions may compute quite different things!
			For instance, the vector field in {\tt M} commutes with itself:
		Example
			commutator(M)
			bracket(M,M)
		Text
			but the vector fields in the module generated by {\tt M} do not:
		Example
			commutator(image M)
			bracket(M,y*M)
		Text
			In other cases, the @TO Matrix@ and @TO Module@ versions of a function
			compute identical things, perhaps returning different types.
			For instance, here we apply vector fields to a function:
		Example
			applyVectorField(M,x*y-z^2)
			applyVectorField(image M,x*y-z^2)
		Text
			See @TO "differences between certain bracketing functions"@ for more details.

			To show some capabilities of this package,
			let us use @TO derlog@ to find the vector fields on the ambient space
			that are tangent to $xy-z^2=0$:
		Example
			D=derlog(ideal (x*y-z^2))
		Text
			Actually, $M$ is one of these vector fields:
		Example
			isSubset(image M,D)
		Text
			We may also check some properties of $D$:
		Example
			isVectorField(gens D)
			isVectorField(D)
			isLieAlgebra(D)
		Text

			Note that this package has been rewritten since version 1.0.
			Any users of the earlier version should upgrade with care, and
			read the comments inside the package's source code.

	SeeAlso
		commutator
		bracket
		applyVectorField
		derlog
		isVectorField
		isLieAlgebra
///);

---------------------------------- COMMON UTILITY FUNCTIONS
mydoc=concatenate(mydoc,///
Node
	Key
		isVectorField
		(isVectorField,Module)
		(isVectorField,Matrix)
	Headline
		test whether a module or matrix can be interpreted as a collection of vector fields
	Usage
		b=isVectorField(m)
		b=isVectorField(M)
	Inputs
		m:Module
		M:Matrix
	Outputs
		b:Boolean
			whether the provided module or matrix can be interpreted as a collection of vector fields
	Description
		Text
			This determines whether the provided @TO Module@ or @TO Matrix@ can be interpreted
			as a collection of vector fields.

			For a @TO Module@ to be interpreted as a module of vector fields, the
			module must be presented as a submodule of a free module whose rank
			equals the number of variables in the ring.
		Example
			R=QQ[x,y];
			isVectorField(image matrix {{x,y^2}})
			isVectorField(image matrix {{x,y^2},{0,0}})
		Text

			For a @TO Matrix@, it must have the correct number of rows.
		Example
			isVectorField(matrix {{x,y^2}})
			isVectorField(matrix {{x,y^2},{0,0}})
	Caveat
		Despite the name, the function does not check the number of vector fields provided.
	SeeAlso
		VectorFields
///);
isVectorField = method(TypicalValue=>Boolean);

isVectorField(Module) := (m) -> (
	return (relations(m)==0 and rank(ambient(m))==numgens(ring(m))); 
);

isVectorField(Matrix) := (m) -> (
	return (numRows(m)==numgens(ring(m)));
);


-*
applyVectorField(Matrix,RingElement) => RingElement
applyVectorField(Vector,RingElement) => RingElement
applyVectorField(Matrix or Vector,List of RingElements) => List of RingElements

applyVectorField(Module,RingElement) => Ideal
applyVectorField(Module,Ideal) => Ideal

Skip:
applyVectorField(Matrix,Ideal)
applyVectorField(Vector,Ideal)
because there's no obvious return type

*-

mydoc=concatenate(mydoc,///
Node
	Key
		applyVectorField
		(applyVectorField,Matrix,RingElement)
		(applyVectorField,Vector,RingElement)
		(applyVectorField,Matrix,List)
		(applyVectorField,Vector,List)
		(applyVectorField,Module,RingElement)
		(applyVectorField,Module,Ideal)
	Headline
		apply a vector field to a function or functions
	Usage
		g=applyVectorField(M,f)
		g=applyVectorField(V,f)
		L=applyVectorField(M,l)
		L=applyVectorField(V,l)
		J=applyVectorField(m,f)
		J=applyVectorField(m,I)
	Inputs
		M:Matrix
			of vector fields
		V:Vector
			a column from a matrix of vector fields
		f:RingElement
			to apply the first parameter to
		l:List
			of RingElements to apply the first parameter to
		m:Module
			of vector fields
		I:Ideal
			an ideal of functions to apply the first parameter to 
	Outputs
		g:RingElement
		L:List
			of RingElements
		J:Ideal
	Description
		Text
			Apply the vector field(s) represented by {\tt M}, {\tt V}, or {\tt m}, to the
			second parameter, that is, use the vector field(s) as differential operator(s).

			When the first parameter is a @TO Matrix@ or a @TO Vector@, then the return type matches
			the second parameter.
			If that is a @TO RingElement@, then there must only be 1 column in the matrix.
			When the second parameter is a @TO List@ {\tt l} of @TO RingElement@s, then the returned 
			@TO List@ is formed by applying each vector field in turn to the first
			entry of {\tt l}, then each vector field to the second entry of {\tt l},
			etc. 
		Example
			R=QQ[x,y,z];
			A=matrix {{x},{y},{0}};
			B=matrix {{0},{0},{z}};
			f=x*y-z^2;
			applyVectorField(A,f)
			applyVectorField(A,{f,x*f})
			applyVectorField(A|B,{f,x*f})
		Text

			When the first parameter is a @TO Module@, then the return type is an @TO Ideal@.
			Applying a @TO Module@ to a @TO RingElement@ gives the @TO Ideal@ generated
			by applying each generator to the function.
			Applying a @TO Module@ to an @TO Ideal@ {\tt I} produces
			the @TO Ideal@ generated by applying every element of the module to
			every element of {\tt I}; this is generated by not only
			the functions formed by
			applying each generator of the module to each generator of {\tt I}
			(as might be computed by the (Matrix,List) version of this function),
			but also {\tt I} times the ideal of coefficients of the generators of
			the module.
		Example
			applyVectorField(image A,f)
			applyVectorField(image A,ideal (f))
			trim(ideal(applyVectorField(A,f))+ideal(f)*minors(1,A))
	SeeAlso
		VectorFields
		derlog
		der
///);

-- internal.  takes (Matrix,List); matrix can have >1 column.  Returns a List
applyVectorFieldInternal := (m,l) -> (
	if not(isVectorField(m)) then error "applyVectorField: expected a matrix of vector fields";
	if not(all(l,f->instance(f,RingElement))) then error "applyVectorField: expected a list of RingElements";
	R:=ring(m);
	if not(all(l,f->(R===ring(f)))) then error "applyVectorField: expected all parameters to be in the same ring";

	T:=transpose(jacobian(ideal(l)));
	-- If l={f1,...fk}, then we have:
	--     /  diff(x1,f1)   ...  diff(xn,f1)  \
	--  T= |                                  |
	--     \  diff(x1,fk)   ...  diff(xn,fk)  /
	-- so (T)*(m) will be a matrix with the answers.
	return( flatten(entries(T*m)) );
);

applyVectorField=method();

applyVectorField(Matrix,RingElement) := RingElement => (m,f) -> (
	if not(numColumns(m)==1) then error "applyVectorField: expected 1 column matrix";
	return (applyVectorFieldInternal(m,{f}))_0;
);

applyVectorField(Vector,RingElement) := RingElement => (V,f) -> (
	-- Using a Vector from a Module (instead of a Matrix) can make this fail.
	if not(isVectorField(matrix {V})) then error "applyVectorField: expected a Vector from a Matrix of vector fields";
	return (applyVectorFieldInternal(matrix {V},{f}))_0;
);

applyVectorField(Matrix,List) := List => (m,l) -> applyVectorFieldInternal(m,l);

applyVectorField(Vector,List) := List => (V,l) -> (
	-- Using a Vector from a Module (instead of a Matrix) can make this fail.
	if not(isVectorField(matrix {V})) then error "applyVectorField: expected a Vector from a Matrix of vector fields";
	return applyVectorFieldInternal(matrix {V},l);
);


applyVectorField(Module,RingElement) := Ideal => (M,f) -> (
	if not(isVectorField(M)) then error "applyVectorField: expected a module of vector fields";
	return ideal(applyVectorFieldInternal(gens M,{f}));
);

applyVectorField(Module,Ideal) := Ideal => (M,I) -> (
	if not(isVectorField(M)) then error "applyVectorField: expected a module of vector fields";
	-- This is certainly contained in the Ideal desired 
	A:=ideal (applyVectorFieldInternal(gens M,flatten entries gens I));
	-- Applying a general elt of the Module to a general elt of the Ideal shows that
	-- we also need the product of I and the ideal generated by the coefficients of the
	-- generators of M.
	B:=I*(ideal(flatten entries gens M));
	return trim(A+B);
);


---------------------------------- HOMOGENEITY 

-- Check if the coefficients of vf are all homogeneous,
-- and if so, return the degree.
-- Return false if it is NOT homogeneous, and -infinity if it is zero.
getDegreeInternal := (vf) -> (
	-- vf is a Vector
	assert(instance(vf,Vector));
	vfentries:=entries vf;
	zero:=0_(ring(vf));

	-- get the degrees of the variables in that ring
	degs:=apply(flatten entries vars(ring(vf)),degree);

	-- Find the first nonzero entry
	firstnz:=position(vfentries,x->not(x===zero));
	if firstnz===null then return -infinity;

	-- note that vfentries_firstnz could fail to be homogeneous
	apparentdeg:=degree((vfentries)_firstnz)-(degs_firstnz);

	if all(length(vfentries),i->vfentries_i===zero or (isHomogeneous(vfentries_i) and degree(vfentries_i)-(degs_i)==apparentdeg)) then
		return apparentdeg
	else
		return false;
);


mydoc=concatenate(mydoc,///
Node
	Key
		homogeneousVectorFieldDegree
		(homogeneousVectorFieldDegree,Matrix)
		(homogeneousVectorFieldDegree,Module)
	Headline
		check if vector fields are homogeneous, and of what degree
	Usage
		l=homogeneousVectorFieldDegree(M)
		l=homogeneousVectorFieldDegree(m)
	Inputs
		M:Matrix
			of vector fields
		m:Module
			of vector fields
	Outputs
		l:List
			either of degrees or {\tt false} if a vector field is
			not homogeneous
	Description
		Text
			This finds the degree of each vector field.
			When given a @TO Matrix@, the function checks the degree of each column.
			When given a @TO Module@, the function checks the degree of each generator.

			In a coordinate system $x_1,\ldots,x_n$ with $x_i$ having degree $k_i$,
			each $\partial_{x_i}$ has degree $-k_i$.
			Hence, a non-zero vector field $\sum_i f_i\partial_{x_i}$
			has degree $d$ if and only if for each $i$, $f_i$ is either
			$0$ or weighted homogeneous of degree $d+k_i$.
			The zero vector field has degree $-\infty$.

		Example
			R=QQ[x,y];
			M=matrix {{x^2,1,0,x^2,x*y},{y^2,0,0,y^4,y^2}}
			homogeneousVectorFieldDegree(M)
			homogeneousVectorFieldDegree(image M)
		Text

			We also handle non-standard degrees:
		Example
			R=QQ[x,y,Degrees=>{{3},{1}}];
			M=matrix {{x^2,1,0,x^2,x*y},{y^2,0,0,y^4,y^2}}
			homogeneousVectorFieldDegree(M)
		Text
			and multidegrees:
		Example
			R=QQ[x,y,Degrees=>{{3,1},{1,1}}];
			M=matrix {{x^2,1,0,x^2,x*y},{y^2,0,0,y^4,y^2}}
			homogeneousVectorFieldDegree(M)
	SeeAlso
		VectorFields
		isHomogeneousVectorField	
///);

homogeneousVectorFieldDegree = method(TypicalValue=>List);

homogeneousVectorFieldDegree(Module) := (M) -> (
	if not(isVectorField(M)) then error "homogeneousVectorFieldDegree: expected a module of vector fields";
	return homogeneousVectorFieldDegree(gens M);
);

homogeneousVectorFieldDegree(Matrix) := (m) -> (
	if not(isVectorField(m)) then error "homogeneousVectorFieldDegree: expected a matrix of vector fields";
	return apply(numColumns(m),i->getDegreeInternal(m_i));
);


-- Check if the vector fields are all homogeneous...
mydoc=concatenate(mydoc,///
Node
	Key
		isHomogeneousVectorField
		(isHomogeneousVectorField,Matrix)
		(isHomogeneousVectorField,Module)
		(isHomogeneousVectorField,Matrix,Set)
		(isHomogeneousVectorField,Module,Set)
		(isHomogeneousVectorField,Matrix,List)
		(isHomogeneousVectorField,Module,List)
	Headline
		determine whether a matrix or module is generated by homogeneous vector fields
	Usage
		b=isHomogeneousVectorField(M)
		b=isHomogeneousVectorField(m)
		b=isHomogeneousVectorField(M,s)
		b=isHomogeneousVectorField(m,s)
	Inputs
		M:Matrix
			of vector fields
		m:Module
			of vector fields
		s:List
			of degrees
		s:Set
			of degrees
	Outputs
		b:Boolean
			whether the vector fields are homogeneous, or homogeneous of a type in {\tt s}
	Description
		Text
			Determine whether the matrix or module is generated by homogeneous vector fields,
			or homogeneous vector fields of degrees appearing in {\tt s}.
			See @TO homogeneousVectorFieldDegree@ for more information on degrees.

		Text
			This is not homogeneous because of $1 \partial_x + 2x \partial_y$: 
		Example
			R=QQ[x,y];
			M1=derlog(ideal (x^2-y))
			isHomogeneousVectorField(gens M1)
		Text

			This is homogeneous, of degree -1 and 0:
		Example
			M2=gens derlog(ideal (x))
			homogeneousVectorFieldDegree(M2)
			isHomogeneousVectorField(M2)
			isHomogeneousVectorField(M2,{{-1},{0}})
		Text

			This is homogeneous, of degrees 0, 1, and -infinity:
		Example
			M3=matrix {{x,0,0},{0,y^2,0}}
			homogeneousVectorFieldDegree(M3)
			isHomogeneousVectorField(M3)
			isHomogeneousVectorField(M3,{{1},{0}})
			isHomogeneousVectorField(M3,{-infinity,{1},{0}})
		Text

			If the parameter is a module, then the generators
			given by @TO gens@ are studied.
			Consequently, the routine may return {\tt false} even though
			a module may have a homogeneous basis:
		Example
			m=matrix {{0,0},{x,x^3+x}}
			isHomogeneousVectorField(image m)
			isHomogeneousVectorField(trim image m)
	Caveat
		Despite the name, the function does not check the number of vector fields provided.
	SeeAlso
		VectorFields
		homogeneousVectorFieldDegree
///);

isHomogeneousVectorField = method(TypicalValue=>Boolean);

isHomogeneousVectorField(Matrix,Set) := (m,s) -> (
	if not(isVectorField(m)) then (
		return false;
	);
	return all(numColumns(m),i-> member(getDegreeInternal(m_i),s));
);

isHomogeneousVectorField(Matrix) := (m) -> (
	if not(isVectorField(m)) then (
		return false;
	);
	return all(numColumns(m),i-> not(getDegreeInternal(m_i)===false));
);

isHomogeneousVectorField(Module,Set) := (M,s) -> (
	if not(isVectorField(M)) then (
		return false;
	);
	return isHomogeneousVectorField(gens M,s);
);

isHomogeneousVectorField(Module) := (M) -> (
	if not(isVectorField(M)) then (
		return false;
	);
	return isHomogeneousVectorField(gens M);
);

isHomogeneousVectorField(Matrix,List) := (M,s) -> isHomogeneousVectorField(M,set(s));

isHomogeneousVectorField(Module,List) := (M,s) -> isHomogeneousVectorField(M,set(s));


---------------------------------- BRACKETING 

-- Compute the derivation f |-> vf1(vf2(f))-vf2(vf1(f)), but
-- try to write the code to be flexible and (for speed) avoid
-- any explicit loops.
--
-- When have coordinates x_1,\ldots,x_n and we indicate the
-- \partial_{x_i}-component by _i, we have:
--   ([vf1,vf2])_i=\sum_j ( (vf1)_j*partial_{x_j}(vf2_i)-(vf2)_j*partial_{x_j}(vf1_i) )
-- 
-- Internal only
bracketFast := (M1,M2,variables,indices) -> (
	-- M1,M2 are matrices, variables is a list
	-- indices is a list of pairs like (1,2)
	n:=numRows(M1);
	R:=ring(M1);

	ans:=map(R^n,R^(length(indices)),
		(row,col) -> (
			-- Calculate the d/d(variables_row)--component of
			-- [M1_((indices_col)_0),M2_((indices_col)_1)].
			l:=(indices_col)_0;
			r:=(indices_col)_1;
			sum(n, j->
				M1_(j,l)*diff(variables_j,M2_(row,r))
				 - M2_(j,r)*diff(variables_j,M1_(row,l)) )
			)
		);
	return ans;
);


mydoc=concatenate(mydoc,///
Node
	Key
		bracket
		(bracket,Matrix,Matrix)
		(bracket,Matrix,Matrix,List)
		(bracket,Vector,Vector)
		(bracket,Module,Module)
	Headline
		compute the Lie bracket of vector fields
	Usage
		M=bracket(M1,M2)
		M=bracket(M1,M2,L)
		V=bracket(V1,V2)
		m=bracket(m1,m2)
	Inputs
		M1:Matrix
			of vector fields
		M2:Matrix
			of vector fields
		L:List
			of 2-element lists, giving indices of columns of {\tt M1} and
			of {\tt M2}
		V1:Vector
			vector from a matrix of vector fields
		V2:Vector
			vector from a matrix of vector fields
		m1:Module
			of vector fields
		m2:Module
			of vector fields
	Outputs
		M:Matrix
		V:Vector
		m:Module
			where the output type matches the input type
	Description
		Text
			This computes the Lie bracket of pairs of vector fields from
			the first and second parameters.
			If $D_1$ and $D_2$ are vector fields, then the Lie bracket of $D_1$ and
			$D_2$, denoted $[D_1,D_2]$, is the vector field $F$ such that
			$F(g)=D_1(D_2(g))-D_2(D_1(g))$ for all $g$, where $V(f)$ denotes the operation of
			applying a vector field to a function (see @TO applyVectorField@).

			When the first two parameters are matrices {\tt M1} and {\tt M2},
			this function
			returns a @TO Matrix@ of {\tt (numColumns(M1))*(numColumns(M2))}
			columns, consisting of all possible brackets of a column from
			{\tt M1} with a column from {\tt M2}.
			If a specific ordering (or subset) of the brackets
			is desired, then a @TO List@ of 
			column index pairs should also be provided.

			When the first two parameters are @TO Vector@s, this returns the
			@TO Vector@ formed by the Lie bracket of the two vector fields. 	

			When the parameters are @TO Module@s {\tt m1} and {\tt m2}, then we
			compute the module generated by $[X,Y]$, for $X$ in {\tt m1}
			and $Y$ in {\tt m2}.
			This is a superset of the module generated by the brackets
			of the generators!
			(A calculation shows that {\tt bracket(m1,m2)} is generated by
			{\tt image bracket(gens m1,gens m2)}, {\tt I1*m2}, and {\tt I2*m1},
			where {\tt I1} is the ideal generated by the coefficients of the
			generators of {\tt m1}, and similarly {\tt I2}.)

			See 
			@TO "differences between certain bracketing functions"@ for
			more details on the differences.

		Example
			R=QQ[x,y];
			A=matrix {{0},{x}};
			B=matrix {{x^2},{y}};
			C=matrix {{1},{x}};
			bracket(A,B)
			bracket(B,A)==-bracket(A,B)
			bracket(A,C)
			bracket(A_0,C_0)
			bracket(A,B|C)
			bracket(A|C,B|C,{(0,0),(1,1)})===bracket(A,B)|bracket(C,C)
			bracket(A|C,B|C,{{0,1}})===bracket(A,C)
		Text
			Providing @TO Module@s computes something much different: 
		Example
			trim bracket(image(A),image(B))
		Text
			because, for example,
		Example
			(2*x+1)*bracket(A,B)-bracket(x*A,B)
			--trim image bracket(A|x*A|y*A,B|x*B|y*B)
		Text

			An action of SL_2 on GL_2 differentiates to the following vector fields:
		Example
			R=QQ[a,b,c,d];
			e=matrix {{c},{d},{0},{0}};
			f=matrix {{0},{0},{a},{b}};
			h=matrix {{-a},{-b},{c},{d}};
			L=e|f|h;
		Text
			Verify that this is a representation of sl_2, where [e,f]=h, [h,f]=-2f, [h,e]=2e.
		Example
			bracket(e,f)-h==0
			bracket(h,f)+2*f==0
			bracket(h,e)-2*e==0
		Text
			Of course we should have [L,L]=L:
		Example
			bracket(L,L)
			image bracket(L,L)==image L
	Caveat
		The @TO Matrix@ and @TO Module@ versions of this routine compute different
		things;
		see @TO "differences between certain bracketing functions"@.
	SeeAlso
		commutator
		"differences between certain bracketing functions"
		VectorFields
///);

-- bracket should return matrices or modules, depending on its inputs.
bracket = method();

bracket(Matrix,Matrix) := Matrix => (m1,m2) -> (
	if not(isVectorField(m1) and isVectorField(m2) and ring(m1)===ring(m2)) then error "bracket: expected matrices of vector fields over the same ring";
	variables:=flatten entries vars(ring(m1));
	-- list all possible pairs of indices
	indices:=flatten table(numColumns(m1),numColumns(m2),identity);
	return bracketFast(m1,m2,variables,indices);
);

bracket(Vector,Vector) := Vector => (v1,v2) -> (bracket(matrix {v1},matrix {v2}))_0;

bracket(Matrix,Matrix,List) := Matrix => (m1,m2,l) -> (
	if not(isVectorField(m1) and isVectorField(m2) and ring(m1)===ring(m2)) then error "bracket: expected matrices of vector fields over the same ring";

	-- allow the use of {1,2} or (1,2)
	if not(instance(l,List) and all(l, i-> instance(i,VisibleList)) and all(l, i-> (#i)==2)) then error "bracket: expected a list of 2-element VisibleLists";
	nc1:=numColumns(m1);
	nc2:=numColumns(m2);
	if not(all(l, i-> (instance(i_0,ZZ) and instance(i_1,ZZ) and 0<= i_0 and i_0< nc1 and 0<= i_1 and i_1< nc2))) then error "bracket: list contains invalid indices";

	variables:=flatten entries vars(ring(m1));
	return bracketFast(m1,m2,variables,l);
);

-- Similarly, we should have a way to compute the bracket of
-- two modules...
-- For X in m1, Y in m2, f in R, we have
--  [fX,Y] =f*[X,Y]-Y(f)*X
--  [X,fY] =f*[X,Y]+X(f)*Y
--  [fX,gY]=fg[X,Y]+f X(g) Y-g Y(f) X
bracket(Module,Module) := Module => (m1,m2) -> (
	if not(isVectorField(m1) and isVectorField(m2) and ring(m1)===ring(m2)) then error "bracket: expected modules of vector fields over the same ring";
	gm1:=gens(m1);
	gm2:=gens(m2);
	tmp:=image bracket(gm1,gm2);
	-- If m1 or m2 has no generators, then we may accidentally get a ZZ ideal.  Force it
	-- to be over the correct ring.
	I1:=promote(ideal (flatten entries gm1),ring(m1));
	I2:=promote(ideal (flatten entries gm2),ring(m1));
	return (tmp+(I1*m2)+(I2*m1));
);




mydoc=concatenate(mydoc,///
Node
	Key
		commutator
		(commutator,Matrix)
		(commutator,Module)
	Headline
		the commutator of a collection of vector fields
	Usage
		N=commutator(M)
		n=commutator(m)
	Inputs
		M:Matrix
			of vector fields
		m:Module
			of vector fields
	Outputs
		N:Matrix
			with columns containing the brackets of pairs of columns of {\tt M}
		n:Module
			the module generated by brackets of elements of {\tt m}
	Description
		Text
			When given a @TO Matrix@, this returns a @TO Matrix@
			with columns containing the brackets of pairs of columns of {\tt M}.

			When given a @TO Module@, this returns the @TO Module@
			generated by $[X,Y]$, for $X$ and $Y$ in the module {\tt m}.
			This is a superset of the module generated by the brackets
			of generators of {\tt m};
			see @TO bracket@ for more details.

			In either case, @TO commutator@ computes much the same thing as
			calling @TO bracket@ with repeated parameters,
			but is more efficient because it takes advantage of skew-symmetry.

			See 
			@TO "differences between certain bracketing functions"@
			for more information.

		Example
			R=QQ[x,y];
			D=derlog(ideal (x*y*(x-y)))
			commutator(gens D)
			bracket(gens D,gens D)
			commutator(D)
			bracket(D,D)
	Caveat
		The @TO Matrix@ and @TO Module@ versions of this routine compute different
		things;
		see @TO "differences between certain bracketing functions"@.
	SeeAlso
		bracket
		"differences between certain bracketing functions"
		VectorFields
///);

commutator = method();

commutator(Matrix) := Matrix => (vfs) -> (
	if not(isVectorField(vfs)) then error "commutator: expected a matrix of vector fields";
	-- Use the symmetry
	variables:=flatten entries vars(ring(vfs));
	ss:=subsets(numColumns(vfs),2);
	return bracketFast(vfs,vfs,variables,ss);
);


commutator(Module) := Module => (m) -> (
	if not(isVectorField(m)) then error "commutator: expected a module of vector fields";
	gm:=gens m;
	mat:=commutator(gm);
	I:=promote(ideal (flatten entries gm),ring(m));
	return (image(mat)+I*m);
);





mydoc=concatenate(mydoc,///
Node
	Key
		isLieAlgebra
		(isLieAlgebra,Module)
		-- TODO: implement this in the future?
		--(isLieAlgebra,Matrix)
	Headline
		check that a module of vector fields is closed under the Lie bracket
	Usage
		b=isLieAlgebra(m)
		--b=isLieAlgebra(M)
	Inputs
		m:Module
			of vector fields
		--M:Matrix
		--	of vector fields
	Outputs
		b:Boolean
			whether the module is a Lie algebra of vector fields
	Description
		Text
			Checks whether the module generated by the provided vector fields is closed under
			the Lie bracket of vector fields (see @TO bracket@) and thus forms a Lie algebra.
		Example
			R=QQ[a,b,c,d];
		Text
			An action of SL_2 on GL_2 differentiates to the following vector fields:
		Example
			e=matrix {{c},{d},{0},{0}};
			f=matrix {{0},{0},{a},{b}};
			h=matrix {{-a},{-b},{c},{d}};
		Text
			Verify that this is sl_2, where [e,f]=h, [h,f]=-2f, [h,e]=2e.
		Example
			bracket(e,f)-h==0
			bracket(h,f)+2*f==0
			bracket(h,e)-2*e==0
		Text
			In particular, the module these generate form a Lie algebra:
		Example
			isLieAlgebra(image (e|f|h))
	Caveat
		There is no {\tt isLieAlgebra(Matrix)}, yet.
	SeeAlso
		VectorFields
///);

isLieAlgebra = method(TypicalValue=>Boolean);

isLieAlgebraInternal := (vfs) -> (
	if not(isVectorField(vfs)) then error "isLieAlgebra: expected a matrix of vector fields";
	variables:=flatten entries vars(ring(vfs));
	-- each column of vfs represents a different vector field.
	ss:=subsets(numColumns(vfs),2);
	return all(ss,combo->
		isSubset(image bracketFast(vfs,vfs,variables,{combo}), image vfs) );
);

-*
isLieAlgebra(Matrix) := (vfs) -> isLieAlgebraInternal(vfs);
*-

-- Note: a calculation shows we only need to check that the brackets of the
-- generators are in the module
isLieAlgebra(Module) := (M) -> (
	if not(isVectorField(M)) then error "isLieAlgebra: expected a module of vector fields";
	return isLieAlgebraInternal(gens M);
);


-- information about the difference:
mydoc=concatenate(mydoc,///
Node
	Key
		"differences between certain bracketing functions"
	Headline
		The difference between certain bracketing functions
	Description
		Text
			This package contains a variety of functions that calculate
			or work with brackets, including
			@TO bracket@, @TO commutator@, @TO derivedSeries@, and @TO lowerCentralSeries@.
			Each function has two variants:
			those that work on only the provided vector fields,
			and those that work on a module of vector fields.
			The former type accept a @TO Matrix@ as the parameter, while the
			latter type accept a @TO Module@ as the parameter.

			The documentation for each function explains what is
			calculated, and how.

			Here is an example to illustrate the difference.
		Example
			R=QQ[x,y];
			D=gens derlog(ideal (x*y))
			bracket(D,D)
			commutator(D)
		Text
			The generating vector fields of {\tt D} commute
			but not all pairs of elements from the module {\tt D} are commutative:
		Example
			co1=commutator(D|x*D|y*D)
			commutator(D|x*D|y*D|x^2*D|x*y*D|y^2*D)
		Text
			In this case only the first-order terms are necessary to achieve the
			results calculated by the @TO Module@ versions of the functions:
		Example
			bracket(image D,image D)
			commutator(image D)
			bracket(image D,image D)==image co1
			commutator(image D)==image co1
	SeeAlso
		bracket
		commutator
		derivedSeries
		lowerCentralSeries
		VectorFields
///); 



mydoc=concatenate(mydoc,///
Node
	Key
		derivedSeries
		(derivedSeries,ZZ,Matrix)
		(derivedSeries,ZZ,Module)
	Headline
		compute the derived series of a set of vector fields
	Usage
		L=derivedSeries(k,M)
		L=derivedSeries(k,m)
	Inputs
		k:ZZ
			a nonnegative integer
		M:Matrix
			of vector fields
		m:Module
			of vector fields
	Outputs
		L:List
			containing {\tt m} or {\tt M}, and the first {\tt k} terms of the
			derived series.
			The types of the terms of the list agrees with the input type.
	Description
		Text
			Compute the first few terms of the {\em derived series} of a set of
			vector fields.
			For an object $n$, if we define 
			$n^{(0)}=n$ and $n^{(i+1)}=bracket(n^{(i)},n^{(i)})$, then
			$n^{(i)}$ is the $i$th term of the derived series, and will be
			in the $i$th index position of the returned list.

			As with @TO bracket@, this function computes different things,
			depending on the type of the parameter.
			See @TO "differences between certain bracketing functions"@ for
			more information.

			The following Lie algebra is generated by a finite-dimensional
			representation of gl_2.
		Example
			R=QQ[a,b,c];
			D=derlog(a*c-b^2)
		Text
			The derived series (when provided with a @TO Matrix@)
			stabilizes as a representation of sl_2:
		Example
			ds=derivedSeries(3,gens D)
			apply(ds,f->gens trim image f)
		Text
			The derived series (when provided with a @TO Module@)
			stabilizes immediately 
			with a linear part isomorphic to sl_2, and a higher-order part
			equal to {\tt ideal (a,b,c)*D}.
		Example
			derivedSeries(3,D)
			trim((image commutator(gens D))+(ideal (a,b,c))*D)
		Text

			This Lie algebra is generated by a finite-dimensional representation
			of t_2, a solvable Borel subalgebra of gl_2.
			Consequently, the linear part disappears: 
		Example
			D=derlog(a*(a*c-b^2))
			derivedSeries(3,gens D)
			derivedSeries(3,D)
	SeeAlso
		commutator
		lowerCentralSeries
		"differences between certain bracketing functions"
		VectorFields
///);

derivedSeries=method(TypicalValue=>List);

-- internal only
derivedSeriesInternal := (k,t,fn) -> (
	if not(k>=0) then error "derivedSeries: expected a nonnegative integer";
	lastone:=t;
	l:=for i from 0 when i<k list (
		-- We may want to simplify along the way... 
		lastone=fn(commutator(lastone))
	);
	return {t}|l;
);

derivedSeries(ZZ,Matrix) := (k,m) -> (
	if not(isVectorField(m)) then error "derivedSeries: expected a matrix of vector fields";
	-- `trim' is not appropriate here, but if we don't do something then
	-- we quickly get many many columns.
	-- TODO: we could throw away the zero columns?
	return derivedSeriesInternal(k,m,identity);
);

derivedSeries(ZZ,Module) := (k,M) -> (
	if not(isVectorField(M)) then error "derivedSeries: expected a module of vector fields";
	return derivedSeriesInternal(k,M,trim);
);


mydoc=concatenate(mydoc,///
Node
	Key
		lowerCentralSeries
		(lowerCentralSeries,ZZ,Matrix)
		(lowerCentralSeries,ZZ,Module)
	Headline
		compute the lower central series of a set of vector fields
	Usage
		L=lowerCentralSeries(k,M)
		L=lowerCentralSeries(k,m)
	Inputs
		k:ZZ
			a nonnegative integer
		M:Matrix
			of vector fields
		m:Module
			of vector fields
	Outputs
		L:List
			a list containing {\tt m} or {\tt M} and the first {\tt k} terms of the
			lower central series.
			The types of the terms of the list agrees with the input type.
	Description
		Text
			Compute the first few terms of the {\em lower central series}
			of a set of vector fields.
			For an object $n$, if we define
			$n^{(0)}=n$ and $n^{(i+1)}=bracket(n,n^{(i)})$, then
			$n^{(i)}$ is the $i$th term of the lower central series, and will be
			in the $i$th index position of the returned list.

			As with @TO bracket@, this function computes different things,
			depending on the type of the parameter.
			See @TO "differences between certain bracketing functions"@ for
			more information.

			The following Lie algebra is generated by a finite-dimensional
			representation of gl_2.
		Example
			R=QQ[a,b,c];
			D=derlog(a*c-b^2)
		Text
			The lower central series (when provided with a @TO Matrix@)
			stabilizes as a representation of sl_2:
		Example
			lcs=lowerCentralSeries(2,gens D)
			apply(lcs,f->gens trim image f)
		Text
			The lower central series (when provided with a @TO Module@)
			stabilizes with a linear part isomorphic to sl_2, and a higher-order
			part equal to {\tt ideal (a,b,c)*D}.
		Example
			lowerCentralSeries(2,D)
			trim((image commutator(gens D))+(ideal (a,b,c))*D)
		Text

			In this example, with 1 generator, the difference between the two versions
			is particularly stark.
		Example
			D=image matrix {{0},{a},{2*b}};
			lowerCentralSeries(5,gens D)
			lowerCentralSeries(5,D)
	SeeAlso
		bracket
		derivedSeries
		"differences between certain bracketing functions"
		VectorFields
///);

lowerCentralSeries = method(TypicalValue=>List);

-- internal only
lowerCentralSeriesInternal := (k,t,fn) -> (
	if not(k>=0) then error "lowerCentralSeries: expected a nonnegative integer";
	lastone:=t;
	l:=for i from 0 when i<k list (
		-- We may want to simplify along the way... 
		lastone=fn(bracket(t,lastone))
	);
	return {t}|l;
);

lowerCentralSeries(ZZ,Matrix) := (k,m) -> (
	if not(isVectorField(m)) then error "lowerCentralSeries: expected a matrix of vector fields";
	-- TODO: may want to simplify? throw away zero columns or gens trim image 
	return lowerCentralSeriesInternal(k,m,identity);
);

lowerCentralSeries(ZZ,Module) := (k,M) -> (
	if not(isVectorField(M)) then error "lowerCentralSeries: expected a module of vector fields";
	return lowerCentralSeriesInternal(k,M,trim);
);



---------------------------------- LOGARITHMIC VECTOR FIELD FUNCTIONS 

mydoc=concatenate(mydoc,///
Node
	Key
		der
		(der,Ideal,Ideal)
		(der,VisibleList,Ideal)
	Headline
		compute the module of vector fields which send one set to another
	Usage
		m=der(I,J)
		m=der(L,J)
	Inputs
		J:Ideal
		I:Ideal
			over the same ring as {\tt J}
		L:VisibleList
			of RingElements in the same ring as {\tt J}
	Outputs
		m:Module
			the module of derivations sending {\tt I} or {\tt L} to {\tt J}
	Description
		Text
			This computes the module of vector fields that, as derivations, send each 
			element of {\tt I} (or {\tt L}) to an element of {\tt J}.
			This can be used to calculate, for example, the module of vector fields
			tangent to an algebraic variety (see @TO derlog@).

			Note that {\tt der(I,J)} is always a subset of
			{\tt der(list of generators of I,J)}, and frequently a proper subset.

			For {\tt der(L,J)}, the computation is done by
			finding the syzygies between the partial derivatives of the
			entries of {\tt L} and the generators of {\tt J}.
			This method of computation was adapted from Singular's
			{\tt KVequiv.lib}, written by Anne Frühbis-Krüger.

			For {\tt der(I,J)}, we intersect {\tt der(list of generators of I,J)} with 
			the free module consisting of vector fields with coefficients
			in {\tt J:I}; the latter is unnecessary when {\tt I} is a subset of {\tt J}.

			For example, consider the following ideals.
		Example
			R=QQ[x,y];
			I=ideal (x*y);
			J=ideal (0_R);
			K=ideal (x,y);
		Text
			Every vector field sends the zero ideal to zero:
		Example
			der(J,I)
			der(J,K)
		Text

			This finds the vector fields tangent to {\tt x*y=0} (see @TO derlog@):
		Example
			D=der(I,I)
			applyVectorField(D,I)
		Text

			This finds the vector fields annihilating {\tt x*y} (see @TO derlogH@):
		Example
			D=der({x*y},J)
		Text
			This is different than 
		Example
			der(I,J)
		Text
			because, for example, the generator of {\tt D} does not
			annihilate {\tt x^2*y}:
		Example
			applyVectorField(gens D,x^2*y)
		Text

			Another illustration of the difference is:
		Example
			der({x},ideal (y))
			der(ideal (x),ideal (y))
		Text
	
			This illustrates a basic identity:
		Example
			intersect(der(ideal (x),K),der(ideal (y),K))==der(K,K)
	SeeAlso
		VectorFields
		"Ideal : Ideal"
		derlog
		derlogH
///);

der= method(TypicalValue=>Module);

-- Returns the module of vector fields which send all elements of
-- the list to elements of J.
-- TODO: doing this for each generator of I and then intersecting could be faster.
der(VisibleList,Ideal) := (L,J) -> (
	R:=ring(J);
	if not(all(L,f->instance(f,RingElement)) and all(L,f->ring(f)===R)) then error "der: expected a list of RingElements in the same ring as the given ideal";

	Ln:=length(L);
	if (Ln==0) then (
		L={0_R};
		Ln=1;
	);
	Jn:=numgens(J);
	-- dimension of the space containing the vfs.
	d:=numgens(R);

	-- We want to find the vfs eta such that
	--             eta(l_i)=\sum_k (-alpha_{i,k})*(kth generator of J)
	-- for all i.  If we move everything over to the left side, then this amounts
	-- to finding relations between the partials of l_i and the generators of J,
	-- except that for each i there is a different copy of the generators of J.
	-- Thus we construct a large matrix, and find the kernel/syzygies of it.
	-- (idea from Singular's "KVequiv.lib", written by Anne Frühbis-Krüger) 

	-- First we need the derivatives of the elts of L.
	--   Shape: Ln x d
	mat:=transpose(jacobian(ideal L));

	-- Then we need Jn copies of multiples of the Ln x Ln identity matrix,
	-- concatenated horizontally.  We could loop like this:
	--   for j from 0 when j<Jn do ( mat=mat|diagonalMatrix(R,Ln,Ln,toList(Ln:J_j)); );
	-- but repeatedly concatenating matrices is discouraged.
	mat2:=mutableMatrix(R,Ln,Ln*Jn);
	for j from 0 when j<Jn do (
		for i from 0 when i<Ln do (
			mat2_(i,j*Ln+i)=J_j;
		);
	);

	-- Will there ever be a problem with the degrees of the targets not agreeing?
	mat=mat|(matrix mat2);
	-- Now mat is Ln x (d+ Ln*Jn).

	-- Could also use 'ker' here, but that calls modulo(mat,Nothing) which calls syz.
	smat:=syz(mat);

	-- Syzygy will have shape (d+Ln*Jn) x (??), but we only care about the first d rows.
	-- The target module should just be the free one, without gradings 
	return(trim image map(R^d,,submatrix(smat,toList(0..(d-1)),)));
);

-- Returns the module of vector fields which send all elements of I to
-- elements of J.
der(Ideal,Ideal) := (I,J) -> (
	R:=ring(I);
	if not(R===ring(J)) then error "der: expected a pair of ideals over the same ring";
	d:=numgens R;

	-- If I is a subset of J, then der(list-of-gens-of I,J) is enough, but that's not always
	-- the case.
	K:=(J:I);
	D2:=K*(image id_(R^d));
	-- avoid time-consuming computation of der(...,...) if possible
	if (K==ideal (0_R)) then return D2; 
		
	D1:=der(flatten entries gens I,J);

	return intersect(D1,D2);
);


mydoc=concatenate(mydoc,///
Node
	Key
		derlog
		(derlog,Ideal)
		(derlog,RingElement)
		derlogH
		(derlogH,List)
		(derlogH,RingElement)
	Headline
		compute the logarithmic (tangent) vector fields to an ideal 
	Usage
		m=derlog(I)
		m=derlog(f)
		n=derlogH(L)
		n=derlogH(f)
	Inputs
		I:Ideal
		f:RingElement
		L:List
			nonempty, of RingElements
	Outputs
		m:Module
			the module of logarithmic vector fields for {\tt I} or {\tt ideal(f)}
		n:Module
			the module of vector fields that annihilates {\tt f} or the entries of {\tt L}
	Description
		Text
			{\tt derlog} computes the module of {\it logarithmic vector fields} to an
			affine
			variety defined by {\tt I} or {\tt f}; these are the ambient vector
			fields tangent to the variety.

			{\tt derlogH} computes the module of ambient
			vector fields tangent to all level sets of {\tt f} or 
			of the entries of {\tt L}.

			Note that {\tt derlog(I)=der(I,I)} and
			{\tt derlogH(L)=der(L,0)};
			see @TO der@.
			
		Example
			R=QQ[x,y,z];
			f=x*y-z^2;
			derlog(ideal (f))
			derlogH(f)
			dH=derlogH({f})
		Text
			Although every element of {\tt dH} annihilates {\tt f}, they do not annihilate the
			ideal generated by {\tt f}:
		Example
			applyVectorField(dH,f)
			applyVectorField(dH,ideal(f))
	SeeAlso
		VectorFields
		der
		isLogarithmic
///);

derlog=method();

derlog(Ideal) := Module => (I) -> der(I,I);

derlog(RingElement) := Module => (f) -> derlog(ideal (f));


derlogH=method();

derlogH(List) := Module => (L) -> (
	if #L==0 then (
		error "derlogH: expected nonempty list";
	);
	if not(all(L,f->instance(f,RingElement)) and all(L,f->ring(f)===ring(L#0))) then (
		error "derlogH: expected list of RingElements over the same ring";
	);
	R:=ring(L#0); 
	return der(L,ideal (0_(R)));
);

derlogH(RingElement) := Module => (f) -> derlogH({f});



mydoc=concatenate(mydoc,///
Node
	Key
		isLogarithmic
		(isLogarithmic,Matrix,Ideal)
		(isLogarithmic,Vector,Ideal)
		(isLogarithmic,Module,Ideal)
	Headline
		check if the given vector fields are logarithmic
	Usage
		b=isLogarithmic(M,I)
		b=isLogarithmic(V,I)
		b=isLogarithmic(m,I)
	Inputs
		M:Matrix
			of vector fields
		V:Vector
			a column from a matrix of vector fields
		m:Module
			of vector fields
		I:Ideal
			the ideal
	Outputs
		b:Boolean
			whether the vector fields are in {\tt derlog(I)} 
	Description
		Text
			Check if the vector field(s) given are in the module of logarithmic
			vector fields of {\tt I} (see @TO derlog@).
			This function does not compute {\tt derlog(I)}, but instead applies the vector fields
			to the generators of {\tt I} and checks if the result lies in {\tt I}.

		Example
			R=QQ[x,y,z];
			f=x*y-z^2;
			I=ideal (f);
			M=matrix {{x,2*z,2*z},{y,0,0},{z,y,x}};
			applyVectorField(M,{f})
			isLogarithmic(M,I)
			isLogarithmic(M_{0,1},I)
			isLogarithmic(derlog(I),I)
	SeeAlso
		VectorFields
		derlog
		applyVectorField
///);

-- Note: for der(I,J) we would need to do more, but for der(I,I) we only
-- need to check the generators of I.
isLogarithmic=method(TypicalValue=>Boolean);

isLogarithmic(Matrix,Ideal) := (M,I) -> (
	if not(isVectorField(M)) then error "isLogarithmic: expected a matrix of vector fields";
	if not(ring(M)===ring(I)) then error "isLogarithmic: expected parameters to have the same ring";
	return all(applyVectorFieldInternal(M,flatten entries gens I),f->isSubset(ideal (f),I));
);

isLogarithmic(Vector,Ideal) := (V,I) -> (
	-- Using a Vector from a Module (instead of a Matrix) can make this fail.
	if not(isVectorField(matrix {V})) then error "isLogarithmic: expected a Vector from a Matrix of vector fields";
	return isLogarithmic(matrix {V},I);
);

isLogarithmic(Module,Ideal) := (m,I) -> (
	if not(isVectorField(m)) then error "isLogarithmic: expected a module of vector fields";
	return isLogarithmic(gens m,I);
);


---------------------------------- FREE DIVISORS

mydoc=concatenate(mydoc,///
Node
	Key
		isFreeDivisor
		(isFreeDivisor,Matrix)
		(isFreeDivisor,Module)
		(isFreeDivisor,RingElement)
	Headline
		check if the provided information is associated with a free divisor
	Usage
		t=isFreeDivisor(M)
		t=isFreeDivisor(m)
		t=isFreeDivisor(f)
	Inputs
		M:Matrix
			of vector fields, usually square
		m:Module
			of vector fields
		f:RingElement
			a polynomial defining a hypersurface
	Outputs
		t:Boolean
			whether {\tt M} or {\tt m} generate the module of vector fields for a
			free divisor, or whether {\tt f=0} defines a free divisor
	Description
		Text
			Determine whether the given object is associated to a free divisor,
			using a variety of methods.
			A {\em free divisor} is a 
			hypersurface germ $X$ for which the associated module of
			logarithmic vector fields (see @TO derlog@) is free.

			When given a @TO RingElement@ {\tt f}, this tests whether
			{\tt f=0} is a free divisor by 
			computing {\tt derlog(ideal (f))} and
			then testing if this is a free module by computing a partial
			@TO resolution@.
			This method may give false negatives
			if the resolution computed is not minimal;
			use with caution.

			When given a @TO Matrix@ {\tt M}, this tests whether
			the given vector fields are a free set of generators of
			the module of logarithmic vector fields for a free divisor.
			A criterion of Kyoji Saito is used:
			if the number of vector fields provided equals the
			dimension of the ambient space (i.e., {\tt M} is square), 
			the vector fields generate a Lie algebra,
			and the determinant of the matrix is reduced (square-free),
			then the provided vector fields are a
			free basis of the free module {\tt derlog(ideal det(M))}, and
			{\tt det(M)=0} defines a free divisor.
			This method may give false negatives
			if redundant generators are provided.
 
			When given a @TO Module@, this tests whether
			the module is the module of logarithmic vector fields of
			a free divisor.
			The method applies the @TO Matrix@ version of the command
			to the generators given by {\tt gens(m)}.
			If the generators provided by @TO gens@ are not minimal,
			then this may give false negatives; use @TO trim@ first.

			For example, this is not a free divisor:
		Example
			R=QQ[a,b,c];
			f=a*c-b^2;
			M=derlog(ideal (f))
			isFreeDivisor(gens M)
			isFreeDivisor(M)
			isFreeDivisor(f)
		Text

			This is a free divisor:
		Example
			f=a*(a*c-b^2);
			M=derlog(ideal (f))
			isFreeDivisor(gens M)
			isFreeDivisor(M)
			isFreeDivisor(f)
		Text

			This is a free divisor:
		Example
			f=a;
			D=derlog(ideal (f))
			isFreeDivisor(f)
			isFreeDivisor(D)
		Text
			but the vector fields in {\tt M} are not
			a generating set of the logarithmic vector fields:
		Example
			M=matrix {{a,0,0},{0,a,0},{0,0,1}};
			isSubset(image M,D)
			isFreeDivisor(M)
		Text

			False negatives can occur with a set of generators that is not minimal:
		Example
			isFreeDivisor((gens D)|M)
			isFreeDivisor(trim image ((gens D)|M))
	SeeAlso
		VectorFields
		isLieAlgebra
		derlog
///);

isFreeDivisor = method(TypicalValue => Boolean);

isFreeDivisor(Matrix) := (vfs) -> (
	if not(isVectorField(vfs)) then error "isFreeDivisor: expected a matrix of vector fields";
	if not(numColumns(vfs)==numRows(vfs)) then
		return false;
	if not(isLieAlgebra(image vfs)) then
		return false;
	f:=det(vfs);
	if f==0 then
		return false;
	-- Is f reduced?
	return (ideal(f)==radical ideal (f));
);

isFreeDivisor(Module) := (m) -> (
	if not(isVectorField(m)) then error "isFreeDivisor: expected a module of vector fields";
	return isFreeDivisor(gens m);
);

isFreeDivisor(RingElement) := (p) -> (
	-- p is a polynomial.
	R:=res(derlog(ideal (p)),LengthLimit=>1);
	if (length(R)==0) then
	(
		return true;
	);
	-- We could be saying that things aren't free divisors when they are, but...
	return false;
);


---------------------------------- STRATIFICATIONS

mydoc=concatenate(mydoc,///
Node
	Key
		StratificationByRank
	Headline
		a type to hold a rank computation
	Description
		Text
			This is a type of @TO HashTable@ with positive integer keys, used to store the
			output of @TO stratifyByRank@.
			If {\tt M} is a module of vector fields and
			{\tt h=stratifyByRank(M)} is the StratificationByRank object, then
			{\tt h#i} is the radical ideal defining the set of points $p$ such that
			the generators of $M$ evaluated at $p$ span a subspace of dimension
			less than $i$.
	
			These objects are primarily used by @TO isFiniteStratification@.

			If {\tt M} is a Lie algebra, then this gives some information about
			the 'orbits' or maximal integral submanifolds of the vector fields.

	SeeAlso
		stratifyByRank
		isFiniteStratification
		VectorFields
///);

StratificationByRank = new Type of HashTable;


mydoc=concatenate(mydoc,///
Node
	Key
		stratifyByRank
		(stratifyByRank,Matrix)
		(stratifyByRank,Module)
	Headline
		compute ideals describing where the vector fields have a particular rank
	Usage
		H=stratifyByRank(M)
		H=stratifyByRank(m)
	Inputs
		M:Matrix
			of vector fields
		m:Module
			of vector fields
	Outputs
		H:StratificationByRank
			a type of @TO HashTable@ with integer keys: {\tt i} maps to the radical ideal defining the
			set where the vector fields have rank less than {\tt i}.
	Description
		Text
			Computes ideals describing where the provided vector fields have a
			particular rank.
			For $1\leq i\leq n$, where $n$ is the dimension of the space,
			{\tt (stratifyByRank(M))#i} will be an ideal defining
			the set of points $p$ such that 
			the generators of $M$ evaluated at $p$ span a subspace of
			dimension less than $i$.
			If the vector fields generate a Lie algebra, then this gives some
			information about their 'orbits' or their maximal integral
			submanifolds.

			For details on the parts of the calculation,
			make @TO "debugLevel"@ positive.

		Example
			R=QQ[a,b,c];
			f=a*b*(a-b)*(a-c*b)
			D=derlog(ideal (f))
			S=stratifyByRank(D);
		Text
			{\tt D} has rank 0 on $a=b=0$, that is, the vector fields all vanish
			there:
		Example
			S#1
		Text
			{\tt D} has rank <3 precisely on the hypersurface $f=0$, and hence rank 3
			off the hypersurface:
		Example
			S#3
		Text
			This submodule of {\tt D} has rank $<3$ everywhere since it only has
			2 generators:
		Example
			Df=derlogH(f)
			isSubset(Df,D)
			S=stratifyByRank(Df);
			S#3
	SeeAlso
		VectorFields
		StratificationByRank
		isFiniteStratification
///);

stratifyByRank = method(TypicalValue=>StratificationByRank);

stratifyByRank(Matrix) := (N) -> (
	if not(isVectorField(N)) then error "stratifyByRank: expected a matrix of vector fields";
	-- N is a matrix generating a Lie algebra of vector fields (assumed)?  
	-- returns a hash table where i maps to the radical ideal defining the
	--   set where the vfs have rank less than i.
	n:=numRows(N);
	mindim:=min({numColumns(N),n});
	-- When it makes sense to take minors, do so.
	ranklt:=for i from 1 when i<=mindim list (
		if debugLevel>0 then stdio<<"stratifyByRank: Taking minors of order "<<toString(i)<<endl;
		I:=trim minors(i,N);
		if debugLevel>0 then stdio<<"stratifyByRank:   Taking radical..."<<endl;
		i=>radical I
	);
	ranklt=ranklt|(for i from mindim+1 when i<=n list (
		i=>ideal (0_(ring(N)))
	));
	if debugLevel>0 then stdio<<"stratifyByRank: Done."<<endl;
	return new StratificationByRank from ranklt;
);

stratifyByRank(Module) := (m) -> (
	if not(isVectorField(m)) then error "stratifyByRank: expected a module of vector fields";
	return stratifyByRank(gens m);
);


mydoc=concatenate(mydoc,///
Node
	Key
		isFiniteStratification	
		(isFiniteStratification,StratificationByRank)
	Headline
		checks if a stratification by integral submanifolds is finite
	Usage
		b=isFiniteStratification(strat)
	Inputs
		strat:StratificationByRank
			the output of @TO stratifyByRank@ applied to a collection of vector fields
	Outputs
		b:Boolean
			whether {\tt strat} is a finite stratification
	Description
		Text
			Let {\tt strat} be the output of @TO stratifyByRank@ applied to a
			module $L$ of vector fields.
			When $L$ is a Lie algebra, {\tt strat} contains information about
			the integral submanifolds of $L$;
			under the assumption that $L$ is a Lie algebra,
			this function checks whether there are a finite number
			of connected integral submanifolds.

			The algorithm used,
			and perhaps even the term {\em integral submanifold},
			is only valid in real or complex geometry.
			This routine checks that, for all $j$, each component of 
			{\tt strat#j} has dimension $<j$.
			It is up to the user to check that the answers obtained
			by Macaulay2 (e.g., in {\tt QQ[x,y,z]})
			would not change if the calculation was done
			over the real or complex numbers.
			-- Ex: though x^2-5 doesn't factor in QQ[x], M2 correctly computes the dim as though it's in CC[x].

			The algorithm is motivated by the results of section 4.3 of 
			``James Damon and Brian Pike. 
			Solvable groups, free divisors and nonisolated matrix singularities II:
			Vanishing topology. {\it Geom. Topol.}, 18(2):911-962, 2014'',
			available at \url{http://dx.doi.org/10.2140/gt.2014.18.911} or
			\url{http://arxiv.org/abs/1201.1579}.

			To display progress reports, make @TO "debugLevel"@$>1$.

		Example
			R=QQ[a,b,c];
			f=a*b*(a-b)*(a-c*b)
			D=derlog(ideal (f))
			S=stratifyByRank(D);
		Text
			Since {\tt D} has rank 0 on $a=b=0$, that is, the vector fields all vanish:
		Example
			S#1
		Text
			the stratification cannot be finite (every point on $a=b=0$ is its own stratum):
		Example
			isFiniteStratification(S)
		Text

			This stratification is finite:
		Example
			D=derlog(ideal (a*b*c))
			isFiniteStratification(stratifyByRank(D))
	Caveat
		The assumption that $L$ is a Lie algebra is not checked.
	SeeAlso
		VectorFields
		stratifyByRank
		isHolonomic
		isHHolonomic
///);

isFiniteStratification = method(TypicalValue=>Boolean);

isFiniteStratification(StratificationByRank) := (strat) -> (
	-- dimension of the space
	n:=max(keys(strat));
	R:=ring(strat#n);
	-- Make sure this is a reasonable stratification
	if not(dim(R)==n and numColumns(vars(R))==n) then error "isFiniteStratification: unexpected behavior from the stratification";
	if not(sort(keys(strat))==toList(1..n)) then error "isFiniteStratification: unexpected key behavior from stratification";

	-- Let D_k be the locus of rank <=k, so it is defined by strat#(k+1)
	-- Check that each irreducible component of D_k has dimension <=k, for 0<=k<n.
	-- Check that each irreducible component of D_k has dimension <k+1, for 0<=k<n.
	--   (Translating indices (j=k+1))
	-- Check that each irreducible component of D_(j-1) has dimension <j, for 1<=j<n+1.
	-- Check that each irreducible component of V(strat#j) has dimension <j, for 1<=j<=n.
	for j from 1 when j<=n do (
		-- Make sure that all components of strat#j have dimension <j.
		l:=primaryDecomposition (strat#j);
		ld:=apply(l,dim);
		if debugLevel>1 then (
			for i from 0 when i<length(l) do (
				stdio<<"isFiniteStratification, #"<<j<<": component "<<toString(l#i)<<" has dimension "<<toString(ld#i)<<endl;
			);
		);
		if not(all(ld,i->i<j)) then (
			-- List any strata that have more than the expected dim
			for i from 0 when i<length(l) do (
				if not(ld#i<j) then (
					stdio<<"isFiniteStratification: Component "<<toString(l#i)<<" has dim "<<ld#i<<" but should be of dim <"<<j<<" to have a finite stratification."<<endl;
				);
			);
			return false;
		);
	);
	return true;
);


mydoc=concatenate(mydoc,///
Node
	Key
		isHolonomic
		(isHolonomic,Ideal)
		(isHolonomic,RingElement)
	Headline
		test whether an algebraic set is holonomic
	Usage
		b=isHolonomic(I)
		b=isHolonomic(f)
	Inputs
		I:Ideal
			defining an algebraic set
		f:RingElement
			a nonzero function defining a hypersurface
	Outputs
		b:Boolean
			whether the algebraic set is holonomic
	Description
		Text
			Test if $X$, the algebraic set defined by {\tt I} or {\tt f}, is holonomic.
			Let $D$ be the module of logarithmic vector fields for {\tt I}.
			Then $X$ is called {\em holonomic} if at any point $p$ in $X$,
			the generators of $D$ evaluated at $p$ span the tangent space of
			the stratum containing $p$ of the canonical Whitney stratification
			of $X$;
			equivalently, the maximal integral submanifolds of $D$ equal the 
			the canonical Whitney stratification of $X$
			(except that the complement of $X$ forms additional integral submanifold(s)).
			
			The algorithm used amounts to computing
			{\tt isFiniteStratification(stratifyByRank(derlog(I)))}
			(see @TO isFiniteStratification@, @TO stratifyByRank@, @TO derlog@).
			Details may be found in section 4.3 of 
			``James Damon and Brian Pike. 
			Solvable groups, free divisors and nonisolated matrix singularities II:
			Vanishing topology. {\it Geom. Topol.}, 18(2):911-962, 2014'',
			available at \url{http://dx.doi.org/10.2140/gt.2014.18.911} or
			\url{http://arxiv.org/abs/1201.1579}.
			The basic idea, however, is present in (3.13) of
			``Kyoji Saito.
			Theory of logarithmic differential forms and logarithmic vector fields.
			{\it J. Fac. Sci. Univ. Tokyo Sect. IA Math.}, 27: 265-291, 1980''.
			
		Example
			R=QQ[a,b,c];
		Text
			This hypersurface is not holonomic, since {\tt gens(D)} has rank 0 on the
			$1$-dimensional space $a=b=0$:
		Example
			f=a*b*(a-b)*(a-c*b);
			D=derlog(ideal (f))
			isHolonomic(f)
		Text

			This is holonomic; the stratification consists of the origin,
			and the rest of the surface $ac-b^2=0$:
		Example
			f=a*c-b^2;
			D=derlog(ideal (f))
			isHolonomic(f)
			S=stratifyByRank(D);
			S#1
			S#2
			S#3
		Text

			The Whitney Umbrella is also holonomic; the stratification consists
			of the origin, the rest of the line $a=b=0$, and the rest of the surface:
		Example
			f=a^2-b^2*c;
			D=derlog(ideal (f));
			isHolonomic(f)
			S=stratifyByRank(D);
			S#1
			S#2
			S#3
	Caveat
		See the warnings in @TO isFiniteStratification@.

		Also, this usage of {\em holonomic} originates with Kyoji Saito and
		may vary from other meanings of the word, particularly in D-module theory.
	SeeAlso
		isHHolonomic
		derlog
		stratifyByRank
		isFiniteStratification
		VectorFields
///);

isHolonomic = method(TypicalValue=>Boolean);

isHolonomic(Ideal) := (I) -> (
	-- I is an Ideal defining an algebraic set.
	DV:=gens derlog(I);
	sV:=stratifyByRank(DV);

	-- sanity checks 
	n:=numColumns vars(ring(I));
	if not(numColumns(DV)>=n) then error "isHolonomic: too few logarithmic vector fields (this should not happen)";
	if not(I==ideal (0_(ring(I))) or I==ideal (1_(ring(I))) or sV#n==radical I) then error "isHolonomic: sanity checks on derlog failed!"; 

	return isFiniteStratification(sV);
); 
	
isHolonomic(RingElement) := (H) -> (
	-- H is a ringElement defining a hypersurface.
	return isHolonomic(ideal (H));
);


mydoc=concatenate(mydoc,///
Node
	Key
		isHHolonomic
		(isHHolonomic,RingElement)
	Headline
		test whether a hypersurface is H-holonomic
	Usage
		b=isHHolonomic(H)
	Inputs
		H:RingElement
			a nonzero function defining a hypersurface
	Outputs
		b:Boolean
			whether the hypersurface is H-holonomic
	Description
		Text
			Test if $X$, the algebraic set defined by {\tt H}, is
			H-holonomic.
			Let $D$ be the module of vector fields annihilating $H$
			(see @TO derlogH@), a submodule of the module of logarithmic
			vector fields for $H$.
			Then $X$ is called {\em H-holonomic} if at any point $p$ in $X$,
			the generators of $D$ evaluated at $p$ span the tangent space of
			the stratum containing $p$ of the canonical Whitney stratification
			of $X$;
			equivalently, the collection of maximal integral submanifolds of $D$
			that are contained in $X$ will equal the canonical Whitney
			stratification of $X$.
			The ``H'' in H-holonomic refers to the name of the function.

			The algorithm used essentially checks the dimensions of the components
			of the sets given by {\tt stratifyByRank(derlog(ideal (H)))}
			(see @TO isHolonomic@ and @TO isFiniteStratification@)
			and then compares these sets with those given by
			{\tt stratifyByRank(derlogH(H))}.
			Details may be found in section 4.3 of 
			``James Damon and Brian Pike. 
			Solvable groups, free divisors and nonisolated matrix singularities II:
			Vanishing topology. {\it Geom. Topol.}, 18(2):911-962, 2014'',
			available at \url{http://dx.doi.org/10.2140/gt.2014.18.911} or
			\url{http://arxiv.org/abs/1201.1579}.
			-- The term {\em H-holonomic} seems to date to at least
			-- ``James Damon. On the legacy of free divisors: discriminants and
			-- Morse-type singularities. {\it Am. J. of Math.}, 120(3):453-492, 1998.''

			To display progress reports, set @TO "debugLevel"@>0.

		Example
			R=QQ[a,b,c];
		Text
			This hypersurface is not even holonomic, as the logarithmic
			vector fields have rank 0 on $a=b=0$:
		Example
			f=a*b*(a-b)*(a-c*b);
			derlog(ideal (f))
			derlogH(f)
			isHHolonomic(f)
			isHolonomic(f)
		Text

			This hypersurface is H-Holonomic, although the generators of {\tt D}
			are linearly dependent everywhere:
		Example
			f=a*c-b^2;
			D=gens derlogH(f)
			det(D)
			isHHolonomic(f)
			S=stratifyByRank(D)
			S#1
			S#2
			S#3
		Text

			The Whitney Umbrella is also H-Holonomic:
		Example
			f=a^2-b^2*c;
			isHHolonomic(f)
	Caveat
		See the warnings in @TO isFiniteStratification@.
	SeeAlso
		isHolonomic
		isFiniteStratification
		derlog
		derlogH
		stratifyByRank
		VectorFields
///);

isHHolonomic = method(TypicalValue=>Boolean);

isHHolonomic(RingElement) := (H) -> (
	DV:=gens derlog(ideal (H));
	sV:=stratifyByRank(DV);

	-- sanity checks
	n:=numColumns vars(ring(H));
	if not(numColumns(DV)>=n) then error "isHHolonomic: too few logarithmic vector fields (this should not happen)";
	if not(H==0_(ring(H)) or H==1_(ring(H)) or sV#n==radical ideal (H)) then error "isHHolonomic: sanity checks on derlog failed!"; 

	-- Test if holonomic
	if not(isFiniteStratification(sV)) then (
		if debugLevel>0 then stdio<<"isHHolonomic: not even holonomic"<<endl;
		return false;
	);

	-- So we're holonomic.  Are we H-holonomic?
	DH:=gens derlogH(H);
	sH:=stratifyByRank(DH);

	-- Note that DH may have fewer than n vector fields, so we can't take
	--   n x n minors.  Instead, stop at (n-1).
	
	-- The test is the same as that for isHolonomic, except we also check
	-- that sH and sV give us the same thing, at least below level n.
	for j from 1 when j<n do (
		-- Check if sH#j and sV#j are the same.
		if not(sH#j==sV#j) then (
			stdio<<"isHHolonomic: DerlogH and DerlogV differ in their rank<"<<j<<" sets"<<endl;
			if debugLevel>0 then stdio<<"isHHolonomic: holonomic but not H-holonomic"<<endl;
			return false;
		);
	);
	return true;
);	



---------------------------------- DOCUMENTATION
beginDocumentation();
--print(mydoc);
multidoc(mydoc);

---------------------------------- TESTS 

-- isVectorField
TEST ///
R=QQ[x,y];
assert(isVectorField(map(R^2, ,{{x,y^3+x^3},{24/3,x*y}})))
assert(isVectorField(map(R^{2:-1}, ,{{x},{24/3}})))
assert(not isVectorField(map(R^{3:-1}, ,{{x},{24/3},{y}})))
assert(not isVectorField(map(R^1, ,{{x,y}})))

assert(isVectorField(image map(R^2, ,{{x,y^3+x^3},{24/3,x*y}})))
assert(isVectorField(image map(R^{2:-1}, ,{{x},{24/3}})))
assert(not isVectorField(image map(R^{3:-1}, ,{{x},{24/3},{y}})))
assert(not isVectorField(image map(R^1, ,{{x,y}})))

assert(not isVectorField(subquotient(matrix {{x,y},{y,x}},matrix {{x^2},{y^2}})))

assert(isVectorField(matrix mutableMatrix(R,2,0)));
assert(isVectorField(image matrix mutableMatrix(R,2,0)));
///

-- applyVectorField
TEST ///
R=QQ[x,y,z];
V=matrix {{y^3},{x*z},{3}};

assert(applyVectorField(V,x)===y^3);
assert(applyVectorField(V,y)===x*z);
assert(applyVectorField(V,z)===3_R);

f=y*(y^3)+x*z*x;
assert(applyVectorField(V          ,x*y)===f);
assert(applyVectorField(V_0        ,x*y)===f);
assert(applyVectorField(image V    ,x*y)===ideal (f));
-- This should NOT work:
--assert(applyVectorField((image V)_0,x*y)===f);

f=9*z^2;
assert(applyVectorField(V          ,z^3)===f);
assert(applyVectorField(V_0        ,z^3)===f);
assert(applyVectorField(image V    ,z^3)===ideal (f));
-- This should NOT work:
--assert(applyVectorField((image V)_0,z^3)===f);

-- list of functions
ans={y^3,x*z,3_R};
assert(applyVectorField(V,{x,y,z})===ans);
assert(applyVectorField(V_0,{x,y,z})===ans);
assert(applyVectorField(image V,ideal(x,y,z))==ideal (ans));

ans2={y^3,y^3,x*z,x*z,3_R,3_R};
assert(applyVectorField(V|V,{x,y,z})===ans2);
assert(applyVectorField(V|V,{x,y,z,x,y,z})===ans2|ans2);
assert(applyVectorField(V_0,{x,y,z,x,y,z})===ans|ans);
assert(applyVectorField(image (V|V),ideal (x,y,z))==ideal (ans2));
///


-- homogeneousVectorFieldDegree
TEST ///
R=QQ[x];
m=matrix {{0_R,4_R,x,x^67,x^3+x}};
ans={-infinity,{-1},{0},{66},false};
assert(homogeneousVectorFieldDegree(m)===ans);
assert(homogeneousVectorFieldDegree(image(m))===ans);

assert(homogeneousVectorFieldDegree(matrix mutableMatrix(R,1,0))==={});
assert(homogeneousVectorFieldDegree(image matrix mutableMatrix(R,1,0))==={});

-- Test with a non-standard multidegree, too...
R=QQ[x,y,Degrees=>{{3},{1}}];
m=matrix {{x^2,1,0,x^2,x*y},
          {y^2,0,0,y^4,y^2}}
ans={false,{-3},-infinity,{3},{1}};
assert(homogeneousVectorFieldDegree(m)===ans);
assert(homogeneousVectorFieldDegree(image(m))===ans);

R=QQ[x,y,Degrees=>{{3,1},{1,1}}];
m=matrix {{x^2,1,0,x^2,x*y},
          {y^2,0,0,y^4,y^2}}
ans={false,{-3,-1},-infinity,false,{1,1}};
assert(homogeneousVectorFieldDegree(m)===ans);
assert(homogeneousVectorFieldDegree(image(m))===ans);
///

-- isHomogeneousVectorField
TEST ///
R=QQ[x];
m=matrix {{0_R,4_R,x,x^67,x^3+x}};
assert(not isHomogeneousVectorField(transpose m));

--ans={-infinity,{-1},{0},{66},false};
assert(isHomogeneousVectorField(m_{0,1,2}));
assert(isHomogeneousVectorField(m_{0}));
assert(isHomogeneousVectorField(m_{1}));
assert(isHomogeneousVectorField(m_{2}));
assert(isHomogeneousVectorField(m_{3}));
assert(not isHomogeneousVectorField(m_{4}));

assert(isHomogeneousVectorField(image m_{0,1,2}));
assert(isHomogeneousVectorField(image m_{0}));
assert(isHomogeneousVectorField(image m_{1}));
assert(isHomogeneousVectorField(image m_{2}));
assert(isHomogeneousVectorField(image m_{3}));
assert(not isHomogeneousVectorField(image m_{4}));

-- List
assert(not isHomogeneousVectorField(m_{0,1,2},{{0},{-1}}));
assert(not isHomogeneousVectorField(m_{0,1,2},{{0},-infinity}));
assert(isHomogeneousVectorField(m_{0,1,2},{{0},{-1},-infinity}));

assert(not isHomogeneousVectorField(image m_{0,1,2},{{0},{-1}}));
assert(not isHomogeneousVectorField(image m_{0,1,2},{{0},-infinity}));
assert(isHomogeneousVectorField(image m_{0,1,2},{{0},{-1},-infinity}));

-- Set
assert(not isHomogeneousVectorField(m_{0,1,2},set {{0},{-1}}));
assert(not isHomogeneousVectorField(m_{0,1,2},set {{0},-infinity}));
assert(isHomogeneousVectorField(m_{0,1,2},set {{0},{-1},-infinity}));

assert(not isHomogeneousVectorField(image m_{0,1,2},set {{0},{-1}}));
assert(not isHomogeneousVectorField(image m_{0,1,2},set {{0},-infinity}));
assert(isHomogeneousVectorField(image m_{0,1,2},set {{0},{-1},-infinity}));

-- depends on generators
assert(not isHomogeneousVectorField(image matrix {{x,x^3+x}}));
assert(isHomogeneousVectorField(trim image matrix {{x,x^3+x}}));

-- zero parameter
assert(isHomogeneousVectorField(matrix mutableMatrix(R,1,0)));
assert(isHomogeneousVectorField(image matrix mutableMatrix(R,1,0)));

-- Test with a non-standard multidegree, too...
R=QQ[x,y,Degrees=>{{3},{1}}];
m=matrix {{x^2,1,0,x^2,x*y},
          {y^2,0,0,y^4,y^2}}
--ans={false,{-3},-infinity,{3},{1}}
assert(not isHomogeneousVectorField(m_{0,1,2}));
assert(not isHomogeneousVectorField(m_{0}));
assert(isHomogeneousVectorField(m_{1}));
assert(isHomogeneousVectorField(m_{2}));
assert(isHomogeneousVectorField(m_{3}));

assert(not isHomogeneousVectorField(image m_{0,1,2}));
assert(not isHomogeneousVectorField(image m_{0}));
assert(isHomogeneousVectorField(image m_{1}));
assert(isHomogeneousVectorField(image m_{2}));
assert(isHomogeneousVectorField(image m_{3}));

assert(isHomogeneousVectorField(m_{1,2,3},{{-3},{3},-infinity}));
assert(not isHomogeneousVectorField(m_{1,2,3},{{-3},{3}}));
assert(not isHomogeneousVectorField(m_{1,2,3},{{-3},-infinity}));

assert(isHomogeneousVectorField(image m_{1,2,3},{{-3},{3},-infinity}));
assert(not isHomogeneousVectorField(image m_{1,2,3},{{-3},{3}}));
assert(not isHomogeneousVectorField(image m_{1,2,3},{{-3},-infinity}));
///

-- bracketing
-- Generate random, complicated vfs and check the bracket calc 
TEST ///
bracketTest = (vf1,vf2) -> (
	stdio<<"bracketTest: testing "<<toExternalString(vf1)<<" and "<<toExternalString(vf2)<<endl;
	br:=bracket(vf1,vf2);
	for x in flatten entries vars(ring(vf1)) do (
		assert(
			applyVectorField(vf1,applyVectorField(vf2,x))-applyVectorField(vf2,applyVectorField(vf1,x))===
			applyVectorField(br,x)  );
	);
);

-- Try a bunch of random vfs
R=QQ[x,y,z];
M=random(R^3,R^{10:-3});
for i from 0 when i<numColumns(M) do (
	for j from 0 when j<numColumns(M) do (
		bracketTest(M_{i},M_{j});
	);
);
///

-- Also do some brackets "by hand"
TEST ///
R=QQ[x,y];
px=matrix {{1_R},{0_R}};
py=matrix {{0_R},{1_R}};

-- generators commute
D=derlog(ideal (x));
-- generators commute
E=derlog(ideal (x*y));
-- generators commute, except [deg 0,deg 1]=deg 1
F=derlog(ideal (x*y*(x-y)));

I=ideal (x,y);

-- Test bracket(Matrix,Matrix)
assert(bracket(  px,  px)=== 0*px+0*py);
assert(bracket(  px,  py)=== 0*px+0*py);
assert(bracket(  px,0*px)=== 0*px+0*py);

assert(bracket(  px,x*px)=== 1*px+0*py);
assert(bracket(  px,x*py)=== 0*px+1*py);
assert(bracket(  px,y*px)=== 0*px+0*py);
assert(bracket(  px,y*py)=== 0*px+0*py);

assert(bracket(  py,  px)=== 0*px+0*py);
assert(bracket(  py,  py)=== 0*px+0*py);
assert(bracket(  py,0*px)=== 0*px+0*py);

assert(bracket(  py,x*px)=== 0*px+0*py);
assert(bracket(  py,x*py)=== 0*px+0*py);
assert(bracket(  py,y*px)=== 1*px+0*py);
assert(bracket(  py,y*py)=== 0*px+1*py);

assert(bracket(x*px,x*px)=== 0*px+0*py);
assert(bracket(x*px,x*py)=== 0*px+x*py);
assert(bracket(x*px,y*px)===-y*px+0*py);
assert(bracket(x*px,y*py)=== 0*px+0*py);

assert(bracket(x*py,x*px)=== 0*px-x*py);
assert(bracket(x*py,x*py)=== 0*px+0*py);
assert(bracket(x*py,y*px)=== x*px-y*py);
assert(bracket(x*py,y*py)=== 0*px+x*py);

assert(bracket(y*px,x*px)=== y*px+0*py);
assert(bracket(y*px,x*py)===-x*px+y*py);
assert(bracket(y*px,y*px)=== 0*px+0*py);
assert(bracket(y*px,y*py)===-y*px+0*py);

assert(bracket(y*py,x*px)=== 0*px+0*py);
assert(bracket(y*py,x*py)=== 0*px-x*py);
assert(bracket(y*py,y*px)=== y*px+0*py);
assert(bracket(y*py,y*py)=== 0*px+0*py);

m1=px|x*py;
m2=x*px|y*py;
assert(bracket(m1,m2)===px|0*px|(-x*py)|x*py);

assert(image bracket(gens D,gens E)==image (matrix {{0_R},{1_R}}));
assert(image bracket(gens D,gens F)==image (matrix {{0_R},{1_R}}));
assert(image bracket(gens E,gens F)==image (matrix {{0_R,0},{x*y,-y^2}}));

-- Test bracket(Matrix,Matrix,List)
assert(bracket(m1,m2,{(0,1),(1,0)})===(0*px)|(-x*py));
assert(bracket(m1,m2,{(1,0)})===-x*py);
assert(bracket(m1,m2,{{1,0}})===-x*py);
assert(bracket(m1,m2,{})===matrix mutableMatrix(R,2,0));

-- test bracket(Vector,Vector)
assert(bracket((y*py)_0,(x*px)_0)==(map(R^2,R^1,{{0},{0}}))_0);
assert(bracket((y*py)_0,(x*py)_0)==(map(R^2,R^1,{{0},{(-x)}}))_0);
assert(bracket((y*py)_0,(y*px)_0)==(map(R^2,R^1,{{y},{0}}))_0);
assert(bracket((y*py)_0,(y*py)_0)==(map(R^2,R^1,{{0},{0}}))_0);

-- Also test bracket(Module,Module)
-- Note that we must use == here, not ===
px=image px;
py=image py;
whatIsExpected = (a,b) -> image bracket(
  (gens a)|(gens ((ideal (x,y))*a))|(gens ((ideal (x^2,x*y,y^2))*a)),
  (gens b)|(gens ((ideal (x,y))*b))|(gens ((ideal (x^2,x*y,y^2))*b)));
testBracketModule = (a,b) -> bracket(a,b)==whatIsExpected(a,b);

assert(testBracketModule(  px,  px));
assert(testBracketModule(  px,  py));
assert(testBracketModule(  px,0*px));

assert(testBracketModule(  px,x*px));
assert(testBracketModule(  px,x*py));
assert(testBracketModule(  px,y*px));
assert(testBracketModule(  px,y*py));

assert(testBracketModule(  py,  px));
assert(testBracketModule(  py,  py));
assert(testBracketModule(  py,0*px));

assert(testBracketModule(  py,x*px));
assert(testBracketModule(  py,x*py));
assert(testBracketModule(  py,y*px));
assert(testBracketModule(  py,y*py));

assert(testBracketModule(x*px,x*px));
assert(testBracketModule(x*px,x*py));
assert(testBracketModule(x*px,y*px));
assert(testBracketModule(x*px,y*py));

assert(testBracketModule(x*py,x*px));
assert(testBracketModule(x*py,x*py));
assert(testBracketModule(x*py,y*px));
assert(testBracketModule(x*py,y*py));

assert(testBracketModule(y*px,x*px));
assert(testBracketModule(y*px,x*py));
assert(testBracketModule(y*px,y*px));
assert(testBracketModule(y*px,y*py));

assert(testBracketModule(y*py,x*px));
assert(testBracketModule(y*py,x*py));
assert(testBracketModule(y*py,y*px));
assert(testBracketModule(y*py,y*py));

-- calc
assert(bracket(D,E)==D);
assert(bracket(D,F)==D);
-- brute-force it.
assert(bracket(E,F)==bracket(E+I*E+I^2*E,F+I*F+I^2*F));
assert(bracket(E,F)==I*E);


-- test bracket(...involving something with zero columns...)
Z=matrix mutableMatrix(R,2,0);
assert(bracket(matrix {{x},{0}},Z)==Z);
assert(bracket(Z,matrix {{x},{0}})==Z);
assert(bracket(Z,Z)==Z);

assert(bracket(x*px,image Z)==image Z);
assert(bracket(image Z,x*px)==image Z);
assert(bracket(image Z,image Z)==image Z);

assert(bracket(D,image Z)==image Z);
assert(bracket(image Z,E)==image Z);
///

-- commutator
TEST ///
R=QQ[x,y];
-- generators commute
D=derlog(ideal (x));
-- generators commute
E=derlog(ideal (x*y));
-- generators commute, except [deg 0,deg 1]=deg 1
F=derlog(ideal (x*y*(x-y)));

I=ideal (x,y);

Z=image matrix mutableMatrix(R,2,0);

assert(image commutator(gens D)==Z);
assert(image commutator(gens E)==Z);
assert(image commutator(gens F)==image matrix {{0},{x*y-y^2}});

assert(commutator(D)==D);
assert(commutator(E)==I*E);
assert(commutator(F)==(image matrix {{0},{x*y-y^2}})+I*F);

-- zero columns
assert(image commutator(gens Z)==Z);
assert(commutator(Z)==Z);

-- commutator of gl_2 is sl_2
R=QQ[a,b,c,d];
E11=matrix {{a},{b},{0},{0}};
E12=matrix {{c},{d},{0},{0}};
E21=matrix {{0},{0},{a},{b}};
E22=matrix {{0},{0},{c},{d}};
assert(image commutator(E11|E12|E21|E22)==image ((E11-E22)|E12|E21));
assert(commutator(image (E11|E12|E21|E22))==
  (image ((E11-E22)|E12|E21))
  +(ideal (a,b,c,d))*(image (E11|E12|E21|E22)));
///

-- isLieAlgebra
TEST ///
R=QQ[a,b,c,d];
E11=matrix {{a},{b},{0},{0}};
E12=matrix {{c},{d},{0},{0}};
E21=matrix {{0},{0},{a},{b}};
E22=matrix {{0},{0},{c},{d}};

assert(isLieAlgebra(image (E11|E12|E21|E22)))
assert(isLieAlgebra(image (E11-E22|E12|E21)))
assert(isLieAlgebra(image (E11|E12|E22)))
assert(isLieAlgebra(image (E11|E21|E22)))
assert(isLieAlgebra(image (E11|E22)))
assert(isLieAlgebra(image (E11|E12)))
assert(isLieAlgebra(image (E11-E22)))
assert(not isLieAlgebra(image (E12|E21)))
assert(not isLieAlgebra(image (E11|E12|E21)))

assert(isLieAlgebra(image (E11|E12|E21|E22)))


-- gen by nothing
Z=matrix mutableMatrix(R,4,0);
assert(isLieAlgebra(image Z));
///


-- derivedSeries
TEST ///
R=QQ[a,b,c];
D=derlog(a*c-b^2);
I=ideal (a,b,c);

-- D is generated by a set of linear vfs, a rep of gl_2
--   Matrix version
L=derivedSeries(2,gens D);
assert(length(L)===3);
assert(image (L#0)==D);
assert(image (L#1)==image bracket(L#0,L#0));
assert(image (L#2)==image bracket(L#1,L#1));
--   Module version
L=derivedSeries(2,D);
assert(length(L)===3);
assert(L#0==D);
assert(L#1==bracket(D,D));
assert(L#2==L#1);

-- zeroth central series
L=derivedSeries(0,D);
assert(length(L)===1);
assert(L#0==D);

L=derivedSeries(0,gens D);
assert(length(L)===1);
assert(L#0==gens D);

-- zero gens
Z=matrix mutableMatrix(R,3,0);
L=derivedSeries(2,Z);
assert(length(L)===3);
assert(L#0==Z);
assert(L#1==Z);
assert(L#2==Z);

L=derivedSeries(2,image Z);
assert(length(L)===3);
assert(L#0==image Z);
assert(L#1==image Z);
assert(L#2==image Z);
///;

-- lowerCentralSeries 
TEST ///
R=QQ[a,b,c];
D=derlog(a*c-b^2);
I=ideal (a,b,c);
E=image matrix {{2*b,a,0},{c,0,a},{0,-c,2*b}};
F=image matrix {{0},{b},{2*c}};
assert(D==E+F);

-- D is generated by a set of linear vfs, a rep of gl_2
--   Matrix version
L=derivedSeries(2,gens D);
assert(length(L)===3);
assert(image (L#0)==D);
assert(image (L#1)==image bracket(gens(D),L#0));
assert(image (L#2)==image bracket(gens(D),L#1));
--   Module version
L=lowerCentralSeries(2,D);
assert(length(L)===3);
assert(L#0==D);
assert(L#1==E+I*F);
assert(L#2==L#1);

-- 0th central series
L=lowerCentralSeries(0,D);
assert(length(L)===1);
assert(L#0==D);

L=lowerCentralSeries(0,gens D);
assert(length(L)===1);
assert(L#0==gens D);

-- zero gens
Z=matrix mutableMatrix(R,3,0);
L=lowerCentralSeries(2,Z);
assert(length(L)===3);
assert(L#0==Z);
assert(L#1==Z);
assert(L#2==Z);

L=lowerCentralSeries(2,image Z);
assert(length(L)===3);
assert(L#0==image Z);
assert(L#1==image Z);
assert(L#2==image Z);
///


-- logarithmic vector fields
-- der, derlog{,V,H}
TEST ///
R=QQ[x,y,z];
I=ideal (x*y);
K=ideal (0_R);
L=ideal (1_R);

dI=gens derlog(I);
m=matrix {{x,0,0},{0,y,0},{0,0,1}};
assert(image dI==der(I,I));
assert(image dI==image m);
assert(image dI==derlog(x*y));

dK=gens derlog(K);
m=matrix {{1_R,0,0},{0,1,0},{0,0,1}};
assert(image dK==der(K,K));
assert(image dK==image m);
assert(image dK==derlog(0_R));

assert(image dK==der(L,L));
assert(image dK==image m);
assert(image dK==derlog(1_R));

kI=gens derlogH(flatten entries gens I);
m=matrix {{-x,0},{y,0},{0,1}};
assert(image kI==der({x*y},K));
assert(image kI==image m);
assert(image kI==derlogH(x*y));

assert(image (0*m)==der(I,K));

-- some additional tests for der that are not of the form der(I,I)
I=ideal (x*y);
J=ideal (y*z);
M=der(I,J);
-- should vanish on z, can ignore x? and tangent to V(y)
m=matrix {{z,0,0},{0,z*y,0},{0,0,z}};
assert(M==image m);

I=ideal (x);
J=ideal (x^2);
M=der(I,J);
m=matrix {{x^2,0,0},{0,x,0},{0,0,x}};
assert(M==image m);

M=der(J,I);
m=matrix {{1_R,0,0},{0,1,0},{0,0,1}};
assert(M==image m);

-- some tests for der(list,ideal)
M=der({},ideal (x));
m=matrix {{1_R,0,0},{0,1,0},{0,0,1}};
assert(M==image m);

M=der({x},ideal (x));
m=matrix {{x,0,0},{0,1,0},{0,0,1}};
assert(M==image m);

M=der({x},ideal (y));
m=matrix {{y,0,0},{0,1,0},{0,0,1}};
assert(M==image m);

M=der({x*y},ideal (y));
m=matrix {{1_R,0,0},{0,y,0},{0,0,1}};
assert(M==image m);

M=der({x,y},ideal (y));
m=matrix {{y,0,0},{0,y,0},{0,0,1}};

-- Can have an ideal with zero generators
I=ideal matrix mutableMatrix(R,1,0);
J=ideal (0_R);
assert(der(J,J)==der(I,I));
assert(der(J,J)==der(I,J));
assert(der(J,J)==der(J,I));
assert(derlog(J)==derlog(I));
assert(der(I,ideal(x))==der(J,J));
///

-- isLogarithmic
TEST ///
R=QQ[x,y,z];
f=x*z-y^2;
I=ideal (f);
M=gens derlog(I);

assert(isLogarithmic(M,I));
assert(isLogarithmic(M_{0,1},I));
assert(isLogarithmic(M_0,I));
assert(isLogarithmic(M_1,I));
assert(isLogarithmic(image M,I));
assert(not isLogarithmic(matrix {{2*z},{0},{x}},I));
assert(not isLogarithmic(image matrix {{2*z},{0},{x}},I));

I=ideal (x,y);
assert(isLogarithmic(matrix {{x},{y},{z}},I));
M=matrix {{x,0,0},{0,y,0},{0,0,z}};
assert(isLogarithmic(M_{0},I));
assert(isLogarithmic(M_{2},I));
assert(isLogarithmic(M_0,I));
assert(isLogarithmic(M_2,I));
assert(isLogarithmic(M,I));
assert(isLogarithmic(image M,I));
assert(not isLogarithmic(matrix {{z},{0},{0}},I));
assert(not isLogarithmic(matrix {{0},{z},{0}},I));
assert(not isLogarithmic(image matrix {{z},{0},{0}},I));
assert(not isLogarithmic(image matrix {{0},{z},{0}},I));

-- zero generators
Z=matrix mutableMatrix(R,3,0);
assert(isLogarithmic(Z,I));
assert(isLogarithmic(image Z,I));
///


-- Free divisors
-- isFreeDivisor
TEST ///
R=QQ[a,b,c];
runatest = (f,a)->(
	assert(isFreeDivisor(f)===a);
	M:=gens derlog(ideal (f));
	assert(isFreeDivisor(M)===a);
	assert(isFreeDivisor(image M)===a);
);

runatest(a*c-b^2,false);
runatest(a*(a*c-b^2),true);
runatest(a,true);
runatest(a*b,true);
runatest(1_R,true);
runatest(0_R,true);

M=matrix {{a,0,0},{0,a,0},{0,0,a}};
D=gens derlog(ideal (a));
assert(isFreeDivisor(M)===false);
assert(isFreeDivisor(D)===true);
assert(isFreeDivisor(M|D)===false);
assert(isFreeDivisor(image M)===false);
assert(isFreeDivisor(image D)===true);
assert(isFreeDivisor(image (M|D))===false);
assert(isFreeDivisor(trim image (M|D))===true);
assert(isFreeDivisor(gens trim image (M|D))===true);
-- We need the closure of the Lie bracket.
-- Here, image(M) is not a Lie algebra, but det(M) is reduced.
M=matrix {{a,0,0},{0,b,0},{0,b,c}};
assert(isFreeDivisor(M)===false);
assert(isFreeDivisor(image M)===false);
///;

-- Stratifications
-- stratifyByRank
TEST ///
R=QQ[a,b,c];
D=matrix {{a,0,0},{0,b,0},{0,0,c}};
S=stratifyByRank(D);
assert(S#1==ideal (a,b,c));
assert(S#2==intersect({ideal (a,b),ideal (a,c),ideal (b,c)}));
assert(S#3==ideal (a*b*c));

S=stratifyByRank(image D);
assert(S#1==ideal (a,b,c));
assert(S#2==intersect({ideal (a,b),ideal (a,c),ideal (b,c)}));
assert(S#3==ideal (a*b*c));

D=matrix {{a,0},{0,b},{0,0}};
S=stratifyByRank(D);
assert(S#1==ideal (a,b));
assert(S#2==ideal (a*b));
assert(S#3==ideal (0_R));

D=matrix {{a,0,0},{0,b,0},{0,0,1}};
S=stratifyByRank(D);
assert(S#1==ideal (1_R));
assert(S#2==ideal (a,b));
assert(S#3==ideal (a*b));

Z=matrix mutableMatrix(R,3,0);
S=stratifyByRank(Z);
assert(S#1==ideal (0_R));
assert(S#2==ideal (0_R));
assert(S#3==ideal (0_R));

R=QQ[a,b,c,d];
D=gens derlog(ideal (a*d-b*c));
S=stratifyByRank(D);
assert(S#1==ideal (a,b,c,d));
assert(S#2==ideal (a,b,c,d));
assert(S#3==ideal (a,b,c,d));
assert(S#4==ideal (a*d-b*c));
///

-- isFiniteStratification
TEST ///
R=QQ[x,y]
-- For tests, we don't need these to be lie algebras
 
-- anything with <2 generators should fail 
assert(not isFiniteStratification(stratifyByRank(matrix mutableMatrix(R,2,0))))
assert(not isFiniteStratification(stratifyByRank(matrix {{x},{y}})))
assert(not isFiniteStratification(stratifyByRank(matrix {{y},{-x}})))
assert(not isFiniteStratification(stratifyByRank(matrix {{1_R},{1}})))
-- normal crossings
assert(isFiniteStratification(stratifyByRank(matrix {{x,0},{0,y}})))
-- normal crossings + some other hyperplanes
assert(isFiniteStratification(stratifyByRank(matrix {{x*(x-1)*(x-2),0},{0,y}})))
-- a collection of central hyperplanes
assert(isFiniteStratification(stratifyByRank(derlog(ideal (x*y*(x-y)*(x+y))))))
-- rank 0 on x=0
assert(not isFiniteStratification(stratifyByRank(matrix {{x,0},{0,x*y}})))

R=QQ[x,y,z,w]
assert(isFiniteStratification(stratifyByRank(derlog(ideal (x*y*z*w)))))
assert(isFiniteStratification(stratifyByRank(derlog(ideal (x*y-z*w)))))
assert(isFiniteStratification(stratifyByRank(derlog(ideal (x*y-z^2)))))
-- These two have rank 0 on x=y=0
assert(not isFiniteStratification(stratifyByRank(derlog(ideal (x*y*(x-y)*(x-y*z))))))
assert(not isFiniteStratification(stratifyByRank(derlog(ideal (x*y*(x-y)*(x*w-y*z))))))
///

-- isHolonomic, isHHolonomic
TEST ///
R=QQ[a,b,c];

-- some special cases
assert(isHolonomic(0_R));
assert(isHHolonomic(0_R));
assert(isHolonomic(ideal (0_R)));

assert(isHolonomic(1_R));
assert(isHHolonomic(1_R));
assert(isHolonomic(ideal (1_R)));

-- Some easy singularities
assert(isHolonomic(a*c-b^2));
assert(isHolonomic(a*(a*c-b^2)));
assert(isHolonomic(a^2-b^2*c));
assert(isHolonomic(a*b*c));
assert(not isHolonomic(a*b*(a-b)*(a-b*c)));

assert(isHolonomic(ideal (a*c-b^2)));
assert(isHolonomic(ideal (a*(a*c-b^2))));
assert(isHolonomic(ideal (a^2-b^2*c)));
assert(isHolonomic(ideal (a*b*c)));
assert(not isHolonomic(ideal (a*b*(a-b)*(a-b*c))));

assert(isHHolonomic(a*c-b^2));
assert(isHHolonomic(a*(a*c-b^2)));
assert(isHHolonomic(a^2-b^2*c));
assert(isHHolonomic(a*b*c));
assert(not isHHolonomic(a*b*(a-b)*(a-b*c)));

-- More complicated examples from my dissertation
R=QQ[a,b,c,d,e,f];
m=matrix {{a,b,c},{b,d,e},{c,e,f}};
H=det(m);
Ha=sub(H,{a=>0_R});
Hf=sub(H,{f=>0_R});

--assert(isHolonomic(H));
--assert(isHolonomic(b*Ha));
assert(isHolonomic(c*Ha));
assert(not isHolonomic(b*c*Ha));
assert(isHolonomic(a*c*Hf));
assert(isHolonomic((a*d-b^2)*Hf));

assert(isHHolonomic(c*Ha));
assert(not isHHolonomic(b*c*Ha));
assert(isHHolonomic(a*c*Hf));
assert(isHHolonomic((a*d-b^2)*Hf));
///

end
