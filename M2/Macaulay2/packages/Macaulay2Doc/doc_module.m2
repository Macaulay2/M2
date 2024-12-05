-- also includes: vector
-- TODO: (module, Ideal)

document {
     Key => module,
     Headline => "make or get a module"
     }

document { Key => (module, Ring),
     Usage => "module R",
     Inputs => {"R"},
     Outputs => {{"the free module of rank 1 over the ring R"}},
     EXAMPLE lines ///
     	  ZZ
	  module ZZ
     ///}

document { Key => (module, Vector),
     Headline => "the module of a vector",
     Usage => "module v",
     Inputs => {"v"},
     Outputs => {{"the module that contains the vector ", TT "v"}},
     "The class of ", TT "v", " is also equal to the module of ", TT "v", ".",
     EXAMPLE lines ///
     	  F = ZZ^4
	  v = F_2
	  module v
	  class v
     ///}

document {
     Key => {relations,(relations, Module)},
     Headline => "the defining relations",
     TT "relations M", " -- produce the relations defining a module M.",
     PARA{},
     "The relations are represented as a matrix, and if not stored
     in the module under M.relations, the matrix is understood to be
     empty.",
     PARA{},
     SeeAlso => {"generators","subquotient"}}

doc ///
  Key
    vector
    (vector, Module, List)
    (vector, Module, Matrix)
    (vector, Module, Number)
    (vector, Module, RingElement)
    (vector, List)
    (vector, Matrix)
    (vector, Number)
    (vector, RingElement)
  Headline
    make a vector
  Usage
    vector(M, x)
    vector x
  Inputs
    M:Module
    x:{List, Matrix, Number, RingElement}
  Outputs
    :Vector
  Description
    Text
      For any $R$-module $M$, there exists an isomorphism between
      $\operatorname{Hom}(R,M)$ and $M$ given by $f\mapsto f(1)$.
      Therefore, internally all @TO Vector@ objects representing
      elements of $M$ correspond to matrices with source $R^1$ and
      target $M$.  A vector may be constructed from such a matrix using
      @SAMP "vector"@.
    Example
      R = QQ[x,y,z]
      f = matrix {{x}, {y}, {z}}
      vector f
    Text
      Alternatively, $M$ may be specified if it differs from the target
      of the matrix.
    Example
      g = matrix {{1}, {2}, {3}}
      vector(R^3, g)
    Text
      If the matrix would have only one element, then that element may be
      given instead.  If the module is not provided, then the result will
      be an element of the free module of rank one of the ring of the given
      element.
    Example
      vector 2
      vector x
      vector(R^1, 2)
    Text
      Alternatively, a list of elements may be provided.  If the module is
      not specified, then the vector will be an element of a free module over
      a ring containing all the elements of the list.
    Example
      vector {1, 2, 3}
      vector {1, x, y}
      vector(R^3, {1, 2, 3})
///

document {
     Key => {super,(super, GradedModule),(super, Matrix),(super, Module),(super,Vector)},
     Headline => "get the ambient module",
     TT "super M", " -- yields the module that the module ", TT "M", " is a submodule of.",
     BR{},
     TT "super f", " -- if ", TT "f", " is a map whose target is a submodule 
     of ", TT "M", ", yields the composite of ", TT "f", " with the inclusion into ", TT "M", ".",
     PARA{},
     SeeAlso => { "cover", "ambient" }}
