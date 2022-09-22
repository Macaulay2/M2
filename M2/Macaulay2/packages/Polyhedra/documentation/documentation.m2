doc ///
   Key
      facets
      (facets,Cone)
		(facets,Polyhedron)
   Headline
      Giving the facet inequalities of a cone or polyhedron.
   Usage
      fC = facets C
		fP = facets P 
   Inputs
      C:Cone
      P:Polyhedron
   Outputs
      M:Matrix 
      L:List 
   Description
      Text
			For a cone one matrix is returned. 

			For a polyhedron the affine inequalities are returned as a 
			pair of two matrices, where the second matrix consists of a
			single row.
         
         Please see @TO "V- and H-representation"@ on the conventions we use for
         cones and polyhedra.


      Example
			fC = facets posOrthant 2
			fP = facets hypercube 2

///

doc ///
   Key
      (minimalNonFaces, Fan)
   Headline
      Giving the minimal non-faces of a fan..
   Usage
      S = minimalNonFaces Phi
   Inputs
      Phi:Fan
   Outputs
      S:List
        List of minimal non-faces of Phi
   Description
      Text
         Returns a List with the indices of the minimal non-faces of a fan.
      Example
         S = minimalNonFaces normalFan hypercube 2
///

doc ///
   Key
      (stanleyReisnerRing, Fan)
   Headline
      Give the Stanley–Reisner ring of a fan.
   Usage
      SR = stanleyReisnerRing Phi
   Inputs
      Phi:Fan
   Outputs
      SR:Ring
         The Stanley–Reisner Ring of Phi
   Description
      Text
         Returns the Stanley–Reisner Ring of a fan, whose variables are indexed by the rays.
      Example
         SR = stanleyReisnerRing normalFan hypercube 2
///

doc ///
   Key
      (faces, PolyhedralObject)
   Headline
      Giving the faces of a polyhedral object.
   Usage
      fk = faces PO
   Inputs
      PO:PolyhedralObject
   Outputs
      M:HashTable
   Description
      Text
         Returns a HashTable with the faces of a polyhedral object sorted
         by codimension. The faces are given as lists containing the indices of the
         rays/vertices of the original object contained in the face.

      Example
         fC = faces hypercube 2
         fC#1

///

doc ///
   Key
      linSpace
      (linSpace,Cone)
      (linSpace,Polyhedron)
      (linSpace,Fan)
   Headline
      Deprecated version of @TO "linealitySpace"@
///

doc ///
   Key
      latticeVolume
      (latticeVolume,Polyhedron)
   Headline
      Returning the lattice volume of a polyhedron.
   Usage
      v = latticeVolume P
   Inputs
      P:Polyhedron
   Outputs
      v:QQ
   Description
      Text
         The lattice volume describes how many unit simplices fit into the polyhedron.
         It is computed via a triangulation of the polyhedron and then summing over the
         volumes of the simplices.

      Example
         latticeVolume hypercube 3
///

doc ///
   Key
      facesAsPolyhedra
      (facesAsPolyhedra,ZZ,Polyhedron)
   Headline
      Returns the faces of a polyhedron as actual polyhedra.
   Usage
      fP = facesAsPolyhedra(n,P)
   Inputs
      P:Polyhedron
      n:ZZ
         codimension of the faces
   Outputs
      fP:List
   Description
      Text
         While @TO faces@ returns a list of lists of indices for the vertices that make
         up the codimension {\tt n} faces, this gives a list of actual polyhedra. It
         is just a small wrapper around @TO faces@. Whenever possible, use @TO faces@
         instead.

      Example
         facesAsPolyhedra(1, hypercube 2)
///

doc ///
   Key
      facesAsCones
      (facesAsCones,ZZ,Cone)
      (facesAsCones,ZZ,Fan)
   Headline
      Returns the faces of a cone as actual cones.
   Usage
      fC = facesAsCones(n,C)
   Inputs
      C:Cone
      n:ZZ
         codimension of the faces
   Outputs
      fC:List
   Description
      Text
         While @TO faces@ returns a list of lists of indices for the rays that make
         up the codimension {\tt n} faces, this gives a list of actual cones. It
         is just a small wrapper around @TO faces@. Whenever possible, use @TO faces@
         instead.

      Example
         L = facesAsCones(1, posOrthant 2)
         rays L#0
         rays L#1

      Example
         F = normalFan hypercube 2
         L = facesAsCones(1, F)
         rays L#0

///

doc ///
   Key
      (isWellDefined,Cone)
      (isWellDefined,Fan)
      (isWellDefined,Polyhedron)
      (isWellDefined,PolyhedralComplex)
      (isWellDefined,PolyhedralObject)
   Headline
      Checks whether a polyhedral object is well-defined.
   Usage
      b = isWellDefined PO
   Inputs
      PO:Cone
         or @ofClass Fan@
         or @ofClass Polyhedron@
         or @ofClass PolyhedralComplex@
   Outputs
      b:Boolean
   Description
      Text
         This method checks whether all description given and computed for a polyhedral
         object yield the same. 
         
         Furthermore, for a fan or polyhedral complex, it will
         check whether the intersection of two subobjects always yields a face.

         This method can be very expensive computationally.

      Example
         isWellDefined normalFan hypercube 3
///

doc ///
   Key
      (fan,Matrix,Matrix,List)
      (fan,Matrix,List)
      (fan,Matrix,Sequence)
      (fan,Matrix,Matrix,Sequence)
   Headline
      Constructing a fan.
   Usage
      F = fan(M,N,L)
   Inputs
      M:Matrix
         Matrix containing the rays as columns.
      N:Matrix
         Matrix containing generators of the lineality space as columns.
      L:List
         List containing lists with indices of the rays of the maximal cones.
   Outputs
      F:Fan
   Description
      Text
         Basic constructor for fans that takes a matrix containing the rays of the fan and
         a list of lists with the indices of the rays in the maximal cones. Optionally
         one may provide a lineality space.

         This constructor does not check well-definedness, see @TO isWellDefined@.

      Example
         M = matrix{{1,0},{0,1}}
         L = {{0,1}}
         F = fan(M,L)
         N = matrix{{1},{1}}
         F1 = fan(M,N,L)
///

doc ///
   Key
      (polyhedralComplex,Matrix,Matrix,Matrix,List)
      (polyhedralComplex,Matrix,Matrix,List)
      (polyhedralComplex,Matrix,List)
   Headline
      Constructing a polyhedral complex.
   Usage
      PC = polyhedralComplex(V,R,N,L)
   Inputs
      V:Matrix
         Matrix containing the vertices as columns
      R:Matrix
         Matrix containing rays as columns (optional)
      N:Matrix
         Matrix containing generators of the lineality space as columns (optional)
      L:List
         List containing lists with indices of the vertices of the maximal cells.
   Outputs
      PC:PolyhedralComplex
   Description
      Text
         Basic constructor for polyhedral complices that takes a matrix containing the
         vertices of the polyhedral complex and a list of lists with the indices of the
         vertices and rays in the maximal cells. Both the rays and the lineality space
         are optional arguments. If two matrices are provided, then the second matrix
         is considered to contain rays. To input a lineality space, one must provide
         three matrices.
         
         This constructor does not check well-definedness, see @TO isWellDefined@.

      Example
         M = matrix {{0,1,2}}
         L = {{0,1},{1,2}}
         PC = polyhedralComplex(M,L)
      
      Example
         C = hypercube 2
         F = faces(1,C)
         V = vertices C
         L = linealitySpace C
         PC = polyhedralComplex(V,L,F)
         vertices PC
         maxPolyhedra PC
         dim PC
///


doc ///
   Key
      coneFromHData
      (coneFromHData, Matrix)
      (coneFromHData, Matrix, Matrix)
   Headline
      Constructing a polyhedral cone as intersection of halfspaces.
   Usage
      C = coneFromHData H
      C = coneFromHData(H, E)
   Inputs
      H:Matrix
         Matrix containing the halfspaces as rows.
      E:Matrix
         Matrix containing equations as rows.
   Outputs
      C:Cone
   Description
      Text
         Basic constructor for a cone that takes one or two matrices. The cone
         consists of all points that evaluate positive with the rows of the
         first matrix and zero with the rows of the second matrix.
         
         Please see @TO "V- and H-representation"@ on the conventions we use for
         cones and polyhedra.
///


doc ///
   Key 
      regularSubdivision 
      (regularSubdivision,Polyhedron,Matrix)
      (regularSubdivision,Matrix,Matrix)
   Headline 
      Computes the regular cell decomposition
   Usage 
      L = regularSubdivision(P,w)
      L = regularSubdivision(M,w)
   Inputs
      P:Polyhedron
      w:Matrix 
         Row matrix containing the weights.
      M:Matrix 
         Matrix containing the points. 
   Outputs
      L:List
         List of polyhedra or 
         List of lists of indices indicated which points form a cell. 
   Description
      Text
         This function computes the regular subdivision of {\tt P} given by the
         weight vector {\tt w}.      This is computed by placing the i-th
         lattice point of {\tt P} on height {\tt w}_i in n+1 space, taking the
         convexHull of these with the ray (0,...,0,1), and projecting the
         compact faces into n space. Note that the polyhedron must be compact,
         i.e. a polytope and the length of the weight vector must be the number
         of lattice points.

         This function can also be used to compute the regular subdivision
         given a matrix {\tt M} of points and a weight vector {\tt w}. The
         points are lifted to the weights given by the matrix {\tt w}, and the
         lower envelope is computed. 
     
      Example
        P = crossPolytope 3
        w =  matrix {{1,2,2,2,2,2,1}}
        L = regularSubdivision(P,w)
        apply(L,vertices)

      Example 
         M = matrix {{1,0,1,0},{1,1,0,0}}; 
         w = matrix {{1,0,0,1}};
         S = regularSubdivision (M,w) 
///

doc ///
   Key
      regularTriangulation
      (regularTriangulation, Polyhedron)
   Headline
      Computes a regular triangulation of a given polytope.
   Usage
      T = regularTriangulation P
   Inputs
      P:Polyhedron
   Outputs
      T:List
   Description
      Text
         This method computes a regular triangulation of a polytope using the package {\tt Topcom.m2}. The output is a list of list of indices which vertices of {\tt P} give a simplex in the triangulation.

      Example
         T = regularTriangulation hypercube 2
///

doc ///
   Key
      simplex
      (simplex, ZZ)
      (simplex, ZZ, QQ)
      (simplex, ZZ, ZZ)
   Headline
      Produces a full-dimensional simplex
   Usage
      S = simplex d
      S = simplex(d, a)
   Inputs
      d:ZZ
         The dimension
      a:QQ
         The dilation factor
   Outputs
      S:Polyhedron
   Description
      Text
         Returns the {\tt d}-dimensional simplex that is the convex hull of the origin and the unit vectors in {\tt QQ^d}.
      Example
         S = simplex 2
         S = simplex(2,2)
///

doc ///
   Key
      polyhedronFromHData
      (polyhedronFromHData, Matrix, Matrix)
      (polyhedronFromHData, Matrix, Matrix, Matrix, Matrix)
   Headline
      Constructing a polyhedron from its H-representation, i.e. inequalities and equations
   Usage
      P = polyhedronFromHData(I, v)
      P = polyhedronFromHData(I, v, E, w)
   Inputs
      I: Matrix
         The inequalities
      v: Matrix
         The right hand side of the inequalities
      E: Matrix
         The equations
      w: Matrix
         The right hand side of the equations
   Outputs
      P: Polyhedron
   Description
      Text
         Constructs a polyhedron from inequalities and equations. Uses the
         negative halfspace defined by the equations, the normal vectors of
         every inequality point outside the polyhedron.

         The polyhedron is defined by $\{x\in\mathbb{R}\ |\ I * x\le v,\ E * x=w\}$.

         Please see @TO "V- and H-representation"@ on the conventions we use for
         cones and polyhedra.

      Example
         S = simplex 2
         facets S
         SCopy = polyhedronFromHData facets S
         assert(vertices S == vertices SCopy)

      Example
         S = stdSimplex 2
         facets S
         hyperplanes S
         SCopy = polyhedronFromHData(join(facets S, hyperplanes S))
         assert(vertices S == vertices SCopy)
///

doc ///
   Key
      fanFromGfan
      (fanFromGfan, List)
   Headline
      Construct a fan from output data of {\tt Gfan}
   Usage
      F = fanFromGfan L
   Inputs
      L: List
   Outputs
      F: Fan
   Description
      Text
         Method used by the package
         @ HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.12/share/doc/Macaulay2/Tropical/html/", "Tropical.m2") @ to construct a fan from {\tt Gfan} output.
         Gfan produces more data than just rays and maximal cones and this constructor will save that data to avoid recomputation.
///


doc ///
   Key 
      hypercube
      (hypercube,ZZ)
      (hypercube,ZZ,QQ)
      (hypercube,ZZ,ZZ)
      (hypercube,ZZ,QQ,QQ)
      (hypercube,ZZ,QQ,ZZ)
      (hypercube,ZZ,ZZ,QQ)
      (hypercube,ZZ,ZZ,ZZ)
   Headline 
      Returns the d-dimensional hypercube
   Usage
      P = hypercube d
      P = hypercube(d, s)
      P = hypercube(d, a, b)
   Inputs
      d: ZZ
         the dimension, a strictly positive integer
      s: QQ
         a positive rational scaling factor
      a: QQ
      b: QQ
   Outputs
      P: Polyhedron
   Description
      Text
         Produces the {\tt d}-dimensional hypercube $[-1,1]^d$. If provided with a scaling factor $s$, the hypercube $[-s,s]^d$ is returned. Alternatively one can specify the edge directly as $[a,b]$, then the hypercube $[a,b]^d$ is returned

      Example
         P = hypercube 3
         vertices P
      
      Example
         P = hypercube(3,2)
         vertices P

      Example
         P = hypercube(3,0,1)
         vertices P
///

doc ///
   Key
      (cone, Polyhedron)
   Headline
      Take the cone over a polyhedron
   Usage
      C = cone P
   Inputs
      P: Polyhedron
   Outputs
      C: Cone
   Description
      Text
         The polyhedron is embedded at height one, then the cone is taken over it.

      Example
         P = hypercube 2
         vertices P
         C = cone P
         rays C
///

doc ///
   Key
      polyhedron
      (polyhedron, Cone)
   Headline
      Turn a cone into a polyhedron
   Usage
      P = polyhedron C
   Inputs
      C: Cone
   Outputs
      P: Polyhedron
   Description
      Text
         Every cone is naturally a polyhedron with a single vertex, the origin, and the rays originating from the origin. This method converts a cone into a polyhedron.

      Example
         C = posOrthant 2
         rays C
         P = polyhedron C
         vertices P
         rays P
///

doc ///
   Key
      nVertices
      (nVertices, Polyhedron)
   Headline
      Returns the number of vertices of a polyhedron
   Usage
      n = nVertices P
   Inputs
      P: Polyhedron
   Outputs
      n: ZZ
   Description
      Text
         Returns the number of vertices of a polyhedron

      Example
         C = hypercube 2
         nVertices C
///

doc ///
   Key
      isFullDimensional
      (isFullDimensional, PolyhedralObject)
   Headline
      Determine whether a polyhedral object is full-dimensional
   Usage
      b = isFullDimensional PO
   Inputs
      PO: PolyhedralObject
   Outputs
      b: Boolean
   Description
      Text
         Checks whether the dimension of the polyhedral object is equal to its ambient dimension.

      Example
         C = hypercube 2
         dim C
         ambDim C
         isFullDimensional C

      Example
         F = normalFan hypercube 2
         dim F
         ambDim F
         isFullDimensional F

      Example
         S = stdSimplex 2
         vertices S
         dim S
         ambDim S
         isFullDimensional S
///

doc ///
   Key
      (fan, PolyhedralComplex)
   Headline
      Take the fan over a polyhedral complex
   Usage
      F = fan PC
   Inputs
      PC: PolyhedralComplex
   Outputs
      F:Fan
   Description
      Text
         The polyhedral complex is embedded at height one and the fan is taken over it.
      Example
         C = hypercube 2
         F = faces(1,C)
         V = vertices C
         L = linealitySpace C
         PC = polyhedralComplex(V,L,F)
         vertices PC
         maxPolyhedra PC
         dim PC
///

doc ///
   Key
      (polyhedralComplex, Fan)
   Headline
      Turn a fan into a polyhedral complex
   Usage
      PC = polyhedralComplex F
   Inputs
      F: Fan
   Outputs
      PC: PolyhedralComplex
   Description
      Text
         Every fan is naturally a polyhedral complex, since every cone is naturally a polyhedron. This method converts a fan into a polyhedral complex.

      Example
         F = normalFan hypercube 2
         rays F
         maxCones F
         PC = polyhedralComplex F
         vertices PC
         rays PC
         maxPolyhedra PC
///


