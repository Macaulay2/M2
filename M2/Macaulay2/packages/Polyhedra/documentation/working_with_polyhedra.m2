doc ///
   Key 
      "Working with polyhedra"
   Description
      Text
         Just like cones, polyhedra have two descriptions. One description as
         the convex hull of finitely many points (and optionally rays and
         lineality), the V-representation. Another description as the
         intersection of finitely many half-spaces, the H-representation.
         Using the method @TO convexHull@ we can create a polyhedron in 2-space
         which is the @TO convexHull@ of a given set of points.

      Example 
         V = matrix {{0,2,-2,0},{-1,1,1,1}}
         P = convexHull V

      Text
         @TO Polyhedra@ uses the principle of lazy evaluation: Properties of
         the combinatorial objects are only computed on demand and then they
         are stored with the object. For example we can ask for the vertices of
         {\tt P} using @TO vertices@:

      Example
         vertices P

      Text
         Here we see that the point (0,1) is not a vertex and {\tt P} is
         actually a triangle.

      Example
         (HS,v) = facets P

      Text
         This gives the defining affine half-spaces, i.e. {\tt P} is given by
         all {\tt p} such that {\tt HS*p <= v} and that lie in the defining
         affine hyperplanes. The rows of the matrix {\tt HS} are the outer
         normals of the polyhedron {\tt P}. To get the defining hyperplanes we
         use:

      Example
         hyperplanes P

      Text
         There are none, so the polyhedron is of full dimension. It is also
         compact, since {\tt P} has no rays and the lineality space is of
         dimension zero.

      Example
         isFullDimensional P
         ambDim P
         dim P
         rays P
         linealitySpace P

      Text
         Internally, polyhedra are realized as cones, by embedding the
         polyhedron at height one and then taking the positive hull. To get at
         this cone, use @TO cone@. The height is the first coordinate of the
         rays of the cone, comparing the matrices of @TO rays@ and @TO
         vertices@ for the example one can see the correspondence:

      Example
         C = cone P
         rays C
         vertices P

      Text
         We can also construct the convex hull of a set of points and a
         set of rays.

      Example
         R = matrix {{1},{0},{0}}
         V1 = V || matrix {{1,1,1,1}}
         P1 = convexHull(V1,R)
         vertices P1

      Text
         This polyhedron is not compact anymore and also not of full dimension.

      Example
         isCompact P1
         isFullDimensional P1
         rays P1
         hyperplanes P1

      Text
         On the other hand we can construct a polyhedron as the intersection of
         affine half-spaces and affine hyperplanes, given via inequalities and
         equations:

      Example
         inequalities = transpose (V || matrix {{-1,2,0,1}})
         v = matrix {{1},{1},{1},{1}}
         equations = matrix {{1,1,1}}
         w = matrix {{3}}
         P2 = polyhedronFromHData(inequalities,v,equations,w)

      Text
         This is a triangle in 3-space with the following vertices.

      Example
         isFullDimensional P2
         vertices P2

      Text
         If we don't intersect with the hyperplane we get a full dimensional
         polyhedron.

      Example
         P3 = polyhedronFromHData(inequalities,v)
         vertices P3
         linealitySpace P3
         isFullDimensional P3

      Text
         Note that the vertices are given modulo the lineality space. Besides
         constructing polyhedra by hand, there are also some basic polyhedra
         implemented such as the @TO hypercube@, in this case with edge-length
         four.

      Example
         P4 = hypercube(3,2)
         vertices P4

      Text
         Another on is the @TO crossPolytope@, in this case with diameter six. 

      Example
         P5 = crossPolytope(3,3)
         vertices P5

      Text
         Furthermore the standard simplex (@TO stdSimplex@).

      Example
         P6 = stdSimplex 2
         vertices P6

      Text
         Now that we can construct polyhedra, we can turn to the functions that
         can be applied to polyhedra. First of all, we can apply the @TO
         convexHull@ function also to a pair of polyhedra:

      Example
         P7 = convexHull(P4,P5)
         vertices P7

      Text
         Or we can intersect them by using @TO intersection@:

      Example
         P8 = intersection(P4,P5)
         vertices P8

      Text
         Furthermore, both functions can be applied to a list containing any
         number of polyhedra and matrices defining vertices/rays or affine
         half-spaces/hyperplanes.  All of these must be in the same ambient
         space. For example:

      Example
         P9 = convexHull {(V1,R),P2,P6}
         vertices P9

      Text
         Further functions are for example the Minkowski sum (@TO
         minkowskiSum@) of two polyhedra.

      Example
         Q = convexHull (-V)
         P10 = P + Q
         vertices P10

      Text
         In the other direction, we can also determine all Minkowski summands
         (see @TO minkSummandCone@) of a polyhedron.

      Example
         (C,L,M) = minkSummandCone P10
         apply(values L, vertices)

      Text
         Here the polyhedra in the hash table {\tt L} are all possible
         Minkowski summands up to scalar multiplication and the columns of {\tt
         M} give the minimal decompositions. So the hexagon {\tt P10} is not
         only the sum of two triangles but also the sum of three lines.
         Furthermore, we can take the direct product of two polyhedra.

      Example
         P11 = P * Q
         vertices P11

      Text
         The result is in QQ^4.

      Example
         ambDim P11

      Text
         To find out more about this polyhedron use for example.

      Example
         fVector P11

      Text
         The function @TO fVector@ gives the number of faces of each dimension,
         so it has 9 vertices, 18 edges and so on. We can access the faces of a
         certain codimension via:

      Example
         L = faces(1,P11)
         vertP11 = vertices P11
         apply(L, l -> vertP11_(l#0))

      Text
         We can compute all lattice points of the polyhedron with @TO
         latticePoints@.

      Example
         L = latticePoints P11
         #L

      Text
         Evenmore the tail/recession cone of a polyhedron with @TO tailCone@.

      Example
         C = tailCone P1
         rays C

      Text
         Finally, there is also a function to compute the polar of a
         polyhedron, i.e. all points in the dual space that are greater than -1
         on all points of the polyhedron:

      Example
         P12 = polar P11
         vertices P12
///

