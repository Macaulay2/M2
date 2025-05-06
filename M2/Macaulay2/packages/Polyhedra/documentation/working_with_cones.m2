doc ///
   Key
      "Working with cones"
   Description
      Text
         Every cone can be described via generating rays or via inequalities.
         The description via rays (or vertices for polyhedra) is often referred
         to as the V-representation. The description via inequalities is
         called the H-representation. To create a cone in 2-space which
         is the positive hull of a given set of rays use @TO coneFromVData@:

      Example
         R = matrix {{1,1,2},{2,1,1}}
         C = coneFromVData R
         ambDim C

      Text
         After creating the cone, one can use @TO rays@ to ask for its minimal
         rays.

      Example
         rays C

      Text
         We see that (1,1) is not an extremal ray of the cone.

      Example
         HS = facets C
         hyperplanes C
         isFullDimensional C

      Text
         The function @TO facets@ gives the defining linear half-spaces, the
         H-representation, i.e.  {\tt C} is given by all {\tt p} in the
         defining linear hyperplanes that satisfy {\tt HS*p >= 0}. In this case
         there are no hyperplanes, so the cone is of full dimension. The
         rows of the matrix {\tt HS} are the inner normals of the cone.
         Furthermore, we can construct the positive hull of a set of rays and a
         linear subspace.

      Example
         R1 = R || matrix {{0,0,0}}
         LS = matrix {{1},{1},{1}}
         C1 = coneFromVData(R1,LS)
         rays C1

      Text
         Note that the rays are given modulo the lineality space. On the other
         hand we can construct cones as the intersection of linear half-spaces
         and hyperplanes. The first argument of @TO coneFromHData@ takes the
         inequalities defining the cone, while the second takes equations.

      Example
         HS = transpose R1
         equations = matrix {{1,1,1}}
         C2 = coneFromHData(HS,equations)
         dim C2
         ambDim C2

      Text
         This is a two dimensional cone in 3-space with the following rays:

      Example
         rays C2

      Text
         If we don't intersect with the hyperplane we get a full dimensional
         cone.

      Example
         C3 = coneFromHData HS
         rays C3
         linealitySpace C3
         isFullDimensional C3

      Text
         Again, the rays are given modulo the lineality space. Also, one can
         use given cones, for example the positive orthant (@TO posOrthant@):

      Example
         C4 = posOrthant 3
         rays C4

      Text
         Now that we can construct cones, we can turn to the functions that can
         be applied to cones. First of all, we can apply the @TO intersection@
         function also to a pair of cones in the same ambient space:

      Example
         C5 = intersection(C1,C2)
         rays C5
         dim C5

      Text
         On the other hand, we can take their positive hull by using @TO
         coneFromVData@:

      Example
         C6 = coneFromVData(C1,C2)
         rays C6
         linealitySpace C6

      Text
         Furthermore, both functions (@TO coneFromHData@ and @TO
         coneFromVData@) can be applied to a list containing any number of
         cones and matrices defining rays and lineality space or linear
         half-spaces and hyperplanes. These must be in the same ambient space.
         For example:

      Example
         R2 = matrix {{2,-1},{-1,2},{-1,-1}}
         C7 = coneFromVData {R2,C3,C4}
         rays C7
         linealitySpace C7

      Text
         Taking the positive hull of several cones is the same as taking their
         Minkowski sum, so in fact:

      Example
         C6 == C1 + C2

      Text
         We can also take the Minkowski sum of a cone and a polyhedron. For
         this, both objects must lie in the same ambient space and the
         resulting object is then a polyhedron:

      Example
         P = crossPolytope 3
         P1 = C6 + P
         (vertices P1,rays P1)

      Text
         Furthermore, we can take the direct product
         (@TO (directProduct, Cone, Cone)@) of two cones.

      Example
         C8 = C * C1
         rays C8
         linealitySpace C8
         ambDim C8

      Text
         The result is contained in ${\mathbb Q}^5$.

      Example
         ambDim C8

      Text
         To find out more about this cone use for example @TO fVector@:

      Example
         fVector C8

      Text
         This function gives the number of faces of each dimension, so it has 1
         vertex, the origin, 1 line, 4 two dimensional faces and so on. We can
         access the faces of a certain codimension via @TO faces@. The output
         of @TO faces@ is a list of list of indices that indicate which rays
         form a face. The following shows how to get the corresponding rays of
         the faces.

      Example
         L = faces(1,C8)
         raysC8 = rays C8
         apply(L, l -> raysC8_l)

      Text
         We can also check if the cone is smooth:

      Example
         isSmooth C8

      Text
         Finally, there is also a function to compute the dual cone, i.e.  the
         set of all points in the dual space that are positive on the cone.

      Example
         C9 = dualCone C8
         rays C9
///

