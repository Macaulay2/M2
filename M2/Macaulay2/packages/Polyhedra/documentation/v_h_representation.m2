doc ///
   Key
      "V- and H-representation"
   Description
      Text
         {\bf Short summary and conventions}

         Both cones and polyhedra can be described either by giving generators,
         the so-called {\it V-representation} or by giving inequalities, the
         so-called {\it H-representation}. We have the following conventions:

         1. Rays, vertices, and generators of the lineality space are given as {\bf columns} of matrices.

         2. Inequalities and hyperplanes are given as {\bf rows} of matrices.

         3. The inequality description of a cone is $A\cdot x\ge 0$.

         4. The inequality description of a polyhedron is $A\cdot x\le b$.

      Text
         {\bf Conventions for cones}
         
         For cones we have the convention that the scalar product of generators
         with inequalities is {\bf positive}:
      
      Example
         C = coneFromVData matrix {{1,0,0},{1,1,0},{1,0,1},{1,1,1}}
         rays C
         facets C
         (facets C) * (rays C)

      Text
         The @TO hyperplanes@ of a cone evaluate to zero with the @TO rays@
         of a cone, just like the @TO linealitySpace@ evaluates to zero with
         the @TO facets@.

      Example
         (hyperplanes C) * (rays C)
         (facets C) * (linealitySpace C)

      Text
         {\bf Conventions for polyhedra}

         For a polyhedron the situation is slightly different, as we have a
         right hand side to take into account, since we are dealing with affine
         hyperplanes instead of just hyperplanes.

      Example
         P = hypercube(2,0,1)
         V = vertices P
         (A, b) = facets P
         A * V

      Text
         The convention is that for any point $p$ in the polyhedron we have
         $A\cdot p\le b$. This means we have $0\le b - A\cdot p$. Again, this
         may be handled differently elsewhere.

      Example
         for i from 0 to numColumns V - 1 do (
               test := b - A*V_{i};
               << "Vertex " << i << " " << (flatten entries V_{i}) << ": " << all(flatten entries test, e -> e>= 0) << endl;
            )

      Text
         From the above convention it follows that the @TO facets@ evaluate
         negatively with the @TO rays@ and @TO linealitySpace@ of a polyhedron.
         Conversely to @TO hyperplanes@ evaluate to constants on the @TO
         vertices@ of a polyhedron.

      Example
         P = convexHull(matrix{{1,0},{0,1},{2,2}}, matrix {{1},{1},{2}})
         vertices P
         rays P
         (A, b) = facets P
         A * (vertices P)
         A * (rays P)
         (E, v) = hyperplanes P
         E * (rays P)
         E * (vertices P)
            
      Text
         {\bf Full representations}

         1. The pair (@TO rays@, @TO linealitySpace@) is a valid V-representation of a cone.

         2. The pair (@TO facets@, @TO hyperplanes@) is a valid H-representation of a cone.

         3. The triple (@TO vertices@, @TO rays@, @TO linealitySpace@) is a valid V-representation of a polyhedron.
         
         4. The triple (@TO facets@, @TO hyperplanes@) is a valid H-representation of a polyhedron.

         That means we have the following identities:

      Example
         C == coneFromVData(rays C, linealitySpace C)
         C == coneFromRays(rays C, linealitySpace C)
         C == coneFromHData(facets C, hyperplanes C)
         C == coneFromInequalities(facets C, hyperplanes C)
         P == convexHull (vertices P, rays P, linealitySpace P)
         F = facets P
         H = hyperplanes P
         P == polyhedronFromHData(F#0, F#1, H#0, H#1)
         P == polyhedronFromInequalities(F#0, F#1, H#0, H#1)

   SeeAlso
      coneFromVData
      coneFromHData
      convexHull
      polyhedronFromHData
      facets
      hyperplanes
      linealitySpace
      (rays, PolyhedralObject)
      vertices


///

