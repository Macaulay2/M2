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
         {\bf Conventions for cones}

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
         {\bf Cones}

         A cone can be described in two ways. On the one hand one can take a
         set of vectors $\{v_1,\dots,v_n\}$ in $\QQ^d$ and then take their
         positive hull, namely the set of all linear combinations $a_1\cdot
         v_1+\ldots+a_n\cdot v_n$ with positive coefficients $a_i\in\QQ_{\ge
         0}$.
         This is the decription of a cone via generators, the so-called {\it
         V-representation}. In M2 a cone can be made from generators with @TO
         coneFromVData@:

      Example
         V = matrix {{0,1,2},{1,1,1}}
         Cv = coneFromVData V

      Text
         Note that the generators are the {\bf columns} of the matrix {\tt V}.

         Alternatively one can describe a cone as the intersection of finitely
         many linear halfspaces. This is the so-called {\it H-representation}:
         Given finitely many vectors $\{h_1,\dots,h_m\}$ from $\QQ^d$, take the
         set of all vectors $x\in\QQ^d$ such that the scalar product $\langle
         x,h_i\rangle\ge 0$ is positive for all $i=1,\dots,m$. In M2 a cone can
         be made from halfspaces with @TO coneFromHData@:

      Example
         H = matrix {{1,0},{1,1},{-1,2}}
         Ch = coneFromHData H
         Cv == Ch

      Text
         Note that the hyperplanes are the {\bf columns} of the matrix {\tt H}.

         Further note that neither representation is unique. To get a minimal
         set of generators use @TO rays@, to get a minimal set of inequalities
         use @TO facets@:

      Example
         rays Ch
         facets Cv
         A = (facets Cv) * (rays Ch)

      Text
         It is an important convention that we require the scalar product of
         generators with inequalities to be positive, i.e. that all the entries
         of the matrix {\tt A} are positive. Other software and parts of
         mathematics may require this scalar product to be negative.



///

