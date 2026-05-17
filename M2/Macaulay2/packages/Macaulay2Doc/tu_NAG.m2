-- {\it Numerical algebraic geometry} uses techniques from numerical analysis
-- to study and approximate solutions of systems of polynomial equations.
--
-- Traditionally, polynomial systems are studied using symbolic methods
-- such as Gr\"obner bases, resultants, and elimination techniques.
-- While these methods are powerful, they often become prohibitively slow
-- for large or complicated systems.
-- Numerical algebraic geometry provides an alternative by replacing exact symbolic computations
-- with high-precision numerical approximations.



-----------------------------------------------
-- A. Homotopy continuation
-----------------------------------------------
-- The central tool in numerical algebraic geometry is {\it homotopy continuation}.
-- This method constructs a homotopy between two polynomial systems
-- and tracks solutions from one system to the other.
--
-- Typically, one system is chosen so that its solutions are already known
-- or easy to compute; the {\it start system}.
-- The other system is the polynomial system of interest, called the {\it target system}.
--
-- One common constructions is the {\it straight-line homotopy}.
-- Given a target system $f$ and a start system $g$, the homotopy is defined by
-- $H(x,t) = (1-t)g(x) + tf(x)$.
-- Starting from a known solution $x^*$ of $g$ at $t=0$,
-- it tracks the solution path as $t$ moves toward $1$,
-- approximating a solution of $f$.
--
-- The package {\tt NumericalAlgebraicGeometry} provides fundamental tools
-- in numerical algebraic geometry.

--^
needsPackage "NumericalAlgebraicGeometry"
--$

R = CC[x,y]
g = {x^2-1,y^2-1}
f = {x^2+y^2-1, (x-y)^2-1}
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)}
track(g,f,solsS)

-- In this example, the start system is a {\it B\'ezout start system}.
-- giving a total number of paths equal to the product of the degrees
-- of the defining polynomials. This number is an upper bound
-- for the number of isolated solutions of the target system.

-- The same computation can also be simply carried out using:
--^
R = CC[x,y];
--$

f = {x^2+y^2-1, (x-y)^2-1};
solveSystem f


-----------------------------------------------
-- B. Witness sets and numerical irreducible decomposition
-----------------------------------------------
-- The idea of homotopy continuation for finding a solution to zero-dimensional systems
-- extends to the study positive-dimensional varieties.
-- The key observation is that intersecting a positive-dimensional variety
-- with codimension-many generic linear slices produces a zero-dimensional variety.
--
-- The data consisting of a polynomial system, a collection of generic linear slices,
-- and the resulting finite set of points is called a {\it witness set}.
-- Witness sets provide a numerical representation of
-- positive-dimensional algebraic varieties and can be used to compute:
--
-- 1. dimension
--
-- 2. degree
--
-- 3. sampling from a variety
--
-- 4. irreducible decomposition of a variety
--
-- This computation can be carried out using:
--^
R = CC[x,y,z];
--$
 
I = ideal {(x^2+y^2+z^2-1)*(y-x^2), (x^2+y^2+z^2-1)*(z-x^3)};
W = components numericalIrreducibleDecomposition I -- the variety consists of two components
sample first W -- sampling a random point on the first component
