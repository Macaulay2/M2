-- TerraciniLoci - Macaulay2 package for computing the Terracini locus
-- of a projective variety

-- Copyright (c) 2023-2025 Francesco Galuppi, Pierpaola Santarsiero,
-- Doug Torrance, and Ettore Teixeira Turatti

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301, USA.

--

-- This work is partially supported by the Thematic Research Programme
-- "Tensors: geometry, complexity and quantum entanglement", University
-- of Warsaw, Excellence Initiative – Research University and the Simons
-- Foundation Award No. 663281 granted to the Institute of Mathematics of
-- the Polish Academy of Sciences for the years 2021-2023. Work was begun
-- during the "Geometry of secants" workshop during AGATES
-- (https://agates.mimuw.edu.pl/).

newPackage("TerraciniLoci",
    Headline => "Terracini loci of projective varieties",
    Version => "0.3",
    Date => "May 10, 2025",
    Authors => {
	{
	    Name => "Francesco Galuppi",
	    Email => "galuppi@mimuw.edu.pl"},
	{
	    Name => "Pierpaola Santarsiero",
	    Email => "pierpaola.santarsiero@unibo.it"},
	{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"},
	{
	    Name => "Ettore Teixeira Turatti",
	    Email => "ettore.t.turatti@uit.no"}},
    HomePage => "https://github.com/d-torrance/terracini-loci",
    Keywords => {"Projective Algebraic Geometry"},
    PackageImports => {
	"CorrespondenceScrolls",
	"FastMinors",
	"MinimalPrimes"})

---------------
-- ChangeLog --
---------------

-*

0.3 (2025-05-10, M2 1.25.05)
* add citation information
* update link from arXiv -> DOI
* update author email addresses

0.2 (2024-10-19, M2 1.24.11)
* stop exporting "Threads" symbol from FastMinors; now exported by Core

0.1 (2023-11-16, M2 1.23)
* initial release

*-

export {
    "terraciniLocus"
    }

importFrom("Core", {"concatRows"})

terraciniLocus = method(Options => {Threads => 0})

terraciniLocus(ZZ, Matrix, Ideal) := o -> (r, A, I) -> (
    if r < 1 then error "expected positive integer";
    if ring A =!= ring I then error "expected rings to agree";
    R := ring A;
    s := numRows A;
    t := numColumns A;
    rk := if zero I then rank A else codim I;
    n := numgens R - 1;
    Q := productOfProjectiveSpaces(toList(r : n),
	CoefficientField => coefficientRing R,
	VariableName => "z");
    if r == 1 then return ideal 1_Q;
    opts := apply(r, i -> apply(n + 1, j -> R_j => Q_((n + 1) * i + j)));
    Az := concatRows apply(r, i -> sub(A, opts#i));
    Ir := ideal apply(r, i -> sub(I, opts#i));
    result := trim(recursiveMinors(min(r * rk, t), Az, o) + Ir);
    Z := genericMatrix(Q, n + 1, r);
    duplicate := intersect apply(subsets(r, 2), ij ->
	recursiveMinors(2, Z_ij, o));
    result = saturate(result, duplicate);
    blocksingular := recursiveMinors(rk, A, o);
    singular := intersect apply(r, i -> sub(blocksingular, opts#i));
    radical result : radical singular)

terraciniLocus(ZZ, RingMap) := o -> (r, f) -> (
    terraciniLocus(r, jacobian matrix f, ideal 0_(target f), o))

terraciniLocus(ZZ, Ideal) := o -> (r, I) -> (
    terraciniLocus(r, transpose jacobian I, I, o))

beginDocumentation()

doc ///
  Key
    TerraciniLoci
  Headline
    package for computing Terracini loci
  Description
    Text
      This package implements the algorithms from Section 8 of the paper
      @HREF("https://doi.org/10.1142/S0219199725500531",
	  "Geometry of first nonempty Terracini loci")@
      by F. Galuppi, P. Santarsiero, D. Torrance, and E. Turatti.

      The Terracini locus of projective variety $X$ is a subvariety of
      the symmetric power $X^{(r)}$ containing the closure of all
      sets $\{p_1,\ldots,p_r\}$ of smooth points in $X$ for which the space
      $\langle T_{p_1}X,\ldots,T_{p_r}X\rangle$ has less than the expected
      dimension.

      This package exports one method, @TO terraciniLocus@, for computing the
      ideals of these varieties.
  Citation
    @article{Galuppi_2025,
      title={Geometry of First Nonempty Terracini Loci},
      ISSN={1793-6683},
      url={http://dx.doi.org/10.1142/S0219199725500531},
      DOI={10.1142/s0219199725500531},
      journal={Communications in Contemporary Mathematics},
      publisher={World Scientific Pub Co Pte Ltd},
      author={Galuppi, Francesco and Santarsiero, Pierpaola and Torrance, Douglas A. and Turatti, Ettore Teixeira},
      year={2025},
      month=apr }
///

doc ///
  Key
    terraciniLocus
    (terraciniLocus, ZZ, Matrix, Ideal)
    (terraciniLocus, ZZ, RingMap)
    (terraciniLocus, ZZ, Ideal)
    [terraciniLocus, Threads]
  Headline
    compute the Terracini locus of a projective variety
  Usage
    terraciniLocus(r, X)
  Inputs
    r:ZZ
    X:{RingMap,Ideal}
    Threads => ZZ
      the number of threads used during the computation.  This option
      is passed to @TO "FastMinors::recursiveMinors"@.
  Outputs
    :Ideal
  Description
    Text
      There are two methods to compute the Terracini locus of a
      projective variety.

      First, consider a rational variety parametrized by a polynomial
      map $f:\mathbb P^n\dashrightarrow\mathbb P^m$.  In Macaulay2,
      this may be represented using a @TO RingMap@ object from the
      coordinate ring of $\mathbb P^m$ to the coordinate ring of
      $\mathbb P^n$.  We consider the twisted cubic in $\mathbb P^3$.
    Example
      R = QQ[s,t]
      S = QQ[x_0..x_3]
      f = map(R, S, {s^3, s^2*t, s*t^2, t^3})
    Text
      In this case, the ideal of the preimage of the Terracini locus
      in $(\mathbb P^n)^r$ is returned.  So in our twisted cubic
      example, if $r=2$, then we get the ideal of the pairs of points
      in $\mathbb P^1\times\mathbb P^1$ whose images under $f$ belong
      to the 2nd Terracini locus.
    Example
      terraciniLocus(2, f)
    Text
      We see that the Terracini locus is empty, which is true for all
      rational normal curves.

      We may also consider varieties in $\mathbb P^n$ defined by an
      ideal.  Let us continue with the twisted cubic example.
    Example
      I = ker f
    Text
      In this case, we may only use $r=2$.  The ideal of the pairs of
      points in $\mathbb P^n\times\mathbb P^n$ belonging to the
      Terracini locus is returned.  So for the twisted cubic, we get
      an ideal in the coordinate ring of $\mathbb P^3\times\mathbb P^3$.
    Example
      terraciniLocus(2, I)
    Text
      For more examples, see
      @HREF "https://github.com/d-torrance/terracini-loci"@.
///

-----------
-- tests --
-----------
-- just the faster (< 1s) examples

TEST ///
-- rational normal curves
needsPackage "Resultants"
assertEmptyTerracini = (r, f) -> assert Equation(terraciniLocus(r, f), 1)

-- ring map
assertEmptyTerracini(2, veronese(1, 3))
assertEmptyTerracini(2, veronese(1, 4))

-- ideal (slower)
assertEmptyTerracini(2, ker veronese(1, 3))

-- also check Threads option
assert Equation(terraciniLocus(2, veronese(1, 3), Threads => 2), 1)
///

TEST ///
-- del pezzo surfaces
delPezzoSurface = t -> (
    kk := ZZ/32003;
    d := 9 - t;
    (x, y) := (symbol x, symbol y);
    R := kk[y_0..y_2];
    S := kk[x_0..x_d];
    P := intersect \\ ideal \ {
	{y_0, y_1}, {y_0, y_2}, {y_1, y_2}, {y_0 - y_1, y_0 - y_2}
	}_{0..t - 1};
    map(R, S, super basis(3, P)))

assert Equation(
    apply(primaryDecomposition terraciniLocus(2, delPezzoSurface 1),
	I -> dim I - 2), {3})
assert Equation(
    apply(primaryDecomposition terraciniLocus(2, delPezzoSurface 2),
	I -> dim I - 2), {3, 3})
assert Equation(
    apply(primaryDecomposition terraciniLocus(2, delPezzoSurface 3),
	I -> dim I - 2), {3, 3, 3})
assert Equation(
    apply(primaryDecomposition terraciniLocus(2, delPezzoSurface 4),
	I -> dim I - 2), {3, 3, 3, 3, 3})
///

TEST ///
-- veronese
needsPackage "Resultants"

assert Equation(terraciniLocus(2, veronese(2, 3)), 1)
///

TEST ///
-- segre-veronese

segreVeronese = (n, d) -> (
    x := symbol x;
    r := #n;
    R := QQ new Array from splice apply(r, i -> x_(i, 0)..x_(i, n#i));
    y := symbol y;
    S := QQ[y_0..y_(product(n, d, (ni, di) -> binomial(ni + di, di)) - 1)];
    map(R, S, flatten entries first tensor apply(r, i -> (
		vector apply(subsets(n#i + d#i, d#i), A -> product(d#i, j ->
			x_(i, A#j - j)))))))

assert Equation(
    apply(
	primaryDecomposition terraciniLocus(2, segreVeronese({1, 1}, {1, 2})),
	I -> dim I - 4), {3, 3})
///
