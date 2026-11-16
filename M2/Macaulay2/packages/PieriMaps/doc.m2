document {
     Key => PieriMaps,
     Headline => "Pieri inclusions",
     Citation => {///@article{Sam2009,
    AUTHOR = {Sam, Steven V},
    TITLE = {Computing inclusions of {S}chur modules},
    JOURNAL = {The Journal of Software for Algebra and Geometry: Macaulay2},
    VOLUME = {1},
    YEAR = {2009},
    PAGES = {5--10},
    DOI = {10.2140/jsag.2009.1.5},
    URL = {https://msp.org/jsag/2009/1-1/p02.xhtml}
}///},
     "For mathematical background of this package and some examples of use, see:",
     BR{},
     "Steven V Sam, Computing inclusions of Schur modules, arXiv:0810.4666",
     BR{},
     "Some other references:",
     BR{},
     "Andrzej Daszkiewicz, On the Invariant Ideals of the Symmetric Algebra $S.(V \\oplus \\wedge^2 V)$, J. Algebra 125, 1989, 444-473.",
     BR{},
     "David Eisenbud, Gunnar Fl\\o ystad, and Jerzy Weyman, The existence of pure free resolutions, arXiv:0709.1529.",
     BR{},
     "William Fulton, Young Tableaux: With Applications to Representation Theory and Geometry, London Math. Society Student Texts 35, 1997.",
     BR{},
     "Jerzy Weyman, Cohomology of Vector Bundles and Syzygies, Cambridge University Press, 2002.",
     BR{},
     BR{},     
     "Let ", TEX "$V$", " be a vector space over ", TEX "$K$", ". If ",
     TEX "$K$", " has characteristic 0, then given the partition ",
     TEX "$\\mu$", " and the partition ", TEX "$\\mu'$", " obtained from ",
     TEX "$\\mu$", " by removing a single box, there is a unique (up to
     nonzero scalar) ", TEX "$GL(V)$", "-equivariant inclusion ",
     TEX "$S_\\mu V \\to V \\otimes S_{\\mu'} V$", ", where ",
     TEX "$S_\\mu$", " refers to the irreducible representation of ",
     TEX "$GL(V)$", " with highest weight ", TEX "$\\mu$",
     ". This can be extended uniquely to a map of ",
     TEX "$P = \\mathrm{Sym}(V)$", "-modules ",
     TEX "$P \\otimes S_\\mu V \\to P \\otimes S_{\\mu'} V$",
     ". The purpose of this package is to write down matrix
     representatives for these maps. The main function for doing so is ",
     TO pieri,
     ". Here is an example of the use of the package PieriMaps, which is also
     designed to check whether the maps are being constructed correctly
     (important, since it is notoriously difficult to get the signs and
     coefficients right.) ",
     "We will construct by hand the free resolution in 3 variables corresponding
     to the degree sequence (0,2,3,6). Let's start with the packaged code:",
     EXAMPLE lines ///
     R = QQ[a,b,c];
     f = pureFree({0,2,3,6}, R)
     betti res coker f
     ///,
     "By the general theory from Eisenbud-Fl\\o ystad-Weyman, each of these free
     modules should be essentially Schur functors corresponding to the
     following partitions.",
     EXAMPLE lines ///
     needsPackage "SchurRings"
     schurRing(s,3)
     dim s_{2,2}
     dim s_{4,2}
     dim s_{4,3}
     dim s_{4,3,3}
     ///,
     "This package also provides a routine ",
     TO schurRank,
     " for computing this dimension:",
     EXAMPLE lines ///
     schurRank(3, {2,2})
     schurRank(3, {4,2})
     schurRank(3, {4,3})
     schurRank(3, {4,3,3})
     ///,
     "We now use ",
     TO pieri, 
     " to construct each of the maps of the resolution separately.",
     EXAMPLE lines ///
     f1 = pieri({4,2,0},{1,1}, R)
     f2 = pieri({4,3,0},{2}, R)
     f3 = pieri({4,3,3},{3,3,3}, R)
     ///,
     "Fix the degrees (i.e. make sure that the target of f2 is the source of f1,
     etc). Otherwise the test of exactness below would fail.",
     EXAMPLE lines ///
     f1
     f2 = map(source f1,,f2)
     f3 = map(source f2,,f3)
     f1 * f2
     f2 * f3
     ///,
     "Check that the complex is exact.",
     EXAMPLE lines ///
     ker f1 == image f2
     ker f2 == image f3
     ///,
     "Looks great! Now let's try it modulo some prime numbers and see if we get exactness.",
     EXAMPLE lines ///
     p = 32003
     R = ZZ/p[a,b,c];
     f1 = pieri({4,2,0},{1,1},R)
     betti res coker f1
     f2 = pieri({4,3,0},{2},R)
     f3 = pieri({4,3,3},{3,3,3},R)
     f2 = map(source f1,,f2)
     f3 = map(source f2,,f3)
     f1 * f2
     f2 * f3
     ker f1 == image f2
     ker f2 == image f3
     ///,
     "These do not piece together well. The reason is that ",
     TO pieri,
     " changes the bases of the free modules in a way which is not invertible
     (over ZZ) when the ground field has positive characteristic.",
     PARA{},
     BOLD "Version 2.0 overhaul.", "  This release extends the original
     PieriMaps package in three respects:",
     UL {
	  {BOLD "Multiple basis conventions.", " Every map can be expressed in
	   the package's original convention (default ",
	   TT "Convention => \"Row\"", "), the column-form ", TT "Filling",
	   " basis from ",
	   TO2 {"SchurFunctors :: SchurFunctors", "SchurFunctors"}, " (",
	   TT "Convention => \"Filling\"", "), or the divided-power Weyl-module
	   basis (", TT "Convention => \"Weyl\"", ").  See ", TO Convention, "."},
	  {BOLD "Both directions of every map.", " In addition to the inclusion
	   direction (", TO pieri, ", ", TO pieriColumn, ", ", TO lrMap,
	   "), the projection direction is provided via ",
	   TO dualPieri, ", ", TO dualPieriColumn, ", and ", TO dualLR,
	   ".  Each pair satisfies ",
	   TEX "$\\mathrm{dual} \\circ \\mathrm{incl} = \\mathrm{Id}$",
	   ", the canonical ", TEX "$GL$",
	   "-equivariant projection given by Schur's lemma."},
	  {BOLD "Verifications.", " ", TO verifyEquivariant, " checks ",
	   TEX "$GL_n$",
	   "-equivariance of any matrix between Schur modules via the
	   Chevalley-generator commutativity test; ", TO verifyWellDefined,
	   " checks compatibility with the Garnir relations of the chosen
	   convention."}
	  },
     BOLD "Examples.",
     PARA{},
     "The single-box Pieri inclusion ",
     TEX "$S_{(2,1)} V \\to V \\otimes S_{(1,1)} V$",
     " in three variables.  Each row of ",
     TT "symbolicForm",
     " displays a source standard tableau on the left and its image as a sum of (target tableau, coefficient) pairs on the right:",
     EXAMPLE lines ///
	  R = QQ[a,b,c];
	  M = pieri({2,1}, {1}, R);
	  symbolicForm M
	  ///,
     "The same map in the column-form (Filling) convention is the literal change of basis.  Both matrices are equivariant injections and have the same rank:",
     EXAMPLE lines ///
	  R = QQ[a,b,c];
	  Mrow = pieri({2,1}, {1}, R, Convention => "Row");
	  Mfil = pieri({2,1}, {1}, R, Convention => "Filling");
	  (rank Mrow, rank Mfil)
	  ///,
     "The projection ", TO dualPieri, " left-inverts ", TO pieri,
     ".  Verified by ",
     TEX "$\\mathrm{dualPieri} \\circ \\mathrm{applyPieri}(T_\\mu) = T_\\mu$",
     " on the highest-weight vector:",
     EXAMPLE lines ///
	  R = QQ[a,b,c];
	  Tmu = (standardTableaux(3, {2,1}))#0;
	  img = applyPieri(({2,1}, {1}), Tmu, R);
	  back = flatten for term in img list applyDualPieri(({2,1}, {1}), term#0, term#1, 3);
	  back
	  ///,
     "When the Littlewood-Richardson coefficient ",
     TEX "$c^\\lambda_{\\mu,\\nu}$", " exceeds 1, distinct LR tableaux ",
     TEX "$Q$", " produce ", TEX "$c$",
     " linearly independent inclusions ",
     TEX "$\\Psi_Q : S_\\lambda V \\hookrightarrow S_\\nu V \\otimes S_\\mu V$",
     ":",
     EXAMPLE lines ///
	  Qs = lrTableaux({3,2,1}, {2,1}, {2,1});  -- c^{(3,2,1)}_{(2,1),(2,1)} = 2
	  M1 = lrMap(({3,2,1}, {2,1}, {2,1}), Qs#0, 3);
	  M2 = lrMap(({3,2,1}, {2,1}, {2,1}), Qs#1, 3);
	  rank(M1 | M2) == 16    -- 2 * dim S_{(3,2,1)} -- linearly independent
	  ///,
     "Each ", TO dualLR, " projects onto exactly its own ",
     TEX "$Q$", "-summand and annihilates the others:",
     EXAMPLE lines ///
	  Qs = lrTableaux({3,2,1}, {2,1}, {2,1});
	  M0 = lrMap(({3,2,1},{2,1},{2,1}), Qs#0, 3);
	  M1 = lrMap(({3,2,1},{2,1},{2,1}), Qs#1, 3);
	  N0 = dualLR(({3,2,1},{2,1},{2,1}), Qs#0, 3);
	  N0 * M0 == id_(QQ^(numColumns M0))
	  N0 * M1 == 0
	  ///,
     TO verifyEquivariant,
     " checks Chevalley-generator commutativity for any matrix between Schur reps -- a direct ",
     TEX "$GL_n$",
     "-equivariance test:",
     EXAMPLE lines ///
	  R = QQ[a,b,c];
	  M = pieri({3,2,1}, {2}, R);   -- inclusion at the interior row
	  verifyEquivariant(M, {3,2,1}, {2}, R)
	  ///,
     "And ", TO verifyWellDefined,
     " confirms that an LR map factors through the Garnir relations of the chosen convention:",
     EXAMPLE lines ///
	  Qs = lrTableaux({3,2,1}, {2,1}, {2,1});
	  verifyWellDefined(({3,2,1},{2,1},{2,1}), Qs#0, 3,
	       Convention => "Filling", Verbose => false)
	  ///,
     SeeAlso => {pieri, pieriColumn, dualPieri, dualPieriColumn,
	  lrMap, dualLR, applyLR, applyPieri, applyDualPieri,
	  Convention, verifyEquivariant, verifyWellDefined,
	  symbolicForm, pureFree, schurRank}
  }

document {
     Key => {standardTableaux, (standardTableaux, ZZ, List)},
     Headline => "list all standard tableaux of a certain shape with bounded labels",
     SeeAlso => schurRank,
     Usage => "standardTableaux(dim, mu)",
     Inputs => { 
	  "dim" => {ofClass ZZ, ", number of labels to be used"},
	  "mu" => {ofClass List, ", a partition which gives the shape"}
	  },
     Outputs => {
	  List => {"list of all standard tableaux of shape ", TEX "$\\mu$",
	       " with labels ", TEX "$0, \\ldots, \\mathrm{dim}-1$"}
	  },
     EXAMPLE lines ///
     	  standardTableaux(3, {2,2}) -- lists all standard tableaux on the 2x2 square with entries 0,1,2
	  ///
     }

document {
     Key => {straighten, (straighten, List), (straighten, List, MutableHashTable)},
     Headline => "computes straightening of a tableau",
     Usage => concatenate("straighten(t)", "straighten(t, h)"),
     Inputs => {
	  "t" => {ofClass List, ", a tableau to straighten; a tableau looks like {{3,4}, {1,2}} for example, where we list the entries from left to right, top to bottom"},
	  "h" => {ofClass MutableHashTable, ", where the answers should be stored"}
	  },
     Consequences => { "If provided, the hashtable h is updated with any calculations which are performed as a result of calling this function. " },
     "If a hashtable h is provided, then this outputs nothing, it simply just modifies h. When looking up values, remember that the keys are stored with rows weakly increasing. ",
     "If no hashtable is provided, then the user is simply given the straightening of the tableau in terms of semistandard tableaux. ",
     "The answer is in the form a hashtable: each key is a semistandard tableaux, and the value of the key is the coefficient of that semistandard tableaux used to write the input t as a linear combination. ",
     EXAMPLE lines ///
     	  h = new MutableHashTable from {}
	  straighten({{3,4}, {1,2}}, h)
	  h#{{3,4}, {1,2}} -- get the coefficients
	  straighten({{3,4}, {1,2}}) -- just get the answer instead
	  ///
     }

document {
     Key => {pieri, (pieri, List, List, PolynomialRing), [pieri, Convention]},
     Headline => "computes a matrix representation for a Pieri inclusion of representations of a general linear group",
     SeeAlso => {pureFree, pieriColumn, lrMap},
     Usage => "pieri(mu, boxes, P)\npieri(mu, boxes, P, Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "mu" => {ofClass List, ", a partition ",
	       TEX "$(\\mu_1, \\ldots, \\mu_r)$", " where ", TEX "$\\mu_i$",
	       " is the number of boxes in the ", TEX "$i$", "th row"},
	  "boxes" => {ofClass List, ", a list of rows from which to remove boxes (boxes are always removed from the end of the row). This specifies which map of ",
	       TEX "$GL(V)$",
	       " representations we want. The row indices start from 1 and not 0, and this must specify a horizontal strip in ",
	       TEX "$\\mu$", " (see description below). "},
	  "P" => {ofClass PolynomialRing, ", a polynomial ring over a field ",
	       TEX "$K$", " in ", TEX "$n$", " variables" },
	  Convention => String => {", the tableau basis convention used to label the source ",
	       TEX "$S_\\mu V$", " and target ", TEX "$S_\\lambda V$",
	       ".  One of ",
	       TT "\"Row\"", " (default; PieriMaps row-form SSYT basis), ",
	       TT "\"Filling\"", " (SchurFunctors column-form ", TT "Filling", " basis on both sides), or ",
	       TT "\"Weyl\"", " (data-equivalent to Row, but the bases are interpreted as ", TT "WeylFilling", "s on the divided-power side).  ",
	       "The strip factor ", TEX "$(\\mathrm{Sym}^k V)$", " stays in P regardless."}
	  },
     Outputs => {
	  Matrix => {"If ", TEX "$K$",
	       " has characteristic 0, then given the partition ", TEX "$\\mu$",
	       " and the partition ", TEX "$\\mu'$", " obtained from ",
	       TEX "$\\mu$",
	       " by removing a single box, there is a unique (up to nonzero scalar) ",
	       TEX "$GL(V)$", "-equivariant inclusion ",
	       TEX "$S_\\mu V \\to V \\otimes S_{\\mu'} V$", ", where ",
	       TEX "$S_\\mu$", " refers to the irreducible representation of ",
	       TEX "$GL(V)$", " with highest weight ", TEX "$\\mu$",
	       ". This can be extended uniquely to a map of ",
	       TEX "$P = \\mathrm{Sym}(V)$", "-modules ",
	       TEX "$P \\otimes S_\\mu V \\to P \\otimes S_{\\mu'} V$",
	       ". This method computes the matrix representation for the composition of maps that one obtains by
	       iterating this procedure of removing boxes, i.e., the final output is a ",
	       TEX "$GL(V)$", "-equivariant map ",
	       TEX "$P \\otimes S_\\mu V \\to P \\otimes S_\\lambda V$",
	       " where ", TEX "$\\lambda$", " is the partition obtained from ",
	       TEX "$\\mu$", " by deleting a box from row ",
	       TEX "$\\mathrm{boxes}_0$", ", a box from row ",
	       TEX "$\\mathrm{boxes}_1$", ", etc.
	       If ", TEX "$K$",
	       " has positive characteristic, then the corresponding map is calculated over ",
	       TEX "$\\mathbb{Q}$", ", lifted to a ", TEX "$\\mathbb{Z}$",
	       "-form of the representation which has
	       the property that the map has a torsion-free cokernel over ",
	       TEX "$\\mathbb{Z}$", ", and then the coefficients are reduced to ",
	       TEX "$K$", "."
	       }
	  },
     "Convention: the partition ", TEX "$(d)$",
     " represents the ", TEX "$d$", "th symmetric power, while the partition ",
     TEX "$(1, \\ldots, 1)$", " represents the ", TEX "$d$",
     "th exterior power. ",
     "Using the notation from the output, ", TEX "$\\mu/\\lambda$",
     " must be a horizontal strip. Precisely, this means that ",
     TEX "$\\lambda_i \\geq \\mu_{i+1}$", " for all ", TEX "$i$",
     ". If this condition is not satisfied, the program throws an error because a nonzero equivariant map of the desired form will not exist. ",
     PARA{},
     "The ", TT "Convention",
     " option re-expresses the same equivariant map in different tableau bases on the source ",
     TEX "$S_\\mu V$", " and target ", TEX "$S_\\lambda V$", ".  In ",
     TT "\"Row\"",
     " (default), the bases are PM-style SSYT (rows weakly increasing).  In ",
     TT "\"Filling\"",
     ", they are SchurFunctors-style column-form Fillings (the matrix is obtained by post-hoc basis change with the equivariant iso ",
     TO pmToFilling, ").  In ", TT "\"Weyl\"",
     ", the matrix data is the same as ", TT "\"Row\"",
     " but is interpreted on the WeylFilling (divided-power) side.  All three give matrices of the same rank.",
     EXAMPLE lines ///
	  pieri({3,1}, {1}, QQ[a,b,c]) -- removes the last box from row 1 of the partition {3,1}
	  res coker oo -- resolve this map
	  betti oo -- check that the resolution is pure
	  ///,
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  Mrow = pieri({2,1}, {1}, P, Convention => "Row")
	  ///,
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  Mfil = pieri({2,1}, {1}, P, Convention => "Filling")
	  ///,
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  Mwey = pieri({2,1}, {1}, P, Convention => "Weyl")
	  ///,
     "The Row and Weyl matrices share raw data (only the source/target bases are reinterpreted on the divided-power side); the Filling matrix is obtained by post-hoc basis change.  All three have the same rank.",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  Mrow = pieri({2,1}, {1}, P, Convention => "Row");
	  Mfil = pieri({2,1}, {1}, P, Convention => "Filling");
	  Mwey = pieri({2,1}, {1}, P, Convention => "Weyl");
	  rank Mrow, rank Mfil, rank Mwey
	  ///,
     "The Betti table of the cokernel is the same in every convention (the
     resolutions are isomorphic up to an equivariant change of basis):",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  betti res coker pieri({2,1}, {1}, P, Convention => "Row")
	  ///,
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  betti res coker pieri({2,1}, {1}, P, Convention => "Filling")
	  ///,
     PARA{},
     "Multi-box horizontal strips: ", TT "boxes",
     " can list more than one row index (with repeats), and the result is the
     composition of single-box Pieri inclusions.  The same row index can appear
     repeatedly when row 1 has enough boxes; entries become degree-",
     TEX "$d$", " polynomials.",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  pieri({3,1}, {1,1}, P)         -- removes 2 boxes from row 1; entries are deg 2
	  ///,
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  pieri({3,2,1}, {1,3}, P)       -- removes one box from row 1, then row 3
	  ///,
     "And ", TO symbolicForm,
     " makes the basis-by-basis action of a multi-box map readable:",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  symbolicForm pieri({3,1}, {1,1}, P)
	  ///
     }

document {
     Key => {pureFree, (pureFree, List, PolynomialRing), [pureFree, Convention]},
     Headline => "computes a GL(V)-equivariant map whose resolution is pure, or the reduction mod p of such a map",
     SeeAlso => {pieri},
     Usage => "pureFree(d, P)\npureFree(d, P, Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "d" => {ofClass List, ", a list of degrees (increasing numbers)"},
	  "P" => {ofClass PolynomialRing, ", a polynomial ring over a field ",
	       TEX "$K$", " in ", TEX "$n$", " variables" },
	  Convention => String => {", basis convention used internally by ", TO pieri, ".  Defaults to ", TT "\"Row\"", "; the resulting Betti table does not depend on the choice."}
	  },
     Outputs => {
	  Matrix => {"A map whose cokernel has Betti diagram with degree sequence ",
	       TEX "$d$", " if ", TEX "$K$",
	       " has characteristic 0.  If ", TEX "$K$",
	       " has positive characteristic ", TEX "$p$",
	       ", then the corresponding map is calculated over ",
	       TEX "$\\mathbb{Q}$", " and is lifted to a ", TEX "$\\mathbb{Z}$",
	       "-form which is then reduced mod ", TEX "$p$", "." }
	  },
     "The function translates the data of a degree sequence ", TEX "$d$",
     " for a desired pure free resolution into the data of a Pieri map
     according to the formula of Eisenbud-Fl\\o ystad-Weyman and then applies the function ",
     TO pieri,
     ".  The ", TT "Convention", " option is forwarded to that internal Pieri call so that the resulting matrix is presented in the user's preferred basis convention; the Betti table of the resulting resolution is the same in all conventions.",
     EXAMPLE lines ///
	  betti res coker pureFree({0,1,2,4}, QQ[a,b,c]) -- degree sequence {0,1,2,4}
	  betti res coker pureFree({0,1,2,4}, ZZ/2[a,b,c]) -- same map, but reduced mod 2
	  betti res coker pureFree({0,1,2,4}, GF(4)[a,b,c]) -- can also use non prime fields
	  ///,
     EXAMPLE lines ///
	  -- The Betti table is independent of the chosen convention:
	  P = QQ[a,b,c];
	  bettiR = betti res coker pureFree({0,1,2,4}, P, Convention => "Row");
	  bettiF = betti res coker pureFree({0,1,2,4}, P, Convention => "Filling");
	  bettiR == bettiF
	  ///
     }

document {
     Key => {schurRank, (schurRank, ZZ, List)},
     Headline => "computes the dimension of the irreducible GL(QQ^n) representation associated to a partition",
     SeeAlso => standardTableaux,
     Usage => "schurRank(n, mu)",
     Inputs => {
	  "n" => {ofClass ZZ, ", the size of the matrix group ",
	       TEX "$GL(\\mathbb{Q}^n)$"},
	  "mu" => {ofClass List, ", a partition ",
	       TEX "$(\\mu_1, \\ldots, \\mu_r)$", " where ", TEX "$\\mu_i$",
	       " is the number of boxes in the ", TEX "$i$", "th row"}
	  },
     Outputs => {
	  ZZ => {"The dimension of the irreducible ",
	       TEX "$GL(\\mathbb{Q}^n)$", " representation associated to ",
	       TEX "$\\mu$"}
	  },
     "The dimension is computed using the determinantal formula given by the Weyl character formula.",
     EXAMPLE lines ///
     	  schurRank(5, {4,3}) -- should be 560
     	  ///
     }     	  
     
document {
     Key => {lrTableaux, (lrTableaux, List, List, List)},
     Headline => "enumerate Littlewood-Richardson tableaux",
     Usage => "lrTableaux(lambda, mu, nu)",
     Inputs => {
	  "lambda" => {ofClass List, ", a partition"},
	  "mu" => {ofClass List, ", a partition contained in lambda"},
	  "nu" => {ofClass List, ", a partition with |nu| = |lambda| - |mu|"}
	  },
     Outputs => {
	  List => {"the list of LR fillings of the skew shape lambda/mu with content nu"}
	  },
     "An LR tableau is a semistandard skew tableau (rows weakly increasing, columns strictly increasing) ",
     "whose reverse reading word is a lattice word.  Each filling is returned as a list of rows; row ",
     TEX "$i$", " has length ", TEX "$\\lambda_i - \\mu_i$", " and ",
     "entries in ", TEX "$\\{0, \\ldots, \\#\\nu - 1\\}$",
     " (so label 0 corresponds to row 1 of ", TEX "$\\nu$", ", etc.).  ",
     "The number of LR tableaux equals the Littlewood-Richardson coefficient ",
     TEX "$c^\\lambda_{\\mu,\\nu}$", ".",
     EXAMPLE lines ///
	  #lrTableaux({3,2,1}, {2,1}, {2,1}) -- = c^{(3,2,1)}_{(2,1),(2,1)} = 2
	  #lrTableaux({2,1}, {1}, {1,1})     -- = 1
	  ///,
     SeeAlso => lrMap
     }

document {
     Key => {lrMap, (lrMap, Sequence, List, ZZ), (lrMap, Sequence, List, PolynomialRing), [lrMap, Convention]},
     Headline => "GL(V)-equivariant Littlewood-Richardson inclusion",
     Usage => "lrMap((lambda, mu, nu), Q, n)\nlrMap((lambda, mu, nu), Q, P)\nlrMap(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(lambda, mu, nu)" => {ofClass Sequence, ", three partitions with ",
	       TEX "$|\\lambda| = |\\mu| + |\\nu|$", " and ", TEX "$\\mu$",
	       " contained in ", TEX "$\\lambda$"},
	  "Q" => {ofClass List, ", an LR tableau of shape ",
	       TEX "$\\lambda/\\mu$", " with content ", TEX "$\\nu$",
	       ", as returned by ", TO lrTableaux},
	  "n" => {ofClass ZZ, ", the dimension of ", TEX "$V$",
	       " (or pass a ", TO PolynomialRing, " whose ", TT "numgens",
	       " is used)"},
	  Convention => String => {", basis convention for both the source ",
	       TEX "$S_\\lambda V$", " and the target ",
	       TEX "$S_\\nu V \\otimes S_\\mu V$", ".  ",
	       TT "\"Row\"", " (default), ", TT "\"Filling\"", ", or ", TT "\"Weyl\"", "."}
	  },
     Outputs => {
	  Matrix => {"a matrix over ", TEX "$\\mathbb{Q}$",
	       " whose columns index a basis of ", TEX "$S_\\lambda V$",
	       " and rows index a basis of ",
	       TEX "$S_\\nu V \\otimes S_\\mu V$"}
	  },
     "Builds the ", TEX "$GL(V)$", "-equivariant inclusion ",
     TEX "$\\Psi_Q \\colon S_\\lambda V \\to S_\\nu V \\otimes S_\\mu V$",
     " associated to the LR tableau ", TT "Q",
     ".  The map is constructed by iterating row-Pieri inclusions ",
     TEX "$S_{\\lambda^{(a)}} V \\to \\mathrm{Sym}^{\\nu_a} V \\otimes S_{\\lambda^{(a-1)}} V$",
     " in the order ", TEX "$a = r, r-1, \\ldots, 1$",
     ", then projecting the ",
     TEX "$\\mathrm{Sym}$", " tensor side onto ", TEX "$S_\\nu V$",
     " via straightening.",
     PARA{},
     "The columns of the output are indexed by ", TT "standardTableaux(n, lambda)", " (or the corresponding Filling / WeylFilling basis if a non-default ", TT "Convention", " is chosen).  ",
     "The rows are indexed by pairs ", TEX "$(T_\\nu, T_\\mu)$",
     " in lex order: row ", TEX "$i \\cdot \\#S_\\mu + j$",
     " corresponds to ",
     TEX "$(T_\\nu = N\\nu\\#i, T_\\mu = N\\mu\\#j)$", " where ",
     TT "Nnu = standardTableaux(n, nu)", " and ",
     TT "Nmu = standardTableaux(n, mu padded to length #lambda)",
     ".  When ", TT "Convention => \"Filling\"",
     " is used, the equivariant change of basis ",
     TO pmToFillingMatrix, " is applied on each side, and the rank is preserved.",
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});                  -- a multiplicity-1 example
	  Q = (lrTableaux shapes)#0;
	  Mrow = lrMap(shapes, Q, 3, Convention => "Row")
	  ///,
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  Mfilling = lrMap(shapes, Q, 3, Convention => "Filling")
	  ///,
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  Mweyl = lrMap(shapes, Q, 3, Convention => "Weyl")
	  ///,
     "All three are GL-equivariant injections of ", TEX "$S_\\lambda V$",
     " into ", TEX "$S_\\nu V \\otimes S_\\mu V$",
     ".  The Row and Weyl matrices have the same raw entries (only the source/target bases are reinterpreted on the divided-power side); the Filling matrix is obtained by post-hoc basis change.  All three have the same rank.",
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  Mrow = lrMap(shapes, Q, 3, Convention => "Row");
	  Mfilling = lrMap(shapes, Q, 3, Convention => "Filling");
	  Mweyl = lrMap(shapes, Q, 3, Convention => "Weyl");
	  (rank Mrow, rank Mfilling, rank Mweyl)
	  ///,
     "When ", TEX "$c^\\lambda_{\\mu,\\nu} > 1$",
     ", distinct LR tableaux ", TEX "$Q$",
     " give linearly independent inclusions; the joint matrix has rank ",
     TEX "$c^\\lambda_{\\mu,\\nu} \\cdot \\dim S_\\lambda V$",
     ".  The canonical multiplicity-2 example is ",
     TEX "$c^{(3,2,1)}_{(2,1),(2,1)} = 2$", ":",
     EXAMPLE lines ///
	  Qs = lrTableaux({3,2,1}, {2,1}, {2,1});  -- two distinct LR tableaux
	  M1 = lrMap(({3,2,1}, {2,1}, {2,1}), Qs#0, 3);
	  M2 = lrMap(({3,2,1}, {2,1}, {2,1}), Qs#1, 3);
	  rank M1, rank M2          -- each = dim S_(3,2,1) = 8
	  rank(M1 | M2) == 16       -- 2 * 8: M1 and M2 are linearly independent
	  ///,
     "Use ", TO symbolicForm, " to read off the action of ",
     TEX "$\\Psi_Q$", " on each standard tableau of ",
     TEX "$S_\\lambda V$",
     ", as a sum over basis pairs of ", TEX "$S_\\nu V \\otimes S_\\mu V$", ":",
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  symbolicForm lrMap(shapes, Q, 3)
	  ///,
     SeeAlso => {lrTableaux, pieri, straighten, applyLR, verifyWellDefined, symbolicForm}
     }

document {
     Key => {applyLR, (applyLR, Sequence, List, BasicList, ZZ), [applyLR, Convention]},
     Headline => "apply Psi_Q to a single tableau of shape lambda",
     Usage => "applyLR((lambda, mu, nu), Q, T, n)\napplyLR(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(lambda, mu, nu)" => {ofClass Sequence, ", three partitions specifying the LR map"},
	  "Q" => {ofClass List, ", an LR tableau as returned by ", TO lrTableaux},
	  "T" => {ofClass BasicList, ", a tableau of shape ", TEX "$\\lambda$",
	       ".  May be supplied as a ", TT "List", " or as a ",
	       TT "Filling", ".  In ", TT "\"Row\"", "/", TT "\"Weyl\"",
	       " convention a ", TT "List", " is interpreted as PM row form;
	       in ", TT "\"Filling\"", " convention a ", TT "List",
	       " is interpreted as column form (and wrapped automatically).
	       A ", TT "Filling", " is always treated as column form. ",
	       TT "T", " need not be standard; it is straightened internally
	       using the convention's straightening relations."},
	  "n" => {ofClass ZZ, ", the dimension of ", TEX "$V$"},
	  Convention => String => {", basis convention used to interpret ",
	       TT "T", " and label the output ",
	       TEX "$T_\\nu$", ", ", TEX "$T_\\mu$", "."}
	  },
     Outputs => {
	  List => {"a list of triples ", TT "{c, T_nu, T_mu}",
	       " representing the image as the sum ",
	       TEX "$\\sum c_i \\cdot T_\\nu^{(i)} \\otimes T_\\mu^{(i)}$",
	       " in ", TEX "$S_\\nu V \\otimes S_\\mu V$"}
	  },
     "Computes the image of a single basis vector (or, more generally, of any
     tableau, which is first straightened into the SSYT basis of ",
     TEX "$S_\\lambda V$",
     ") under the LR inclusion ", TEX "$\\Psi_Q$",
     ".  The output is intended for human
     reading or interactive exploration when the full ", TO lrMap, " matrix is
     too large to display.  Use ", TO displayLRImage, " for pretty-printing.",
     PARA{},
     "When ", TT "Convention => \"Filling\"", " is chosen, the input ",
     TT "T", " must be a SchurFunctors ", TT "Filling",
     ", and the output uses Fillings on both target sides.  Straightening uses SchurFunctors' column-Garnir relations.  When ",
     TT "Convention => \"Row\"", " or ", TT "\"Weyl\"", ", ", TT "T",
     " is a list of rows and PieriMaps' row-Garnir straightening is applied.",
     EXAMPLE lines ///
	  Qs = lrTableaux({3,2,1}, {2,1}, {2,1});
	  T = {{0,0,0},{1,1},{2}};   -- highest-weight vector of S_(3,2,1) QQ^3
	  applyLR(({3,2,1}, {2,1}, {2,1}), Qs#0, T, 3)
	  displayLRImage applyLR(({3,2,1}, {2,1}, {2,1}), Qs#0, T, 3)
	  ///,
     EXAMPLE lines ///
	  -- Non-standard input: row-Garnir straightening before applying.
	  Tnonstd = {{1,0},{2}};   -- row 0 not weakly-increasing
	  applyLR(({2,1}, {1}, {1,1}), (lrTableaux({2,1},{1},{1,1}))#0, Tnonstd, 3)
	  ///,
     SeeAlso => {lrMap, lrTableaux, displayLRImage, verifyWellDefined}
     }

document {
     Key => {displayLRImage, (displayLRImage, List)},
     Headline => "pretty-print the result of applyLR",
     Usage => "displayLRImage(image)",
     Inputs => {
	  "image" => {ofClass List, ", as returned by ", TO applyLR}
	  },
     Outputs => {
	  Net => {"a stacked display of the symbolic sum ",
	       TEX "$\\sum c_i \\cdot T_\\nu^{(i)} \\otimes T_\\mu^{(i)}$"}
	  },
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  T = (standardTableaux(3, {2,1}))#0;
	  img = applyLR(shapes, Q, T, 3);
	  displayLRImage img
	  ///,
     SeeAlso => {applyLR, lrMap}
     }

document {
     Key => Convention,
     Headline => "tableau basis convention used by PieriMaps' overhauled functions",
     "An option accepted by ", TO pieri, ", ", TO lrMap, ", ", TO applyLR, ", ", TO pureFree, ", and ", TO verifyWellDefined,
     ".  Possible values:",
     UL {
	  {TT "\"Row\"", " (default): the original PieriMaps row-form basis.  Source/target Schur modules are indexed by SSYTs as lists of weakly-increasing rows."},
	  {TT "\"Filling\"", ": the SchurFunctors column-form basis.  Source/target are indexed by ", TT "Filling", "s, with column-Garnir straightening relations.  The matrix is obtained by post-hoc basis change with the equivariant iso ", TO pmToFilling, "."},
	  {TT "\"Weyl\"", ": the divided-power row-form basis.  Source/target are indexed by ", TT "WeylFilling", "s.  Numerically the same data as the Row matrix; it is just that the bases are interpreted on the Weyl (divided-power) side rather than the Schur side."}
	  },
     "Use ", TO verifyWellDefined, " to verify that the chosen-convention map respects the appropriate straightening relations.",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  pieri({2,1}, {1}, P, Convention => "Row")
	  pieri({2,1}, {1}, P, Convention => "Filling")
	  ///,
     SeeAlso => {pmToFilling, fillingToPM, pmToWeyl, weylToPM, verifyWellDefined}
     }

document {
     Key => {pmToFilling},
     Headline => "convert a PieriMaps row-form tableau to SchurFunctors column-form Fillings",
     Usage => "pmToFilling T",
     Inputs => {
	  "T" => {ofClass List, ", a PieriMaps row-form tableau (list of weakly-increasing rows of the same shape)"}
	  },
     Outputs => {
	  HashTable => {"the image of ", TT "T",
	       " under the symmetrize-then-antisymmetrize equivariant map ",
	       TEX "$\\mathrm{Sym}^\\lambda V \\to \\wedge^{\\lambda'} V$",
	       ", expressed as a sum of standard ", TT "Filling",
	       "s with rational coefficients (after SchurFunctors straightening)."}
	  },
     "PieriMaps' row-form representation ",
     TEX "$S_\\lambda V \\subset \\mathrm{Sym}^{\\lambda_1} V \\otimes \\cdots$",
     " and SchurFunctors' column-form representation ",
     TEX "$S_\\lambda V \\subset \\wedge^{\\lambda'_1} V \\otimes \\cdots$",
     " are equivariant isomorphisms (over ", TEX "$\\mathbb{Q}$",
     ").  ",
     "This function realizes that iso explicitly: each PM monomial ",
     TEX "$T = (m_1, \\ldots, m_r)$", " is symmetrized into ",
     TEX "$V^{\\otimes |\\lambda|}$",
     " via the row-symmetrizer (with normalization ",
     TEX "$1/\\prod \\lambda_i!$",
     "), reordered by columns, and then projected to wedges (sort ",
     TEX "$+$",
     " sign) per column.  The result is then straightened into the standard ",
     TT "Filling", " basis via SchurFunctors' ", TT "straighten", ".",
     PARA{},
     "Round-tripping ", TT "pmToFilling", " then ", TO fillingToPM,
     " gives a uniform shape-dependent scalar ",
     TEX "$c_\\lambda \\cdot \\mathrm{id}$",
     " (the Young symmetrizer normalization).",
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  pmToFilling {{0,1},{2}}
	  pmToFilling {{0,0,0},{1,1},{2}}  -- highest-weight vector of S_(3,2,1) Q^3
	  ///,
     SeeAlso => {fillingToPM, pmToWeyl, pmToFillingMatrix, Convention}
     }

document {
     Key => {fillingToPM},
     Headline => "convert a SchurFunctors column-form Filling to PieriMaps row-form tableaux",
     Usage => "fillingToPM F",
     Inputs => {
	  "F" => {TT "Filling", ", a SchurFunctors column-form Filling"}
	  },
     Outputs => {
	  HashTable => {"the image of ", TT "F",
	       " under the antisymmetrize-then-symmetrize equivariant map ",
	       TEX "$\\wedge^{\\lambda'} V \\to \\mathrm{Sym}^\\lambda V$",
	       ", expressed as a sum of standard PM-form tableaux with rational coefficients (after PieriMaps straightening)."}
	  },
     "Inverse (up to a uniform scalar) of ", TO pmToFilling,
     ".  Each column of ", TT "F", " is wedge-expanded into ",
     TEX "$V^{\\otimes |\\lambda|}$",
     " via the column antisymmetrizer (signed sum over permutations, with normalization ",
     TEX "$1/\\prod \\lambda'_j!$",
     "), reordered by rows, and projected to symmetric (sort) per row.  The result is straightened into the standard PM basis.",
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  F = new Filling from {{0,2},{1}};
	  fillingToPM F
	  ///,
     SeeAlso => {pmToFilling, weylToFilling, fillingToPMMatrix}
     }

document {
     Key => {pmToWeyl},
     Headline => "convert a PieriMaps row-form tableau to a SchurFunctors WeylFilling",
     Usage => "pmToWeyl T",
     Inputs => {"T" => {ofClass List, ", a PieriMaps row-form tableau"}},
     Outputs => {{TT "WeylFilling",
	  " with the same row-data (a SchurFunctors basis element of ",
	  TEX "$K_\\lambda V$", ")"}},
     "PM tableaux and ", TT "WeylFilling", "s have the same data layout (rows weakly increasing).  This is purely a relabel: ",
     TT "pmToWeyl", " wraps a list of rows as a WeylFilling (interpreting it as a basis element of the divided-power module ",
     TEX "$K_\\lambda V$", "), and ", TO weylToPM,
     " unwraps.  The conversion is reversible.",
     "Requires the updated SchurFunctors package that exports ", TT "weyl", " and ", TT "WeylFilling", ".",
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  pmToWeyl {{0,1},{2}}
	  weylToPM pmToWeyl {{0,1},{2}}
	  ///,
     SeeAlso => {weylToPM, pmToFilling, weylToFilling}
     }

document {
     Key => {weylToPM},
     Headline => "convert a SchurFunctors WeylFilling to a PieriMaps row-form tableau",
     Usage => "weylToPM W",
     Inputs => {"W" => {TT "WeylFilling"}},
     Outputs => {List => {"a list of rows (PM-form tableau) with the same data"}},
     "Inverse of ", TO pmToWeyl, ".  Both WeylFilling and PM-form share the row-of-multisets data layout, so this is purely an unwrapping.",
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  W = pmToWeyl {{0,1},{2}}
	  weylToPM W
	  ///,
     SeeAlso => {pmToWeyl, fillingToWeyl}
     }

document {
     Key => {weylToFilling},
     Headline => "convert a WeylFilling to a SchurFunctors Filling (compose pmToFilling with weylToPM)",
     Usage => "weylToFilling W",
     Inputs => {"W" => {TT "WeylFilling"}},
     Outputs => {HashTable => {"image as a sum of standard ", TT "Filling", "s after straightening"}},
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  W = pmToWeyl {{0,1},{2}};
	  weylToFilling W
	  ///,
     SeeAlso => {pmToFilling, weylToPM, fillingToWeyl}
     }

document {
     Key => {fillingToWeyl},
     Headline => "convert a Filling to a sum of WeylFillings (compose pmToWeyl with fillingToPM)",
     Usage => "fillingToWeyl F",
     Inputs => {"F" => {TT "Filling"}},
     Outputs => {HashTable => {"a HashTable {WeylFilling => coeff}"}},
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  F = new Filling from {{0,2},{1}};
	  fillingToWeyl F
	  ///,
     SeeAlso => {pmToWeyl, fillingToPM, weylToFilling}
     }

document {
     Key => {pmToFillingMatrix},
     Headline => "QQ matrix of the PM-to-Filling change of basis on a Schur module",
     Usage => "pmToFillingMatrix(mu, n)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition (the Schur shape)"},
	  "n"  => {ofClass ZZ, ", the dimension of ", TEX "$V$"}
	  },
     Outputs => {
	  Matrix => {"a ", TEX "$\\mathbb{Q}$",
	       " matrix; rows indexed by ", TT "Filling",
	       "s of column heights ",
	       TEX "$\\mu^t$",
	       ", columns indexed by PieriMaps' standard tableaux of shape ",
	       TEX "$\\mu$", ".  Entry ", TEX "$(i, j)$",
	       " is the coefficient of Filling ", TEX "$i$", " in ",
	       TT "pmToFilling(PMtab_j)", "."}
	  },
     "This matrix realizes the equivariant change of basis between PieriMaps' row-form basis of ",
     TEX "$S_\\mu V$",
     " and SchurFunctors' column-form basis.  It is invertible over ",
     TEX "$\\mathbb{Q}$",
     " (up to a shape-dependent overall scalar) and is the workhorse behind ",
     TO [pieri, Convention], "/", TO [lrMap, Convention],
     " for the Filling convention.",
     PARA{},
     "Source/target basis labels are attached so ", TO symbolicForm,
     " can display the change-of-basis on each PM tableau:",
     EXAMPLE lines ///
	  symbolicForm pmToFillingMatrix({2,1}, 3)
	  ///,
     SeeAlso => {fillingToPMMatrix, pmToFilling, symbolicForm}
     }

document {
     Key => {fillingToPMMatrix},
     Headline => "QQ matrix of the Filling-to-PM change of basis on a Schur module",
     Usage => "fillingToPMMatrix(mu, n)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition"},
	  "n"  => {ofClass ZZ}
	  },
     Outputs => {
	  Matrix => {"a ", TEX "$\\mathbb{Q}$",
	       " matrix; rows = PM tableaux of ", TEX "$\\mu$",
	       ", cols = Fillings of ", TEX "$\\mu^t$", ".  Entry ",
	       TEX "$(i, j)$", " = coefficient of PM tableau ", TEX "$i$",
	       " in ", TT "fillingToPM(filling_j)", "."}
	  },
     "Inverse direction of ", TO pmToFillingMatrix, "; same equivariant iso, transposed convention.",
     EXAMPLE lines ///
	  symbolicForm fillingToPMMatrix({2,1}, 3)
	  ///,
     SeeAlso => {pmToFillingMatrix, fillingToPM, symbolicForm}
     }

document {
     Key => {pieriColumn, (pieriColumn, List, List, PolynomialRing), [pieriColumn, Convention]},
     Headline => "native column-form (vertical-strip) Pieri inclusion",
     Usage => "pieriColumn(mu, cols, P)",
     Inputs => {
	  "mu" => {ofClass List, ", a partition"},
	  "cols" => {ofClass List, ", a list of 1-indexed column indices.  One box is removed from the bottom of each listed column; the cumulative removal must form a vertical strip (no two boxes in the same row)."},
	  "P" => {ofClass PolynomialRing, ", a SkewCommutative ",
	       TEX "$\\mathbb{Q}$", "-algebra in ", TEX "$n$",
	       " variables.  Its generators carry the wedge factor ",
	       TEX "$\\wedge^{|\\mathrm{cols}|} V$",
	       " on the output side."}
	  },
     Outputs => {
	  Matrix => {"a matrix from ", TEX "$S_\\mu V$", " to ",
	       TEX "$\\wedge^{|\\mathrm{cols}|} V \\otimes S_\\lambda V$",
	       ", with rows indexed by standard ", TT "Filling", "s of ",
	       TEX "$\\lambda$",
	       " and columns indexed by standard Fillings of ", TEX "$\\mu$",
	       "."}
	  },
     "The dual of ", TO pieri, ".  Where ", TT "pieri(mu, rows, P)",
     " removes a horizontal strip and lands in ",
     TEX "$\\mathrm{Sym}^k V$", ", ", TT "pieriColumn(mu, cols, P)",
     " removes a vertical strip and lands in ", TEX "$\\wedge^k V$", ".  ",
     "Both maps are unique (up to scalar) ", TEX "$GL(V)$",
     "-equivariant inclusions ",
     TEX "$S_\\mu V \\to (\\,?\\text{-tensor}\\,) \\otimes S_\\lambda V$",
     "; ", TT "pieri", " uses Olver's formula with ",
     TEX "$c_J = \\prod (\\mu_{J_q} - \\mu_k + k - J_q)$",
     " on the symmetric (", TEX "$\\mathrm{Sym}$",
     ") side, while ", TT "pieriColumn", " uses the dual formula with ",
     TEX "$c'_J = \\prod (\\mu'_k - \\mu'_{J_q} + J_q - k)$",
     " on the wedge side, where ", TEX "$\\mu'$",
     " is the conjugate partition.  The extra sign in ", TEX "$c'_J$",
     " relative to a literal transpose accounts for the wedge anticommutativity.",
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  P = QQ[e_0..e_3, SkewCommutative => true];
	  M = pieriColumn({3,2,1}, {1}, P);
	  numRows M, numColumns M
	  ///,
     EXAMPLE lines ///
	  -- Multi-strip: remove a vertical strip of length 2.
	  P = QQ[e_0..e_3, SkewCommutative => true];
	  M = pieriColumn({3,2,1}, {1,2}, P);
	  numRows M, numColumns M
	  ///,
     SeeAlso => {pieri, fillingToPM, [pieri, Convention]}
     }

document {
     Key => {verifyWellDefined, (verifyWellDefined, Sequence, List, ZZ),
	  (verifyWellDefined, List, List, PolynomialRing),
	  [verifyWellDefined, Convention], [verifyWellDefined, Verbose],
	  [verifyWellDefined, Direction]},
     Headline => "verify an LR map respects the chosen convention's straightening relations",
     Usage => "verifyWellDefined((lambda, mu, nu), Q, n, Convention => ...)",
     Inputs => {
	  "(lambda, mu, nu)" => {ofClass Sequence, ", three partitions for the LR data"},
	  "Q" => {ofClass List, ", an LR tableau as returned by ", TO lrTableaux},
	  "n" => {ofClass ZZ, ", the dimension of ", TEX "$V$"},
	  Convention => String => {", basis convention to check"},
	  Verbose => Boolean => {", whether to print per-test progress (default true)"}
	  },
     Outputs => {Boolean => {"true if all generated tests pass, false otherwise"}},
     "For skeptical users.  Generates a few non-standard tableaux of shape lambda in the chosen basis convention, then for each:",
     UL {
	  "(a) computes ", TT "applyLR(shapes, Q, T, n, Convention => conv)", " directly (which uses the convention's straightening internally),",
	  "(b) independently straightens T into a sum of standard tableaux,",
	  "(c) computes the same sum applied to the standards,",
	  "(d) asserts (a) and (c) agree."
	  },
     "If the matrix were silently using the wrong straightening relations (e.g. PieriMaps row-Garnir on a SchurFunctors column-form Filling input), the check fails and prints the offending case.",
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  Qs = lrTableaux({3,2,1}, {2,1}, {2,1});
	  verifyWellDefined(({3,2,1},{2,1},{2,1}), Qs#0, 3, Convention => "Filling")
	  verifyWellDefined(({3,2,1},{2,1},{2,1}), Qs#0, 3, Convention => "Row")
	  ///,
     SeeAlso => {lrMap, applyLR, pmToFilling, Convention}
     }

document {
     Key => {dualPieri, (dualPieri, List, List, PolynomialRing), [dualPieri, Convention]},
     Headline => "GL-equivariant projection Sym^d V ⊗ S_lambda V --> S_mu V",
     Usage => "dualPieri(mu, boxes, P)\ndualPieri(mu, boxes, P, Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "mu" => {ofClass List,
	       ", a partition (the target shape): ",
	       TEX "$\\mu = \\lambda + (\\text{horizontal $d$-strip})$",
	       " where ", TEX "$d = \\#\\mathrm{boxes}$", "."},
	  "boxes" => {ofClass List,
	       ", a list of 1-indexed row indices recording the horizontal strip ",
	       TEX "$\\mu/\\lambda$", ".  Same convention as ", TO pieri, "."},
	  "P" => {ofClass PolynomialRing,
	       ", a polynomial ring whose generators carry the symmetric factor ",
	       TEX "$\\mathrm{Sym}^d V$", " on the source side."},
	  Convention => String => {", basis convention for both source and target Schur factors.  ",
	       TT "\"Row\"", " (default), ", TT "\"Filling\"", ", or ", TT "\"Weyl\"", "."}
	  },
     Outputs => {
	  Matrix => {"a ", TEX "$K$", "-matrix of size ",
	       TEX "$\\dim S_\\mu V \\times \\bigl(\\dim S_\\lambda V \\cdot \\binom{n + d - 1}{d}\\bigr)$",
	       ".  Columns are indexed by pairs ", TEX "$(T_\\lambda, \\alpha)$",
	       " (a standard tableau of ", TEX "$\\lambda$",
	       " paired with a degree-", TEX "$d$", " monomial of ",
	       TT "P", "), in lex order with ", TEX "$T_\\lambda$",
	       " outer and ", TEX "$\\alpha$", " inner."}
	  },
     "Returns the ", TEX "$GL_n$",
     "-equivariant projection ",
     TEX "$\\mathrm{Sym}^d V \\otimes S_\\lambda V \\to S_\\mu V$",
     " (unique up to scalar by Schur's lemma).  By Pieri's rule, ",
     TEX "$\\mathrm{Sym}^d V \\otimes S_\\lambda V$",
     " decomposes as a direct sum of ", TEX "$S_{\\mu'}$", " over all ",
     TEX "$\\mu' = \\lambda + (\\text{horizontal $d$-strip})$",
     "; this function returns the projection onto the summand selected by ",
     TEX "$\\mu$", ".",
     PARA{},
     "The construction stacks the inclusion matrices ", TO pieri,
     " for every addable horizontal ", TEX "$d$",
     "-strip into a square invertible matrix, then reads off the rows of its inverse corresponding to ",
     TEX "$S_\\mu$", ".  This realizes ",
     TEX "$\\mathrm{dualPieri} \\circ \\mathrm{pieri} = \\mathrm{Id}$",
     " on ", TEX "$S_\\mu V$", " exactly.",
     PARA{},
     "When ", TT "Convention => \"Filling\"",
     ", the source ", TEX "$S_\\lambda$", " factor and target ",
     TEX "$S_\\mu$",
     " are expressed in the SchurFunctors column-form basis; ",
     TT "\"Weyl\"",
     " uses the divided-power Weyl-module basis (matrix data is identical to ",
     TT "\"Row\"", ").",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  N = dualPieri({3,2,1}, {1}, P);
	  M = pieri({3,2,1}, {1}, P);
	  numRows N == numColumns M and numRows M == 3
	  ///,
     EXAMPLE lines ///
	  P = QQ[a,b,c,d];  -- interior-row removal mu={3,2,1}, k=2, lambda={3,1,1}
	  N = dualPieri({3,2,1}, {2}, P);
	  numRows N, numColumns N
	  ///,
     PARA{},
     BOLD "Performance.", " ", TO dualPieri, " caches the stacked block
     matrix used to invert the GL_n decomposition, keyed by ",
     TEX "$(\\lambda, d, n, K, \\mathrm{Convention})$",
     ".  The first call at a given key builds every Pieri inclusion ",
     TEX "$S_{\\mu'}\\to S_\\lambda \\otimes \\mathrm{Sym}^d V$",
     " for ", TEX "$\\mu'$", " ranging over all addable horizontal ",
     TEX "$d$", "-strips and stacks them into one square matrix ",
     TEX "$A$",
     "; subsequent calls (for sister summands ", TEX "$\\mu$",
     " of the same ambient tensor product) reuse ", TEX "$A$",
     " and only run a single back-solve, so they cost less than the
     corresponding ", TO pieri, " inclusion.  The selected rows of ",
     TEX "$A^{-1}$", " are computed via ", TT "solve(transpose A, E)",
     " (", TT "E", " a small standard-basis block) rather than by
     forming the full inverse.  ", TT "boxes",
     " is normalized to canonical (sorted ascending) order; pass the
     same canonical order to ", TO pieri, " for the round-trip ",
     TEX "$\\mathrm{dualPieri} \\circ \\mathrm{pieri} = \\mathrm{Id}$",
     ".",
     SeeAlso => {pieri, applyDualPieri, dualLR, verifyEquivariant, Convention}
     }

document {
     Key => {applyDualPieri, (applyDualPieri, Sequence, RingElement, BasicList, ZZ), [applyDualPieri, Convention]},
     Headline => "apply dualPieri to a single basis pair (poly, T_lambda)",
     Usage => "applyDualPieri((mu, boxes), poly, T, n)\napplyDualPieri(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(mu, boxes)" => {ofClass Sequence, ", the same data passed to ", TO dualPieri},
	  "poly" => {ofClass RingElement,
	       ", a homogeneous polynomial of degree ",
	       TEX "$d = \\#\\mathrm{boxes}$",
	       " in the polynomial ring ", TT "P", " (an element of ",
	       TEX "$\\mathrm{Sym}^d V$", ")"},
	  "T" => {ofClass BasicList, ", a tableau of shape ",
	       TEX "$\\lambda$", ".  Accepted as either a ", TT "List",
	       " or a ", TT "Filling", ".  In ", TT "\"Row\"", "/",
	       TT "\"Weyl\"", " convention a ", TT "List",
	       " is the PM row form; in ", TT "\"Filling\"", " convention a ",
	       TT "List", " is column form (auto-wrapped).  A ", TT "Filling",
	       " is always treated as column form.  Non-standard inputs are straightened first using the convention's relations."},
	  "n" => {ofClass ZZ, ", ", TEX "$\\dim V$"},
	  Convention => String => {", basis for source ", TEX "$S_\\lambda$",
	       " and target ", TEX "$S_\\mu$", "."}
	  },
     Outputs => {
	  List => {"a list of pairs ", TT "{coefficient, T_mu}",
	       " representing the image as a sum ",
	       TEX "$\\sum c_i \\cdot T_\\mu^{(i)}$", " in ",
	       TEX "$S_\\mu V$", ".  Zero coefficients are dropped."}
	  },
     "Convenience wrapper for ", TO dualPieri,
     ": evaluates the projection on a single source basis vector ",
     TEX "$\\mathrm{poly} \\otimes T_\\lambda$",
     " and returns a sparse representation of its image.  Useful for hand-checks and interactive exploration when the full ",
     TO dualPieri, " matrix is too large.",
     PARA{},
     "Input flexibility: ", TT "T", " may be in either tableau type.  For ",
     TT "Convention => \"Row\"", ", a Filling input is converted via ",
     TO fillingToPM, " before straightening; for ",
     TT "Convention => \"Filling\"", ", a List input is converted via ",
     TO pmToFilling, ".  Non-standard tableaux straighten first.",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  Tlam = {{0,0},{1}};   -- standard PM tableau of {2,1}
	  applyDualPieri(({3,1}, {1}), a, Tlam, 3)
	  ///,
     EXAMPLE lines ///
	  -- Non-standard input is straightened first (row-Garnir).
	  P = QQ[a,b,c,d];
	  applyDualPieri(({3,2,1}, {3}), a, {{1,0},{2}}, 4)
	  ///,
     SeeAlso => {dualPieri, pieri, applyLR, applyDualLR, Convention}
     }

document {
     Key => {dualLR, (dualLR, Sequence, List, ZZ), (dualLR, Sequence, List, PolynomialRing), [dualLR, Convention]},
     Headline => "GL-equivariant projection S_nu V ⊗ S_mu V --> S_lambda V at a chosen LR tableau",
     Usage => "dualLR((lambda, mu, nu), Q, n)\ndualLR((lambda, mu, nu), Q, P)\ndualLR(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(lambda, mu, nu)" => {ofClass Sequence, ", three partitions with ",
	       TEX "$|\\lambda| = |\\mu| + |\\nu|$", " and ",
	       TEX "$S_\\lambda V$", " appearing in ",
	       TEX "$S_\\nu V \\otimes S_\\mu V$", " (i.e. ",
	       TEX "$c^\\lambda_{\\mu,\\nu} > 0$", ")"},
	  "Q" => {ofClass List, ", an LR tableau of shape ",
	       TEX "$\\lambda/\\mu$", " with content ", TEX "$\\nu$",
	       " (as returned by ", TO lrTableaux,
	       ") singling out one of the ", TEX "$c^\\lambda_{\\mu,\\nu}$",
	       " copies of ", TEX "$S_\\lambda V$"},
	  "n" => {ofClass ZZ, ", or pass a polynomial ring whose ",
	       TT "numgens", " is used"},
	  Convention => String => {", basis convention for the three Schur factors."}
	  },
     Outputs => {
	  Matrix => {"a ", TEX "$\\mathbb{Q}$", "-matrix of size ",
	       TEX "$\\dim S_\\lambda V \\times (\\dim S_\\nu V \\cdot \\dim S_\\mu V)$",
	       ", the ", TEX "$GL_n$",
	       "-equivariant projection onto ", TT "Q",
	       "'s specific copy of ", TEX "$S_\\lambda V$", " inside ",
	       TEX "$S_\\nu V \\otimes S_\\mu V$"}
	  },
     "Returns the ", TEX "$GL_n$",
     "-equivariant projection onto the LR-summand of ",
     TEX "$S_\\nu V \\otimes S_\\mu V$", " indexed by ", TT "Q",
     " (unique up to scalar by Schur's lemma).  When the LR coefficient ",
     TEX "$c^\\lambda_{\\mu,\\nu}$",
     " exceeds 1, distinct LR tableaux ", TT "Q",
     " give different projections, each onto its own copy of ",
     TEX "$S_\\lambda V$", "; ",
     TEX "$\\mathrm{dualLR}_Q \\circ \\mathrm{lrMap}_{Q'} = 0$",
     " when ", TEX "$Q \\neq Q'$", ", and ",
     TEX "$\\mathrm{dualLR}_Q \\circ \\mathrm{lrMap}_Q = \\mathrm{Id}$",
     ".",
     PARA{},
     "Construction: stack the LR inclusions ", TO lrMap,
     " for every ", TEX "$(\\lambda', Q')$", " with ",
     TEX "$c^{\\lambda'}_{\\mu,\\nu} > 0$",
     " into a square invertible matrix on ",
     TEX "$S_\\nu V \\otimes S_\\mu V$",
     " (square by the LR/Pieri rule) and read off the rows of its inverse corresponding to ",
     TEX "$(\\lambda, Q)$", ".",
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  M = lrMap(shapes, Q, 3);   -- inclusion S_(2,1) V -> S_(1,1) V ⊗ S_(1) V
	  N = dualLR(shapes, Q, 3);  -- projection back onto Q's copy
	  N * M == id_(QQ^(numColumns M))
	  ///,
     EXAMPLE lines ///
	  Qs = lrTableaux({3,2,1}, {2,1}, {2,1});  -- multiplicity-2 case
	  M0 = lrMap(({3,2,1},{2,1},{2,1}), Qs#0, 3);
	  M1 = lrMap(({3,2,1},{2,1},{2,1}), Qs#1, 3);
	  N0 = dualLR(({3,2,1},{2,1},{2,1}), Qs#0, 3);
	  N0 * M0 == id_(QQ^(numColumns M0))   -- recovers Q_0's copy
	  N0 * M1 == 0                          -- annihilates Q_1's copy
	  ///,
     SeeAlso => {lrMap, lrTableaux, applyDualLR, dualPieri, verifyEquivariant, Convention}
     }

document {
     Key => {applyDualLR, (applyDualLR, Sequence, List, List, ZZ), [applyDualLR, Convention]},
     Headline => "apply dualLR to a single basis pair (T_nu, T_mu)",
     Usage => "applyDualLR((lambda, mu, nu), Q, {T_nu, T_mu}, n)\napplyDualLR(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(lambda, mu, nu)" => {ofClass Sequence},
	  "Q" => {ofClass List, ", an LR tableau"},
	  "{T_nu, T_mu}" => {ofClass List,
	       ", a 2-element list containing the source basis pair (a tableau of shape ",
	       TEX "$\\nu$", " and a tableau of shape ", TEX "$\\mu$",
	       ").  Each may be a ", TT "List", " or a ", TT "Filling",
	       ", interpreted in the natural form of the convention (see ",
	       TO applyPieri, "), independently."},
	  "n" => {ofClass ZZ},
	  Convention => String
	  },
     Outputs => {
	  List => {"list of pairs ", TT "{coefficient, T_lambda}",
	       " representing the image in ", TEX "$S_\\lambda V$"}
	  },
     "Note: the ", TT "{T_nu, T_mu}", " argument must be a List (not a Sequence), since M2 flattens nested Sequences in function-call arg lists but does not flatten Lists.  Non-standard inputs are straightened first.",
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  applyDualLR(shapes, Q, {{{0},{1}}, {{0},{}}}, 3)
	  ///,
     SeeAlso => {dualLR, lrMap, applyLR, applyDualPieri, Convention}
     }

document {
     Key => {verifyEquivariant,
	  (verifyEquivariant, Matrix, List, List, PolynomialRing),
	  (verifyEquivariant, Matrix, Sequence, ZZ),
	  [verifyEquivariant, Convention],
	  [verifyEquivariant, Verbose],
	  [verifyEquivariant, Direction],
	  Direction
	  },
     Headline => "rigorously verify GL_n-equivariance of a Schur-rep matrix",
     Usage => "verifyEquivariant(M, mu, boxes, P)               -- pieri-type inclusion\nverifyEquivariant(M, shapes, n)                  -- lrMap-type inclusion\nverifyEquivariant(M, mu, boxes, P, Direction => \"Dual\")  -- dualPieri-type projection\nverifyEquivariant(M, shapes, n, Direction => \"Dual\")     -- dualLR-type projection",
     Inputs => {
	  "M" => {ofClass Matrix, ", the matrix to test for ", TEX "$GL_n$",
	       "-equivariance"},
	  Convention => String => {", basis convention used by ", TT "M",
	       "'s rows and columns.  Default ", TT "\"Row\"", "."},
	  Verbose => Boolean => {", if ", TT "true",
	       " print the first failing ", TEX "$(i, j, T)$",
	       " when the test fails.  Default ", TT "false", "."},
	  Direction => String => {", ", TT "\"Inclusion\"", " (default) or ",
	       TT "\"Dual\"", " to indicate the direction of the map."}
	  },
     Outputs => {
	  Boolean => {"true if ", TT "M",
	       " commutes with every Chevalley generator ", TEX "$E_{i,j}$",
	       " of ", TEX "$\\mathfrak{gl}_n$", " (",
	       TEX "$i \\neq j$", "); false otherwise"}
	  },
     "Tests whether a matrix ", TT "M",
     " between Schur-functor representations is genuinely ", TEX "$GL_n$",
     "-equivariant by checking that ",
     TEX "$M \\circ \\rho_{\\mathrm{src}}(E_{i,j}) = \\rho_{\\mathrm{tgt}}(E_{i,j}) \\circ M$",
     " for every Chevalley generator ", TEX "$E_{i,j}$", ".  Since the ",
     TEX "$E_{i,j}$ ($i \\neq j$)",
     " generate the strictly upper- and strictly lower-triangular parts of ",
     TEX "$\\mathfrak{gl}_n$",
     ", and our matrices commute with the diagonal automatically (entries are weight-homogeneous), commutativity with all ",
     TEX "$E_{i,j}$", " is equivalent to full ", TEX "$GL_n$",
     "-equivariance.",
     PARA{},
     "Action conventions: ", TEX "$E_{i,j}$", " acts on ",
     TEX "$V = K^n$", " by ",
     TEX "$x_l \\mapsto \\delta_{l,j}\\, x_i$",
     " (i.e. the differential operator ",
     TEX "$x_i\\, \\partial/\\partial x_j$", " on polynomials), on a Schur module ",
     TEX "$S_\\lambda V$", " by replacing each entry equal to ",
     TEX "$j$", " with ", TEX "$i$",
     " in a tableau (summed over positions, then straightened), and on a tensor product by Leibniz.",
     PARA{},
     "Four overloads cover the maps in this package:",
     UL {
	  {TT "verifyEquivariant(M, mu, boxes, P)", " -- ",
	   TEX "$M \\colon S_\\mu V \\to \\mathrm{Sym}^d V \\otimes S_\\lambda V$",
	   " (or ", TEX "$\\wedge^d V$",
	   " for skew-commutative ", TT "P",
	   "), the inclusion produced by ", TO pieri, " or ", TO pieriColumn, "."},
	  {TT "verifyEquivariant(M, (lambda, mu, nu), n)", " -- ",
	   TEX "$M \\colon S_\\lambda V \\to S_\\nu V \\otimes S_\\mu V$",
	   ", the inclusion produced by ", TO lrMap, "."},
	  {TT "verifyEquivariant(M, mu, boxes, P, Direction => \"Dual\")",
	   " -- ",
	   TEX "$M \\colon \\mathrm{Sym}^d V \\otimes S_\\lambda V \\to S_\\mu V$",
	   ", the projection produced by ", TO dualPieri, "."},
	  {TT "verifyEquivariant(M, (lambda, mu, nu), n, Direction => \"Dual\")",
	   " -- ",
	   TEX "$M \\colon S_\\nu V \\otimes S_\\mu V \\to S_\\lambda V$",
	   ", the projection produced by ", TO dualLR, "."}
	  },
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  M = pieri({3,2,1}, {2}, P);
	  verifyEquivariant(M, {3,2,1}, {2}, P)
	  ///,
     EXAMPLE lines ///
	  P = QQ[a,b,c];   -- verify the dual Pieri projection
	  N = dualPieri({3,2,1}, {2}, P);
	  verifyEquivariant(N, {3,2,1}, {2}, P, Direction => "Dual")
	  ///,
     EXAMPLE lines ///
	  Q = (lrTableaux({3,2,1}, {2,1}, {2,1}))#0;  -- multiplicity-2 LR inclusion
	  M = lrMap(({3,2,1}, {2,1}, {2,1}), Q, 3);
	  verifyEquivariant(M, ({3,2,1}, {2,1}, {2,1}), 3)
	  ///,
     SeeAlso => {pieri, lrMap, dualPieri, dualLR, verifyWellDefined}
     }

document {
     Key => {applyPieri, (applyPieri, Sequence, BasicList, PolynomialRing),
	  [applyPieri, Convention]},
     Headline => "apply pieri to a single source tableau T_mu",
     Usage => "applyPieri((mu, boxes), T, P)\napplyPieri(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(mu, boxes)" => {ofClass Sequence, ", the same data passed to ", TO pieri},
	  "T" => {ofClass BasicList, ", a tableau of shape ", TEX "$\\mu$",
	       ".  May be a ", TT "List", " or a ", TT "Filling", ".  In ",
	       TT "\"Row\"", "/", TT "\"Weyl\"",
	       " convention a ", TT "List", " is the PM row form; in ",
	       TT "\"Filling\"", " convention a ", TT "List",
	       " is column form (auto-wrapped).  A ", TT "Filling",
	       " is always treated as column form."},
	  "P" => {ofClass PolynomialRing, ", whose generators carry ",
	       TEX "$\\mathrm{Sym}^d V$"},
	  Convention => String
	  },
     Outputs => {
	  List => {"a list of pairs ", TT "{poly, T_lambda}",
	       " where each poly is a homogeneous degree-", TEX "$d$",
	       " form and ", TEX "$T_\\lambda$",
	       " is a standard tableau of ", TEX "$\\lambda$",
	       "; the image is ",
	       TEX "$\\sum \\mathrm{poly}_i \\otimes T_{\\lambda,i}$", " in ",
	       TEX "$\\mathrm{Sym}^d V \\otimes S_\\lambda V$"}
	  },
     "Convenience point-evaluator for ", TO pieri,
     ": applies the inclusion map to a single source basis vector ",
     TT "T",
     " and returns its image as a sparse representation.  Non-standard inputs are straightened first.",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  T = (standardTableaux(3, {3,2,1}))#0;
	  applyPieri(({3,2,1}, {1}), T, P)
	  ///,
     SeeAlso => {pieri, applyDualPieri, applyLR, applyPieriColumn, Convention}
     }

document {
     Key => {applyPieriColumn,
	  (applyPieriColumn, Sequence, BasicList, PolynomialRing),
	  [applyPieriColumn, Convention]},
     Headline => "apply pieriColumn to a single source tableau T_mu",
     Usage => "applyPieriColumn((mu, cols), T, P)\napplyPieriColumn(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(mu, cols)" => {ofClass Sequence, ", the same data passed to ", TO pieriColumn},
	  "T" => {ofClass BasicList, ", a tableau of shape ", TEX "$\\mu$",
	       ".  May be a ", TT "List", " or a ", TT "Filling",
	       " (interpreted in the natural form of the convention; see ",
	       TO applyPieri, ")."},
	  "P" => {ofClass PolynomialRing, ", a SkewCommutative ring whose generators carry ",
	       TEX "$\\wedge^d V$"},
	  Convention => String
	  },
     Outputs => {
	  List => {"a list of pairs ", TT "{wedge_poly, T_lambda}",
	       " representing the image in ",
	       TEX "$\\wedge^d V \\otimes S_\\lambda V$"}
	  },
     "Column-form analogue of ", TO applyPieri, ".",
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  P = QQ[e_0..e_2, SkewCommutative => true];
	  sfStdTab = value (SchurFunctors#"private dictionary"#"standardTableaux");
	  F = (sfStdTab(3, {2,1}))#0;
	  applyPieriColumn(({2,1}, {1}), F, P)
	  ///,
     SeeAlso => {pieriColumn, applyPieri, dualPieriColumn, Convention}
     }

document {
     Key => {dualPieriColumn,
	  (dualPieriColumn, List, List, PolynomialRing),
	  [dualPieriColumn, Convention]},
     Headline => "GL-equivariant projection wedge^d V ⊗ S_lambda V --> S_mu V",
     Usage => "dualPieriColumn(mu, cols, P)\ndualPieriColumn(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "mu" => {ofClass List, ", the target shape: ",
	       TEX "$\\mu = \\lambda + (\\text{vertical $d$-strip})$",
	       " with ", TEX "$d = \\#\\mathrm{cols}$"},
	  "cols" => {ofClass List, ", a list of column indices recording the strip ",
	       TEX "$\\mu/\\lambda$"},
	  "P" => {ofClass PolynomialRing, ", a SkewCommutative ring (",
	       TEX "$\\wedge^d V$", " is encoded there)"},
	  Convention => String
	  },
     Outputs => {
	  Matrix => {"a ", TEX "$K$", "-matrix of size ",
	       TEX "$\\dim S_\\mu V \\times \\bigl(\\dim S_\\lambda V \\cdot \\binom{n}{d}\\bigr)$",
	       ".  Columns indexed by ",
	       TEX "$(T_\\lambda, \\alpha)$",
	       " pairs (", TEX "$\\alpha$", " an exterior monomial)."}
	  },
     "Column-form analogue of ", TO dualPieri,
     ": the ", TEX "$GL_n$",
     "-equivariant projection from ",
     TEX "$\\wedge^d V \\otimes S_\\lambda V$", " onto the chosen ",
     TEX "$S_\\mu V$",
     " summand (unique up to scalar).  Construction stacks ",
     TO pieriColumn,
     " inclusions for every addable vertical ", TEX "$d$",
     "-strip and inverts.",
     EXAMPLE lines ///
	  P = QQ[e_0..e_2, SkewCommutative => true];
	  M = pieriColumn({2,1}, {1}, P);
	  N = dualPieriColumn({2,1}, {1}, P);
	  numRows N, numColumns N
	  ///,
     SeeAlso => {pieriColumn, dualPieri, applyDualPieriColumn, Convention}
     }

document {
     Key => {applyDualPieriColumn,
	  (applyDualPieriColumn, Sequence, RingElement, BasicList, ZZ),
	  [applyDualPieriColumn, Convention]},
     Headline => "apply dualPieriColumn to a single basis pair (wedge_poly, T_lambda)",
     Usage => "applyDualPieriColumn((mu, cols), poly, T, n)\napplyDualPieriColumn(..., Convention => \"Row\" | \"Filling\" | \"Weyl\")",
     Inputs => {
	  "(mu, cols)" => {ofClass Sequence, ", the data passed to ", TO dualPieriColumn},
	  "poly" => {ofClass RingElement, ", a homogeneous degree-",
	       TEX "$d$", " wedge polynomial"},
	  "T" => {ofClass BasicList, ", a tableau of shape ", TEX "$\\lambda$",
	       ".  May be a ", TT "List", " or a ", TT "Filling",
	       " (interpreted in the natural form of the convention; see ",
	       TO applyPieri, ")."},
	  "n" => {ofClass ZZ, ", ", TEX "$\\dim V$"},
	  Convention => String
	  },
     Outputs => {
	  List => {"a list of pairs ", TT "{coefficient, T_mu}",
	       " representing the image in ", TEX "$S_\\mu V$"}
	  },
     EXAMPLE lines ///
	  needsPackage "SchurFunctors";
	  P = QQ[e_0..e_2, SkewCommutative => true];
	  sfStdTab = value (SchurFunctors#"private dictionary"#"standardTableaux");
	  T = (sfStdTab(3, {1,1}))#0;
	  applyDualPieriColumn(({2,1}, {1}), e_0, T, 3)
	  ///,
     SeeAlso => {dualPieriColumn, pieriColumn, applyDualPieri, Convention}
     }

document {
     Key => {symbolicForm, (symbolicForm, Matrix)},
     Headline => "print a Schur-rep matrix as a basis-labeled symbolic map",
     Usage => "symbolicForm M",
     Inputs => {
	  "M" => {ofClass Matrix, ", typically the output of ", TO pieri, ", ",
	       TO pieriColumn, ", ", TO lrMap, ", ", TO dualPieri, ", ",
	       TO dualPieriColumn, ", or ", TO dualLR, " (which attach
	       source/target basis labels to ", TT "M.cache",
	       ").  Plain matrices are accepted too: integer indices ",
	       TT "1..rank", " are used as labels."}
	  },
     Outputs => {
	  Net => {"a netList; each row shows a source basis element ",
	       TT "i", " and the nonzero terms of ", TT "M(i)",
	       " as a list of ", TT "(target_label, coefficient)", " pairs"}
	  },
     "Pretty-prints the matrix as the GL-equivariant map it represents on the
     labeled bases.  Each row of the netList corresponds to a source basis
     element (a tableau, or a (tableau, monomial) pair, etc.) and lists the
     image as a sum over target basis elements with their coefficients.  Rows
     where the image is identically zero appear with an empty list.",
     PARA{},
     "After ", TT "symbolicForm M", " is called once, the rule is cached on ",
     TT "M.cache#\"rule\"", " so subsequent calls reuse it.  ",
     "For interactive exploration, you can also evaluate the rule on a single
     basis element directly via ", TT "(M.cache#\"rule\")(label)", ".",
     EXAMPLE lines ///
	  P = QQ[a,b,c];
	  M = pieri({2,1}, {1}, P);
	  symbolicForm M
	  ///,
     EXAMPLE lines ///
	  shapes = ({2,1}, {1}, {1,1});
	  Q = (lrTableaux shapes)#0;
	  N = lrMap(shapes, Q, 3);
	  symbolicForm N
	  ///,
     SeeAlso => {pieri, lrMap, dualPieri, dualLR, applyLR, applyPieri}
     }
