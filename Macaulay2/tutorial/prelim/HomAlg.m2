-- Tutorial:Homological Algebra
--
-- \font\xmplbx = cmbx10 scaled \magstephalf
-- \font\xmplbxti = cmbxti10 scaled \magstephalf
-- \def\section#1{\bigskip\centerline{\xmplbx #1}\bigskip}
-- \def\example#1#2{\vglue .7\baselineskip
-- \leftline{\xmplbx Example #1.\quad\xmplbxti #2}
-- \vglue .5\baselineskip}
--
-- \example{}{Generic projection of an Enriques surface}
-- We study the generic projection of a specific Enriques
-- surface in $P^5$.  The ideal of this surface is generated
-- by the 3 by 3 minors of a random symmetric 4 by 4 matrix
-- of linear forms.

R = ZZ/32003[vars(0..9)]
m = genericSymmetricMatrix(R,a,4)
S = ZZ/32003[a..f]
F = map(S,R,random(S^1, S^{10:-1}))
m = F m
I = minors(3,m)
degree I
betti I

-- Form the generic projection to the cdef plane:
S1 = ZZ/32003[a..f,MonomialOrder=>ProductOrder{2,4}]
J = substitute(I,S1)
time gb J

-- If we set the poincare polynomial, we get better performance...
J1 = substitute(I,S1)
(coker gens J1).poincare = poincare coker gens I
time gb J1

p = leadTerm(2, J1)
p1 = coefficients({0,1},transpose p);
monoms = p1#0
p1 = p1#1
Ka2 = compress flatten submatrix(p1, {4})
Kab = compress flatten submatrix(p1, {5})
Kb2 = compress flatten submatrix(p1, {6})
Ka = compress flatten submatrix(p1, {7})
Kb = compress flatten submatrix(p1, {8})

-- Every point in the base will have a2, ab in the Groebner basis.
Ka2 
Kab
decompose ideal Kab

-- What about Kb2 ?
Kb2
Kb2comps = decompose ideal Kb2
Kb2comps/codim
Kb2comps/degree
-- So Kb2 is a set of 4 points

Ka
degree coker Ka
dim coker Ka
-- So Ka is a set of 38 points

-- These two sets of points are disjoint:
codim ideal(Ka | Kb2)

degree coker Kb
codim coker Kb
-- So V(Kb) is a curve of degree 30

-- It is easy to check that all 38+4 points lie on this curve:
Kb % Ka
Kb % Kb2

-- The final results:
-- Let the image of the surface be Y, a surface of degree 10.
-- Let C in Y be the curve of degree 30.
-- Let X1, X2 be the 4 points (resp 38 points), these are contained in C.

-- The initial ideal of a fiber off C is (a,b).  (map is 1-1)
-- The in ideal of a general fiber on C (i.e. not on X1,X2) is
--  $(a, b^2)$  (double point)
--
-- The in ideal of a fiber of X2 is $(a^2, a*b, b^2)$ (triple point)
-- The in ideal of a fiber of X1 is $(a, b^3)$  (triple point, but
-- where the three points are collinear!!! Counterexample to published
-- results, which destroys several well known theorems, at least their proofs).

-- \example{}{Simplicial Homology}

-- The following will be used in a bit...
mybasis = (d,I) -> matrix (basis(d, coker I))

-- The first example is the torus.
R = ZZ/101[a..g]/(a^2,b^2,c^2,d^2,e^2,f^2,g^2)
toZZ = map(ZZ,R,{1,1,1,1,1,1,1})

--$
faces = matrix{{
     a*b*d, b*c*e, c*d*f, d*e*g, e*f*a, f*g*b, g*a*c,
     b*d*e, c*e*f, d*f*g, e*g*a, f*a*b, g*b*c, a*c*d}}
--
torus = mybasis(3,faces)

vertices = mybasis(1,torus)
edges = mybasis(2,torus)
triangles = mybasis(3,torus)

koszul(vertices, edges)

-- The following function defines the boundary map
--$
boundarymap = (i,delta) -> (
     i1faces := mybasis(i-1,delta);
     ifaces := mybasis(i,delta);
     m := koszul(i1faces, ifaces);
     toZZ m)
--
-- Put them all together into a chain complex
--$
simplicialChainComplex = (delta) -> (
     bds := apply(2..4, i -> boundarymap(i,delta));
     chainComplex toSequence bds)
--
boundarymap(1,torus)
boundarymap(2,torus)
boundarymap(3,torus)
boundarymap(4,torus)

L = simplicialChainComplex torus
L.dd
H = HH_1(L)
-- The following line finds a presentation of this module
-- (pruning to a minimal presentation is not implemented yet).
m = modulo(generators H, relations H)
gens gb m
-- 2 dimensional, as expected.

-- Next example: the real projective plane
R = ZZ/101[a..f]/(a^2,b^2,c^2,d^2,e^2,f^2)
toZZ = map(ZZ,R,{1,1,1,1,1,1})

--$
RP2 = matrix{{a*b*c, a*b*e, a*c*d, a*d*f, a*e*f, 
	  b*c*f, b*d*e, b*d*f, c*d*e, c*e*f}}
--

LRP2 = simplicialChainComplex RP2
H = HH_2(LRP2)
H = HH_1(LRP2)
m = modulo(gens H, relations H)
gens gb m
-- result is ZZ/2.
