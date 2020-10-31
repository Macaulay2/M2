-- Given a variety $X$ in projective
-- $r$-space ${\bf P}^r$, the Fano scheme
-- $Fano_k(X)$ is the natural parameter 
-- space for the linear $k$-planes
-- lying on $X$.  In this tutorial we explore
-- the methods for computing it. The tutorial
-- is in three parts
--
--
-- A. The twenty-seven lines
--
-- B. General methods
--
-- C. Surfaces of degree $4$ in ${\bf P}^5$
--
-- In section A, we treat by hand the Fano
-- variety of lines on a nonsingular cubic
-- surface in ${\bf P}^3$, and find that there are
-- indeed $27$ lines lying on the surface.
--
-- In section B, we explain a general purpose
-- function, written to compute Fano schemes.
--
-- There are (up to linear transformations) just
-- $3$ nondegenerate smooth surfaces 
-- of degree $4$ in ${\bf P}^5$: the Veronese embedding
-- of ${\bf P}^2$ and the rational normal scrolls
-- $S(1,3)$ and $S(2,2)$.  Can they be distinguished
-- by their Fano varieties of lines?  
-- To find out, read section C!

-----------------------------------------------
-- A. Lines on the nonsingular cubic in ${\bf P}^3$
-----------------------------------------------
-- First make the homogeneous coordinate
-- ring of the ambient projective $3$-space
R = ZZ/32003[a,b,c,d]

-- and the ideal of a nonsingular cubic
X = ideal(a^3+b^3+c^3+d^3)

-- We make a parametrized indeterminate line in 
-- our projective space, adding parameters $s,t$
-- for the line and two points $p_0..p_3$ and
-- $q_0..q_3$ representing the points 0 and infinity
-- on the line. 

KK = coefficientRing R 

S = KK [s,t,p_0..p_3,q_0..q_3]

-- Then we make a map $F$ from $R$ to the new ring
-- sending the variables to the coordinates
-- of the general point on the line

--^
F = map(S,R,
       s*matrix{{p_0..p_3}} +
       t*matrix{{q_0..q_3}}
       )
--$
-- We now apply $F$ to the ideal of $X$
FX = F X
-- and the condition we want becomes the condition
-- that {\tt FX} vanishes identically in $s,t$.
-- The following line produces the coefficients:
cFX = last coefficients(gens FX, Variables => {s,t})

-- The interface to the {\tt coefficients} routine is a bit
-- baroque, and might change in the future.  For now,
-- the $\{0,1\}$ says to find the coefficients of each column 
-- of the matrix, with respect to the first two variables.
-- The routine returns a list of two matrices, the
-- second one being the one we need (index 1, since
-- all indices start at 0 in Macaulay2)
--
-- We can get rid of some of the variables of $S$,
-- to ease the computation:

S1 = KK[p_0..p_3,q_0..q_3]
cFX = substitute(cFX, S1)

-- The ring we want is the quotient

S1bar = S1/ideal cFX

-- Now we want to move to the Grassmannian,
-- so we take a new polynomial ring in 6 variables
-- that will correspond to the minors of the
-- matrix with rows $p_0..p_3$ and $q_0..q_3$,

GR = coefficientRing R[x_0..x_5]

-- We define a map sending the $x_i$ to the minors,
-- regarded as elements of S1bar
--^
M = substitute(
    exteriorPower(2, matrix{{p_0..p_3},{q_0..q_3}}),
    S1bar)
--$
gr = map (S1bar, GR, M)
fano = trim ker gr

-- {\tt trim} replaces the given set of generators
-- with a minimal set of generators.
-- We get an ideal representing points:
codim fano
-- and the number of these points -- the number
-- of the corresponding lines - is 27:
degree fano

-- It is interesting to note 
-- that the ideal of the
-- Fano scheme that we have produced is NOT
-- saturated, as the number (25) of cubics it
-- contains is less than $56 - 27 = 29$:
betti fano

-- Possible next steps in this computation would
-- be to compute the Jacobian ideal of Fano to
-- show that we really got 27 distinct lines, etc.

---------------------------------------------
-- B. General methods
---------------------------------------------
--    The first step in writing a program is to 
-- decide what the program should do, and it
-- is just as well to write the documentation
-- at this point .
--
-- The documentation has the following form.
--^
needsPackage "Text"
--$
--^
document {
     Key => Fano2, 
        TT "Fano2(k,X,GR) or  Fano2(k,X)", " -- computes 
        the ideal of a Fano scheme in the Grassmannian.",
        PARA{},
        "Given an ideal X representing a projective variety 
        in P^r, a positive integer k<r, and optionally a 
        ring GR with (exactly) r+1 choose k+1 variables, 
        representing the ambient space of the Grassmannian of 
        k-planes in P^r, this routine returns the ideal in
        GR of the Fano scheme that parametrizes the k-planes 
        lying on X. If the optional third argument is not 
        present, the routine fabricates its own local ring, 
        and returns an ideal over it."
        };
--$
-- If we take the variety that is the whole
-- of ${\bf P}^r$, we get the Grassmannian.  
-- It is useful to be able to 
-- make the ring representing the
-- ambient space of the Grassmannian beforehand
-- by hand, so the ideals of several Fano 
-- varieties can be compared.  But often
-- we won't need this.  Thus we make the
-- function capable of accepting this ambient
-- ring as an argument, or of fending for
-- itself if no ambient ring is given.

--^
document {
     Key => symbol Grassmannian2, 
    TT "Grassmannian2(k,r,R) or 
        Grassmannian2(k,r)",
       "-- Given natural numbers k <= r,
        and optionally a ring R with at least binomial(r+1,k+1)
        variables, the routine defines the ideal of the 
        Grassmannian of projective k-planes in P^r, using 
        the first binomial(r+1,k+1) variables of R. 
        If R is not given, the routine makes and uses
        ZZ/31991[vars(0..binomial(r+1,k+1)-1]."
        };
--$
-- In order to make {\tt Fano2} handle an optional
-- number of arguments, we make it a method
-- instead of a function, documenting it as follows.
Fano2 = method()
-- Here is the code for the first case, with
-- comments interspersed:
--^
Fano2(ZZ,Ideal,Ring) := (k,X,GR) -> (
  -- Get info about the base ring of X:
  -- The coefficient ring (to make new rings of
  -- the same characteristic, for example)
  -- and the number of variables
  KK:=coefficientRing ring X;
  r := (numgens ring X) - 1;
  -- Next make private variables for our 
  -- intermediate rings, to avoid interfering
  -- with something outside:
  t:=symbol t;
  p:=symbol p;
  -- And rings
  S1 := KK[t_0..t_k];
  S2 := KK[p_0..p_(k*r+k+r)];
  S := tensor(S1,S2);
  -- Over S we have a generic point of a generic
  -- line, represented by a row vector, which
  -- we use to define a map from the base ring
  -- of X
  F := map(S,ring X,
          genericMatrix(S,S_0,1,k+1)*
          genericMatrix(S,S_(k+1),k+1,r+1)
          );
  -- We now apply F to the ideal of X
  FX := F X;
  -- and the condition we want becomes the condition
  -- that FX vanishes identically in the t_i.
  -- The following line produces the matrix of
  -- coefficients of the monomials in the 
  -- variables labelled 0..k:
  cFX := last coefficients (gens FX, Variables => toList apply(0..k, i -> S_i));
  -- We can get rid of the variables t_i
  -- to ease the computation:
  cFX = substitute(cFX, S2);
  -- The ring we want is the quotient
  S2bar := S2/ideal cFX;
  -- Now we want to move to the Grassmannian,
  -- represented by the ring GR
  -- We define a map sending the variables of GR
  -- to the minors of the generic matrix in the
  -- p_i regarded as elements of S1bar
  gr := map(S2bar,GR,
            exteriorPower(k+1, 
            genericMatrix(S2bar,S2bar_0,k+1,r+1)
            )
           );
  -- and the defining ideal of the Fano variety is
  ker gr
)
--$
-- Here is the code for the second case, which reduces to the first.
--^
Fano2(ZZ, Ideal) := (k,X) -> (
  KK:=coefficientRing ring X;
  r := (numgens ring X) - 1;
  -- We can specify a private ring with binomial(r+1,k+1)
  -- variables as follows
  GR := KK[Variables => binomial(r+1,k+1)];
  -- the work is done by
  Fano2(k,X,GR)
)
--$
-- With the zero ideal we get the Grassmannian
-- of projective $k$-planes in ${\bf P}^r$:

Grassmannian2 = method()
--^
Grassmannian2(ZZ,ZZ,Ring) := (k,r,R) ->( 
        KK := coefficientRing R;
        RPr := KK[Variables => r+1];
        Pr := ideal(0_RPr);
        Fano2(k,Pr)
     )
--$
--^
Grassmannian2(ZZ,ZZ) := (r,k) -> (
        R := ZZ/31991[
               vars(0..(binomial(r+1,k+1)-1))
                    ];
        Grassmannian2(k,r,R)
                     )
--$
-- As a first example we can try
-- the Fano of lines on the nonsingular quadric
-- in ${\bf P}^3$

KK = ZZ/31991
R = KK[a,b,c,d]
X = ideal(a*b-c*d)
I = Fano2(1,X)

-- we investigate by checking its dimension
-- and degree

dim I

-- The answer ``2'' means that $I$ is the ideal
-- of a curve in ${\bf P}^5$, the ambient space of
-- the Grassmannian of lines.

degree I

-- The answer is 4.  In fact, the ideal $I$ represents
-- the union of two conics.

----------------------------------------------
-- C. Surfaces of degree $4$ in ${\bf P}^5$
----------------------------------------------

-- We now turn to the three surfaces of 
-- degree $4$ in ${\bf P}^5$, and make their ideals:
--
-- The ring of ${\bf P}^5$
KK = ZZ/31991
P5 = KK[a..f]

-- It happens that the ideals of
-- all three surfaces are generated by minors
-- of suitable matrices:
--
-- The Veronese embedding of ${\bf P}^2$:

MVero = genericSymmetricMatrix(P5,a,3)
Vero = minors(2,MVero)
        
-- The other scrolls are defined by the minors
-- of matrices that are made from 
-- ``catalecticant'' blocks, that is, from
-- matrices such as
-- $$\begin{pmatrix}b & c & d & e \\
--             c & d & e & f \end{pmatrix}$$
-- which are manufactured by

--^
catalecticant = (R,v,m,n) -> 
        map(R^m,n,(i,j)-> R_(i+j+v))
--$
-- for example
catalecticant(P5,1,2,4)
-- produces the example above.

-- The rational normal scroll {\tt S13}, which is
-- the union of lines joining a line with
-- the corresponding points of a twisted cubic
-- in a disjoint subspace of ${\bf P}^5$

--^
M13 = catalecticant(P5,0,2,1) |
           catalecticant(P5,2,2,3)
--$
S13 = minors(2,M13)

-- Finally, the rational normal scroll {\tt S22},
-- which is made by a similar construction 
-- starting with two conics in ${\bf P}^5$

--
M22 = catalecticant(P5,0,2,2) | catalecticant(P5,3,2,2)
--
S22 = minors(2, M22)
      
-- It is interesting to note that the numerical
-- invariants of these surfaces are very hard
-- to distinguish.  In particular, the graded
-- betti numbers

Verores = res coker gens Vero
S22res = res coker gens S22
S13res = res coker gens S13
betti Verores
betti S22res
betti S13res

-- coincide, so the three cannot be distinguished
-- on the basis of these or on the basis of 
-- the (weaker) invariants the Hilbert series
-- or Hilbert polynomials. But the Fano
-- varieties are more obviously different:
--
-- We compute the Fano varieties of lines
-- on each of our surfaces.

FVero = Fano2(1, Vero)
betti gens FVero
-- The ideal contains all $120$ quadrics,
-- and represents the empty set:  The
-- Veronese surface contains no lines!

FS13 = Fano2(1, S13)

-- It turns out that the dimension (1) and 
-- degree (4) of these varieties coincide!
-- Moreover, since the ideals are not saturated,
-- one cannot directly compare the Hilbert
-- series or free resolutions (of course
-- one could first compute a saturation).
-- But there is the arithmetic genus,
-- that is, $1-H(0)$, where $H$ is the Hilbert
-- polynomial.  

hilbertPolynomial coker gens FS13

-- The output, $4 {\bf P}^1 - 2 {\bf P}^0$, means ``four times the
-- Hilbert polynomial of the projective line
-- minus 2''; that is, the polynomial is 
-- $H(d) = 4d + 2$; so arithmetic genus is $-1$.

FS22 = Fano2(1, S22)
hilbertPolynomial coker gens FS22

-- The output, $4 {\bf P}^1 - 3 {\bf P}^0$, means $H(d) = 4d + 1$,
-- arithmetic genus $-1$.

-- In fact, the Fano variety of {\tt S22} consists of
-- a projective line, embedded as a smooth
-- rational quartic; 
-- while the Fano variety of {\tt S13} consists of 
-- a smooth rational quartic (corresponding to
-- the rulings of the surface {\tt S13} and an
-- isolated point, corresponding to the section
-- of negative self-intersection on the surface).
