-- 	In this tutorial we introduce a number
-- of basic operations using Gröbner bases, and
-- at the same time become familiar with a range
-- of useful Macaulay2 constructs. The sections are:
--
-- A.  First steps; example with a monomial curve
--
-- B.  Random regular sequences
--
-- C.  Division with remainder
--
-- D.  Elimination theory
--
-- E.  Quotients and saturation


------------------------------------------------
-- A. First Steps; example with a monomial curve
------------------------------------------------

-- To compute the Gröbner basis of an ideal
-- $(x^2y,xy^2+x^3)$ in the polynomial ring in
-- four variables we proceed as follows:
--
-- Our favorite field
KK = ZZ/31991

-- The polynomial ring

R = KK[x,y,z,w]

-- and the ideal

I = ideal(x^2*y,x*y^2+x^3)

-- now the punch line:

J = gens gb I

-- From this we can for example compute the
-- codimension, dimension,
-- degree, and the whole Hilbert
-- function and polynomial.  

-- This will be more fun if we work with an
-- example having some meaning.  We choose
-- to work with the ideal defining the
-- rational quartic curve in $\PP^3$ given
-- parametrically in an affine representation
-- by 
--        $$t \mapsto{} (t,t^3,t^4).$$
-- (The reader who doesn't understand this
-- terminology may ignore it for the moment,
-- and treat the ideal given below as a 
-- gift from the gods... .)
-- We obtain the ideal by first making the 
-- polynomial ring in 4 variables (the
-- homogeneous coordinate ring of $\PP^3$)

R = KK[a..d]

-- and then using a function {\tt monomialCurveIdeal}, which we shall
-- treat for now as a black box

I = monomialCurveIdeal(R,{1,3,4})

-- From Macaulay2's point of view, $I$ is an
-- ideal, and the codimension of its support
-- is 2, while its dimension is 2:

codim I
dim I

-- This is the codimension of $R/I$ in $R$ 
-- and the dimension of $R/I$.  We could work with
-- the module $R/I$ as well.
-- Precision requires writing $R^1$ instead
-- of $R$ ($R$ is a ring, and $R^1$ is
-- the free module of rank 1 over it)

codim (R^1/(I*R^1))

-- We could also extract the generators of
-- $I$ (as a matrix) and take the cokernel to
-- get the same thing:

M = coker gens I
codim M
dim M

-- And similarly for the degree:

degree I
degree M

-- As one might expect, the degree of the quartic
-- is 4 !
-- 
-- The Hilbert polynomial is obtained by

hilbertPolynomial M

-- The term $\PP^i$ represents the Hilbert polynomial of
-- projective $i$-space.  This formula tells
-- us that the Hilbert polynomial of $M$ is
-- $H(m) = 4(m+1) - 3 = 4m + 1$.  Thus the degree
-- is four, the dimension of the projective variety
-- which is the support of $M$ is 1 (and so the affine
-- dimension is 2),
-- and that the (arithmetic) genus is 0 (1 minus the
-- constant term, for curves).
--
-- The Hilbert series of $M$ (the generating function
-- for the dimensions of the graded pieces of $M$) is

hilbertSeries M

-- The indeterminate in this expression is \$T.
--
-- Another way to get information about
-- the module $M$ is to see its free resolution

Mres = res M

-- To get more precise information about {\tt Mres},
-- we could do

betti Mres

-- The display is chosen for compactness:

--B    total: 1 4 4 1
--B        0: 1 . . .
--B        1: . 1 . .
--B        2: . 3 4 1

-- the first line gives the total betti 
-- numbers, the same information given when
-- we type the resolution.  The remaining
-- lines express the degrees of each of the
-- generators of the free modules in the
-- resolution.  The $j$th column after the colons
-- gives the degrees of generators of the
-- $j$th module(counting from $0$); 
-- an $n$ in the $j$th column in the
-- row headed by ``$d$:'' means that the $j$th
-- free module has $n$ generators of degree
-- $n+j$.  Thus for example in our case, the
-- generator of the third (last) free module in the
-- resolution has degree $3+2=5$.


------------------------------------------------
-- B. Random regular sequences
------------------------------------------------


-- An interesting and illustrative open problem
-- is to understand the initial ideal (and 
-- the Gröbner basis) of a ``generic'' 
-- regular sequence.  To study a very simple case
-- we take a matrix of 2 random forms 
-- in a polynomial ring in
-- 3 variables:

R = KK[x,y,z]
F = random(R^1, R^{-2,-3})

-- makes $F$ into a $1 \times 2$ matrix whose elements
-- have degrees $2,3$ (that is, $F$ is a random map
-- to the free module $R^1$, which has its one
-- generator in the (default) degree, $0$, from
-- the free module with generators in the listed
-- degrees, $\{2,3\}$).  We now can compute

GB = gens gb F
LT = leadTerm gens gb F
betti LT

-- shows that there are Gröbner basis elements
-- of degrees 2,3, and 4.  This result is
-- dependent on the monomial order in the ring $R$;
-- for example we could take the lexicographic
-- order

R = KK[x,y,z, MonomialOrder => Lex]

-- (see {\tt help MonomialOrder} for other possibilities).

-- We get 
F = random(R^1, R^{-2,-3})
GB = gens gb F
LT = leadTerm gens gb F
betti LT

-- and there are Gröbner basis elements of degrees 
-- $2,3,4,5,6.$


-----------------------------------------------
-- C. Division With Remainder
-----------------------------------------------
-- A major application of Gröbner bases is
-- to decide whether an element is in a given
-- ideal, and whether two elements reduce to
-- the same thing modulo an ideal.  For
-- example, everyone knows that the trace
-- of a nilpotent matrix is 0. We can produce
-- an ideal $I$ that defines the variety $X$ of 
-- nilpotent $3 \times 3$ matrices by taking the cube
-- of a generic matrix and setting the entries
-- equal to zero.  Here's how:

R = KK[a..i]
M = genericMatrix(R,a,3,3)
N = M^3
I = flatten N

-- (actually this produces a 1 x 9 matrix of
-- of forms, not the ideal: {\tt J = ideal I};
-- the matrix will be more useful to us).
-- But the trace is not in $I$!  This is obvious
-- from the fact that the trace has degree $1$,
-- but the polynomials in $I$ are of degree $3$.
-- We could also check by division with
-- remainder:

Tr = trace M 
Tr //I  -- the quotient, which is 0
Tr % I  -- the remainder, which is Tr again

-- (Here {\tt Tr} is an element of $R$, not a matrix.
-- We could do the same thing with a $1 \times 1$ matrix
-- with {\tt Tr} as its element.)
-- This is of course because the entries of $I$ do
-- NOT
-- generate the ideal of all forms 
-- vanishing on $X$ -- this we may find with
-- {\tt J = radical ideal I},
-- (but this takes a while: see the documentation for
-- {\tt radical} on a faster way to find this)
-- which shows that the radical is generated by
-- the trace, the determinant, and the sum of 
-- the principal $2 \times 2$ minors, that is, by the
-- coefficients of the characteristic polynomial.
-- In particular, we can try the powers of the
-- radical:

Tr^2 % I
Tr^3 % I
Tr^4 % I
Tr^5 % I
Tr^6 % I
Tr^7 % I

-- The seventh power is the first one in the 
-- ideal!  (Bernard Mourrain has worked out a
-- formula for which power in general.)
-- In this case

Tr^6 // I

-- is not 0.  It is a matrix that makes the
-- following true:

Tr^6 == I * (Tr^6 // I) + (Tr^6 % I)

----------------------------------------------
-- D. Elimination Theory
----------------------------------------------
-- Consider the problem of projecting the
-- ``twisted cubic'', a curve in $\PP^3$ defined
-- by the three $2 \times 2$ minors of a certain
-- $2 \times 3$ matrix into the plane.  
-- Such problems can be solved in a very 
-- simple and direct way using Gröbner bases.
-- The technique lends itself to many extensions,
-- and in its developed form can be used to find
-- the closure of the image of any map of 
-- affine varieties.  
--
--    In this section we shall first give a 
-- simple direct treatment of the problem above,
-- and then show how to use Macaulay2's 
-- general tool to solve the problem.

-- We first
-- clear the earlier meaning of {\tt x} to make it
-- into a subscripted variable

x = global x

-- and then set

R = KK[x_0..x_3] 
-- the homogeneous coordinate ring of $\PP^3$
-- and
M = map(R^2, 3, (i,j)->x_(i+j))
I = gens minors(2,M)
-- a matrix whose image is 
-- the ideal of the twisted cubic.

-- As projection center we
--  take the point defined by
pideal = ideal(x_0+x_3, x_1, x_2)

-- To find the image we must intersect the ideal
-- $I$ with the subring generated by the 
-- generators of {\tt pideal}.  We make a change of
-- variable so that these generators become
-- the last three variables in the ring; that
-- is, we write the ring as $KK[y_0..y_3]$
-- where 
--  $$y_0 = x_0, y_1 = x_1, y_2 = x_2, y_3 = x_0+x_3$$
-- and thus
-- $x_3 = y_3-y_0$, etc. 
-- We want the new ring to have an ``elimination
-- order'' for the first variable.

y = global y
S = KK[y_0..y_3,MonomialOrder=> Eliminate 1]

-- Here is one way to make the substitution

I1 = substitute(I, matrix{{y_0,y_1,y_2,y_3-y_0}})

-- The elimination of 1 variable from the 
-- matrix of Gröbner basis elements proceeds
-- as follows:

J = selectInSubring(1,gens gb I1)

-- and gives (a matrix with element)
-- the cubic equation of a rational
-- curve with one double point in the plane.
-- However, we are still in a ring with 4 
-- variables, so if we really want a plane
-- curve (and not the cone over one) we must
-- move to yet another ring:

S1 = KK[y_1..y_3]
J1 = substitute(J, S1)

-- This time we didn't have to give so much
-- detail to the {\tt substitute} command because of 
-- the coincidence of the names of the variables.
--
-- Having shown the primitive method, we
-- now show a much more flexible and transparent
-- one:  we set up a ring map from the polynomial
-- ring in $3$ variables (representing the plane)
-- to $R/I$, taking the variables $y$ to the three
-- linear forms that define the projection 
-- center.  Then we just take the kernel of
-- this map!  (``Under the hood'', 
-- Macaulay2 is doing a more refined version
-- of the same computation as before.)

-- Here is the ring map
Rbar = R/(ideal I)
f = map(Rbar, S1, matrix(Rbar,{{x_0+x_3, x_1,x_2}}))
-- and the desired ideal
J1 = ker f

----------------------------------------------
-- E. Quotients and saturation
----------------------------------------------

-- Another typical application of 
-- Gröbner bases and syzygies is to the
-- computation of ideal quotients and 
-- saturations.  Again we give an easy example
-- that we can treat directly, and then 
-- introduce the tool used in Macaulay2 to 
-- treat the general case.
--
-- If $I$ and $J$ are ideals in a ring $R$, we define
-- $(I:J)$, the ideal quotient, by 
--   $$(I:J) = \{f \in R \mid fJ \subset I\}.$$
--
-- In our first examples we consider 
-- the case where $J$ is 
-- generated by a single element $g$.
-- This arises in practice, for example, in the
-- problem of homogenizing an ideal.  Suppose
-- we consider the affine space curve
-- parametrized by
-- $t \mapsto{} (t,t^2,t^3)$.  The ideal of polynomials
-- vanishing on the curve is easily seen to
-- be $(b-a^2, c-a^3)$ (where we have taken
-- $a,b,c$ as the coordinates of affine space).
-- To find the projective closure of the curve
-- in $\PP^3$, we must homogenize these equations
-- with respect to a new variable d, getting
-- $db-a^2, d^2c-a^3$.  But these forms do NOT
-- define the projective closure! In general,
-- homogenizing the generators of the ideal $I$ of
-- an affine variety one gets an ideal $I_1$ that
-- defines the projective closure UP TO
-- a component supported on the hyperplane
-- at infinity (the hyperplane $d=0$).  To see
-- the ideal of the closure we must remove
-- any such components, for example by
-- replacing $I_1$ by the union $I_2$ of all the
-- ideals $(I_1:d^n)$, where $n$ ranges over positive
-- integers.  This is not so hard as it seems:
-- First of all, we can successively compute
-- the increasing sequence of ideals
-- $(I_1:d), (I_1:d^2), \ldots $ until we get two 
-- that are the same; all succeeding ones
-- will be equal, so we have found the union.
-- A second method involves a special property
-- of the reverse lex order, and is much more
-- efficient in this case.  We shall illustrate
-- both. First we set up the example above:

R = KK[a,b,c,d]
I1 = ideal(d*b-a^2, d^2*c-a^3)

-- How to compute the ideal quotient:
-- If $I$ is generated by $f_1,\ldots,f_n$, we see that
-- $s\in (I:g)$ iff there are ring elements 
-- $r_i$ such that 
--        $$\sum_{i=1}^{n} r_i f_i + s g = 0. $$
-- Thus it suffices to compute the kernel
-- (syzygies) of the $1 \times (n+1)$ matrix
--           $$(f_1, ... ,f_n, g)$$
-- and collect the coefficients of $g$, that is,
-- the entries of the last row of a matrix
-- representing the kernel. 
-- Thus in our case we may compute $(I_1:d)$
-- by concatenating the matrix for $I_1$
-- with the single variable $d$

I1aug = (gens I1) | matrix{{d}}
augrelations = gens ker I1aug

-- There are 3 rows (numbered 0,1,2 !) and
-- 2 columns, so to extract the last row we
-- may do

I21 = submatrix(augrelations, {2}, {0,1})

-- But this is not an ``ideal'' properly speaking:
-- first of all, it is a matrix, not an ideal,
-- and second of all its target is not $R^1$
-- but $R(-1)$, the free module of rank 1 with
-- generator in degree 1.  We can fix both
-- of these problems by

I21 = ideal I21

-- This is larger than the original ideal, having
-- two quadratic generators instead of a 
-- quadric and a cubic, so
-- we continue.  Instead of doing the same
-- computation again, we introduce the built-in
-- command

I22 = I21 : d

-- which is again larger than {\tt I21}, having
-- three quadratic generators. Repeating,

I23 = I22 : d

-- we get another ideal with three quadratic
-- generators.  It must be the same as {\tt I21},
-- but the generators are written differently
-- because of the route taken to get it, so
-- (being suspicious) we might check with

(gens I23) % (gens I22)
 
-- which returns 0, showing that {\tt I23} is 
-- contained in (gives remainder 0 when divided
-- by) {\tt I22}.  Thus the homogeneous ideal {\tt I2} of 
-- the projective closure is equal to {\tt I23}
-- (this is the homogeneous ideal of 
-- the twisted cubic, already encountered above).
--
-- A more perspicuous way of approaching the
-- computation of the union of the $(I:d^n)$,
-- which is called the saturation of $I$ with
-- respect to $d$, and written $(I:d^\infty)$,
-- is first to compute a reverse lex Gröbner
-- basis.

gens gb I1

-- This yields {\tt (a2-bd, abd-cd2, b2d2-acd2)},
-- meaning 
--  $$(a^2-bd, abd-cd^2, b^2d^2-acd^2).$$
-- We see that the second generator is divisible
-- by $d$, and the third is divisible by $d^2$.
-- General theory says that we get the right
-- answer simply by making these divisions,
-- that is, the saturation is
--  $$(a^2-cd, ab-cd, b^2-ac),$$
-- as previously computed.  The same thing
-- can be accomplished in one line by

I2 = divideByVariable(gens gb I1,d)

-- This saturation may be found directly in Macaulay2:

saturate(I1, d)
