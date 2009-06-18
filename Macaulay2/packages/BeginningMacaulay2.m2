newPackage(
     "BeginningMacaulay2",
     Version => "0.1", 
     Date => "June 2, 2009",
     Authors => {{Name => "David Eisenbud", 
	       Email => "de@msri.org", 
	       HomePage => "http://www.msri.org/~de"},
	  {Name => "Mike Stillman",
	   Email => "mike@math.cornell.edu",
	   HomePage => "http://www.math.cornell.edu/~mike"}},
     Headline => "Mathematicians' Introduction to  Macaulay2",
     DebuggingMode => true
     )
beginDocumentation()


{*
document {
     Headline => "Mathematicians' Introduction to  Macaulay2",
     Key => "BeginningMacaulay2",
     UL {
	  TO "BeginningMacaulay2"
     },

*}
document {
     Headline => "Mathematicians' Introduction to  Macaulay2",
     Key => "BeginningMacaulay2",
     --get "~/Documents/M2/tutorial.m2"
     


     tutorial ///
-- We assume you've installed Macaulay2 and can type
--
-- M2
--
-- on a command line to bring up the program. You should see something like
--
-- Macaulay2, version 1.2.1
-- with packages: Elimination, IntegralClosure, LLLBases, PrimaryDecomposition, ReesAlgebra, SchurRings,
--               TangentCone
--

------------------
-- PART I. Arithmetic with integers, rings and ideals.
------------------
-- You can immediately do arithmetic with integers, such as
2+2
107*431
25!
binomial(5,4)
factor 32004
-- Most Macaulay2 applications involve polynomial rings over fields,
-- and their factor rings. Fields can be made in various ways, such as
kk=ZZ/101
kk=QQ
kk=GF(2^5)
kk=toField (QQ[i]/(i^2+1))
-- after which we might do
1/i
-- Computation is often fastest and needs least
-- memory when performed over finite prime fields, and when
-- these are large they look a lot like the rational numbers,
-- so we mostly use fields of the form ZZ/p. The prime p
--can range up to 32003.
--
-- To make a polynomial ring in 5 variables over ZZ/101
kk=ZZ/101
S=kk[x_1..x_5]
-- or
S=kk[a,b,c,d,e] 
-- and then one can do arithmetic on polynomials such as
(3*a^2+1)^5
-- We can make an ideal in S with
I=ideal(a^3-b^3, a+b+c+d+e)
-- and the corresponding factor ring with
R=S/I
-- Another way to make an ideal, with more compact notation (which will be
-- familiar to anyone who used the classic Macaulay) is
use S
I=ideal"3(a+b)3, 4c5"
-- Note the command "use S" which specifies 
-- that we want to work in the polynomial ring S again;
-- otherwise M2 would have assumed that the variables a,b,c 
-- had the values last given to them, so they would have
-- been variables in R, not S.
--
-- Arithmetic works for ideals, too, for example
I^2
I*I
I+ideal"a2"
-- In case you forget any of these things, help is available! The most
-- useful way to get it is often to type something like 
--
--viewHelp Ideal
--
-- Since we capitalized "Ideal", we get the documentation for the class of all ideals.
-- To get just the documentation for the function "ideal" that we've been using
-- to create ideals one would do
--
--viewHelp ideal
--
-- and then choose either "ideal(List)" or "ideal(String)" (since the expression 
-- in quotes is a string.)
--
--Some other basic types are defined similarly
M= matrix{{a,b,c},{b,c,d},{c,d,e}}
M^2
determinant M
trace M
M-transpose M
-- To get the entries of a matrix do
flatten entries M
-- (the command
entries M
--produces a list of lists, one for each row of M.)
--
-- If you want a particular entry, say the one in the upper left corner, do
M_(0,0)
-- Note that here as everywhere in M2, all indexing starts with 0.
-- For example
I_0
-- is the first generator of I, and you can a list of the generators with
I_*
--
-- You can define a module as a cokernel, kernel, or image
P=coker M     
Q=image M
R=kernel matrix"a,b,0;0,a,b"
-- Note the alternate syntax for defining a matrix, parallel to that for an ideal.
--
-- Before going on, the reader might want to explore the many possibilities under
--
-- viewHelp
--
-- and click on the Macaulay2 link at the top.

------------------------------------------------
-- II. Properties of Ideals and Modules
------------------------------------------------

--------------------
-- A.  First steps; example with a monomial curve
--------------------

-- To compute the Groebner basis of an ideal
-- $(x^2y,xy^2+x^3)$ in the polynomial ring in
-- four variables we proceed as follows:
-- Our favorite field
kk = ZZ/32003
-- The polynomial ring
R = kk[x,y,z,w]
-- and the ideal
I = ideal(x^2*y,x*y^2+x^3)
-- now the punch line: we can compute the Groebner basis with
J = gens gb I
-- (here gb I is an object with lots of information in it; gens gb I
-- extracts just what we want, the generators in a Groebner basis.)
-- Note that Groebner bases are always computed with respect to a particular
-- monomial order on the ring. In fact, the ring we defined above has
-- a default monomial order, the graded reverse lex order. For many
-- other possibilities see
--
-- viewHelp MonomialOrder
--
-- The analogue for ideals of factorization is primary decomposition.
-- For example, we can begin by intersecting three ideals
I= intersect (ideal"x2,y3", ideal"y2,z3", (ideal"x,y,z")^4)
-- and then take the primary decomposition with
primaryDecomposition I
-- Although the three ideals we intersected were all primary, we
-- did not get the last one back -- we got a different primary ideal
-- instead. This is because only the primary components corresponding
-- to minimal primes are unique.
-- For larger examples, this can be computationally challenging!
-- Somewhat easier is to find just the minimal primes. We use:
decompose I
-- From Groebner bases we can also compute the
-- codimension, dimension,
-- degree, and the whole Hilbert
-- function and polynomial.  
-- This will be more fun if we work with an
-- example having some meaning.  We choose
-- to work with the ideal defining the
-- rational quartic curve in ${\bf P}^3$ given
-- parametrically in an affine representation
-- by 
--        $$t \mapsto{} (t,t^3,t^4).$$
-- (The reader more interested in algebra than geometry
-- may simply treat the ideal given below as a 
-- gift from the gods... .)
-- We obtain the ideal by first making the 
-- polynomial ring in 4 variables (the
-- homogeneous coordinate ring of ${\bf P}^3$)
R = kk[a..d]
-- and taking the kernel of the ring map $R \to kk[s,t]$ defined by
phi = map(kk[s,t],R,{s^4, s^3*t, s*t^3, t^4})
-- Here the syntax puts the target ring first (maps in M2 generally go from right to left!)
-- The last entry is a list of the elements to which to send the variables of the source ring.
-- The ideal we want is the kernel of this map:
I = ker phi
-- This is such a useful construction that there's a shortcut notation:
I = monomialCurveIdeal(R,{1,3,4})
-- We can compute the dimension,  codimension (=height) and degree of this ideal with
dim I
codim I
degree I
-- The Hilbert polynomial is obtained by
hilbertPolynomial(R/I)
-- This may not be what the user expected:
-- The term ${\bf P}_i$ represents the Hilbert polynomial of
-- projective $i$-space.  This formula tells
-- us that the Hilbert polynomial of $M$ is
-- $H(m) = -3+4(m+1) = 4m + 1$.  Thus the degree
-- is four, the dimension of the projective variety
-- which is the support of $M$ is 1 (and so the affine
-- dimension is 2), and the (arithmetic) genus is 0 (1 minus the
-- constant term of the Hilbert polynomial.)
--
-- The more usual expression for the Hilbert polynomial can
-- also be obtained: Use
hilbertPolynomial(R/I, Projective => false)
-- In this the construction Projective => false is the way that M2 specifies
-- options to functions. The form we used first could also have been written
hilbertPolynomial(R/I, Projective => true)
-- since the default value for this option is "true".
-- The Hilbert series of $M$ (the generating function
-- for the dimensions of the graded pieces of $M$) is
hilbertSeries (R/I)
-- This is expressed as a rational function with denominator equal to (1-T)^n, where
-- n is the number of variables in R. Since R/I has dimension 2, it can also be written
-- with numerator (1-t)^2. To see it in this form, use
reduceHilbert hilbertSeries (R/I)
-- It's possible to manipulate the numerator and denominator of this
-- expression, but the syntax for doing so isn't obvious; see
--
-- viewHelp hilbertSeries 
--
-- One way to get information about
-- a module $M$ is to see its free resolution. For an example, we begin
-- by turning R/I into a module. Here R^1 is the free module of rank 1 over R.
M=R^1/I
Mres = res M
-- To get more precise information about {\tt Mres},
-- we could do
betti Mres
-- The display is chosen for compactness:
-- The first line specifies the number of 
-- steps back in the resolution.
-- The next line gives the total betti 
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
--
-- Commonly computed homological invariants
-- such as projective dimension and regularity
-- are (also) available directly:
pdim M
regularity M
--
------------------------------------------------
-- B. Division With Remainder
-----------------------------------------------
-- A major application of GrÃ¶bner bases is
-- to give a normal form for an element modulo an
-- ideal, allowing one, for example, to decide whether
-- it is zero (that is, in the ideal).
-- For example, we can decide which power of the trace
-- of a generic 3x3 matrix is expressible in terms of the entries of the 
-- cube of the matrix with the following code:
R = kk[a..i]
M = genericMatrix(R,a,3,3)
I = ideal M^3
-- This gives the ideal of entries of the matrix. In the expression
-- "M = genericMatrix(R,a,3,3)" the arguments R,a,3,3 specify the
-- ring, the first variable to use, and the number of rows and columns
-- desired. The ring must have enough variables! -- in this case 9.
Tr = trace M 
for p from 1 to 10 do print (Tr^p % I)
-- The expression Tr^p % I is the normal form for the p-th power
-- of the trace Tr with respect to the Groebner basis of I computed
-- in R. The expression "for p from 1 to 10 do" specifies a 
-- "for loop" that executes the following "print (Tr^p % I)"
-- with 10 consecutive values of p. For more information on such loops see
--
-- viewHelp "for"
--
-- (Here we have put quotes around "for" because without the quotes
-- "for" would be a keyword. In general, it's always safe to use
-- quotes with viewHelp.)

-- We see from the output of these commands that the 6-th power
-- of the trace is NOT in the ideal of entries of the cube of M,
-- but the 7-th power is. We can compute the expression for it 
-- by using the division algorithm, denoted in this setting by //:
Tr^7//(gens I)
--

----------------------------------------------
-- C. Elimination Theory
----------------------------------------------
-- Consider the problem of projecting the
-- ``twisted cubic'', a curve in ${\bf P}^3$ defined
-- by the three $2 \times 2$ minors of a certain
-- $2 \times 3$ matrix into the plane.  
-- We already have the simplest tools for solving
-- such a problem.
-- We first clear the earlier meaning of {\tt x} 
-- to allow it to be used as a subscripted variable
x = symbol x
-- and then set
R = kk[x_0..x_3] 
-- and
M = map(R^2, 3, (i,j)->x_(i+j))
I = minors(2,M)
-- the ideal of the twisted cubic.
-- As projection center we
--  take the point defined by
pideal = ideal(x_0+x_3, x_1, x_2)
-- To find the ideal J of the image under the projection from this point,
-- we take the kernel of the ring map sending the variables
-- of the polynomial ring in two variables to the generators of pIdeal,
-- regarded as elements of R/I. (This is the same as taking
-- $$J = I \cap kk[x_0+x_3, x_1, x_x],$$ 
-- the more usual formulation.)
-- To this end, we first substitute pIdeal into R/I, and then form
-- the corresponding ring map.
Rbar = R/I
pideal = substitute(pideal, Rbar)
S = kk[u,v,w]
J=kernel map (Rbar, S, gens pideal)
-- The ideal J defines a curve with one singular point.
-- we can compute the ideal of the singular locus with
K = ideal singularLocus(J)
-- This doesn't look like the ideal of a reduced point! But
-- that's because it isn't yet saturated:
saturate K
-- We have just seen the "saturate" command in its most
-- common use: to saturate with respect to the maximal ideal.
-- but we can also find the saturation of any ideal with
-- respect to another, as in
saturate (ideal"u3w,uv", ideal"u")
-- or take the quotient of one ideal with
-- respect to another, where by definition the "ideal quotient"
-- I:J is defined as the set of elements f of the ring  such that
-- f*J is contained in I:
ideal"u3w,uv":ideal"u"
--

------------------------
D. defining functions and loading packages
-----------------------

-- It is easy to define your own functions in M2, and this
-- can save a lot of typing. Functions are defined with the 
-- symbol ->. For example, the famous Collatz Conjecture
-- (also called the "hailstone problem") asks
-- about the following procedure: given an integer n divide it
-- by 2 if possible, and else multiply by 3 and add 1. If we repeat this over and over,
-- does the process always end with 1? Here is a function that does
-- this, producing a list of the intermediate results.
--^
Collatz = n ->
      while n !=1 list if n%2 == 0 then n=n//2 else n=3*n+1
--$
-- For example,
Collatz 27
-- If you don't understand this code easily, try typing
--
-- viewHelp Function
--
--and/or
--
-- viewHelp "while"
--
-- In order to understand a process it is often useful to tabulate the 
-- results of applying it many times. One feature of the Collatz process
-- is how many steps it takes to get to 1. We can tabulate this statistic
-- for the first 25 values of n with the command "tally", as follows
tally for n from 1 to 30 list length Collatz n
-- a line of the form
--
-- 18 => 3
--
-- in the result means that a Collatz sequence of length 18
-- was seen 3 times. 
-- To see the successive "record-breakers", 
-- that is the numbers with longer Collatz sequences than any
-- number before them, we might do
record  = length Collatz (1)
L={}
--^
for n from 2 to 1000 do if (l=length Collatz n) > record then (
     record = l;
     L=L|{(n,l)} 
     )
--$
L
-- and to see just a list of the successive records, we can apply the 
-- function "last" to each element of the list L. A convenient way to do this is
L/last
--
-- One important thing to note in this example is that in 
-- writing functions of more than one expression (usually
-- there's one expression per line), the expressions must be
-- separated by semicolons. For example in the "for" loop
-- above, the first expression was "record = l". 
-- Outside of a function (or after the last expression 
-- of a function) a semicolon has  effect of 
-- suppressing output--this can be very useful when the output
-- would be large. See
--
-- viewHelp "The Macaulay2 language" 
--
-- (you don't need to italicize!) for more information.
-- There are many packages of ready-made functions available for
-- your use, many written by other users (perhaps you'll contribute one
-- someday!) A list can be found with
--
-- viewHelp 
--
-- For example, there is such a package called EdgeIdeals. 
-- Click on its link on the page you just got to in order
-- to see what's in it. Then, after
loadPackage "EdgeIdeals"
-- you can call it's functions, such as randomGraph and edgeIdeal:
R = kk[vars(0..10)]
G=randomGraph (R,20)
K=edgeIdeal G
hilbertSeries K
betti res K
-- Often one wants to run a random function a LOT of times.
-- Here's some typical code that one might use to study
-- a random graph ideal.  The first block defines a list L
-- and suppresses it's printing by ending the function that creates
-- it with a ";". Each entry of L is a triple consisting of the
-- codimension, degree, and Betti table of a graph ideal
-- on 10 vertices having only 4 edges.
-- The line tally L tells how many examples
-- were found with each combination of codimension and degree and Betti table.
-- The line #tally L tells how many distinct patterns were found.
R = ZZ/2[vars(0..10)]
--^
L=for j from 1 to 100 list(
      G=randomGraph (R,5);
      I= edgeIdeal G;
      (codim I, degree I,betti res I));
--$
tally L
#tally L
--

------------------------
E. Ext, Tor, and cohomology
-----------------------

-- Macaulay2 can compute the homology of complexes;
-- for example, let's compute the homology of a
-- Koszul complex that is not a resolution,
-- $$ {\bf K}(x^2, x*y^2):\ \  0 --> S(-5) --> S(-3)\oplus S(-2) --> S. $$
-- In the construction that follows, 
S^{-2,-3} 
-- is the M2 notation
-- for $S(-2)\oplus S(-2)$.
S = kk[x,y]
phi1 = map(S^1, S^{-2,-3}, matrix"x2,xy2")
phi2 = map(S^{-2,-3}, S^{-5}, matrix"xy2;-x2")
-- Let's check that this is will really make a complex:
phi1*phi2
-- To get the homology we can compute 
(ker phi1)/(image phi2)
-- or, more formally, we can define the structure of a chain complex
-- and use the built-in facility to take homology (in our case H_1):
FF = chainComplex(phi1,phi2)
homology FF
presentation (homology FF)_1
-- Either way, homology is S/x = (x^2):(xy^2)/x^2, in accord
-- with general theory.
--
-- Since Macaulay2 can compute resolutions and homology, it can
-- compute things like Ext, Tor and sheaf cohomology, as in the 
-- following examples. The first uses Serre's formula to  compute
--  the multiplicity with which a 2-plane meets the union
-- of two 2-planes in 4-space; this is the first case in which
-- the length of the intersection scheme is NOT the right answer:
-- The notation M**N denotes the tensor product of the modules M and N.
-- We take the opportunity to show off the syntactical forms
-- "for j from 0 to 4 list" to list some results and
-- "sum(0..4, j-> )" to sum some results.
S=kk[a,b,c,d]
IX = intersect(ideal(a,b), ideal(c,d))
IY = ideal(a-c, b-d)
degree ( (S^1/IX) ** (S^1/IY))
L=for j from 0 to 4 list degree Tor_j(S^1/IX, S^1/IY)
sum(0..4, j-> (-1)^j*degree Tor_j(S^1/IX, S^1/IY))
--Similarly, we can compute Hom and Ext:
Hom(IX, S^1/IX)
Ext^1(IX, S^1/IX)
-- or the cohomology of the sheaf associated to a module. 
-- Here is how to compute
-- the first cohomology of the structure
-- sheaf twisted by -2 of the curve Proj(S/IX) -- 
-- in this case the disjoint union of two
-- lines in P^3.
HH^1(sheaf (S^{-2}**(S^1/IX)))
///
     }

end
restart
installPackage "BeginningMacaulay2"
viewHelp "BeginningMacaulay2"
uninstallPackage "BeginningMacaulay2"


