-- In this tutorial, we describe how to work with modules in Macaulay2.

----------------------------------
-- A. Making modules from matrices
----------------------------------

-- First, let's define a ring.
R = QQ[a..f];
m = matrix{{a,b,d,e},{b,c,e,f}}

-- Use standard notation for cokernels, images and kernels (coker, cokernel,
-- image, ker, kernel).
M = coker m
N = image m
K = kernel m

-- Given a module, one can find its presentation matrix.
presentation M -- this is just the original matrix
presentation N -- this one requires computation

------------------------------
-- B. Submodules and quotients
------------------------------
-- To define a submodule $IN$ of a module $N$, 
-- where $I$ is an ideal, use
ideal(a,b)*N
a*N + b*N
-- In order to define a submodule of N generated by some
-- elements of N, one way is the following.
N0 = image (a**N_{1}|N_{2}-N_{3})
-- To understand what Macaulay2 is doing here, let's break
-- this down.  {\tt N_{i}} defines a matrix $R^1 \to N$, which maps 1 to
-- the i th generator of N. (See Section XX below for more
-- information about module homomorphisms).
N_{1}
-- One could use {\tt a*N_{1}}, but it turns out that {\tt a ** N_{1}}
-- works better:
a ** N_{1}
-- Next, remember that the vertical bar concatenates matrices.
a ** N_{1} | N_{2}-N_{3}
-- Now take the image of this matrix
N0 = image(a ** N_{1} | N_{2}-N_{3})
-- The main advantage for using ** rather than * is that **
-- preservers homogeneity if possible.
isHomogeneous N0

-- Quotients are defined using standard mathematical notation.
Nbar = N/N0
-- Notice that this returns a subquotient module.  We treat these
-- later.

-- Ideals and modules are treated differently in Macaulay2 (and in commutative 
-- algebra in general).  For example, asking for the dimension of an ideal I
-- in a ring R gives the dimension of the quotient R/I, but the dimension of the
-- module I gives a potentially very different answer.
-- Use ideal and module to move between the two.
I = ideal(a^2, a*b, c^2)
J = module I
I == ideal J
codim I
codim J

-----------------------------------
-- C. Syzygies and free resolutions
-----------------------------------

-- Create a free resolution of an ideal (or module) using res.
C = res I
-- View the differential
C.dd
-- The (graded) betti numbers
betti C
-- Use {\tt help betti} for a detailed description
-- of what this display means.  Basically, it says that I has three
-- generators of degree 2, one syzygy of degree 3, 2 syzygies of degree 4, and
-- one second syzygy of degree 5.

-- The free resolution of a module that is not a cokernel:
C = res Nbar
betti C
C.dd

-- Here is a problem to experiment with.  What different betti diagrams
-- are possible with an ideal generated by 3 homogeneous quadric polynomials,
-- in a polynomial ring in any number of variables?
-- Here is one to get you started.
R = QQ[a..h];
J = ideal(a*c+b*d,a*e+b*f,a*g+b*h)
betti res J
-- After that, try ideals generated by 4 quadrics.

-------------------
-- D. Subquotients
-------------------
-- Recall that the module N/N0 above displayed as something called a
-- subquotient module.  As Macaulay2 often returns such objects, it
-- is useful to understand and be able to manipulate them.
--P
-- The most common modules are quotients of free modules, or submodules 
-- of free modules.  A useful generalization, which covers both of these
-- types, are subquotients: submodules of quotients of free modules.
--P
-- A subquotient module is determined by two matrices $f : R^m \to R^n$
-- and $g : R^p \to R^n$.  The subquotient module with generators f, relations
-- g is by definition the module M = (image f) + (image g) / (image g).
-- Thus, if f is the identity map, M = coker g, and if g = 0, then M = image f.
use ring M
M
N = a*M
M/N
-- The two matrices f and g mentioned above are recovered using
-- the routines: generators, relations.
generators N
relations N

-- It is often necessary to find a presentation matrix for such modules.
presentation N

-- Often the given representation of a module is not very efficient.
-- Use trim to keep the module as a subquotient of the same ambient free module,
-- but change the generators and relations to be minimal, or in the nonlocal or
-- non-graded case, at least more efficient.
trim N
-- Use minimalPresentation  to also allow the ambient free module to be improved.
-- This returns a quotient of a free module, but in the future might not do that.
minimalPresentation N
-- prune is a synonym for minimalPresentation N
prune N

-- Given a subquotient module N, there are several useful modules associated
-- to N.
-- The free module of which N is a subquotient is obtained using ambient.
ambient N
-- This is the same as the target of either the generator or relation matrix.
ambient N == target generators N
ambient N == target relations N

-- N is a submodule of a quotient module $R^n/image(g)$.  The routine super
-- returns this quotient module
super N
-- This is the same as 
super N == coker relations N

-- The cover of N is basically the source of the matrix of generators.
cover N
cover N == source generators N

-----------------------------------
-- E. Homomorphisms between modules
-----------------------------------
-- A homomorphism $f : M \to N$ is represented as a matrix from the 
-- generators of M to the generators of N.
A = QQ[x,y]/(y^2-x^3)
M = module ideal(x,y)

-- One homomorphism $F : M \to A$ is $x \mapsto y, y \mapsto x^2$ (multiplication by y/x)
-- We write this as:
F = map(A^1,M,matrix{{y,x^2}})
-- Notice that as is usual in Macaulay2, the target comes before the source.
source F == M
target F == A^1
matrix F

-- The image of F lies in the submodule M of $A^1$.  To obtain the map
-- $M \to M$, we use //.  But first we need the inclusion map
-- of M into $A^1$:
-- Later we explain this, but for now, we just write down this map:
inducedMap(A^1,M)
-- Now we use // to lift $F : M \to A$ along $M \to A^1$, to get $M \to M$:
G = F // inducedMap(A^1,M)
source G
target G
-- G is now a map from $M \to M$.
isWellDefined G

---------------------------------------------
-- F. Canonical maps associated with modules
---------------------------------------------
-- Let's start with a module M, and a submodule N.
R = QQ[x,y,z,w]
M = ideal(x,y,z)/ideal(x^2,y^2,z*w)
N = z*M
M/N

-- If two modules have the same ambient free module, then there is
-- often a canonical map between them.
-- Some modules having the same ambient free module:
M
ambient M
N = z*M
ambient(M/N)
super M
super N
image generators M
--
-- If two modules M and N have the same ambient module $R^n$, then inducedMap(M,N)
-- makes the canonical map $N \to M$ between them, if one exists.  If a map
-- doesn't exist, the returned map might not be a homomorphism.
inducedMap(M,M) == id_M
inducedMap(super M,M) == map(super id_M) -- the map $(P+Q)/Q \to R^n/Q$, where $M=(P+Q)/Q$.
inducedMap(super M,ambient M) -- the quotient map $R^n \to R^n/Q$
inducedMap(M,N) -- the inclusion map
-- The projection map $M \to M/N$
inducedMap(M/N,M) -- the projection map
-- The projection map $N \to M/N$, which is the zero map
inducedMap(M/N,N) -- the zero map
-- Not all such maps can be defined.  The functions 'inducedMap' normally checks that the
-- result is a well-defined homomorphism.  The option 'Verify' controls that behavior.
inducedMap(M,M/N,Verify => false)
inducedMap(M/N,x*M)
inducedMap(M/N,M) * inducedMap(M,x*M) == inducedMap(M/N,x*M)

--
-- 
-- Before doing interesting homomorphisms, let's see how to write down
-- some canonical homomorphisms associated to M.


-- exercises:
-- 1. isomorphism theorems.  Given submodules M and N of a module P,
--    (a) find $(M+N)/M$
--    (b) find $N/(M \cap N)$
--    (c) find in Macaulay2, an isomorphism between them.
--
-- 2. Given a homomorphism $M \to A$.  Suppose that
--    the image lies in M (M is a submodule of $A^1$).
--    Find the map $M \to M$.


---------------------------
-- G. Homomorphisms and Hom
---------------------------
A = QQ[x,y,Degrees=>{2,3}]/(y^2-x^3)
M = module ideal(x,y)
H = Hom(M,M)
-- The elements of H correspond to homomorphisms $M \to A$.
-- The homomorphism associated to elements of H may be obtained
-- using the routine homomorphism.
F = homomorphism(H_{0})
G = homomorphism(H_{1})
source F == M
target F == M
ker F
coker F
m = matrix{{x,y},{y,x}}
Hom(m,A^2)
Hom(A^2,m)

---------------------
-- H. Tensor products
---------------------
-- In Macaulay2, ** denotes the tensor product operator.
m ** m
(coker m) ** (coker m)
-- Notice that tensor products of matrices and of modules are
-- very different.
M = coker m
M2 = prune(M ** M)
A = QQ[a,b,c]
A ** A
-- Macaulay2 renames variables whose names collide.
-- Alternatively, one can give the variables as an option to tensor.
tensor(A,A,Variables=>{a,b,c,d,e,f})
