-- Homological Algebra 2
-- 
-- \font\xmplbx = cmbx10 scaled \magstephalf
-- \font\xmplbxti = cmbxti10 scaled \magstephalf
-- \def\section#1{\bigskip\centerline{\xmplbx #1}\bigskip}
-- \def\prob#1#2{\vglue .7\baselineskip
-- \leftline{\xmplbx Problem #1.\quad\xmplbxti #2}
-- \vglue .5\baselineskip}
--
-- \section{Problems in computational homological algebra}
--
-- \section{Basic concepts}
--
-- \prob{1}{Modules and homomorphisms}
--
-- To determine a method of implementing modules and homomorphisms: how
-- we should represent these objects.
--
-- \prob{2}{Kernels and syzygies}
--
-- To determine how to compute the kernel of a homomorphism or $R$-modules,
-- given that we know how to compute the kernel of a matrix of free modules.
--
-- \prob{3}{Ideal membership and lifting maps}
--
-- To implement for free modules the ``defining'' property of projective modules:
-- An $R$-module $P$ is {\it projective} iff for every pair of homomorphisms 
-- $g: A \rightarrow B$
-- and $h : P \rightarrow B$, such that $image(h) \subset image(g)$, then
-- there exists a map $f := lift(h,g) : P \rightarrow A$ such that $gf = h$.
-- 
-- \prob{4}{Tensor products of modules}
--
-- To determine the representation of a tensor product $A \otimes_R B$ of
-- finitely generated $R$-modules.
--
-- \prob{5}{Hom}
-- 
-- To determine a presentation of the $R$-module $Hom_R(A,B)$,
-- given $R$-modules $A$ and $B$, and given an element of this
-- module, to find the corresponding homomorphism $A \rightarrow B$.
--
-- \section{Chain complexes and resolutions}
--
-- \prob{6}{Comparison map}
--
-- To compute the extension $f : C \rightarrow D$ of the $R$-map
-- $g : A \rightarrow B$, where $C$ (respectively $D$) is a free resolution of
-- $A$ (respectively $B$).
-- 
-- \prob{7}{Chain homotopies}
-- 
-- To find a chain homotopy between two chain maps of free resolutions.
--
-- \prob{8}{Mapping cone}
--
-- To construct the mapping cone of a map of chain complexes.
--
-- An important special case is the construction of a free resolution of
-- the $R$-module $C = B/A$, given free resolutions of $A$ and $B$, where 
-- $A \rightarrow B$ is an injective map.
--
-- \prob{8A}{Diana Taylor resolution}
--
-- To construct a free resolution (not usually minimal) of an ideal generated
-- by monomials in a polynomial ring.
--
-- We use successive mapping cones to construct the resolution.
--
-- \prob{8B}{Resolutions of stable monomial ideals}
--
-- To construct a minimal free resolution of a so-called ``stable'' monomial ideal.
-- The method is almost identical to the construction of the Taylor resolution.
--
-- \prob{8C}{Simplicial homology}
--
-- Given a simplicial complex $\Delta$, compute the simplicial homology of $\Delta$.
--
-- \prob{9}{Horseshoe resolution}
--
-- To find the ``horseshoe free resolution '' $P_B$ of $B$,
-- given a short exact sequence 
-- $$0 \longrightarrow A \longrightarrow B \longrightarrow C \longrightarrow 0$$
-- of $R$-modules, and free resolutions $P_A$ of $A$ and $P_C$ of $C$.
-- By definition, the horsehoe resolution $P_B$
-- is a free resolution of $B$, together with a short exact sequence of
-- chain complexes
-- $$0 \longrightarrow P_A \longrightarrow P_B \longrightarrow P_C \longrightarrow 0$$
-- extending the original short exact sequence.
--
-- The horseshoe resolution is used to construct the connecting homomorphism
-- for any left-exact, or contravariant right exact functor, e.g. Tor and Ext.
-- 
-- We apply the technique to a specific example
R = ZZ/101[symbol a..symbol f]
I = minors(2,genericMatrix(R,a,2,3))
J = ideal(I_0,I_1)
K = J : I_2
A = coker gens K
B = coker gens J
C = coker gens I
fdot = matrix{{I_2}}
gdot = id_(R^1)
F = map(B,A,fdot)
G = map(C,B,gdot)
-- Let's check that we have an exact sequence:
ker G == image F
-- Now we want to find a horseshoe resolution for {\tt B}
PA = res A
PC = res C
bbar0 = presentation B
-- Given the maps f0,g0,a1,bbar1,c1, find f1,g1,b1, bbar2
-- First, f1 and g1 are easy:
id0 = id_(PA_0 ++ PC_0)
f0 = id0_{0}
g0 = submatrix(id0,{1},null)
-- To find b1 we must lift
lambda0 = id_(PC_0) // (gdot | PC.dd_1)
lambda0 = submatrix(lambda0,toList (0..numgens source gdot-1),null)
b0 = fdot | lambda0
bbar1 = modulo(b0, presentation B)

id1 = id_(PA_1 ++ PC_1)
f1 = id1_{0..numgens PA_1 - 1}
g1 = submatrix(id1,{numgens PA_1..numgens source id1 - 1},null)
lambda0 = PC.dd_1 // (g0*bbar1)
lambda0 = bbar1 * lambda0
b1 = f0*PA.dd_1 | lambda0
bbar2 = syz b1

-- We document our new routine
horseshoe = method()
--$
document {
     Key => horseshoe, 
     TT "horseshoe", "(F,G) -- Compute the horseshoe resolution of target F == source G,
     given homomorphisms F,G of R-modules",
     PARA,
     "Given a short exact sequence 0 --> A --F--> B --G--> C --> 0 of modules,
     return a list of two chain complex maps, FF, GG, such that
     0 --> PA --FF--> PB --GG--> PC --> 0 is a short exact sequence of chain complexes,
     and PA, PB, PC are free resolutions of A,B,C, respectively."
     }
--$
zeroChainComplexMap = method()
--$
zeroChainComplexMap(ChainComplex,ChainComplex,ZZ) := (C,D,d) -> (
     R := ring C;
     if ring D =!= R then 
         error "source and target chain complex have different base rings";
     E := new ChainComplexMap;
     E.source = D;
     E.target = C;
     E.degree = d;
     E.ring = ring C;
     E
     )
-- And here it is:
--$
horseshoe1(Matrix,Matrix) := (F,G) -> (
     B := target F;  -- and source G as well...
     PA := res source F;
     PC := res target G;
     -- We construct three things:
     PB := new ChainComplex;
     PB.ring = ring B;
     FF := zeroChainComplexMap(PB,PA,0);
     GG := zeroChainComplexMap(PC,PB,0);
     -- Now we fill in FF,GG completely, and the
     -- free modules in PB:
     len := max(length PA, length PC);
     scan(0..len, i -> (
	 PB#i = PA_i ++ PC_i;
	 id1 := id_(PB#i);
	 if i < length PA then
	     FF#i = submatrix(id1, {0..numgens PA_i - 1});
	 if i < length PC then
	     GG#i = submatrix(id1, {(numgens PB#i) - (numgens PC_i) 
		                       .. (numgens PB#i)-1}, 
				 null);
	 ));
     -- Get the process started: We must set PB.dd#1.  This is
     -- the first step we did above.
     f = matrix F;
     g = matrix G;
     lambda0 = id_(PC_0) // (g | PC.dd_1);
     lambda0 = submatrix(lambda0, {0..numgens source g-1},null);
     b0 = f | lambda0;
     bbar = modulo(b0, presentation B);
     -- Now for each i >= 1, compute the i th level of PB
     scan(1..len, i -> (
         lambda1 = PC.dd_i // (GG_(i-1) * bbar);
	 lambda2 = bbar * lambda1;
	 PB.dd#i = (FF_(i-1) * PA.dd_i) | lambda2;
	 bbar = syz PB.dd#i;
	 ));
     {FF,GG}
     )
-- We wish to determine from the exact sequence
-- $$ 0 \longrightarrow A \longrightarrow B \longrightarrow C \longrightarrow 0,$$
-- the matrix $C_1 \rightarrow A_0$, where $A_0$ is the free module mapping onto $A$,
-- and $C_1 \rightarrow C_0$ is the presentation matrix of $C$.  This element
-- descends to an element of $Ext^1_R(C,A)$ as well (but this is a story for later),
-- and also becomes the connecting homomorphism.
extension = method()
--$
extension(Matrix,Matrix) := (F,G) -> (
    f = matrix F;
    g = matrix G;
    c = presentation target G;
    a = presentation source F;
    lambda0 = id_(target c) // (g | c);
    lambda0 = submatrix(lambda0, {0..numgens source g-1},null);
    b0 = f | lambda0;
    bbar = modulo(b0, presentation B);
    g0 = submatrix(id_(target a ++ target c), {numgens target a..
	      numgens target a + numgens target c - 1}, null);
    lambda1 = c // (g0 * bbar);
    lambda2 = bbar * lambda1;
    submatrix(lambda2, {0..numgens target a-1}, null)
    )
--$
horseshoe(Matrix,Matrix) := (F,G) -> (
     f = extension(F,G);
     PA = res source F;
     PC = res target G;
     ff = extend(PA,PC[1],f);
     cone ff)
-- 
-- \prob{10}{The snake lemma}
--
-- To find the map $\ker h \rightarrow coker f$
-- in the snake diagram.
-- 
-- \prob{10}{The connecting homomorphism}
--
-- To compute the connecting homomorphisms $\delta_i : H_i(C) \longrightarrow H_i(A)$,
-- given a short exact sequence
-- $$0 \longrightarrow A \longrightarrow B \longrightarrow C \longrightarrow 0$$
-- of chain complexes of $R$-modules.
--
-- As an application we apply this to compute the connecting homomorphisms
-- in Tor, Ext.
--
-- \prob{14}{The tensor product of complexes}
--
-- To define and compute the double complex $C \otimes_R D$, where
-- $C$ and $D$ are chain complexes over $R$.
--
-- \prob{15}{Total complex}
--
-- To define and compute the total complex of a double complex.
--
-- \section{Tor}
--
-- \prob{11}{Tor}
--
-- To compute $Tor^R_i(A,B)$ and $Tor^R_i(f,B)$.
--
-- \prob{12}{Balancing Tor}
--
-- To compute the isomorphism $Tor^R_i(A,B) \longrightarrow Tor^R_i(B,A)$
-- given $R$-modules $A$ and $B$.
--
-- A very useful application is to find the correspondence between minimal $i+1$-syzygies
-- of $A = R/I$ and elements of Koszul cohomology $Tor^R_i(k,R/I)$, where $R$ is a 
-- finitely generated $k$-algebra, and $k$ is a field.
--
-- \prob{12}{Algebra structure on Tor}
--
-- Compute the DG-algebra structure on $Tor^R_*(k,k)$, where $R$ is an affine ring
-- defined over $k$.
--
-- A {\it DG-algebra} (differential graded algebra) is ...
--
-- \section{Ext}
--
-- \prob{13}{Ext}
--
-- To compute $Ext_R^i(A,B)$ and $Ext_R^i(f,B)$.
--
-- \prob{14}{Ext and extensions}
--
-- Give the relationship between elements of $Ext^1(C,A)$ and extensions
-- $$0 \longrightarrow A \longrightarrow B \longrightarrow C \longrightarrow 0,$$
-- where $A,B,C$ are all $R$-modules.
--
-- \section{Change of rings}
--
-- \prob{}{Pushforward}
--
-- \prob{}{Dual of a module}
--
-- Let $S$ be an $R$-algebra, and let $A$ be a $S$-module which is finitely
-- generated over $R$.  The {\it $R$-dual} of $A$ is the $S$-module
-- $dual A := Hom_R(A,R)$.  The problem here is to compute $dual A$.
-- 
--
-- \section{Operations on modules}
--
-- \section{Double complexes and spectral sequences}
--
-- 
