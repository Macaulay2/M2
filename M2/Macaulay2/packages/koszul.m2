----From: Neil Epstein <epstein@math.uchicago.edu>
----Subject: koszul.m2
----To: Macaulay2@math.uiuc.edu
----Date: Mon, 18 Feb 2002 20:02:47 -0600 (CST)
----
----Enclosed, please find Macaulay 2 code for Koszul complexes, Koszul
----(co)homology, and the depth of a module with respect to an ideal.
----Enjoy.
----
----	Sincerely,
----	-Neil Epstein

-- begin koszul.m2 here --

koszul(ZZ,Ideal) := Matrix => (i,I) -> koszul(i,generators trim I);

koszul(ZZ,Matrix,Module) := Matrix => (i,f,M) -> (
     koszul(i,f) ** M);

koszul(ZZ,Ideal,Module) := Matrix => (i,I,M) -> koszul(i,generators trim I,M);

koszulComplex = method();

koszulComplex(Matrix) := ChainComplex => (f) -> (
     C = new ChainComplex;
     C.ring = ring f;
     n := numgens source f;
     C#0 = target koszul(1,f);
     i := 1;
     while i<=n do (
	  k := koszul(i,f);
	  C#i = source k;
	  C.dd#i = k;
	  i=i+1;
	  );
     C);

koszulComplex(Ideal) := ChainComplex => (I) -> koszulComplex generators trim I;

koszulComplex(Matrix,Module) := ChainComplex => (f,M) -> (
     koszulComplex(f) ** M
     );

koszulComplex(Ideal,Module) := ChainComplex => (I,M) -> (
     koszulComplex(I) ** M
     );

args := method();
args(Thing,Sequence) := (i,seq) -> prepend(i,seq);
args(Thing,Thing) := identity;

KK = new ScriptedFunctor from {
     subscript => (
	  i -> new ScriptedFunctor from {
	       argument => (
		    if instance(i,ZZ)
		    then X -> koszul args(i,X)
		    else if instance(i,String)
		    then X -> koszulComplex X
		    else error "bad input to KK."
		    )
	       }
	  ),
     superscript => (
	  i -> new ScriptedFunctor from {
	       argument => (
		    if instance(i,ZZ)
		    then X -> cokoszul args(i,X)
		    else if instance(i,String)
		    then X -> cokoszulComplex X
		    else error "bad input to KK."
		    )
	       }
	  ),
     argument => (
	  X -> koszulComplex X
	  )
     }	    

HK = new ScriptedFunctor from {
     subscript => (
	  i -> new ScriptedFunctor from {
	       argument => (
		    if instance(i,ZZ)
		    then X -> koszulHomology args(i,X)
		    else if instance(i,String)
		    then X -> koszulHomology X
		    else error "bad input to HK."
		    )
	       }
	  ),
     superscript => (
	  i -> new ScriptedFunctor from {
	       argument => (
		    if instance(i,ZZ)
		    then X -> koszulCohomology args(i,X)
		    else if instance(i,String)
		    then X -> koszulCohomology X
		    else error "bad input to HK."
		    )
	       }
	  ),
     argument => (
	  X -> koszulHomology X
	  )
     }	    

koszulHomology = method();

koszulHomology(ZZ,Matrix) := Module => (i,f) -> (
     homology(koszul(i,f), koszul(i+1,f))
     );

koszulHomology(Matrix) := GradedModule => (f) -> HH koszulComplex(f);

koszulHomology(ZZ,Ideal) := Module => (i,I) -> koszulHomology(i,generators trim I);

koszulHomology(Ideal) := GradedModule => (I) -> HH koszulComplex(I);

koszulHomology(ZZ,Matrix,Module) := Module => (i,f,M) -> (
     homology(koszul(i,f,M), koszul(i+1,f,M))
     );

koszulHomology(Matrix,Module) := GradedModule => (f,M) -> (
     HH koszul(f,M)
     );

koszulHomology(ZZ,Ideal,Module) := Module => (i,I,M) -> (
     koszulHomology(i,generators trim I, M)
     );

koszulHomology(Ideal,Module) := GradedModule => (I,M) -> (
     HH koszul(I,M)
     );

----
needs "hom.m2";	    -- does it really need it?

cokoszul = method();

cokoszulComplex = method();

cokoszul(ZZ,Matrix,Module) := Matrix => (i,f,M) -> (
     Hom(koszul(i,f),M));

cokoszulComplex(Matrix,Module) := ChainComplex => (f,M) -> (
     Hom(koszul f, M)
     );

cokoszul(ZZ,Matrix) := Matrix => (i,f) -> cokoszul(i,f,(ring f)^1);

cokoszulComplex(Matrix) := ChainComplex => (f) -> (
     Hom(koszulComplex f, R^1);
     )

cokoszul(ZZ,Ideal,Module) := Matrix => (i,I,M) -> (
     Hom(koszul(i,I),M)
     );

cokoszulComplex(Ideal,Module) := ChainComplex => (I,M) -> (
     Hom(koszulComplex I, M)
     );

cokoszulComplex(Ideal) := ChainComplex => (I) -> (
     Hom(koszulComplex I, R^1)
     );

koszulCohomology = method();

koszulCohomology(ZZ,Matrix) := Module => (i,f) -> (
     homology(cokoszul(i,f), cokoszul(i-1,f))
     );

koszulCohomology(Matrix) := GradedModule => (f) -> (
     HH cokoszul f
     );

koszulCohomology(ZZ,Ideal) := Module => (i,I) -> koszulCohomology(i,generators trim I);

koszulCohomology(Ideal) := GradedModule => (I) -> (
     HH cokoszul I
     );

koszulCohomology(ZZ,Matrix,Module) := Module => (i,f,M) -> (
     homology(cokoszul(i,f,M), cokoszul(i-1,f,M))
     );

koszulCohomology(Matrix,Module) := GradedModule => (f,M) -> (
     HH cokoszul(f,M)
     );

koszulCohomology(ZZ,Ideal,Module) := Module => (i,I,M) -> (
     koszulCohomology(i,generators trim I, M)
     );

koszulCohomology(Ideal,Module) := GradedModule => (I,M) -> (
     HH cokoszul(I,M)
     );

--
depth(Ideal,Module) := (I,M) -> (
     if M == I*M then infinity
     else (
     	  i := n := numgens I;
     	  while true do (
	       H = koszulHomology(i,I,M);
	       if H != 0 then break;
	       i=i-1;
	       if i<0 then break;
	       );
     	  if i<0 then infinity else n-i));

-- calculates the "*depth" of a module over a graded-local ring.
depth Module := (M) -> (
     R := ring M;
     if not isField coefficientRing R
     then error "not implemented yet for modules over rings
     which are not graded-local";
     depth(ideal gens R, M));
----

document {
     Key => KK,
     Headline => "scripted functor for the Koszul complex.",
     TT "KK x", " -- the Koszul complex of the matrix or ideal ", TT "x", ".",
     BR{},
     TT "KK_i (x)", " -- the ", TT "i", "-th differential of this complex.",
     BR{},
     TT "KK (x,M)", " -- ", TT "KK(x) ** M", " where ", TT "M", " is a module
     over the ring of ", TT "x", ".",
     BR{},
     TT "KK_i (x,M)", " -- the ", TT "i", "-th differential of this complex.",
     BR{},
     TT "KK^\"*\" (x)", " -- ", TT "Hom(KK(x),R)", ", where ", TT "R", " is the
     ring of ", TT "x", ".",
     BR{},
     TT "KK^i (x)", " -- the ", TT "i", "-th differential of this complex.",
     BR{},
     TT "KK^\"*\" (x,M)", " -- ", TT "Hom(KK(x), M)", " where ", TT "M", " is
     a module over the ring of ", TT "x", ".",
     BR{},
     TT "KK^i (x,M)", " -- the ", TT "i", "-th differential of this complex.",
     PARA{}, TT "KK", " is a scripted functor which calls one of four functions:
     ", TT "KK_i(..)", " calls ", TO "koszul", ",
     ", TT "KK^i(..)", " calls ", TO "cokoszul", ",
     ", TT "KK(..)", " calls ", TO "koszulComplex", ", and
     ", TT "KK^\"*\"", " calls ", TO "cokoszulComplex", ".",
     SeeAlso => {koszul, cokoszul, koszulComplex, cokoszulComplex, HK}
     }

document {
     Key => koszul,
     Headline => "a differential in the Koszul complex"
     }

document {
     Key => cokoszul,
     Headline => "a differential in the \"co-Koszul\" complex",
     TT "cokoszul(i,x)", " or ", TT "cokoszul(i,x,M)", ".",
     PARA{}, TT "cokoszul(i,x,M)", " is equal to ", 
     TT "Hom(koszul(i,x) ** M)", ".
     ", TT "cokoszul(i,x)", " is shorthand for ",
     TT "cokoszul(i,x,(ring x)^1)", "."}

document {
     Key => koszulComplex,
     -- this node has to be replace by three
     Headline => "the Koszul complex",
     -- Usage => {TT "koszulComplex x", " or ", TT "koszulComplex(x,M)"},
     PARA{}, "Here, ", TT "x", " may be either an ideal or a ", TT "1", " by
     ", TT "n", "matrix.  ", TT "M", " is a module over the ring 
     of ", TT "x", "."
     }

document {
     Key => cokoszulComplex,
     -- this node has to be replace by three
     Headline => "the \"co-Koszul\" complex",
     -- Usage => {TT "cokoszulComplex x", " or ", TT "cokoszulComplex(x,M)"},
     PARA{}, TT "cokoszulComplex(x,M)", " is the complex ", TT "Hom(KK x, M)",
     ".
       ", TT "cokoszulComplex x", " is the same as ",
     TT "cokoszulComplex(x,(ring x)^1)", "."}

document {
     Key => HK,
     Headline => "scripted functor for Koszul (co)homology",
     TT "HK x", " -- the Koszul homology of the matrix or ideal ", TT "x", ".",
     BR{},
     TT "HK_i (x)", " -- the ", TT "i", "-th piece of this graded module.",
     BR{},
     TT "HK (x,M)", " -- ", TT "HH KK(x,M)", ".",
     BR{},
     TT "HK_i (x,M)", " -- the ", TT "i", "-th piece of this graded module.",
     BR{},
     TT "HK^\"*\" (x)", " -- ", TT "HH KK(x,ring x)", ".",
     BR{},
     TT "HK^i (x)", " -- the ", TT "i", "-th piece of this graded module.",
     BR{},
     TT "HK^\"*\" (x,M)", " -- ", TT "HH KK(x,M)", ".",
     BR{},
     TT "HK^i (x,M)", " -- the ", TT "i", "-th piece of this graded module.",
     PARA{}, TT "HK", " is a scripted functor which calls one of two functions:
     ", TT "HK_i(..)", " and ", TT "HK(..)", " call ", TO "koszulHomology",
     ", 
     while ", TT "HK^i(..)", " and ", TT "HK^\"*\"(..)", " call ",
     TO "koszulCohomology", ".",
     SeeAlso => {koszulHomology, koszulCohomology, KK}
     }

document {
     Key => (depth,Ideal,Module),
     Headline => "the I-depth of M",
     Usage => 	  "d = depth(I,M)",
     Inputs => { "I", "M" },
     Outputs => { "d" }
     }

document {
     Key => (depth,Module),
     Headline => "the depth of a module",
     PARA{}, "Over a ring whose coefficient ring is a field, find the 
     depth of the module with respect to the graded-maximal ideal."
     }

-- end --
