-- From: Neil Epstein <epstein@math.uchicago.edu>
-- Subject: code to the enhancements
-- To: Macaulay2@math.uiuc.edu
-- Date: Tue, 12 Feb 2002 17:06:35 -0600 (CST)
-- In-Reply-To: <Pine.LNX.4.21.0202112223330.18615-100000@sg12.math.cornell.edu> from "Mike Stillman - Mail Account" at Feb 11, 2002 10:24:40 PM
-- 
-- > Definitely!  Do you have documentation on your additions, and/or tests of
-- > them as well?
-- 
-- Yes to the first question; no to the second.  I have done some ad-hoc
-- tests of some of them, but I haven't recorded the tests.  Basically, I
-- only did just enough to satisfy myself that everything (probably)
-- works.  I'll be happy to do more, if you like.  Maybe you could give
-- me examples of modules that one would expect to fail if anything does,
-- or I could try to come up with such examples myself.
-- 
-- By the way, I expect that Hom(Module,ChainComplex) and
-- Hom(Module,ChainComplexMap) should now work, for non-Free inputs of
-- class "Module", with no further alterations to the code.  I'm thinking
-- that the next logical step would be to implement such methods as
-- Hom(Matrix,ChainComplexMap), Hom(ChainComplex,ChainComplex), and so
-- forth.  Also I suppose that, for completeness, we should have methods
-- such as homEvaluation(Matrix,Matrix,Matrix), so as to be able to
-- create homEvaluation(ChainComplex,ChainComplex,ChainComplex).
-- 
-- I documented all my new functions except "conditionalTrim" (which I
-- don't see as having widespread use anyway), both with "--"-style
-- comments and with comments available to the Macaulay 2 help system.
-- It is all included in the same file.
-- 
-- Anyway, here's the code and documentation.  I've been calling it
-- "hom.m2".
-- 
-- 	Sincerely,
-- 	-Neil Epstein

Hom(Module,Module) := Module => (M,N) -> (
    -- This is almost the original code for Hom, but with "trim" 
    -- performed only if it leaves the ambient free module alone,
    -- since I'm not sure if my new "Hom" routines will work
    -- with trimmed Hom modules.
    m := presentation M;
    mdual := transpose m;
    n := presentation N;
    h1 := modulo(mdual ** target n, target mdual ** n);
    MN := conditionalTrim subquotient(h1,source mdual ** n);
    MN.Hom = {M,N,source mdual,target n};
    MN);

conditionalTrim = method();

conditionalTrim(Module) := Module => (M) -> (
    if not M#?trim then M.trim=trim M;
    if ambient M.trim == ambient M then M.trim else M);

-- The following commented-out functions are the versions before I
-- decided to make "Hom(Matrix,Matrix)" my basic function.

--Hom(Matrix,Module) := Matrix => (f,N) -> (
--    if isFreeModule source f and isFreeModule target f
--    then transpose f ** N
--    else (
--        fp := prune f;
--        L := source f;
--        M := target f;
--        fpl := (fp * (coverMap source fp)) // (coverMap target fp);
--        inducedMap(Hom(M,N), Hom(L,N), transpose fpl ** cover prune N,
--	           Verify => true)));

--Hom(Module,Matrix) := Matrix => (L,g) -> (
--    if isFreeModule L then dual L ** g
--    else (
--        gp := prune g;
--        M := source g;
--        N := target g;
--        gpl := (gp * (coverMap source gp)) // (coverMap target gp);
--        inducedMap(Hom(L,N),Hom(L,M), (dual cover prune L) ** gpl,
--	           Verify => true));

Hom(Matrix,Matrix) := Matrix => (f,g) -> (
    if isFreeModule source f and isFreeModule target f
    then transpose f ** g
    else (
	fp := prune f;
	K := source f; L := target f;
	fpl := (fp * (coverMap source fp)) // (coverMap target fp);
	gp := prune g;
	M := source g; N := target g;
	gpl := (gp * (coverMap source gp)) // (coverMap target gp);
	inducedMap(Hom(K,N),Hom(L,M), transpose fpl ** gpl,
		       Verify => true)));

Hom(Matrix,Module) := Matrix => (f,N) -> Hom(f,id_N);

Hom(Matrix,Ring) := Matrix => (f,R) -> (
    if ring f =!= R then error "expected matrix to have the given ring";
    Hom(f,R^1));

Hom(Module,Matrix) := Matrix => (L,g) -> Hom(L,id_g);

homomorphism Matrix := Matrix => (f) -> (
    -- This is almost the original code for homomorphism, but 
    -- with one change at the end, as marked.  I think I
    -- may have fixed a bug here.
    if not isFreeModule(source f) or
       numgens source f =!= 1 or
       not (target f).?Hom
       then error "homomorphism may only be determined for maps R --> Hom(M,N)";
    MN := (target f).Hom;
    M := MN#0;
    Mp := prune M;  -- added line
    N := MN#1;
    Np := prune N;  -- added line
    M0 := MN#2;
    N0 := MN#3;
    deg := (degrees source f)#0;
    -- My final two lines replace the original ending line:
    -- map(N,M,adjoint1(super f, M0, N0),Degree=>deg)).  I
    -- needed to make sure the resulting map would be well-
    -- defined, and I wasn't sure the existing code would do that;
    a := map(Np,Mp,adjoint1(super f, M0, N0),Degree=>deg);
    Np.pruningMap * a * Mp.pruningMap^-1);

homElement = method();
-- The reverse of "homomorphism".  It takes as input a homomorphism
-- from M to N, and returns as output an 'element' of Hom(M,N);

document{ homElement,
     Headline => "get the element of Hom from a homomorphism",
     Synopsis => {
          "h = homElement f",
          "f" => "M ---> N",
          "h" => "the corresponding map R^1 ---> Hom(M,N), where R is the ring of f."
          },
     PARA, "This is designed to be the inverse of the function ",
     TO "homomorphism", ".",
     SEEALSO "homomorphism"
     }

homElement(Matrix) := Matrix => (f) -> (
    deg := degree f;
    fp := prune f;
    H := Hom(source f, target f);
    j := super id_H;
    Mp := source fp; m := coverMap Mp; M0 := source m;
    Np := target fp; n := coverMap Np; N0 := source n;
    fpl := (fp * m) // n;
    assert (fp*m - n*fpl == 0);
    R := ring f;
    F := R^{-deg};	-- makes it a map from R^1 with degrees {deg}.
    a := adjoint(fpl,F,M0);
    map(H,F,a));

tensorHomAdjoint = method();

document{ tensorHomAdjoint,
    Headline => "adjunction, ** to Hom."
    }

tensorHomAdjoint(Matrix,Module,Module) := Matrix => (f,L,M) -> (
    if isFreeModule L and isFreeModule M
       and isFreeModule target f then adjoint(f,L,M)
    else (
	if source f != (L ** M) then
 	  error "expected input (f,L,M), where f is a map L ** M -> N.";
	N := target f;
	Lp := prune L;
	Mp := prune M;
	Np := prune N; n := coverMap Np;
	fpp := Np.pruningMap^-1 * f *
	         (Lp.pruningMap ** Mp.pruningMap);
	fppl := (fpp * t) // n;
	a := adjoint(fppl,L0,M0);
	inducedMap(Hom(M,N),Lp,a, Verify=>true) * Lp.pruningMap^-1));

document{ (tensorHomAdjoint, Matrix, Module, Module),
     Headline => "adjoint map, ** to Hom",
     Synopsis => {
          "g = tensorHomAdjoint(f,L,M)",
          "f" => "L ** M ---> N",
          "L" => null, "M" => null,
          "g" => {"the adjoint L ---> Hom(M,N)",
                  " of ", TT "f", ".", PARA,
                  "In terms of 'elements': ",
                  "g(l)(m) = f(l ** m)."}
          },
     SEEALSO (homTensorAdjoint,Matrix,Module,Module)
     }

tensorHomAdjoint(Module,Module,Module) := Matrix => (L,M,N) -> (
    taInverse := tensorAssociativity(
			      dual cover prune L,
			      dual cover prune M,
			      cover prune N)^-1;
    inducedMap(Hom(L,Hom(M,N)), Hom(L**M,N), taInverse,
	       Verify=>true));

document{ (tensorHomAdjoint, Module, Module, Module),
     Headline => "**-Hom adjunction functor.",
     Synopsis => {
          "g = tensorHomAdjoint(L,M,N)",
          "L" => null,
          "M" => null,
          "N" => null,
          "g" => {"Hom(L ** M,N) ---> Hom(L,Hom(M,N))",
                  PARA, "In terms of 'elements': ",
                  "g(f)(l)(m) = f(l ** m)."}
          },
      SEEALSO (homTensorAdjoint,Module,Module,Module)
      }

homTensorAdjoint = method();

document{ homTensorAdjoint,
    Headline => "adjunction, Hom to **."
    }

homTensorAdjoint(Matrix,Module,Module) := Matrix => (f,L,M) -> (
    if isFreeModule L and isFreeModule M
    and isFreeModule target f then adjoint1(f,L,dual M)
    else (
	H := target f;
	if source f != L or not H#?Hom or H.Hom#0 != M then
	  error "expected input (f,L,M), where f is a map L -> Hom(M,N)";
	N := H.Hom#1;
	Lp := prune L; l := coverMap Lp; L0 := source l;
	Mp := prune M; M0 := cover Mp;
	Np := prune N;
	sf := super f;
	fl := (sf * Lp.pruningMap * l) // (coverMap target sf);
	a := adjoint1(fl,L0,dual cover Mp);
	Np.pruningMap * inducedMap(Np, Lp ** Mp, a, Verify => true)
	  * (Lp.pruningMap ** Mp.pruningMap)^-1));

document{ (homTensorAdjoint, Matrix, Module, Module),
     Headline => "adjoint map, Hom to **",
     Synopsis => {
          "g = homTensorAdjoint(f,L,M)",
          "f" => "L ---> Hom(M,N)",
          "L" => null, "M" => null,
          "g" => {"the adjoint L ** M ---> N", " of ",
                  TT "f", ".", PARA, "In terms of 'elements': ",
                  "g(l ** m) = f(l)(m)."}
          },
     SEEALSO (tensorHomAdjoint,Matrix,Module,Module)
     }

homTensorAdjoint(Module,Module,Module) := Matrix => (L,M,N) -> (
    ta := tensorAssociativity(
			      dual cover prune L,
			      dual cover prune M,
			      cover prune N);
    inducedMap(Hom(L**M,N), Hom(L,Hom(M,N)), ta,Verify=>true));

document{ (homTensorAdjoint, Module, Module, Module),
     Headline => "Hom-** adjunction functor.",
     Synopsis => {
          "g = homTensorAdjoint(L,M,N)",
          "L" => null,
          "M" => null,
          "N" => null,
          "g" => {"Hom(L,Hom(M,N)) ---> Hom(L ** M, N)", PARA,
                  "In terms of 'elements': g(f)(l ** m)= f(l)(m)."}
          },
     SEEALSO (tensorHomAdjoint,Module,Module,Module)
     }

homEvaluation = method();

document{ homEvaluation,
     Headline => "Hom evaluation functor."
     }

document{ (homEvaluation,Module,Module,Module),
     Synopsis => {
          "h = homEvaluation(L,M,N)",
          "L" => null,
          "M" => null,
          "N" => null,
          "h" => {"L ** Hom(M,N) ---> Hom(Hom(L,M),N).", PARA,
                  "In terms of 'elements': h(l ** g)(f) = f(g(l))."}
          },
     SEEALSO { "tensorEvaluation", "bidualMap",
               (homTensorAdjoint,Module,Module,Module),
               (tensorHomAdjoint,Module,Module,Module) }
     }

homEvaluation(Module,Module,Module) := Matrix => (L,M,N) -> (
    Lp := prune L; l := coverMap Lp; L0 := source l;
    Mp := prune M; m := coverMap Mp; M0 := source m;
    Np := prune N; n := coverMap Np; N0 := source n;
    H2 := Hom(M,N);
    k := super id_H2;
    p := coverMap target k;
    ta := tensorAssociativity(L0,dual M0, N0);
    H1 := Hom(L,M); Hp := prune H1;
    j := super id_H1;
    v := (j * Hp.pruningMap * (coverMap Hp)) // coverMap target j;
    vsN := (transpose v) ** N0;
    w := inducedMap(Hom(Hom(L,M),N), Lp ** target k,
		       vsN * ta, Verify => true);
    w * (Lp ** k) * (Lp.pruningMap^-1 ** source k));

tensorEvaluation = method();

document{ tensorEvaluation,
     Headline => "tensor evaluation functor."
     }

document{ (tensorEvaluation,Module,Module,Module),
     Synopsis => {
          "h = tensorEvaluation(L,M,N)",
          "L" => null, "M" => null, "N" => null,
          "h" => {"Hom(L,M) ** N ---> Hom(L, M ** N).", PARA,
                 "In terms of 'elements': h(f ** n)(l) = f(l) ** n."}
          },
     SEEALSO { "homEvaluation",
               (homTensorAdjoint,Module,Module,Module),
               (tensorHomAdjoint,Module,Module,Module) }
     }

tensorEvaluation(Module,Module,Module) := Matrix => (L,M,N) -> (
    H1 := Hom(L,M);
    Lp := prune L; l := coverMap Lp; L0 := source l;
    Mp := prune M; m := coverMap Mp; M0 := source m;
    Np := prune N; n := coverMap Np; N0 := source n;
    j := super id_H1;
    p := coverMap (target j);
    ta := tensorAssociativity(dual L0, M0,N0)^-1;
    w := inducedMap(Hom(L, M**N), (target j) ** Np, ta,
		       Verify => true);
    w * (j ** Np.pruningMap^-1));

bidualMap = method();

document{ bidualMap,
     Headline => "biduality functor",
     Synopsis => {
          "h = bidualMap M",
          "M" => null,
          "h" => {"M ---> Hom(Hom(M,R^1),R^1), where R",
                  " is the ring of M.", PARA, 
                  "In terms of 'elements': h(m)(f) = f(m)."}
          },
     SEEALSO (homEvaluation,Module,Module,Module)
     }

bidualMap(Module) := Matrix => (M) -> (
    R := ring M;
    h := homEvaluation(M,R^1,R^1));
