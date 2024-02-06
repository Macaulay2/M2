-- TODO: cohomology(...,Degree=>...)

document {
     Key => (cohomology,ZZ,SheafOfRings),
     Headline => "cohomology of a sheaf of rings on a projective variety",
     Usage => "HH^d(R)",
     Inputs => {"i"=> ZZ, "R" => SheafOfRings =>{"on a projective variety ", TT "X"}
	  },
     Outputs => {Module=>{"the ", TT "i", "-th cohomology group of ", TT "R", " as a vector space
	       over the coefficient field of ", TT "X"}
	  },
     "The command computes the ", TT "i", "-th cohomology group of ", TT "R",
     " as a vector space over the coefficient field of ", TT "X",".",
     PARA{},
     EXAMPLE {
	  "Cubic = Proj(QQ[x_0..x_2]/ideal(x_0^3+x_1^3+x_2^3))",
	  "HH^1(OO_Cubic)"
	  },
     SeeAlso => {"coherent sheaves",(cohomology,ZZ,SumOfTwists),(cohomology,ZZ,CoherentSheaf),hh,CoherentSheaf}
          }

document {
     Key => (cohomology,ZZ,SumOfTwists),
     Headline => "coherent sheaf cohomology module",
     Usage => "HH^i(F(>=d))",
     Inputs => {
	  "i",
          { TT "F(>=d)", ", notation representing the sum of the twists ", TT "F(n)", " for
               all ", TT "n", " greater than or equal to ", TT "d", ", where ", TT "F", " is
               a coherent sheaf on a projective variety ", TT "X", "." }
	  },
     Outputs => {Module => {" M over the homogeneous coordinate ring of
	   the variety ", TT "X", " which agrees, at least in degrees n greater than
	   or equal to d, with the graded module that in degree n is the", TT "i", "-th
	   cohomology group of ", TT "F(n)", "."}
	  },
     "The command computes a module over the homogeneous coordinate ring of
      the variety ", TT "X", " which agrees, at least in degrees n greater than
      or equal to the given d, with the graded module that in degree n is
      the", TT "i", "-th cohomology group of ", TT "F(n)", ".",
     PARA{},
     "To discard the part of the module M of degree less than d,
     use the ", TO "Truncations::truncate(ZZ,Module)", " command as ", TT "truncate(d,M)", ".",
     PARA{},
     "Use ", TT "HH^i(F(>d))", " to request the twists strictly greater than n.",
     PARA{},
     "Note: use ", TT "HH^i(F(*))", " to try to compute the whole graded module.  The
     computation will fail if the module is not finitely generated.",
     PARA{},
     "As a first example we look at the cohomology of line bundles on the
     projective plane",
     EXAMPLE {
	  "X = Proj(QQ[x_0..x_2])",
	  "HH^0(OO_X^1(>=0))",
	  "HH^1(OO_X^1(>=0))",
	  "HH^2(OO_X^1(>=-3)) -- this should change to * once implemented",
	  "TruncDual = HH^2(OO_X^1(>=-4))",
	  "hilbertFunction(-4, TruncDual)",
	  "hilbertFunction(-3, TruncDual)"
	  },
     PARA{},
     "As a second example we compute the H^1 cohomology module T of
     the Horrocks-Mumford bundle on the projective fourspace.  T is
     an artinian module with Hilbert function (5,10,10,2):",
     EXAMPLE {
          "R = QQ[x_0..x_4];",
          "a = {1,0,0,0,0}",
          "b = {0,1,0,0,1}",
          "c = {0,0,1,1,0}",
          "M1 = matrix table(5,5, (i,j)-> x_((i+j)%5)*a_((i-j)%5))",
          "M2 = matrix table(5,5, (i,j)-> x_((i+j)%5)*b_((i-j)%5))",
          "M3 = matrix table(5,5, (i,j)-> x_((i+j)%5)*c_((i-j)%5))",
          "M = M1 | M2 | M3;",
          "betti (C=res coker M)",
          "N = transpose submatrix(C.dd_3,{10..28},{2..36});",
          "betti (D=res coker N)",
          "Pfour = Proj(R)",
          "HorrocksMumford = sheaf(coker D.dd_3);",
          "T = HH^1(HorrocksMumford(>=-1))",
	  "apply(-1..2, i-> hilbertFunction(i,T))"
           },
     Caveat => {"The computation will fail if the module is not finitely generated. Also
     the version HH^i(F(*)) is not yet implemented."},
     SeeAlso => {"HH", (cohomology, ZZ, CoherentSheaf), (cohomology,ZZ,Module)}
     }

document {
     Key => {(cohomology,ZZ,ProjectiveVariety,CoherentSheaf),(cohomology,ZZ,CoherentSheaf)},
     Headline => "cohomology of a coherent sheaf on a projective variety",
     Usage => "HH^i(X,F)\nHH^i F\ncohomology(i,X,F)\ncohomology(i,F)",
     Inputs => {"i"=> ZZ, "X" => ProjectiveVariety, "F" => CoherentSheaf =>{"on the projective variety ", TT "X"} },
     Outputs => {Module=>{"the ", TT "i", "-th cohomology group of ", TT "F", " as a vector space
	       over the coefficient field of ", TT "X"}
	  },
     "The command computes the ", TT "i", "-th cohomology group of ", TT "F",
     " as a vector space over the coefficient field of ", TT "X",".  For i>0 this
     is currently done via local duality, while for i=0 it is computed as a limit of Homs.
     Eventually there will exist an alternative option for computing sheaf cohomology
     via the Bernstein-Gelfand-Gelfand correspondence",
     PARA{},
     "As examples we compute the Picard numbers, Hodge numbers and
     dimension of the infinitesimal deformation spaces of various
     quintic hypersurfaces in projective fourspace (or their Calabi-Yau
     small resolutions)",
     PARA{
	  TEX ///We will make computations for quintics V in the family given by
	  $$x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-5\lambda x_0x_1x_2x_3x_4=0$$
	  for various values of $\lambda$. If $\lambda$ is general
	  (that is, $\lambda$ not a 5-th root of unity, 0 or $\infty$), then
	  the quintic $V$ is smooth, so is a Calabi-Yau threefold, and in that case
	  the Hodge numbers are as follows.///
	  },
     PARA{
     	  TEX "$$h^{1,1}(V)=1,   h^{2,1}(V) = h^{1,2}(V) = 101,$$"
	  },
     PARA{
     	  "so the Picard group of V has rank 1 (generated by the hyperplane section)
     	  and the moduli space of V (which is unobstructed) has dimension 101:"
	  },
     EXAMPLE {
	  "Quintic = Proj(QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-101*x_0*x_1*x_2*x_3*x_4))",
	  "singularLocus(Quintic)",
	  "omegaQuintic = cotangentSheaf(Quintic);",
	  "h11 = rank HH^1(omegaQuintic)",
          "h12 = rank HH^2(omegaQuintic)"
	  },
     PARA{TEX "By Hodge duality this is $h^{2,1}$.  Directly $h^{2,1}$ could be computed as"},
     EXAMPLE {
	  "h21 = rank HH^1(cotangentSheaf(2,Quintic))"
	  },
     PARA{"The Hodge numbers of a (smooth) projective variety can also be computed
     	  directly using the ", TO "hh", " command:"},
     EXAMPLE {
	  "hh^(2,1)(Quintic)",
	  "hh^(1,1)(Quintic)"
	  },
     PARA{"Using the Hodge number we compute the topological Euler characteristic
     	  of V:"},
     EXAMPLE {
	  "euler(Quintic)"
         },
     PARA{
	  TEX ///When $\lambda$ is a 5th root of unity the quintic V is singular. It has 125
	  ordinary double points (nodes), namely the orbit of the point
	  $(1:\lambda:\lambda:\lambda:\lambda)$ under a natural action of $\ZZ/5^3$.
	  Then $V$ has a projective small resolution $W$ which is a Calabi-Yau threefold
	  (since the action of $\ZZ/5^3$ is transitive on the sets of nodes of $V$, or
	  for instance, just by blowing up one of the $(1,5)$ polarized abelian surfaces
	  $V$ contains). Perhaps the most interesting such 3-fold is the one
	  for the value $\lambda=1$, which is defined over $\QQ$ and is modular
	  (see Schoen's work). To compute the  Hodge numbers of the small
	  resolution $W$ of $V$ we proceed as follows:///
	  },
     EXAMPLE {
	  "SchoensQuintic = Proj(QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-5*x_0*x_1*x_2*x_3*x_4))",
	  "Z = singularLocus(SchoensQuintic)",
	  "degree Z",
	  "II'Z = sheaf module ideal Z"
	  },
     PARA{
	  TEX "The defect of $W$ (that is, $h^{1,1}(W)-1$) can be computed from the
     	  cohomology of the ideal sheaf of the singular locus Z of V twisted by 5
     	  (see Werner's thesis):"},
     EXAMPLE {
	  "defect = rank HH^1(II'Z(5))",
	  "h11 = defect + 1"
	  },
     PARA{
	  TEX "The number $h^{2,1}(W)$ (the dimension of the moduli space of $W$) can be computed (Clemens-Griffiths, Werner)
	  as $\\dim H^0({\\mathbf I}_Z(5))/JacobianIdeal(V)_5$."},
     EXAMPLE {
	  "quinticsJac = numgens source basis(5,ideal Z)",
          "h21 = rank HH^0(II'Z(5)) - quinticsJac"
	  },
     PARA{"In other words W is rigid. It has the following topological Euler characteristic."},
     EXAMPLE {
	  "chiW = euler(Quintic)+2*degree(Z)"
	  },
     SeeAlso => {"coherent sheaves",(cohomology,ZZ,SumOfTwists),(cohomology,ZZ,SheafOfRings),hh,CoherentSheaf}
     }

document {
     Key => {hh,(hh,Sequence,ProjectiveVariety)},
     Headline => "Hodge numbers of a smooth projective variety",
     Usage =>"hh^(p,q)(X)",
     Inputs => { Nothing => { "a pair ", TT "(p,q)", " of non negative integers" }, "X" => ProjectiveVariety },
     Outputs => {ZZ},
     "The command computes the Hodge numbers h^{p,q} of the smooth
     projective variety X. They are calculated as ",
     TT "HH^q(cotangentSheaf(p,X))",
     PARA{},
     "As an example we compute h^11 of a K3 surface (Fermat quartic
     in projective threespace:",
     EXAMPLE {
          "X = Proj(QQ[x_0..x_3]/ideal(x_0^4+x_1^4+x_2^4+x_3^4))",
          "hh^(1,1)(X)"
          },
     Caveat => {"There is no check made if the projective variety X is smooth or not."},
     SeeAlso => {(cohomology,ZZ,SumOfTwists),(cohomology,ZZ,SheafOfRings),(euler,ProjectiveVariety)}
     }

document {
    Key => {(Hom,CoherentSheaf,CoherentSheaf),
	(Hom, SheafOfRings, SheafOfRings),
	(Hom, CoherentSheaf, SheafOfRings),
	(Hom, SheafOfRings, CoherentSheaf)},
    Headline => "global Hom",
    Usage => "Hom(F,G)",
    Inputs => {
	"F", "G" => {"both should be sheaves on a
	    projective variety or scheme ", TT "X = Proj R"},
	    },
	Outputs => {
	    Module => {"over the coefficient field of ", TT "R"}
	    },
	PARA{"If ", TT "F", " or ", TT "G", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way."},
	EXAMPLE lines ///
	R = QQ[a..d];
	P3 = Proj R
	I = monomialCurveIdeal(R,{1,3,4})
	G = sheaf module I
	Hom(OO_P3,G(3))
	HH^0(G(3))
	///,
	SeeAlso => {sheafHom, Ext, sheafExt}
	}

document {
    Key => {
	sheafHom,
	(sheafHom, CoherentSheaf, CoherentSheaf),
	(sheafHom, SheafOfRings, CoherentSheaf),
	(sheafHom, CoherentSheaf, SheafOfRings),
	(sheafHom, SheafOfRings, SheafOfRings)
	},
    Headline => "sheaf Hom",
    Usage => "sheafHom(M,N)",
    Inputs => {"M","N"},
    Outputs => {{"the coherent sheaf of homomorphisms from ", TT "M", " to ", TT "N", ""}},
    "If ", TT "M", " or ", TT "N", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
    PARA{},
    TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X", ".",
    PARA{},
    "The result is the sheaf associated to the graded module Hom(module M, module N).",
    EXAMPLE lines ///
    X = Proj(QQ[x,y])
    sheafHom(OO_X^1(2),OO_X(11)^1)
    ///,
    SeeAlso => {OO, sheafExt, Hom, Ext, HH, (Hom, CoherentSheaf, CoherentSheaf)}
    }

document {
    Key => {
	(Ext, ZZ, CoherentSheaf, SumOfTwists),
	(Ext, ZZ, SheafOfRings, SumOfTwists)
	},
    Usage => "Ext^i(M,N(>=d))",
    Headline => "global Ext",
    Inputs => { "i", "M", "N(>=d)"},
    Outputs => {
	Module => {"The R-module ", TEX ///$\oplus_{j \geq d} Ext^i_X(M,N(j))$///}
	},
    "If ", TT "M", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
    PARA{},
    TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X = Proj R", ".",
    PARA{},
    "As an example, we consider the rational quartic curve in
    ", TEX "$P^3$", ".",
    EXAMPLE lines ///
    S = QQ[a..d];
    I = monomialCurveIdeal(S,{1,3,4})
    R = S/I
    X = Proj R
    IX = sheaf (module I ** R)
    Ext^1(IX,OO_X(>=-3))
    Ext^0(IX,OO_X(>=-10))
    ///,
    PARA{},
    "The method used may be found in:
    Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746",
    PARA{},
    TEX ///If the vector space $Ext^i(M,N)$ is desired, see ///, TO (Ext,ZZ,CoherentSheaf,CoherentSheaf), ".",
    SeeAlso => {resolution,Tor,Hom,HH,sheafExt}
    }

document {
    Key => {
	(Ext, ZZ, CoherentSheaf, CoherentSheaf),
	(Ext, ZZ, SheafOfRings, CoherentSheaf),
	(Ext, ZZ, CoherentSheaf, SheafOfRings),
	(Ext, ZZ, SheafOfRings, SheafOfRings)
	},
    Usage => "Ext^i(M,N)",
    Headline => "global Ext",
    Inputs => { "i", "M", "N"},
    Outputs => {
	Module => {"The global Ext module ", TEX "$Ext^i_X(M,N)$"}
	},
    "If ", TT "M", " or ", TT "N", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
    PARA{},
    TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X", ".",
    PARA{},
    "As an example, we compute Hom_X(I_X,OO_X), and Ext^1_X(I_X,OO_X), for the rational quartic curve in
    ", TEX "$P^3$", ".",
    EXAMPLE lines ///
    S = QQ[a..d];
    I = monomialCurveIdeal(S,{1,3,4})
    R = S/I
    X = Proj R
    IX = sheaf (module I ** R)
    Ext^1(IX,OO_X)
    Hom(IX,OO_X)
    ///,
    "The Ext^1 being zero says that the point corresponding to I on the Hilbert scheme is
    smooth (unobstructed), and vector space dimension of Hom tells us that the
    dimension of the component at the point I is 16.",
    PARA{},
    "The method used may be found in:
    Smith, G., ", EM "Computing global extension modules", ", J. Symbolic Comp (2000) 29, 729-746",
    PARA{},
    TEX ///If the module $\oplus_{d\geq 0} Ext^i(M,N(d))$ is desired, see ///, TO (Ext,ZZ,CoherentSheaf,SumOfTwists), ".",
    SeeAlso => {resolution,Tor,Hom,HH,sheafExt,(Ext,ZZ,CoherentSheaf,SumOfTwists)}
    }

document {
    Key => {
	sheafExt,
	(sheafExt, ZZ, CoherentSheaf, CoherentSheaf),
	(sheafExt, ZZ, SheafOfRings, CoherentSheaf),
	(sheafExt, ZZ, CoherentSheaf, SheafOfRings),
	(sheafExt, ZZ, SheafOfRings, SheafOfRings)},
    Headline => "sheaf Ext of coherent sheaves",
    Usage => "sheafExt^n(F,G)",
    Inputs => { "n", "F", "G" },
    Outputs => { CoherentSheaf => { "the n-th sheaf Ext of ", TT "F", " and ", TT "G" } },
    "If ", TT "F", " or ", TT "G", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
    PARA{},
    TT "F", " and ", TT "G", " must be coherent sheaves on the same projective variety or scheme ", TT "X", ".",
    PARA{},
    "The result is the sheaf associated to the graded module ", TT "Ext^n(module M, module N).",
    EXAMPLE lines ///
    X = Proj(QQ[x,y])
    sheafExt^1(OO_X^1(2),OO_X(-11)^1)
    ///,
    SeeAlso => {OO, sheafHom, Hom, Ext, HH, (Ext, ZZ, CoherentSheaf, CoherentSheaf)}
    }
