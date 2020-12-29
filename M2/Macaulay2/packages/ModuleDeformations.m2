-- ModuleDeformations.m2
--
-- Copyright 2007-08 Bradford Hovinen
-- Licensed under the GNU GPL version 2.0

newPackage(
     "ModuleDeformations",
     Version => "1.0",
     Date => "28 October 2009",
     Authors => {{Name => "Bradford Hovinen",
	          Email => "hovinen@math.uni-hannover.de"}},
     Headline => "versal deformations of maximal Cohen-Macaulay modules",
     Keywords => {"Deformation Theory"},
     PackageImports => { "Truncations" },
     Certification => {
	  "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	  "journal URI" => "http://j-sag.org/",
	  "article title" => "Deformations of Matrix Factorisations with Macaulay2",
	  "acceptance date" => "2010-04-17",
	  "published article URI" => "http://j-sag.org/Volume2/jsag-2-2010.pdf",
	  "published code URI" => "http://j-sag.org/Volume2/ModuleDeformations.m2",
	  "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/ModuleDeformations.m2",
	  "release at publication" => "314a1e7a1a5f612124f23e2161c58eabeb491f46",
	  "version at publication" => "1.0",
	  "volume number" => "2",
	  "volume URI" => "http://j-sag.org/Volume2/"
	  },
     DebuggingMode => false
)

export {"deformMCMModule", "xi"}

xi = new IndexedVariableTable;

-- ShiftRowDegs(M,d)
--
-- Given a homogeneous matrix of degree e, return a new matrix of
-- degree d+e with row degrees shifted by -d

ShiftRowDegs = (M, d) -> (
     map(target M**(ring M)^{-d}, source M, M, Degree=>(d + degree M))
)

-- ShiftColDegs(M,d)
--
-- Given a homogeneous matrix of degree e, return a new matrix of
-- degree d+e with col degrees shifted by d

ShiftColDegs = (M, d) -> (
     map(target M, source M**(ring M)^{d}, M, Degree=>(d + degree M))
)

-- HomBoundaryMaps
--
-- Given a matrix factorization corresponding to an MCM module M,
-- compute the boundary maps in the complex Hom^*(M,M). Since the
-- resolution of M is two-periodic, there are two such maps. They are
-- returned as (2n^2)x(2n^2) matrices, where n is the rank of M. The
-- first map is the even-degree differential, and the second is the
-- odd-degree differential.
--
-- The matrices should be defined over the ring of the hypersurface over
-- which they represent an MCM.

HomBoundaryMaps = (A, B) -> (
     ((ShiftRowDegs(Hom(source A,A) | Hom(A,source B), -degree A)) ||
      (ShiftRowDegs(-Hom(B,source A) | -Hom(source B,B), -degree B)),
      
      ShiftColDegs ((ShiftColDegs(Hom(source A,B) || -Hom(B,source B), -degree B)) |
                    (ShiftColDegs(Hom(A,source A) || -Hom(source B,A), -degree A)), degree B + degree A))
)

-- VectToMatPair
--
-- Convert a matrix pair represented as a column vector to a pair of
-- matrices with the correct degrees.

VectToMatPair = (v, source1, target1, deg1, source2, target2, deg2) -> (
     M := reshape (source1, target1 ++ target2, transpose v);
     
     (map (target1, source1, M_[0], Degree=>deg1),
      map (target2, source2, M_[1], Degree=>deg2))
)

-- MatPairToVect
--
-- Given a pair of matrices, represent it as single column vector with
-- the correct degrees. The row degrees are prescribed.

MatPairToVect = (M1, M2) -> (
     n := numgens target M1;
     N := 2 * n^2;
     reshape ((ring M1)^N,(ring M1)^1, M1 | M2)
)

-- ComputeExtBasisVectors
--
-- Given even and odd differentials of complex, compute a basis of
-- Ext^1. Return the result as columns of a matrix.

ComputeExtBasisVectors = (A,B,i) -> (
     d := HomBoundaryMaps (A,B);
     ExtModule := trim(ker d_i/image d_(1-i));
     
     -- Workaround for bug in Macaulay2 1.1
     if dim ExtModule > 0 then
     	  error "ExtModule has infinite dimension";
     
     ExtMatrix := super basis ExtModule;
     
     -- For some reason, this computation returns the direct sum of
     -- two modules, only the first of which is the correct one.
     ExtMatrix_{0..numgens source ExtMatrix // 2 - 1}
)

-- ComputeExtBasis
--
-- Given even and odd differentials of complex, compute a basis of
-- Ext^1. Return the result as pairs of matrices. Also return the
-- degrees of the generators.

ComputeExtBasis = (A,B,i) -> (
     ExtBasisVect := ComputeExtBasisVectors(A,B,i);
     ExtBasisVect = map (ambient target ExtBasisVect, source ExtBasisVect, ExtBasisVect);
     ExtDegs := - (degrees ExtBasisVect)_1;
     ExtBasis := (i -> VectToMatPair (ExtBasisVect_{i},
				      source A, target A, degree A + (degrees ExtBasisVect)_1_i,
			              source B, target B, degree B + (degrees ExtBasisVect)_1_i)) \
			toList(0..numgens source ExtBasisVect - 1);

     (ExtBasis, ExtDegs)
)

-- ConstructAmbientRing
--
-- Construct the ambient product ring in quotients of which all
-- computations take place

ConstructAmbientRing = (OY,OSigma,Ext1degs,pIdeal) -> (
     dimExt1 := length Ext1degs;

     -- We pull out the variables coming from Sigma and treat them
     -- separately for term ordering purposes
     SigmaVars := if (pIdeal == 0) then {} else flatten entries mingens pIdeal;
     YVars := select (flatten entries vars OY, (v -> not member(v, SigmaVars)));
     SigmaVarDegs := SigmaVars / (v -> degree v);
     YVarDegs := YVars / (v -> degree v);
     
     OYAmbientxSAmbient := QQ(monoid [YVars, SigmaVars, xi_1..xi_dimExt1,
			      Degrees=>(YVarDegs | SigmaVarDegs | Ext1degs),
			      MonomialOrder=>{Position => Down, Weights => Eliminate(length YVars)}]);
     OSAmbient := QQ(monoid [SigmaVars, xi_1..xi_dimExt1,
		     Degrees=>(SigmaVarDegs | Ext1degs)]);
     (OSAmbient, OYAmbientxSAmbient/(substitute (ideal OY, OYAmbientxSAmbient)))
)

-- FirstDeformation
--
-- Deform a module into its first infinitesimal neighbourhood.

FirstDeformation = (OYxSAmbient,pIdeal,A,B,d,Ext1,Ext2Vect) -> (
     dimExt1 := length Ext1;
     
     Xi := ideal (gens OYxSAmbient)_{ -dimExt1..-1};
     OYxS1 := OYxSAmbient / ((substitute(pIdeal, OYxSAmbient) +  Xi)^2);

     At := substitute (A, OYxS1);
     Bt := substitute (B, OYxS1);
     lhs := -At*Bt;
     vlhs := map(substitute(target d_1,OYxS1),, MatPairToVect (lhs, -lhs));
     psolvect := vlhs // substitute (d_1, ring vlhs);

     psol := VectToMatPair (psolvect, source At, target At, degree At, source Bt, target Bt, degree Bt);
     
     n := numgens OYxS1;
     A = fold (plus, ((j -> (OYxS1_(n-dimExt1+j) * substitute ((Ext1_j)_0, OYxS1))) \ (0 .. dimExt1 - 1))) + psol_0 + At;
     B = fold (plus, ((j -> (OYxS1_(n-dimExt1+j) * substitute ((Ext1_j)_1, OYxS1))) \ (0 .. dimExt1 - 1))) + psol_1 + Bt;

     
     (A, B)
)

-- LiftExt2Basis
--
-- Lift a given basis of Ext2 to the given ring

LiftExt2Basis = (Ext2Vect, R) -> (
     map(coker substitute(presentation target Ext2Vect, R), substitute(source Ext2Vect, R), 
	 substitute(map(ambient target Ext2Vect, source Ext2Vect, Ext2Vect), R))
)

-- ExtendDeformation
--
-- Given a deformation, extend it by one more infinitesimal
-- neighbourhood.

ExtendDeformation = (OYxSAmbient,pIdeal,i,A,B,d,dimExt1,Ext2Vect,obstruction) -> (
     Xi := ideal (gens OYxSAmbient)_{ -dimExt1..-1};
     OYxSi := OYxSAmbient / ((pIdeal + Xi)^(i+1));	       
     At := substitute (A, OYxSi);
     Bt := substitute (B, OYxSi);

     lhs1 := -Bt*At;
     lhs2 := -At*Bt;
     
     -- Check whether we are done. If the left-hand-side modulo the
     -- existing obstruction is done, then a particular solution is
     -- zero and no further computation is necessary.
     
     obstruction = obstruction / (f -> substitute (f, OYxSi));
     obsIdeal := ideal obstruction;
     if (all (flatten flatten entries lhs1, f -> (f % obsIdeal == 0)) and
         all (flatten flatten entries lhs2, f -> (f % obsIdeal == 0)))
	 then return (At, Bt, obstruction, false);

     -- This computation must take place in the ambient ring of O_Y,
     -- since otherwise relations in O_Y may screw up the degrees in
     -- the particular solution.
     Xi = ideal (gens ambient OYxSAmbient)_{ -dimExt1..-1};
     OYAmbientxSi := (ambient OYxSAmbient) / ((substitute(pIdeal,ambient OYxSAmbient) + Xi)^(i+1));

     Ext2VectOYAmbientxSi := LiftExt2Basis(Ext2Vect,OYAmbientxSi);
     
     vlhs := map(substitute(target d_1, OYAmbientxSi),, substitute(MatPairToVect (lhs1, -lhs2),OYAmbientxSi));
     vlhsobs := map(target Ext2VectOYAmbientxSi,, vlhs % substitute (d_1, OYAmbientxSi));
     obsi := (vlhsobs // Ext2VectOYAmbientxSi) % (syz map (ambient target Ext2VectOYAmbientxSi,, Ext2VectOYAmbientxSi));
     
     obstruction = flatten entries (substitute(obsi,OYxSi))_0;
     
     rawpsol := substitute(substitute(vlhs,OYAmbientxSi) // substitute (d_1, OYAmbientxSi), OYxSi);

     psol := VectToMatPair (rawpsol, source At, target At, degree At, source Bt, target Bt, degree Bt);

     (At + psol_0, Bt + psol_1, obstruction, false)
)

-- deformMCMModule
--
-- Deform a maximal Cohen-Macaulay module on a hypersurface into a
-- module over the ring of the hypersurface defined by the given polynomial
-- up to the given maximal degree of the indeterminate. Return the
-- resulting matrix factorization and the obstruction.

deformMCMModule = method(Options => {DegreeLimit => 10})
deformMCMModule(Module,RingMap) := o -> (M0,phi) -> (
     -- Check preconditions
     if (codim prune ring M0 > 1) then error "This procedure only works with modules over hypersurfaces.";

     -- If M0 is already free, then it is rigid, so just return the free module
     if (syz presentation M0 == 0) then return (source phi,(target phi)^(degrees prune M0));

     -- Otherwise, make sure M0 is maximal Cohen-Macaulay
     A0 := presentation M0;
     if (numrows A0 != numcols A0 or det lift (A0, ambient ring A0) == 0) then error "M0 is not maximal Cohen-Macaulay";

     if (dim Ext^1(M0,M0) > 0) then error "Ext^1(M0,M0) has infinite dimension. Cannot construct versal deformation.";

     -- Extract the other matrix of the matrix factorization
     Ml := coker substitute (presentation M0, prune ring M0);
     C := chainComplex lift (presentation Ml, ambient ring Ml);
     f := (mingens ideal ring Ml)_0_0;
     B0 := substitute ((nullhomotopy (f * id_C))_0, ring M0);
     
     if (B0*A0 != 0) then error "B0 A0 is not zero";
     if (A0*B0 != 0) then error "A0 B0 is not zero";
     
     OY := target phi;
     OSigma := source phi;
     pIdeal := phi(ideal vars OSigma);

     d := HomBoundaryMaps (A0, B0);
     Ext2BasisVect := ComputeExtBasisVectors (A0, B0, 0);

     dimExt2 := numgens source Ext2BasisVect;
     obstruction := toList (dimExt2 : 0_(OY/pIdeal));

     (Ext1, Ext1degs) := ComputeExtBasis (A0, B0, 1);
     
     (OSAmbient, OYxSAmbient) := ConstructAmbientRing (OY, OSigma, Ext1degs, pIdeal);
     (A,B) := FirstDeformation (OYxSAmbient, pIdeal, A0, B0, d, Ext1, Ext2BasisVect);
     
     pIdeal = substitute(pIdeal, OYxSAmbient);

     for i from 2 to o.DegreeLimit do
            (
		 local done;
		 (A,B,obstruction,done) = ExtendDeformation (OYxSAmbient, pIdeal, i, A, B, d, length Ext1, Ext2BasisVect, obstruction);
        	 if done then break);

     OYAmbientxSAmbient := ambient OYxSAmbient;
     OYAmbientxS := (OYAmbientxSAmbient) / ideal (obstruction / (f -> substitute (f, ambient OYAmbientxSAmbient)));
     OS := (OSAmbient) / ideal (obstruction / (f -> substitute (f, OSAmbient)));
     (OS, coker substitute (A, OYAmbientxSAmbient))
)

-- deformMCMModule
--
-- Deform a maximal Cohen-Macaulay module on a hypersurface. Return
-- the resulting matrix factorization and the obstruction.
deformMCMModule(Module) := o -> (M0) -> (
     R := ring M0;
     OSigma := QQ[];
     phi := map (R, OSigma, matrix (R, {{}}));
     return deformMCMModule (M0, phi, o)
)

beginDocumentation()

document {
     Key => ModuleDeformations,
     Headline => "versal deformations of modules on hypersurfaces",
     "Given a regular element ", TEX "$ f $", " in ", TEX "$ R $", ", ", TEX "$ R $",
     " a regular ring, a ", ITALIC "matrix factorisation", " of ", TEX "$ f $",
     " is a pair ", TEX "$ (A,B) $", " of square matrices of the same size ",
     "over ", TEX "$ S $", " such that ", TEX "$ AB = fI = BA $", ", where ", TEX "$ I $",
     " is the identity matrix of the same dimension. ",
     "Matrix factorisations of ", TEX "$ f $", " are equivalent to maximal ",
     "Cohen-Macaulay (MCM) modules on the hypersurface defined by ",
     TEX "$ f $", ": given a matrix factorisation ", TEX "$ (A,B) $", " of ",
     TEX "$f$", ", the cokernel of ", TEX "$ A $", " is an MCM-module over ",
     TEX "$ S/(f) $", ", and all MCM-modules over ", TEX "$ S/(f) $", " arise in this way. ",
     PARA {},
     "Let ", TEX "$ X $", " be a germ of a hypersurface at a point 0 and ", TEX "$ M_0 $",
     " be a maximal Cohen-Macaulay module over ", TEX "$ X $", ". Let ", TEX "$ Y \\rightarrow{} \\Sigma $",
     " a flat map of germs of affine varieties at 0 whose fibre over 0 of ", TEX "$\\Sigma$", " is isomorphic to ",
     TEX "$X$", ". A ", ITALIC "deformation", " of ", TEX "$ M_0 $", " (in the deformation theory defined by the map ",
     TEX "$ Y \\rightarrow{} \\Sigma $", ") is a pair ",
     TEX "$ (S,M) $", " with ", TEX "$ S $", " a germ equipped with a map ",
     TEX "$ S \\rightarrow{} \\Sigma $", " and ", TEX "$ M $", " a module over ", TEX "$ S \\times_\\Sigma Y $",
     " flat over ", TEX "$ S $", ", whose restriction to ", TEX "$ 0 \\times_\\Sigma X $",
     " is isomorphic to ", TEX "$ M_0 $", ".",
     PARA {},
     "A deformation ", TEX "$ (T,M') $", " is ", ITALIC "induced from", TEX "$ (S,M) $",
     " if there exists a map ", TEX "$ \\phi:T \\rightarrow{} S $", " commuting with the maps ",
     TEX "$ T \\rightarrow{} \\Sigma $", " and ", TEX "$ S \\rightarrow{} \\Sigma $", " and such that ",
     TEX "$ M' $", " is isomorphic to ", TEX "$ (\\phi \\times_\\Sigma id_Y)*M $", ".",
     PARA {},
     "The deformation ", TEX "$ (S,M) $", " is ", ITALIC "versal",
     " if any deformation of ", TEX "$ M_0 $", " is induced from ", TEX "$ (S,M) $", ".",
     PARA {},
     "In this package, a germ is represented as the coordinate ring ", TEX "R", " of an affine variety ",
     "and the distinguished point 0 of a germ is assumed to be the ideal generated by the ",
     "variables of ", TEX "R", ". It is an error if the ideal generated by the variables of ",
     TEX "R", " is not prime, e.g. if it is all of ", TEX "R", ".",
     PARA {},
     "This package contains two entry points. One interface may be used only in the ",
     ITALIC "absolute", " case, where ",
     TEX "$ \\Sigma $", " is a reduced point, and the module ", TEX "$ M_0 $",
     " is deformed over ", TEX "$ Y $", " only. The other interface may be used when ",
     TEX "$ \\Sigma $", " is the base space of a nontrivial deformation of ",
     TEX "$ X $", " and ", TEX "$ Y $", " is the total space of the deformation. ",
     "In such a case, ", TEX "$ M_0 $", " is deformed also along the given deformation of ",
     TEX "$ X $", ".",
     PARA {},
     "The implementation has been designed to take advantage of graded ",
     "structure when available. Thus the user is highly recommended to ",
     "assign degrees to the inputs so as to make the rings, ",
     "homomorphism, and deformation module homogeneous when ",
     "possible. Doing so may greatly improve performance."
}
document {
     Key => {deformMCMModule},
     Headline => "versal deformation of MCM-module on hypersurface",
     "This is the main interface for the package ", TO "ModuleDeformations",
     ". It has two signatures: one for the absolute case and one for the general case. Both ",
     "versions return a pair ", TT "(S,M)", "where ", TT "S", " is a ring ",
     "representing the base space of the deformation and ", TT "M", " is a module ",
     "representing the deformation module.",
     PARA {},
     "The module ", TEX "$M$", " is not defined ",
     "over the product ", TEX "$S \\times{} Y$", " but instead over the polynomial ",
     "ring in which ", TEX "$S \\times{} Y$", " is defined. Attempting to construct ",
     TEX "$M$", " over ", TEX "$S \\times{} Y$", " may result in computational ",
     "blowup in certain cases. Reducing ", TEX "$M$", " modulo the defining equations of ",
     TEX "$S \\times{} Y$", " gives the deformation module itself.",     
     PARA {},
     "The function ", TT "deformMCMModule", " returns an error if the given module ",
     TT "M0", " is not maximal Cohen-Macaulay, if it is not defined ",
     "over a hypersurface, or if its module ", TEX "$Ext^1(M0,M0)$", " of ",
     "first-order deformations is not finite-dimensional as a vector ",
     "space. If ", TT "M0", " is free, then ", TT "deformMCMModule",
     " returns a pair with a trivial base space ", TT "S", " and a free ",
     "module ", TT "M", " generated in the same degrees as ", TT "M0",
     ".",
     PARA {},
     "The parameters of the base space, that is, the coordinates over ",
     "which ", TT "S", " is defined, are labelled ", TT "xi_1",
     " through ", TT "xi_d", ", where ", TT "d", " is the dimension as a ",
     "vector space of the module of first-order deformations of the ",
     "module being deformed.",
     PARA {},     
     "The function ", TT "deformMCMModule", " computes a deformation only up to a ",
     "degree limit given by the optional parameter ", TT "DegreeLimit",
     ", whose default value is 10. The procedure is optimized so that ",
     "specifying too large a degree limit should not result in ",
     "substantial computational difficulties in most cases.",
}

document {
     Key => {(deformMCMModule,Module)},
     Headline => "deformation in the absolute case",
     Usage => "deformMCMModule M0",
     Inputs => { "M0" => Module => "the module to be deformed; it must be maximal Cohen-Macaulay" },
     Outputs => {
	  "S" => Ring => {"a ring representing the base space of the versal deformation of ", TT "M0"},
	  "M" => Module => {"a module representing the module of the versal deformation"}
     },
     "This interface to ", TT "deformMCMModule", " computes a ",
     "deformation in the absolute case. Its inputs consist only of the ",
     "module ", TT "M0", " to deform.",
     PARA {},
     "If ", TT "M0", " is free, then ", TT "deformMCMModule", " returns ", TT "(QQ[], M)", ", ",
     "where ", TT "M", " is isomorphic as a graded module to ",
     TT "M0", ".",
     EXAMPLE lines ///
     	 R = QQ[x,y, Degrees => {3,4}]/(x^4-y^3);
	 deformMCMModule R^1
     ///,
     PARA {},
     "The next example computes a versal deformation of the maximal ",
     "ideal of the E6 singularity. The deformation theory of this module ",
     "is equivalent to that of the residue field, whose versal deformation ",
     "is the Hilbert scheme of one point on the singularity. ",     
     EXAMPLE lines ///
     	 M0 = truncate(1,R^1)
         (S,M) = deformMCMModule M0
	 prune S
     ///,
     "The base space of the deformation is therefore isomorphic to the ",
     "singularity itself. The substitution ", TT "x => -xi_2, y => xi_1",
     " shows that this is indeed the case with the result of the computation ",
     "above.",
     PARA {},
     "The resulting module ", TEX "$M$", ", when restricted to the product of ",
     "the singularity with the base space, is isomorphic to the ideal defining the diagonal ",
     "embedding of the singularity.",
     PARA {},
     "The following example demonstrates the performance advantage of ",
     "making use of a graded structure when possible. We deform the ",
     "ideal generated by ", TEX "$ x^2, xy $", " and ", TEX "$ y^2 $",
     " of the ", TEX "$ E_6 $", " curve singularity. In ",
     "the first case, the module is input as a graded module.",
     EXAMPLE lines ///
     	 use R;  
         N0 = module ideal (x^2,y^2)
         (S,N) = time deformMCMModule N0 
	 prune S
     ///,
     "In the second case, we strip the grading from the module and rerun ",
     "the computation.",
     EXAMPLE lines ///
         N0' = coker matrix entries presentation N0
         (S',N') = time deformMCMModule N0'
	 prune S'
     ///,
     "On the author's computer, the second computation takes approximately ",
     "of 10% more processor time as the first. The user may see by ",
     "inspection that the outputs of the two computations are isomorphic. ",
     "More complicated examples, such as the square of the maximal ideal, ",
     "show a much greater difference."
}

document {
     Key => {(deformMCMModule,Module,RingMap)},
     Headline => "deformation in the relative case",
     Usage => "(S,M) = deformMCMModule(M0,phi)",
     Inputs => {
         "M0" => Module => {"the module to be deformed"}, 
         "phi" => RingMap => {"a ring homomorphism representing the map ", TEX "$\\phi : Y \\rightarrow{} \\Sigma$"} },
     Outputs => {
	  "S" => Ring => {"a ring representing the base space of the versal deformation of ", TT "M0"},
	  "M" => Module => {"a module representing the module of the versal deformation"}
	  },
     "This interface to ", TT "deformMCMModule", " constructs a versal ",
     "deformation of ", TT "M0", " in the general case. That is, there is ",
     "given a map of varieties ", TEX "$\\phi:Y \\rightarrow{}  \\Sigma$", " and a module ", TT "M0", " defined ",
     "over the fibre over 0 of ", TEX "$\\phi$", ". A versal deformation of ", TT "M0",
     " is then constructed in the deformation theory so defined. Thus its ",
     "inputs consist of ", TT "M0", " and a ring homomorphism ",
     TEX "$\\phi^*$", " representing the map ", TEX "$\\phi:Y \\rightarrow{}  \\Sigma$", ". The procedure ",
     "requires that ", TT "M0", " be an MCM module over the ring ",
     TEX "\\mathcal{O}_Y/\\phi^*(m_\\Sigma)", ", where ", TEX "m_\\Sigma",
     " is the ideal generated by the variables of ", TEX "\\mathcal{O}_\\Sigma", ". Otherwise an ",
     "error will result.",
     PARA {},
     "If ", TT "M0", " is free, then ", TT "deformMCMModule", " returns ", TT "(S,M)", ", ",
     "where ", TT "S", " is the source of ", TT "phi", " and ", 
     TT "M", " is a free module over the ambient ring of ", TT "Y", ", ",
     "generated in the same degrees as ", TT "M0", ".",
     PARA {},
     EXAMPLE lines ///
	 OSigma = QQ[x, Degrees=>{2}];
         OY = QQ[y,z,x, Degrees=>{2,3,2}]/(z^2-(y-x)*y^2);
	 phi = map(OY,OSigma, {x})
     	 use OSigma
         OX = trim (OY/phi(ideal x))
         (OS,M) = deformMCMModule(module ideal (y,z),phi)
	 prune OS
     ///,
     "The above example deforms the maximal ideal of the A2 singularity ",
     "onto the Whitney umbrella ", TEX "$Y$", " (whose ring is ", TT "OY",
     " above, of which the former is a hyperplane section given by the ",
     "fibre over 0 of the map ", TEX "$Y \\rightarrow{} \\Sigma$",
     " defined by ", TEX "$\\phi$", " above. The resulting base space ", TT "S",
     " (whose ring ", TT "OS", " is part of the output of ", TT "deformMCMModule", ") is the ",
     "Hilbert scheme of one point on the Whitney umbrella, which is ",
     "isomorphic to ", TEX "$Y$", ". The substitution ",
     TT "y => -xi_2, z => xi_1", " shows this isomorphism.",
     PARA {},
     "The resulting module ", TEX "$M$", ", when restricted to the fibre product ",
     "of ", TEX "$Y$", " and ", TEX "$S$", " over ", TEX "$\\Sigma$", ", is isomorphic to ",
     "the ideal defining the diagonal embedding of ", TEX "$Y$", "."
}

document {
     Key => [deformMCMModule, DegreeLimit],
     Headline => "Compute only up to this exponent",
     "This parameter is an integer ", TEX "$ n $", " such that in the result of the computation ",
     "the versality criterion holds only after reducing the relevant rings modulo ",
     "the maximal homogeneous ideal to the power ", TEX "$ n+1 $", ". Concretely, this means ",
     "that the output may have incorrect terms with total exponent greater than ", TEX "$ n $",
     ". If the input is positively ",
     "graded, all deformation parameters have strictly positive degree, and ",
     TEX "$ n $", " is sufficently large, then no terms of degree greater than ",
     TEX "$ n $", " appear in the output, and the result is versal. In this case, the maximum ",
     "of ", TEX "$ d / deg x $", " where ", TEX "$ d $", " is the degree of the equation of the ",
     "hypersurface and ", TEX "$ x $", " ranges over all variables in the ring of the hypersurface and ",
     "all deformation parameters, should suffice."
     }

document {
     Key => {xi},
     Headline => "Indexed variable of deformation parameters"
     }

TEST ///
R=QQ[x,y,Degrees=>{2,3}]/(x^3-y^2);
(S,M)=deformMCMModule(module ideal (x,y));
RS=ring M
RSp=ring M / ideal (RS_0 + RS_3, RS_1 - RS_2)
ideal R
ideal S
assert (substitute (ideal R, RSp) == substitute (ideal S, RSp))
I=ideal (mingens substitute (ideal R, RS) + mingens substitute (ideal S, RS))
assert (I == ideal det presentation M)
///

TEST ///
OSigma=QQ[x,Degrees=>{2}]
OY=QQ[y,z,x,Degrees=>{2,3,2}]/(z^2-(y-x)*y^2)
phi=map(OY,OSigma,matrix({{x}}))
use OSigma 
OX=trim (OY/phi(ideal x))
(S,M)=deformMCMModule(module ideal (y,z),phi)
RS=ring M
RSp=ring M / ideal (RS_0 + RS_4, RS_1 - RS_3)
assert (substitute (ideal OY, RSp) == substitute (ideal S, RSp))
I=ideal (mingens substitute (ideal OY, RS) + mingens substitute (ideal S, RS))
assert (I == ideal det presentation M)
///

TEST ///
R=QQ[x,y,Degrees=>{3,4}]/(x^4-y^3);
(S,M)=deformMCMModule(module ideal (x^2,y));
assert (isHomogeneous S)
assert (isHomogeneous M)
assert (numgens trim ideal S == 2)
assert (codim ideal S == 2)
assert (codim trim minors (2, jacobian S) == 1)
///


end
--------------------------------------------------------------------------------
restart
uninstallPackage "ModuleDeformations"
installPackage "ModuleDeformations"
check "ModuleDeformations"

needsPackage "ModuleDeformations"
R = QQ[x,y,Degrees=>{3,4}]/(x^4-y^3)
deformMCMModule(module ideal(x^2,y))
x


R = QQ[x,y, Degrees => {3,4}]/(x^4-y^3);
deformMCMModule R^1
M0 = truncate(1,R^1)
(S,M) = deformMCMModule M0
prune S
use R;  
N0 = module ideal (x^2,y^2)
(S,N) = time deformMCMModule N0 


restart
needsPackage "ModuleDeformations";
R = QQ[x,y, Degrees => {3,4}] / (x^4-y^3);
deformMCMModule(module ideal(x^2,y));
xi
xi_1
x
use o3_0
xi
xi_1

restart
needsPackage "ModuleDeformations";
R = QQ[x,y, Degrees => {3,4}] / (x^4-y^3);
(S,M) = deformMCMModule(module ideal(x^2,y));
xi
xi_1
x




--------------------------------------------------------------------------------
restart
needsPackage "ModuleDeformations";
R = QQ[x,y, Degrees => {3,4}] / (x^4-y^3);
(OS,M) = deformMCMModule module ideal (x,y);
describe OS
M
presentation module ideal (x,y)


OSigma = QQ[x, Degrees => {2}];
OY = QQ[y,z,x, Degrees => {2,3,2}] / (z^2-(y-x)*y^2);
phi = map(OY, OSigma, matrix {{x}});
OX = trim (OY / phi ideal OSigma_0);
(S,M) = deformMCMModule(module ideal (y,z), phi);
describe S
M

OSigma = QQ[a_2, Degrees => {2}];
R = QQ[a_2,a_3,a_4, Degrees => {2,3,4}] / 
  ideal(a_4^3-a_3^4+6*a_2*a_3^2*a_4-6*a_2^2*a_4^2-2*a_2^3*a_3^2+9*a_2^4*a_4);
phi = map(R,OSigma, matrix {{a_2}});
use OSigma
Rtilde = R / phi ideal OSigma_0;
(S,M) = deformMCMModule(module (ideal (a_3,a_4))^2, phi);
ambient S
degrees S
dim S
numgens trim ideal S
