newPackage( Dmodules, Version => "1.0")
export (ExternalProduct,TwistMap,twistMap,twistInvMap,projMap1,projMap2,bFunction,NonGeneric,TryGeneric,IntRing,
        globalBFunction,ViaAnnFs,ReducedB,factorBFunction,getIntRoots,Boperator,Bpolynomial,globalBoperator,
	AnnFs,AnnIFs,Dtrace,getDtrace,setHomSwitch,getHomSwitch,localCohom,Walther,OaTa,LocStrategy,
	OaTaWa,pruneLocalCohom,paramBpoly,GroundField,makeCyclic,Generator,AnnG,isHolonomic,DHom,DExt,Special,
	None,Output,Info,PolySols,Alg,GD,Duality,PolySols,PolyExt,RatSols,RatExt,createDpairs,dpairInds,
	dpairVars,Fourier,Dtransposition,singLocus,charIdeal,Drank,Ddim,makeWeylAlgebra,Ddual,Dlocalize,
	Oaku,OTW,Dlocalization,DlocalizationAll,DlocalizeMap,LocModule,GeneratorPower,LocMap,annFS,
	DlocalizeAll,IntegrateBfunction,Bfunction,DlocalizationMap,Dresolution,Schreyer,Vhomogenize,
	Drestriction,Drestrict,DrestrictionClasses,DrestrictClasses,DrestrictIdeal,DrestrictAll,
	DrestrictionComplex,DrestrictionAll,DrestrictionIdeal,DrestrictComplex,HomologyModules,
	GenCycles,Exponents,Cycles,Boundaries,VResolution,Explicit,IntegrateComplex,Dintegration,
	Dintegrate,DintegrateIdeal,DintegrationIdeal,DintegrationComplex,DintegrateClasses,DintegrateComplex,
	DintegrationClasses,DintegrateAll,DintegrationAll,gkz,Vars,Local,Global,AppellF1,PolyAnn,
	RatAnn,WeylClosure,deRham,deRhamAll,TransferCycles,CohomologyGroups,PreCycles,OmegaRes,
	diffOps,PolyGens,BasisElts,putWeylAlgebra,inw,gbw,pInfo,SparseMutableMatrix,sparseMutableMatrix,
	newSparseMatrix,reduceCompress,getEntry,iden,sparsemat,numcols,numrows,setEntry,reducePivots,
	Dprune,Dprune2,optGB,FourierInverse)

needs "Dmodules/Dloadfile.m2"
needs "Dmodules/DMODdoc.m2"

-- erase the internal symbols
scan({"createCommAlgebra", "CommAlgebra", "createAssCommRing", "ThetaRing",
     "createThetaRing", "createIntRing", "createHomWeylAlgebra", "zeroize",
     "DBIGPRIME", "computeLocalization", "invPermute", "gbW1",
     "gbW2", "inW1", "inW2", "WAtoCR", "WAtoCA", "CAtoWA",
     "AnnIFs2", "kerGB", "kerGBstatus", "ResToOrigRing", "CRtoWA",
     "isGeneric", "IRtoR", "CommRing", "RtoIR", "WtoT", "HWAtoWA",
     "WAtoHWA", "HomWeylAlgebra", "RestrictComplex", "BFunction",
     "LocalizeMap", "diagonal", "computeRestriction", "TwistOperator",
     "divideOutGCD"},
  s -> (
       if not Dmodules.Dictionary#?s then error("expected ",s," to be a symbol first defined by the Dmodules package");
       erase Dmodules.Dictionary#s))
