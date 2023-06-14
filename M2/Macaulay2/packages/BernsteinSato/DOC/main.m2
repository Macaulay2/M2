doc ///
Node
  Key
    BernsteinSato
  Headline
    algorithms for b-functions, local cohomology, and intersection cohomology
  Description
    Tree
      :B-functions
	@TOH "bFunction"@
	@TOH "generalB"@
	@TOH "globalB"@
	-- TODO: capital F but lowercase o?
	@TOH "globalBFunction"@
	@TOH "globalBoperator"@
	@TOH "localBFunction"@
	@TOH "factorBFunction"@
	@TOH "bFunctionRoots"@
	@TOH "getIntRoots"@
	@TOH "paramBpoly"@
	@TOH "AnnFs"@
	@TOH "AnnIFs"@

      :Resolutions and Functors
	@TOH "Dresolution"@
	@TOH "Dlocalize"@
	@TOH "WeylClosure"@
	@TOH "Ddual"@
	@TOH "Drestriction"@
	@TOH "Dintegration"@
	@TOH "DHom"@
	@TOH "DExt"@
	@TOH "PolyExt"@
	@TOH "RatExt"@

      :Applications:
	@TOH "localCohom"@
	@TOH "deRham"@
	@TOH "PolySols"@
	@TOH "RatSols"@
	@TOH "populateCechComplexCC"@
	@TOH "pruneCechComplexCC"@
	@TOH "logCohomology"@
	:Multiplier Ideals
	  @TOH "lct"@
	  @TOH "multiplierIdeal"@
	  @TOH "isInMultiplierIdeal"@
	  @TOH "jumpingCoefficients"@
	  @TOH "hasRationalSing"@
  Subnodes
///

end--

restart
uninstallPackage "BernsteinSato"
installPackage "BernsteinSato"
