doc ///
Node
  Key
    BernsteinSato
  Headline
    algorithms for $b$-functions, local cohomology, and intersection cohomology
  Description
    Text
      Algorithms surrounding the Bernstein-Sato polynomial of a polynomial
      $f$, with applications to $b$-functions, $D$-module restriction and
      integration, local cohomology, intersection cohomology, and multiplier
      ideals.
    Tree
      :B-functions
	@TOH "bFunction"@
	@TOH "generalB"@
	@TOH "globalB"@
	@TOH "globalBFunction"@
	@TOH "globalBoperator"@
	@TOH "localBFunction"@
	@TOH "factorBFunction"@
	@TOH "bFunctionRoots"@
	@TOH "getIntRoots"@
	@TOH "paramBpoly"@
	@TOH "AnnFs"@
	@TOH "AnnIFs"@
	@TOH "polynomialAnnihilator"@
	@TOH "rationalFunctionAnnihilator"@

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

      :Applications
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

  References
    See the bibliography at @TO "WeylAlgebras :: Works Cited"@.
///

end--

restart
uninstallPackage "BernsteinSato"
installPackage "BernsteinSato"
