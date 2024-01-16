-- Copyright 1999-2009 by Anton Leykin and Harrison Tsai

doc ///
Node
  Key
    WeylAlgebras
  Headline
    algorithms for D-modules
  Description
    Text
      To begin, read the @TO "Dmodules::D-modules tutorial"@.
    Tree
      :How to make Weyl algebras
	@TOH [monoid, WeylAlgebra]@
	@TOH "makeWeylAlgebra"@
      :Basic commands
	@TOH "gbw"@
	@TOH "inw"@
	@TOH "Fourier"@
	@TOH "Dtransposition"@
	@TOH "stafford"@
	@TOH "makeCyclic"@
	@TOH "Dprune"@
      :Basic invariants of D-modules
	@TOH "Ddim"@
	@TOH "isHolonomic"@
	@TOH "holonomicRank"@
	@TOH "characteristicIdeal"@
	@TOH "DsingularLocus"@
      :Programming aids
	@TOH "createDpairs"@
	@TOH "extractDiffsAlgebra"@
	@TOH "extractVarsAlgebra"@
	--@TOH "createThetaRing"@
	@TOH "Dtrace"@
  Subnodes
    "makeWeylAlgebra"

    "gbw"
    "inw"
    "Fourier"
    "Dtransposition"
    "stafford"
    "makeCyclic"
    "Dprune"

    "Ddim"
    "isHolonomic"
    "holonomicRank"
    "characteristicIdeal"
    "DsingularLocus"

    "Dtrace"
///

-*
-- FIXME: this is excluded because the Macaulay2Doc package owns the WeylAlgebra key
document {
     Key => WeylAlgebra,
     TT "WeylAlgebra", " --
     name for an optional argument for a monoid that
     specifies that a PolynomialRing created from it will
     be a Weyl Algebra.",

     PARA{},
     "The n-th Weyl algebra is the associative ring on 2n variables,
     e.g., K<x_1..x_n, D_1..D_n>, where all the variables commute except
     for (D_i x_i = x_i D_i + 1).  It can be viewed as the ring
     of algebraic differential operators on affine space K^n.",

     PARA{},
     "A simple example:",
     EXAMPLE {
	"W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]",
	"x*Dx",
	"Dx*x"},
     PARA{},
     "Caveats and known problems:",
     UL{"The variables can be called by any name, but for each
	  pair such as x => Dx, the commutative variable (in this case x)
	  must be listed before the derivation variable (in this case Dx)"}
     }
*-

-----------------------------------------------

document {
    Key => {Dtrace, (Dtrace, ZZ), (Dtrace, Sequence)},
    Headline => "set or get the depth of comments made by D-module routines",
    Usage => "Dtrace n\nDtrace()",
    Inputs => { "n" => ZZ => { "new level" } },
    Outputs => { ZZ => { "old level" } }
    }

end
------------------------------------------------------------------------------------------------------------
THE END
restart
loadPackage "WeylAlgebras"
uninstallPackage "WeylAlgebras"
installPackage("WeylAlgebras")
installPackage("WeylAlgebras", SeparateExec=>true, RerunExamples=>true)
check WeylAlgebras
