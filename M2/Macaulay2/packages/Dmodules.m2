-- -*- coding: utf-8 -*-
newPackage("Dmodules",
    Version => "1.4.1.1",
    Date => "February 2023",
    Headline => "D-modules",
    HomePage => "http://people.math.gatech.edu/~aleykin3/Dmodules",
    Authors => {
	{ Name => "Anton Leykin", Email => "leykin@math.gatech.edu" },
	{ Name => "Harrison Tsai" }
	},
    Keywords => { "D-modules" },
    PackageExports => {
	"WeylAlgebras",
	"HolonomicSystems",
	"BernsteinSato",
	}
    )

beginDocumentation()

doc ///
Node
  Key
    Dmodules
  Headline
    D-modules package collection
  Description
    Text
      To begin, read the @TO "D-modules tutorial"@.
    Tree
      :Packages included in this collection
	@TOH WeylAlgebras@
	@TOH HolonomicSystems@
	@TOH BernsteinSato@
  Subnodes
    "D-modules tutorial"
  
  References
    See the bibliography at @TO "Works Cited"@.
  

Node
  Key
    "D-modules tutorial"
  Headline
    Algebraic computations for linear differential equations
  Description
    Text
     D-modules are modules over rings of differential operators over algebraic varieties.
     This package is mostly concerned with computations in the {\em Weyl algebra},
     the ring of differential operators over affine space (over a field of characteristic zero).
     Most algorithms in this package can be found in the book
     {\em Gröbner deformations of Hypergeometric Differential Equations} by Saito, Sturmfels and Takayama,
     hereafter referred to as [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@].
     This is also the best place to learn about computational D-module theory. The book
     {\em Computational Algebraic Geometry with Macaulay2} has a chapter on D-modules and local cohomology.
     A good introduction to D-module theory is {\em A primer of algebraic D-modules} by Coutinho.

     The Weyl algebra $D_n$ is the free associative algebra in $2n$ variables
     $x_1,\dots,x_n$, $\partial_1,\dots,\partial_n$, subject to the following relations:
     the $x$'s commute with each other; the $\partial$'s commute with each other; $x_i$ commutes with $\partial_j$ if $i\neq j$;
     and finally, $\partial_i x_i = x_i \partial_i +1$ (the Leibniz rule).


    Example
     D1 = QQ[z,dz, WeylAlgebra=>{z=>dz}]

    Text
     As a reality check, let us confirm that this is not a commutative ring. Here is the Leibniz rule.

    Example
     dz*z

    Text
     In order to type less, we can use the shortcuts makeWeylAlgebra or makeWA.

    Example
     R = QQ[x_1..x_4]
     D4 = makeWA R
     describe D4

    Text
     Elements and ideals are handled in the usual Macaulay2 way. Let us look at the
     @HREF("https://en.wikipedia.org/wiki/Hypergeometric_function#The_hypergeometric_differential_equation","Gauss hypergeometric equation")@
     for parameters $a=1, b=2, c=3$.

    Example
     use D1
     a = 1, b = 2, c = 3
     g = z*(1-z)*dz^2 + (c-(a+b+1)*z)*dz -a*b
     I = ideal g

    Text
     The holonomicRank function computes the dimension of the solution space of a linear system of differential equations.

    Example
     holonomicRank I

    Text
     A-Hypergeometric systems, also known as GKZ systems (see [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Chapters 3 and 4]) are implemented.

    Example
     needsPackage "HolonomicSystems"
     use D4
     A = matrix{{1,1,1,1},{0,1,3,4}}
     b = {1,2}
     H = gkz(A,b, D4)

    Text
     Holonomic D-ideals are analogous to zero-dimensional ideals in polynomial rings (see [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Section 1.4]).
     Let us confirm that our GKZ systems are holonomic.

    Example
     isHolonomic H

    Text
     Once we know our ideal is holonomic, we can compute its holonomic rank. The above is a famous
     GKZ example because the holonomic rank may change when the parameter vector $b$ is changed.

    Example
     holonomicRank H
     holonomicRank sub(gkz(A,{1,0}), vars D4)

    Text
     We can compute the characteristic ideal and singular locus of a D-ideal [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Section 1.4]. Note that the output of charIdeal
     belongs to a commutative ring, the associated graded ring of $D_n$ with respect to the order filtration.

    Example
     charIdeal H
     singLocus H

    Text
     The singular locus of a GKZ system is the zero set of a polynomial called the Principal A-determinant,
     which is a product of discriminants corresponding to faces of the matrix A
     (see Chapters 8 and 9 of the book {\em Discriminants, Resultants and Multidimensional Determinants}
     by Gelfand, Kapranov and Zelevinsky). Here is how to find the classic cubic discriminant.

    Example
     A1 = matrix{{1,1,1,1},{0,1,2,3}}, b1={0,0}
     H1 = sub(gkz(A1,b1),vars D4)
     factor (singLocus H1)_0

  SeeAlso
   Dmodules
///



--references


doc ///
  Key
    "Works Cited"
  Headline
    bibliography for the D-modules packages
  Description
    Text
      This node collects the references used throughout the packages BernsteinSato, WeylAlgebras, HolonomicSystems.

  References
      [BL10] Berkesch, Leykin, Algorithms for Bernstein-Sato polynomials and
      multiplier ideals, ISSAC 2010 - Proceedings of the 2010 International Symposium on
      Symbolic and Algebraic Computation, 2010.
  
      [CJT01] Castro-Jim\'{e}nez, Takayama, The computation of the logarithmic cohomology
      for plane curves, J. Algebra 11, vol 322, 2009.

      [Ley04] Leykin, Algorithmic proofs of two theorems of Stafford,
      Journal of Symbolic Computation, 38(6):1535-1550, 2004.
  
      [OT99] Oaku, Takayama, An algorithm for de Rham cohomology groups
      of the complement of an affine variety via D-module computation,
      JPAA (139), 1999.
      
      [OT01] Oaku, Takayama, Algorithms for D-modules
      - restriction, tensor product, localization, and local cohomology groups,
      JPAA (156), 2001.

      [OTT01] Oaku, Takayama, Tsai, Polynomial and rational solutions of a holonomic system,
      JPAA (164), 2001.
  
      [OTW00] Oaku, Takayama, Walther, A Localization Algorithm for D-modules,
      J. Symbolic Computation (29), 2000.

      [SST00] Saito, Sturmfels, Takayama, Groebner deformations of hypergeometric differential
      equations, Algorithms and Computation in Mathematics vol. 6, Springer-Verlag, Berlin, 2000.

      [TW01] Tsai, Walther, Computing homomorphisms between holonomic D-modules,
      Journal of Symbolic Computation 6, vol. 32, 2001.
      
      [Wal99] Walther, Algorithmic computation of local cohomology modules and
      the local cohomological dimension of algebraic varieties, JPAA (139), 1999.    
///



end--

uninstallAllPackages()
restart
installPackage "WeylAlgebras"
installPackage "HolonomicSystems"
installPackage "BernsteinSato"

restart
uninstallPackage "Dmodules"
installPackage "Dmodules"
viewHelp Dmodules

restart
needsPackage "Dmodules"
check Dmodules
