newPackage ("Tutorials",
    Version => version#"VERSION",
    Headline => "tutorials and teaching materials for Macaulay2",
    HomePage => "https://macaulay2.com/",
    Authors => { -* see the contributors listed on the main page *- },
    Keywords => { "Documentation" },
    InfoDirSection => "Macaulay2 and its packages",
    AuxiliaryFiles => true
)

beginDocumentation()

doc ///
Node
  Key
    "Tutorials"
  Headline
    curated tutorials and teaching materials for Macaulay2
  Description
    Text
      This package collects Macaulay2's longer guided tutorials and teaching
      materials in one place.
    Text
      Use these pages when you want a sustained walkthrough rather than a
      reference entry for a single command or package. The @TO "Macaulay2Doc::Macaulay2"@
      front page and the subject-area pages in @TO "Macaulay2Doc::commutative algebra"@
      and @TO "Macaulay2Doc::algebraic geometry"@ also link to the individual tutorials
      most relevant to each area.
    Tree
      :Foundational tutorials
        > "Tutorial: Modules in Macaulay2"
        > "Tutorial: Elementary uses of Gröbner bases"
        > "Teaching Materials"
      :Geometry tutorials
        > "Tutorial: Divisors"
        > "Tutorial: Canonical Embeddings of Plane Curves and Gonality"
        > "Tutorial: Fano varieties"
      :Numerical tutorial
        > "Tutorial: Numerical algebraic geometry"
///

ld := fn -> tutorial get (currentFileDirectory|"Tutorials/"|fn)

document {
     Key => "Tutorial: Modules in Macaulay2",
     ld "tu_modules.m2"
     }
document {
     Key => "Tutorial: Elementary uses of Gröbner bases",
     ld "tu_elementary.m2"
     }
document {
     Key => "Tutorial: Canonical Embeddings of Plane Curves and Gonality",
     ld "tu_canEmbed.m2"
     }
document {
     Key => "Tutorial: Divisors",
     ld "tu_divisors.m2"
     }
document {
     Key => {"Elementary uses of Groebner bases I", "Elementary uses of Groebner bases I. Math 634 Fall 2005"},
     ld "tu_elementary1.m2"
     }
document {
     Key => "Tutorial: Fano varieties",
     ld "tu_Fano.m2"
     }
load "./Tutorials/tu_NAG.m2"
load "./Macaulay2Doc/M2-Singular-Book.m2"
load "./Tutorials/teaching_materials.m2"

end

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Tutorials RemakeAllDocumentation=true IgnoreExampleErrors=false"
-- End:
