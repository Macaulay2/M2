
-- should rewrite this to load all the m2 files there
-- perhaps write a "pathFind" routine so we can find files such as Macaulay2 on the path.

load "./doc.m2"
load "./doc1.m2"
load "./doc2.m2"
load "./doc3.m2"
load "./doc4.m2"
load "./doc5.m2"
load "./doc6.m2"
load "./doc7.m2"
load "./doc8.m2"
load "./doc9.m2"
load "./doc10.m2"
load "./doc11.m2"
load "./doc12.m2"
load "./doc13.m2"
load "./doc14.m2"

load "./changes.m2"

load "./overviewA.m2"
load "./overviewB.m2"
load "./overviewC.m2"
load "./overview2.m2"
load "./overview3.m2"
load "./overview4.m2"
load "./macaulay.m2"
load "./overview_packages.m2"
load "./overview_doc.m2"
load "./doc_ideals.m2"
load "./overview_modules.m2"
load "./doc_mutablematrices.m2"
load "./doc_ringmaps.m2"
load "./doc_assignment.m2"

load "./ov_top.m2"
load "./ov_getting_started.m2"
load "./ov_ideals.m2"
load "./ov_ringmaps.m2"
load "./ov_matrices.m2"
load "./ov_language.m2"
load "./ov_files.m2"
load "./threads.m2"

load "./doc_lists.m2"
load "./doc_strings.m2"
load "./doc_tables.m2"
load "./operators/dotdot.m2"
load "./functions.m2"
load "./variables.m2"
load "./shared.m2"

load "./M2-Singular-Book.m2"

ld := fn -> tutorial get concatenate(currentFileDirectory,fn)

document {
     Key => "modules in Macaulay2",
     ld "tu_modules.m2"
     }
document {
     Key => "Tutorial: Elementary uses of Groebner bases",
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
     Key => "Elementary uses of Groebner bases I. Math 634 Fall 2005",
     ld "tu_elementary1.m2"
     }
document {
     Key => "Tutorial: Fano varieties",
     ld "tu_Fano.m2"
     }
