-*
-- TODO
(binomial,RingElement,ZZ)
(rowRankProfile,MutableMatrix)
(columnRankProfile,MutableMatrix)
(nullSpace,MutableMatrix)
*-

load "./doc.m2"
load "./doc3.m2"
load "./doc5.m2"
load "./doc7.m2"
load "./doc8.m2"
load "./doc9.m2" -- ChainComplex methods, to be replaced
load "./doc10.m2"
load "./doc12.m2"
load "./doc14.m2"

load "./changes.m2"

load "./overviewA.m2"
load "./overviewC.m2"
load "./overview2.m2"
load "./overview3.m2"
load "./overview4.m2"
load "./macaulay.m2"
load "./overview_packages.m2"
load "./overview_doc.m2"
load "./doc_ideals.m2"
load "./overview_modules.m2"
load "./doc_module.m2"
load "./doc_mutablematrices.m2"
load "./doc_ringmaps.m2"
load "./doc_assignment.m2"
load "./doc_caching.m2"

load "./ov_top.m2"
load "./ov_getting_started.m2"
load "./ov_ideals.m2"
load "./ov_ringmaps.m2"
load "./ov_matrices.m2"
load "./ov_language.m2"
load "./threads.m2"
load "./system.m2"
load "./repl.m2"
load "./debugging.m2"
load "./experimental.m2"

load "./doc_arithmetic.m2"
load "./doc_intervals.m2"
load "./doc_rings.m2"
load "./doc_ringelement.m2"
load "./doc_lists.m2"
load "./doc_strings.m2"
load "./doc_tables.m2"
load "./operators.m2"
load "./functions.m2"
load "./shared.m2"
load "./types.m2"
load "./methods.m2"
load "./doc_iterators.m2"
load "./doc_augmented_assignment.m2"
load "./doc_atomic.m2"
load "./options.m2" -- this must come last
load "./undocumented.m2"

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
