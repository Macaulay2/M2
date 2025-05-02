-*
-- TODO
(binomial,RingElement,ZZ)
(rowRankProfile,MutableMatrix)
(columnRankProfile,MutableMatrix)
(nullSpace,MutableMatrix)
*-

-- Each section is loaded in order of
-- appearance in the table of contents
load "./ov_top.m2"
load "./ov_preface.m2"
load "./ov_getting_started.m2"
load "./ov_first_session.m2"
load "./ov_editors_emacs.m2"
load "./ov_rings.m2"
load "./ov_monomial_orderings.m2"
load "./ov_ringmaps.m2"
load "./ov_ideals.m2"
load "./ov_matrices.m2"
load "./ov_modules.m2"
load "./ov_examples.m2"
load "./ov_groebner_bases.m2"
load "./ov_analytic_functions.m2"
load "./ov_language.m2"
load "./ov_strings.m2"
load "./ov_lists.m2"
load "./ov_hashtables.m2"
load "./ov_types.m2"
load "./ov_methods.m2"
load "./ov_caching.m2"
load "./ov_debugging.m2"
load "./ov_files.m2"
load "./ov_packages.m2"
load "./ov_documentation.m2"
load "./ov_threads.m2"
load "./ov_system.m2"
load "./ov_repl.m2"
load "./ov_developers.m2"
load "./ov_parallelism.m2"
load "./changes.m2"
load "./macaulay.m2"

load "./doc3.m2"
load "./doc7.m2"
load "./doc12.m2"
load "./doc14.m2"

load "./doc_rings.m2"
load "./doc_ideals.m2"
load "./doc_matrices.m2"
load "./doc_mutablematrices.m2"
load "./doc_module.m2"

load "./experimental.m2"

load "./doc_arithmetic.m2"
load "./doc_intervals.m2"
load "./doc_ringelement.m2"
load "./functions.m2"
load "./operators.m2"
load "./shared.m2"
load "./doc_iterators.m2"
load "./doc_atomic.m2"
load "./options.m2" -- this must come last
load "./undocumented.m2"

load "./M2-Singular-Book.m2"

ld := fn -> tutorial get concatenate(currentFileDirectory,fn)

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
     Key => "Elementary uses of Groebner bases I. Math 634 Fall 2005",
     ld "tu_elementary1.m2"
     }
document {
     Key => "Tutorial: Fano varieties",
     ld "tu_Fano.m2"
     }
