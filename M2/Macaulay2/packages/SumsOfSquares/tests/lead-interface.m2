-- The outer interface of SumsOfSquares, as pinned in bench/interface-original.txt
-- (sections 1, 1b, 2, 2b, 6): exported symbols with their classes, option
-- defaults and methods; the types; the methods on the types; the keys of
-- SOSPoly and SDPResult; the symbols that the PackageExports chain makes
-- visible.  An interface change must show up here (AGENTS.md: the outer
-- interface stays; extensions only after consultation).

-- 1. exported symbols
expectedSymbols = sort {"checkSolver", "GramMatrix", "library", "lowerBound", "MomentMatrix",
    "recoverSolution", "RoundTol", "SDPResult", "solveSOS", "sosdecTernary", "sosInIdeal",
    "SOSPoly", "sosPoly", "Status", "TraceObj"};
assert(sort apply(SumsOfSquares#"exported symbols", toString) == expectedSymbols)
assert(SumsOfSquares#"exported mutable symbols" == {})

for s in {GramMatrix, MomentMatrix, Status, RoundTol, TraceObj} do assert instance(s, Symbol)
assert(instance(SOSPoly, Type) and parent SOSPoly === HashTable)
assert(instance(SDPResult, Type) and parent SDPResult === HashTable)

-- options of the exported functions, with their defaults
assert(options solveSOS === new OptionTable from {RoundTol => 3, Solver => null, TraceObj => false, Verbosity => 0})
assert(options lowerBound === new OptionTable from {RoundTol => 3, Solver => null, Verbosity => 0})
assert(options sosInIdeal === new OptionTable from {RoundTol => 3, Solver => "CSDP", Verbosity => 0})
assert(options sosdecTernary === new OptionTable from {RoundTol => 3, Solver => "CSDP", Verbosity => 0})
assert(options checkSolver === new OptionTable from {Verbosity => 0})
for f in {solveSOS, lowerBound, sosInIdeal, sosdecTernary, checkSolver} do
    assert instance(f, MethodFunctionWithOptions)
for f in {sosPoly, recoverSolution, library} do assert instance(f, MethodFunction)

-- the methods of the exported functions (argument types)
signatures = f -> set apply(toList methods f, m -> drop(toSequence m, 1))
sig = L -> toSequence L
assert(signatures solveSOS === set {sig{RingElement}, sig{RingElement,RingElement}, sig{RingElement,Matrix},
        sig{RingElement,RingElement,Matrix}, sig{RingElement,ZZ}, sig{RingElement,RingElement,ZZ}})
assert(signatures lowerBound === set {sig{RingElement}, sig{RingElement,ZZ}, sig{RingElement,Matrix,ZZ}})
assert(signatures sosInIdeal === set {sig{Matrix,ZZ}, sig{Ring,ZZ}})
assert(signatures sosdecTernary === set {sig{RingElement}})
assert(signatures checkSolver === set {sig{String}, sig{String,String}, sig{String,Function}})
assert(signatures sosPoly === set {sig{Ring,List,List}, sig{List,List}, sig{Matrix,Matrix}, sig{SDPResult}})
assert(signatures recoverSolution === set {sig{Matrix,Matrix}, sig{SDPResult}})
assert(signatures library === set {sig{String,List}, sig{String,Ring}})

-- 1b. the dependencies
assert(SumsOfSquares.Options.PackageExports == {"SemidefiniteProgramming"})
assert(SumsOfSquares.Options.PackageImports == {"FourierMotzkin"})
assert(SumsOfSquares.Options.Certification =!= null)

-- 2b. the 22 methods on the two types
expectedTypeMethods = sort {"(*,Number,SOSPoly)", "(*,SOSPoly,SOSPoly)", "(+,SOSPoly,SOSPoly)",
    "(==,Matrix,SOSPoly)", "(==,RingElement,SOSPoly)", "(==,SOSPoly,Matrix)",
    "(==,SOSPoly,RingElement)", "(==,SOSPoly,SOSPoly)", "(^,SOSPoly,ZZ)", "(clean,RR,SOSPoly)",
    "(coefficients,SOSPoly)", "(expression,SOSPoly)", "(generators,SOSPoly)", "(length,SOSPoly)",
    "(net,SDPResult)", "(net,SOSPoly)", "(recoverSolution,SDPResult)", "(ring,SOSPoly)",
    "(sosPoly,SDPResult)", "(status,SDPResult)", "(substitute,SOSPoly,Ring)", "(value,SOSPoly)"};
typeMethods = sort unique apply(toList methods SOSPoly | toList methods SDPResult, toString)
assert(typeMethods == expectedTypeMethods)

-- 6. keys of the two types
R = QQ[x,y];
s = sosPoly(R, {x+1,y}, {2,3});
assert(class s === SOSPoly)
assert(set keys s === set {ring, gens, coefficients})
assert(gens === generators)
sol = solveSOS(2*x^4+5*y^4-2*x^2*y^2+2*x^3*y);
assert(class sol === SDPResult)
assert(set keys sol === set {Monomials, GramMatrix, MomentMatrix, Parameters, Status})

-- the PackageExports chain: the documented key Parameters is the symbol of
-- NumericalAlgebraicGeometry (re-exported by SemidefiniteProgramming), Solver
-- and the functions of SemidefiniteProgramming are visible after loading
-- SumsOfSquares alone (LOG.md 1.2)
assert(sol#?Parameters and sol#?Monomials and sol#?Status)
assert(toString package Parameters == "NAGtypes" or toString package Parameters == "NumericalAlgebraicGeometry")
assert(toString package Solver == "SemidefiniteProgramming")
for f in {sdp, optimize, PSDdecomposition, roundPSDmatrix, smat2vec, vec2smat} do assert instance(f, Function)
