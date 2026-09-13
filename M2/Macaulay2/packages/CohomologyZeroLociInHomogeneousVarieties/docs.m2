--------------------------------------------------------------------------------
-- Documentation for the package CohomologyZeroLociInHomogeneousVarieties
--------------------------------------------------------------------------------

beginDocumentation()

document {
    Key => CohomologyZeroLociInHomogeneousVarieties,
    Headline => " A package for working with homogeneous vector bundles and their zero loci",
    PARA {"A rational homogeneous variety is a smooth projective variety of the form ",
	TEX///$X = G/P$///, ", where ", TEX///$G$///, " is a semisimple complex Lie group and ",
	TEX///$P \subset G$///, " is a parabolic subgroup. Such an ", TEX///$X$///,
	" is completely described by a root system ", TEX///$R$///,
	" together with a set of marked nodes of its Dynkin diagram, and every irreducible homogeneous ",
	"vector bundle on ", TEX///$X$///, " is described by a weight of ", TEX///$R$///,
	" which is dominant for the semisimple part of ", TEX///$P$///,
	". This package implements that dictionary and uses it to compute cohomological invariants."},
    PARA {"Varieties are created with ", TO homogeneousVariety, ", or with the shortcuts ",
	TO Gr, ", ", TO Fl, ", ", TO OGr, " and ", TO SGr, "; bundles are created from their highest weights with ",
	TO homogeneousVectorBundle, " (completely reducible ones) and with ", TO filtrationBundle,
	" (bundles given by a filtration with completely reducible quotients, such as the tangent ",
	"bundle of a variety of Picard rank greater than one). The cohomology of a completely reducible ",
	"bundle is computed by Bott's theorem; for a filtration bundle it is computed from the long ",
	"exact sequences attached to the filtration, and it may therefore be determined only up to ",
	"unknowns, in which case the answer is a polynomial in some variables together with a ",
	"table of bounds for them."},
    PARA {"The main application is the study of the zero locus ", TEX///$Y \subset X$///,
	" of a general global section of a globally generated completely reducible homogeneous vector bundle ", TEX///$F$///,
	" on ", TEX///$X$///, ". Such a ", TEX///$Y$///, " is created with ", TO embeddedVariety,
	", and its invariants are computed by combining the Koszul complex of the inclusion ", TEX///$Y\subseteq X$///,
        " with the cohomology of homogeneous bundles on ", TEX///$X$///, ": see ",
	TO hodgeNumbers, ", ", TO hochschildNumbers, ", ", TO volumeFano, " and ", TO invariants, "."},
    PARA {"As a first example we take ", TEX///$X = \mathbb{P}^5$///, ", the bundle ",
	TEX///$F = \mathcal{O}(3)$///, " and the cubic fourfold ", TEX///$Y \subset X$///,
	" it cuts out."},
    EXAMPLE {
	"X = Gr{1,6};",
	"dim X",
        "(U,Q) = bundles X;",
        "F = symmetricPower(3, determinant Q);",
	"rank F",
	"Y = embeddedVariety F;",
	"dim Y",
	"displayHN Y"
	},
    PARA {"Most of the representation-theoretic core (such as tensor products, ",
	"symmetric and exterior powers of representations) is delegated to a set of auxiliary Python routines, ",
	"called through the package ", TT "Python."},
    PARA {EM "Acknowledgement: ", "the basic Lie-theoretic constrctions of this package are built on top of the ",
	"package ", TT "WeylGroups", " by Baptiste Calmès and Viktor Petrov. Moreover, we gained inspirations for many representation-theoretic functions ",
        "from the program ", TT "LiE", " and from the the package ", TT "HighestWeights", " by Federico Galetto."
        },
    PARA {EM "References:"},
    UL {
	{"D. Snow, ", EM "Homogeneous vector bundles", ", CMS Conf. Proc. 10, 193-205, 1989."},
	{"G. Ottaviani, ", EM "Rational homogeneous varieties", ", Lecture notes, Cortona, 1995."},
	{"J. Weyman, ", EM "Cohomology of vector bundles and syzygies", ", Cambridge Univ. Press, 2003."},
        {"W. A. de Graaf, ", EM "Lie algebras: theory and algorithms", ", Amsterdam: North-Holland (2000; Zbl 1122.17300)."},
	{"M. A. A. van Leeuwen, A. M. Cohen, B. Lisser, ", EM "LiE, a package for Lie group computations", ", CAN, 1992."}
	},
    PARA {EM "Caveat: ", "the package requires a working Python interpreter, reachable through the ",
	"package ", TT "Python", ", and the auxiliary Python files shipped in the subdirectory ",
	TT "pythonFiles", "."},
    Subnodes => {
	"Parabolic subgroups and homogeneous varieties",
	TO ParabolicGroup,
	TO newParabolic,
	TO HomogeneousVariety,
	TO homogeneousVariety,
	TO Gr,
	TO Fl,
	TO OGr,
	TO SGr,
	TO (dim, HomogeneousVariety),
	TO (bundles, HomogeneousVariety),
	TO (info, HomogeneousVariety),
	"Homogeneous vector bundles",
	TO HomogeneousVectorBundle,
	TO homogeneousVectorBundle,
	TO FiltrationBundle,
	TO filtrationBundle,
	TO structureSheaf,
	TO homogeneousTangentBundle,
	TO homogeneousCotangentBundle,
	TO summands,
	TO (rank, HomogeneousVectorBundle),
	TO (chern, ZZ, HomogeneousVariety),
	TO isAmple,
	TO isGloballyGenerated,
	"Operations on homogeneous vector bundles",
	TO tensorProduct,
	TO (symbol *, HomogeneousVectorBundle, HomogeneousVectorBundle),
	TO (symbol +, HomogeneousVectorBundle, HomogeneousVectorBundle),
	TO (symmetricPower, ZZ, HomogeneousVectorBundle),
	TO (exteriorPower, ZZ, HomogeneousVectorBundle),
	TO (dual, HomogeneousVectorBundle),
	TO (determinant, HomogeneousVectorBundle),
	TO (koszul, HomogeneousVariety, HomogeneousVectorBundle),
	"Weights and representations",
	TO isDominant,
	TO isSingular,
	TO dominantConjugate,
	TO weylFormula,
	TO adjointRepresentation,
	"Zero loci of homogeneous bundles",
	TO EmbeddedVariety,
	TO embeddedVariety,
	TO (dim, EmbeddedVariety),
	"Cohomology and invariants",
	TO (cohomology, FiltrationBundle),
	TO eulerCharacteristic,
	TO eulerCharacteristicStructure,
	TO eulerCharacteristicCotangent,
	TO eulerCharacteristicTangent,
	TO eulerCharacteristicTangentTwisted,
	TO cohomologyRestriction,
	TO hodgeNumbers,
	TO displayHN,
	TO hochschildNumbers,
	TO hochschildNumbersTwisted,
	TO displayHochN,
	TO volumeFano,
	TO (hilbertPolynomial, HomogeneousVectorBundle, HomogeneousVectorBundle),
	TO invariants,
	TO doHodge,
	TO maxChiT
	}
    }

--------------------------------------------------------------------------------
-- parabolic subgroups and homogeneous varieties
--------------------------------------------------------------------------------

document {
    Key => ParabolicGroup,
    Headline => "the class of all parabolic subgroups of a semisimple Lie group",
    PARA {"A parabolic subgroup ", TEX///$P$///, " of the semisimple Lie group with root system ",
	TEX///$R$///, " is determined, up to conjugation, by a subset of the simple roots of ",
	TEX///$R$///, ": the subset consisting of the simple roots which are roots of the ",
	"(semisimple part of the) subgroup, i.e. of the ", EM "unmarked", " nodes of the Dynkin ",
	"diagram. An object of class ", TT "ParabolicGroup", " is a hash table with keys ",
	TT ///"dynkinType"///, " and ", TT ///"parabolic"///, ", the latter being an object of class ",
	TT "Parabolic", " as defined in the package ", TT "WeylGroups", "."},
    EXAMPLE {
	"R = rootSystemA(4);",
	"P = newParabolic(R,set{1,2,4});",
	"P#\"parabolic\"",
	"P#\"dynkinType\""
	},
    SeeAlso => {newParabolic, homogeneousVariety}
    }

document {
    Key => {newParabolic, (newParabolic, RootSystem, Set)},
    Headline => "the parabolic subgroup determined by a subset of the simple roots",
    Usage => "P = newParabolic(R,S)",
    Inputs => {
	"R" => RootSystem => {"the root system of a semisimple Lie group ", TEX///$G$///},
	"S" => Set => {"a set of integers between 1 and the rank of ", TEX///$R$///,
	    ", the indices of the simple roots which belong to ", TEX///$P$///,
	    ", that is, of the unmarked nodes of the Dynkin diagram"}
	},
    Outputs => {
	ParabolicGroup => {"the parabolic subgroup of ", TEX///$G$///, " determined by ", TEX///$S$///}
	},
    PARA {"The complement of ", TEX///$S$///, " in ", TEX///$\{1,\dots,\mathrm{rank}(R)\}$///,
	" is the set of marked nodes; its cardinality is the Picard rank of the homogeneous variety ",
	TEX///$G/P$///, "."},
    EXAMPLE {
	"R = rootSystemE(6);",
	"P = newParabolic(R,set{2,3,4,5,6});",
	"P#\"parabolic\""
	},
    PARA {"An error is raised if the given indices do not lie between 1 and the rank of the root system."},
    SeeAlso => {ParabolicGroup, homogeneousVariety}
    }

document {
    Key => HomogeneousVariety,
    Headline => "the class of all rational homogeneous varieties",
    PARA {"A ", EM "homogeneous variety", " is a projective variety isomorphic to the quotient ",
	TEX///$G/P$///, " of a semisimple Lie group ", TEX///$G$///, " by a parabolic subgroup ",
	TEX///$P$///, ". Objects of this class are mutable hash tables, whose entries are accessible by the commands ", TO peek, " or ", TO info, ". Among them there are"},
    UL {
	{TT ///"rootSystem"///, ", the root system of ", TEX///$G$///, ";"},
	{TT ///"parabolicSubgroup"///, ", the ", TO ParabolicGroup, ";"},
	{TT ///"positiveRoots"///, ", the positive roots of ", TEX///$G$///, " which are not roots of ",
	    TEX///$P$///, ";"},
	{TT ///"picardRank"///, ", the rank of the Picard group of the variety."}
	},
    PARA {"The remaining entries are used as a cache for the invariants computed so far."},
    EXAMPLE {
	"X = Gr{2,5};",
	"dim X",
	"X#\"picardRank\"",
	"info X"
	},
    SeeAlso => {homogeneousVariety, EmbeddedVariety}
    }

document {
    Key => {homogeneousVariety, (homogeneousVariety, RootSystem, ParabolicGroup), (homogeneousVariety, RootSystem, Set)},
    Headline => "the homogeneous variety defined by a root system and a parabolic subgroup",
    Usage => "X = homogeneousVariety(R,P)\nX = homogeneousVariety(R,S)",
    Inputs => {
	"R" => RootSystem => {"the root system of a semisimple Lie group ", TEX///$G$///},
	"P" => ParabolicGroup => {"a parabolic subgroup of ", TEX///$G$///, "; alternatively, a ",
	    TO Set, " of integers describing it, as in ", TO newParabolic}
	},
    Outputs => {
	HomogeneousVariety => {"the quotient ", TEX///$G/P$///}
	},
    PARA {"The marked nodes of the Dynkin diagram are the ones which are ", EM "not", " in the given set."},
    EXAMPLE {
	"R = rootSystemD(5);",
	"X = homogeneousVariety(R,set{1,3,4,5}); -- OGr(2,10)",
	"dim X"
	},
    PARA {"Passing the set directly is a shortcut for building the parabolic subgroup first."},
    EXAMPLE {
	"R = rootSystemE(6);",
	"P = newParabolic(R,set{2,3,4,5,6});",
	"X = homogeneousVariety(R,P);",
	"dim X"
	},
    SeeAlso => {newParabolic, Gr, Fl, OGr, SGr}
    }

document {
    Key => {Gr, (Gr, List)},
    Headline => "a Grassmannian of linear subspaces",
    Usage => "X = Gr{k,n}",
    Inputs => {
	{"a list ", TEX///$\{k,n\}$///, " of two integers"}
	},
    Outputs => {
	HomogeneousVariety => {"the Grassmannian of ", TEX///$k$///, "-dimensional linear subspaces of an ",
	    TEX///$n$///, "-dimensional vector space"}
	},
    PARA {"This is the quotient of a group of type ", TEX///$A_{n-1}$///,
	" by the maximal parabolic subgroup corresponding to the ", TEX///$k$///,
	"-th node of the Dynkin diagram; its dimension is ", TEX///$k(n-k)$///, "."},
    EXAMPLE {
	"X = Gr{2,5};",
	"dim X",
	"X = Gr{1,6};",
	"dim X"
	},
    SeeAlso => {Fl, OGr, SGr, homogeneousVariety}
    }

document {
    Key => {Fl, (Fl, List)},
    Headline => "a flag variety of linear subspaces",
    Usage => "X = Fl{k_1,...,k_r,n}",
    Inputs => {
	{"a list of integers whose last entry is ", TEX///$n$///,
	    " and whose first entries are the dimensions ", TEX///$k_1 < \cdots < k_r$///,
	    " of the subspaces of the flag"}
	},
    Outputs => {
	HomogeneousVariety => {"the variety of flags ", TEX///$V_{k_1} \subset \cdots \subset V_{k_r}$///,
	    " of subspaces of an ", TEX///$n$///, "-dimensional vector space"}
	},
    PARA {"This is the quotient of a group of type ", TEX///$A_{n-1}$///,
	" by the parabolic subgroup whose marked nodes are ", TEX///$k_1,\dots,k_r$///,
	"; its Picard rank is ", TEX///$r$///, "."},
    EXAMPLE {
	"X = Fl{1,3,5};",
	"dim X",
	"X = Fl{1,2,3,6};",
	"dim X",
	"X#\"picardRank\""
	},
    SeeAlso => {Gr, homogeneousVariety}
    }

document {
    Key => {OGr, (OGr, List)},
    Headline => "an orthogonal Grassmannian of isotropic subspaces",
    Usage => "X = OGr{k,m}",
    Inputs => {
	{"a list ", TEX///$\{k,m\}$///, " of two integers"}
	},
    Outputs => {
	HomogeneousVariety => {"the variety of ", TEX///$k$///, "-dimensional isotropic subspaces of an ",
	    TEX///$m$///, "-dimensional vector space equipped with a nondegenerate quadratic form"}
	},
    PARA {"If ", TEX///$m = 2n+1$///, " is odd the variety is the quotient of a group of type ",
	TEX///$B_n$///, " by the maximal parabolic subgroup with marked node ", TEX///$k$///,
	". If ", TEX///$m = 2n$///, " is even the group has type ", TEX///$D_n$///, ": for ",
	TEX///$k \le n-2$///, " and for ", TEX///$k = n$///, " the marked node is ", TEX///$k$///,
	", whereas for ", TEX///$k = n-1$///, " both nodes ", TEX///$n-1$///, " and ", TEX///$n$///,
	" are marked."},
    EXAMPLE {
	"X = OGr{1,7};",
	"dim X",
	"X = OGr{3,10};",
	"dim X"
	},
    SeeAlso => {Gr, SGr, homogeneousVariety}
    }

document {
    Key => {SGr, (SGr, List)},
    Headline => "a symplectic Grassmannian of isotropic subspaces",
    Usage => "X = SGr{k,m}",
    Inputs => {
	{"a list ", TEX///$\{k,m\}$///, " of two integers, with ", TEX///$m$///, " even"}
	},
    Outputs => {
	HomogeneousVariety => {"the variety of ", TEX///$k$///, "-dimensional isotropic subspaces of an ",
	    TEX///$m$///, "-dimensional symplectic vector space"}
	},
    PARA {"This is the quotient of a group of type ", TEX///$C_{m/2}$///,
	" by the maximal parabolic subgroup with marked node ", TEX///$k$///,
	". An error is raised if ", TEX///$m$///, " is odd."},
    EXAMPLE {
	"X = SGr{2,6};",
	"dim X",
	"X = SGr{3,8};",
	"dim X"
	},
    SeeAlso => {Gr, OGr, homogeneousVariety}
    }

document {
    Key => (dim, HomogeneousVariety),
    Headline => "the dimension of a homogeneous variety",
    Usage => "dim X",
    Inputs => {"X" => HomogeneousVariety},
    Outputs => {ZZ => {"the dimension of ", TEX///$X$///}},
    PARA {"The dimension is computed as the difference between the lengths of the longest elements ",
	"of the Weyl group of ", TEX///$G$///, " and of the Weyl group of the semisimple part of ",
	TEX///$P$///, ", that is, as the number of positive roots of ", TEX///$G$///,
	" which are not roots of ", TEX///$P$///, "."},
    EXAMPLE {
	"dim Gr{3,8}",
	"dim OGr{2,8}"
	},
    SeeAlso => {(dim, EmbeddedVariety)}
    }

document {
    Key => (bundles, HomogeneousVariety),
    Headline => "the tautological bundles of a flag variety (of type A)",
    Usage => "bundles X",
    Inputs => {"X" => HomogeneousVariety},
    Outputs => {{"the sequence of the irreducible homogeneous bundles which are the quotients of the ",
	    "tautological flag of subbundles on ", TEX///$X$///}},
    PARA {"These are the building blocks used to write down the cotangent bundle of a flag variety, ",
	"see ", TO homogeneousCotangentBundle, "."},
    EXAMPLE {
	"X = Fl{1,2,4};",
	"B = bundles X;",
	"apply(toList B, rank)"
	},
    PARA {EM "Caveat: ", "implemented only for varieties whose root system has type A; an error is ",
	"raised otherwise."}
    }

document {
    Key => {(info, HomogeneousVariety), (info, FiltrationBundle), (info, EmbeddedVariety)},
    Headline => "a quick description of a variety (or of a bundle)",
    Usage => "info X",
    Inputs => {"X" => HomogeneousVariety => {"or a ", TO FiltrationBundle, ", or an ", TO EmbeddedVariety}},
    Outputs => {{"a description of the object, listing its dimension or rank, its first ",
	    "Chern class, the marked Dynkin diagram it lives on and, for bundles, the weights of the ",
	    "irreducible factors"}},
    EXAMPLE {
	"X = Gr{2,4};",
	"info X",
	"F = homogeneousVectorBundle({weight(X#\"rootSystem\",{0,1,0})},{1},X);",
	"info F",
	"info embeddedVariety F"
	}
    }

document {
    Key => {(symbol ==, HomogeneousVariety, HomogeneousVariety), (symbol ==, ParabolicGroup, ParabolicGroup),
	(symbol ==, HomogeneousVectorBundle, HomogeneousVectorBundle), (symbol ==, FiltrationBundle, FiltrationBundle),
	(symbol ==, EmbeddedVariety, EmbeddedVariety)},
    Headline => "equality of varieties, parabolic subgroups and bundles",
    Usage => "X == X'",
    PARA {"Two parabolic subgroups are equal when they are given by the same set of simple roots, ",
	"two homogeneous varieties when they have the same root system and the same parabolic subgroup, ",
	"two completely reducible bundles when they have the same weights with the same multiplicities, ",
	"two filtration bundles when they have the same list of factors, and two embedded varieties ",
	"when they are the zero loci of the same bundle."},
    EXAMPLE {
	"Gr{2,5} == homogeneousVariety(rootSystemA(4),set{1,3,4})",
	"Gr{2,5} == Gr{3,5}"
	}
    }

--------------------------------------------------------------------------------
-- homogeneous vector bundles
--------------------------------------------------------------------------------

document {
    Key => HomogeneousVectorBundle,
    Headline => "the class of all completely reducible homogeneous vector bundles",
    PARA {"An irreducible homogeneous vector bundle on ", TEX///$X = G/P$///,
	" is the bundle associated to an irreducible representation of ", TEX///$P$///,
	", hence to a weight of the root system of ", TEX///$G$///,
	" whose restriction to the unmarked nodes is dominant. An object of this class is a direct ",
	"sum of such bundles, and it is stored as a list of weights together with a list of ",
	"multiplicities. The relevant entries of the hash table are ", TT ///"underlyingVariety"///,
	", ", TT ///"weights"///, ", ", TT ///"multiplicities"///, " and ", TT ///"parabolicWeights"///,
	", the last being the projections of the weights to the weight lattice of the semisimple ",
	"part of ", TEX///$P$///, "."},
    EXAMPLE {
	"X = Gr{2,5};",
	"R = X#\"rootSystem\";",
	"E = homogeneousVectorBundle({weight(R,{0,1,0,0})},{2},X);",
	"rank E",
	"E#\"irreducible\""
	},
    SeeAlso => {homogeneousVectorBundle, FiltrationBundle}
    }

document {
    Key => {homogeneousVectorBundle, (homogeneousVectorBundle, List, List, HomogeneousVariety)},
    Headline => "the completely reducible homogeneous bundle associated to a list of weights",
    Usage => "E = homogeneousVectorBundle(S,m,X)",
    Inputs => {
	"S" => List => {"a list of weights of the root system of ", TEX///$X$///,
	    ", each of which must be dominant for the semisimple part of the parabolic subgroup"},
	"m" => List => {"the list of the multiplicities of those weights, of the same length as ",
	    TEX///$S$///},
	"X" => HomogeneousVariety
	},
    Outputs => {
	HomogeneousVectorBundle => {"the direct sum of the irreducible bundles attached to the weights of ",
	    TEX///$S$///, ", each taken with the prescribed multiplicity"}
	},
    PARA {"Repeated weights are collected automatically, and the rank of the bundle is computed from ",
	"Weyl's dimension formula applied to the parabolic weights."},
    EXAMPLE {
	"R = rootSystemD(4);",
	"X = homogeneousVariety(R,set{2,4});",
	"l1 = weight(R,{1,0,0,0});",
	"l2 = weight(R,{0,0,1,0});",
	"E = homogeneousVectorBundle({l1,l2},{1,2},X);",
	"rank E",
	"chern(1,E)"
	},
    PARA {"A line bundle is obtained whenever the weight vanishes on all the unmarked nodes; the ",
	"structure sheaf is the special case of the zero weight, see ", TO structureSheaf, "."},
    EXAMPLE {
	"X = Gr{1,4};",
	"L = homogeneousVectorBundle({weight(X#\"rootSystem\",{3,0,0})},{1},X);",
	"rank L",
	"isAmple L"
	},
    SeeAlso => {HomogeneousVectorBundle, filtrationBundle, summands}
    }

document {
    Key => FiltrationBundle,
    Headline => "the class of all homogeneous vector bundles given by a filtration",
    PARA {"A homogeneous vector bundle which is not completely reducible is described by a filtration ",
	TEX///$F = E_0 \supset E_1 \supset \cdots \supset E_r = 0$///,
	" whose quotients ", TEX///$E_i/E_{i+1}$///, " are completely reducible. Objects of this class ",
	"store the list of those quotients under the key ", TT ///"factors"///, ". The class ",
	TO HomogeneousVectorBundle, " is a subclass of this one, corresponding to the case of a ",
	"filtration of length one."},
    EXAMPLE {
	"X = Fl{1,2,4};",
	"Cot = homogeneousCotangentBundle X;",
	"class Cot",
	"rank Cot",
	"#(Cot#\"factors\")"
	},
    SeeAlso => {filtrationBundle, (cohomology, FiltrationBundle)}
    }

document {
    Key => {filtrationBundle, (filtrationBundle, List, HomogeneousVariety)},
    Headline => "the homogeneous bundle defined by the factors of a filtration",
    Usage => "F = filtrationBundle(S,X)",
    Inputs => {
	"S" => List => {"either a list of completely reducible bundles on ", TEX///$X$///,
	    ", to be read as the quotients ", TEX///$E_i/E_{i+1}$///,
	    " of the filtration, or a list of weights, in which case each weight gives one ",
	    "irreducible factor"},
	"X" => HomogeneousVariety
	},
    Outputs => {FiltrationBundle},
    PARA {"The order of the list matters: the first entry is the quotient ", TEX///$E_0/E_1$///,
	" and the last one is the subbundle ", TEX///$E_{r-1}$///,
	". The outcome is the bundle ",
	TEX///$F = E_0 \supset E_1 \supset \cdots \supset E_r = 0$///,"."},
    EXAMPLE {
	"R = rootSystemC(3);",
	"X = homogeneousVariety(R,set{1,3});",
	"E1 = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);",
	"E2 = homogeneousVectorBundle({weight(R,{0,0,1})},{1},X);",
	"F = filtrationBundle({E1,E2},X);",
	"rank F"
	},
    PARA {"Giving a list of weights is a shortcut for the filtration whose factors are the ",
	"corresponding irreducible bundles."},
    EXAMPLE {
	"R = rootSystemA(4);",
	"X = homogeneousVariety(R,set{2,4});",
	"F = filtrationBundle({weight(R,{1,0,0,0}),weight(R,{0,0,0,1})},X);",
	"rank F"
	},
    SeeAlso => {FiltrationBundle, homogeneousVectorBundle}
    }

document {
    Key => {structureSheaf, (structureSheaf, HomogeneousVariety)},
    Headline => "the structure sheaf of a homogeneous variety",
    Usage => "O = structureSheaf X",
    Inputs => {"X" => HomogeneousVariety},
    Outputs => {
	HomogeneousVectorBundle => {"the irreducible bundle attached to the zero weight, that is, ",
	    TEX///$\mathcal{O}_X$///}
	},
    EXAMPLE {
	"X = Gr{2,5};",
	"O = structureSheaf X;",
	"rank O",
	"eulerCharacteristic O"
	},
    SeeAlso => {homogeneousVectorBundle}
    }

document {
    Key => {homogeneousTangentBundle, (homogeneousTangentBundle, HomogeneousVariety)},
    Headline => "the tangent bundle of a homogeneous variety",
    Usage => "T = homogeneousTangentBundle X",
    Inputs => {"X" => HomogeneousVariety},
    Outputs => {
	FiltrationBundle => {"the tangent bundle of ", TEX///$X$///, ", returned as a ",
	    TO HomogeneousVectorBundle, " when it happens to be completely reducible"}
	},
    PARA {"The positive roots of ", TEX///$G$///, " which are not roots of ", TEX///$P$///,
	" are grouped according to their components along the simple roots of the marked nodes; ",
	"each group contributes one factor of the filtration, and the factors are ordered by ",
	"decreasing total degree."},
    EXAMPLE {
	"X = OGr{2,7};",
	"T = homogeneousTangentBundle X;",
	"rank T",
	"chern(1,T) == chern(1,X)"
	},
    SeeAlso => {homogeneousCotangentBundle}
    }

document {
    Key => {homogeneousCotangentBundle, (homogeneousCotangentBundle, HomogeneousVariety)},
    Headline => "the cotangent bundle of a homogeneous variety",
    Usage => "Cot = homogeneousCotangentBundle X",
    Inputs => {"X" => HomogeneousVariety},
    Outputs => {
	FiltrationBundle => {"the cotangent bundle of ", TEX///$X$///, ", returned as a ",
	    TO HomogeneousVectorBundle, " when it happens to be completely reducible"}
	},
    PARA {"For variety of type A and for cominuscule varieties it is already stored;",
	" in the remaining cases it is obtained by ",
	"dualizing ", TO homogeneousTangentBundle, "."},
    EXAMPLE {
	"R = rootSystemE(6);",
	"X = homogeneousVariety(R,set{2,3,4,5,6}); -- Cayley plane (cominuscule)", 
	"Cot = homogeneousCotangentBundle X;",
	"rank Cot",
        "chern(1,X) == -chern(1,Cot)",
	"eulerCharacteristic Cot"
	},
    SeeAlso => {homogeneousTangentBundle, hodgeNumbers}
    }

document {
    Key => {summands, (summands, HomogeneousVectorBundle)},
    Headline => "the irreducible summands of a completely reducible bundle",
    Usage => "S = summands E",
    Inputs => {"E" => HomogeneousVectorBundle},
    Outputs => {
	List => {"the list of the irreducible summands of ", TEX///$E$///,
	    ", each repeated according to its multiplicity"}
	},
    EXAMPLE {
	"R = rootSystemA(4);",
	"X = homogeneousVariety(R,set{2,4});",
	"E = homogeneousVectorBundle({weight(R,{1,0,0,0}),weight(R,{0,0,0,1})},{2,1},X);",
	"S = summands E;",
	"#S",
	"apply(S, rank)"
	},
    PARA {EM "Caveat: ", "an error is raised if the bundle is not completely reducible."}
    }

document {
    Key => {(rank, HomogeneousVectorBundle), (rank, FiltrationBundle)},
    Headline => "the rank of a homogeneous vector bundle",
    Usage => "rank E",
    Inputs => {"E" => HomogeneousVectorBundle => {"or a ", TO FiltrationBundle}},
    Outputs => {ZZ},
    PARA {"For a completely reducible bundle the rank is the sum, over the weights of the bundle, of ",
	"the dimensions given by Weyl's formula, each multiplied by the corresponding multiplicity; ",
	"for a filtration bundle it is the sum of the ranks of the factors."},
    EXAMPLE {
	"X = Gr{2,5};",
	"rank homogeneousTangentBundle X",
	"rank structureSheaf X"
	},
    SeeAlso => {weylFormula}
    }

document {
    Key => {(chern, ZZ, HomogeneousVariety), (chern, ZZ, HomogeneousVectorBundle),
	(chern, ZZ, FiltrationBundle), (chern, ZZ, EmbeddedVariety)},
    Headline => "the first Chern class of a variety or of a bundle",
    Usage => "chern(1,X)",
    Inputs => {
	{"the degree, which at present must be 1"},
	"X" => HomogeneousVariety => {"or a bundle, or an ", TO EmbeddedVariety}
	},
    Outputs => {
	RingElement => {"the first Chern class, written in the basis of the fundamental weights attached ",
	    "to the marked nodes, that is, as a vector of the Néron-Severi lattice of the ambient ",
	    "homogeneous variety"}
	},
    PARA {"For a homogeneous variety this is the sum of the positive roots which are not roots of ",
	TEX///$P$///, "; for an irreducible bundle it is the weight of its determinant; for a ",
	"filtration bundle it is the sum of the classes of the factors; and for a zero locus ",
	TEX///$Y$///, " of a bundle ", TEX///$F$///, " it is ",
	TEX///$c_1(X) - c_1(F)$///, ", by adjunction."},
    EXAMPLE {
	"X = Gr{2,5};",
	"chern(1,X)",
	"F = homogeneousVectorBundle({weight(X#\"rootSystem\",{0,1,0,0})},{2},X);",
	"chern(1,F)",
	"chern(1,embeddedVariety F)"
	},
    PARA {EM "Caveat: ", "only the case of degree one is implemented; an error is raised otherwise."}
    }

document {
    Key => {isAmple, (isAmple, HomogeneousVectorBundle)},
    Headline => "whether a line bundle is ample",
    Usage => "isAmple L",
    Inputs => {"L" => HomogeneousVectorBundle => {"a bundle of rank one"}},
    Outputs => {Boolean => {"whether ", TEX///$L$///, " is ample"}},
    PARA {"A line bundle is ample exactly when its weight pairs positively with every ",
	"positive root of ", TEX///$G$///, " which is not a root of ", TEX///$P$///, "."},
    EXAMPLE {
	"X = Gr{1,4};",
	"R = X#\"rootSystem\";",
	"isAmple homogeneousVectorBundle({weight(R,{1,0,0})},{1},X)",
	"isAmple homogeneousVectorBundle({weight(R,{-1,0,0})},{1},X)"
	},
    PARA {EM "Caveat: ", "an error is raised if the bundle does not have rank one."},
    SeeAlso => {isGloballyGenerated}
    }

document {
    Key => {isGloballyGenerated, (isGloballyGenerated, HomogeneousVectorBundle, HomogeneousVariety)},
    Headline => "whether a homogeneous vector bundle is globally generated",
    Usage => "isGloballyGenerated(E,X)",
    Inputs => {
	"E" => HomogeneousVectorBundle,
	"X" => HomogeneousVariety => {"the variety on which ", TEX///$E$///, " lives"}
	},
    Outputs => {Boolean},
    PARA {"A completely reducible homogeneous bundle is globally generated if and only if all of its ",
	"weights are dominant for the whole root system of ", TEX///$G$///,
	". This is the condition under which a general global section of ", TEX///$E$///,
	" cuts out a smooth subvariety of the expected dimension, see ", TO embeddedVariety, "."},
    EXAMPLE {
	"R = rootSystemA(4);",
	"X = homogeneousVariety(R,set{1,3,4});",
	"isGloballyGenerated(homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X),X)",
	"isGloballyGenerated(homogeneousVectorBundle({weight(R,{0,-1,0,0})},{1},X),X)"
	},
    SeeAlso => {isAmple, isDominant}
    }

--------------------------------------------------------------------------------
-- operations on bundles
--------------------------------------------------------------------------------

document {
    Key => {tensorProduct, (tensorProduct, HomogeneousVectorBundle, HomogeneousVectorBundle),
	(tensorProduct, FiltrationBundle, HomogeneousVectorBundle),
	(tensorProduct, HomogeneousVectorBundle, FiltrationBundle)},
    Headline => "the tensor product of homogeneous vector bundles",
    Usage => "E = tensorProduct(E1,E2)",
    Inputs => {
	"E1" => HomogeneousVectorBundle => {"or a ", TO FiltrationBundle},
	"E2" => HomogeneousVectorBundle => {"or a ", TO FiltrationBundle, ", on the same variety as ",
	    TEX///$E_1$///}
	},
    Outputs => {
	HomogeneousVectorBundle => {"the tensor product, decomposed into irreducible summands; a ",
	    TO FiltrationBundle, " if one of the factors is one"}
	},
    PARA {"The decomposition of the tensor product of two irreducible bundles is obtained from the ",
	"decomposition of the tensor product of the corresponding representations of the semisimple ",
	"part of ", TEX///$P$///, ", computed with Klimyk's formula; the twist by the characters of ",
	TEX///$P$///, " is recovered from the slopes of the summands, so that the first Chern class ",
	"of the product is always ",
	TEX///$\mathrm{rk}(E_2)\,c_1(E_1) + \mathrm{rk}(E_1)\,c_1(E_2)$///, "."},
    EXAMPLE {
	"R = rootSystemA(4);",
	"X = homogeneousVariety(R,set{2,4});",
	"E1 = homogeneousVectorBundle({weight(R,{1,1,0,0})},{1},X);",
	"E2 = homogeneousVectorBundle({weight(R,{0,0,0,1})},{1},X);",
	"E = tensorProduct(E1,E2);",
	"rank E",
	"#(E#\"weights\")"
	},
    SeeAlso => {(symbol *, HomogeneousVectorBundle, HomogeneousVectorBundle), (tensorProduct, List),
	(symmetricPower, ZZ, HomogeneousVectorBundle)}
    }

document {
    Key => (tensorProduct, List),
    Headline => "the tensor product of a list of homogeneous vector bundles",
    Usage => "E = tensorProduct S",
    Inputs => {
	"S" => List => {"a list of bundles to be tensored together from the left"}
	},
    Outputs => {
	HomogeneousVectorBundle => {"the tensor product of the bundles in ", TEX///$S$///,
	    ", decomposed into irreducible summands"}
	},
    PARA {"Several bundles can be multiplied at once by giving a list, as a shortcut for repeatedly ",
	"applying ", TO (tensorProduct, HomogeneousVectorBundle, HomogeneousVectorBundle), "."},
    EXAMPLE {
	"R = rootSystemD(4);",
	"X = homogeneousVariety(R,set{2,4});",
	"E1 = homogeneousVectorBundle({weight(R,{1,0,0,0})},{1},X);",
	"E2 = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);",
	"E3 = homogeneousVectorBundle({weight(R,{0,0,1,0})},{1},X);",
	"rank tensorProduct{E1,E2,E3}"
	},
    SeeAlso => {(tensorProduct, HomogeneousVectorBundle, HomogeneousVectorBundle)}
    }

document {
    Key => {(symbol *, HomogeneousVectorBundle, HomogeneousVectorBundle),
	(symbol *, FiltrationBundle, HomogeneousVectorBundle), (symbol *, HomogeneousVectorBundle, FiltrationBundle)},
    Headline => "the tensor product of two homogeneous vector bundles",
    Usage => "E1 * E2",
    Inputs => {"E1" => HomogeneousVectorBundle, "E2" => HomogeneousVectorBundle},
    Outputs => {HomogeneousVectorBundle},
    PARA {"A shorthand for ", TO tensorProduct, "."},
    EXAMPLE {
	"X = Gr{2,5};",
	"R = X#\"rootSystem\";",
	"E = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);",
	"L = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);",
	"rank (E*L)"
	},
    SeeAlso => {tensorProduct}
    }

document {
    Key => (symbol +, HomogeneousVectorBundle, HomogeneousVectorBundle),
    Headline => "the direct sum of two completely reducible homogeneous bundles",
    Usage => "E1 + E2",
    Inputs => {
	"E1" => HomogeneousVectorBundle,
	"E2" => HomogeneousVectorBundle => {"on the same variety as ", TEX///$E_1$///}
	},
    Outputs => {
	HomogeneousVectorBundle => {"the direct sum, with the multiplicities of the common weights ",
	    "added up"}
	},
    EXAMPLE {
	"X = Gr{2,5};",
	"R = X#\"rootSystem\";",
	"E1 = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);",
	"E2 = homogeneousVectorBundle({weight(R,{0,1,0,0})},{2},X);",
	"(E1+E2)#\"multiplicities\""
	},
    SeeAlso => {summands}
    }

document {
    Key => (symmetricPower, ZZ, HomogeneousVectorBundle),
    Headline => "symmetric powers of a homogeneous bundle",
    Usage => "symmetricPower(n,E)",
    Inputs => {
	"n" => ZZ => {"a nonnegative integer"},
	"E" => HomogeneousVectorBundle
	},
    Outputs => {
	{"the ", TEX///$n$///, "-th symmetric power of ", TEX///$E$///, " (an object of class ", TO HomogeneousVectorBundle, ")"}
	},
    PARA {"The decomposition is computed with the Adams operations, following the same strategy as LiE."},
    EXAMPLE {
	"X = Gr{2,5};",
	"R = X#\"rootSystem\";",
	"E = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);",
	"rank symmetricPower(2,E)"
	},
    SeeAlso => {(exteriorPower, ZZ, HomogeneousVectorBundle), tensorProduct}
    }

document {
    Key => {(exteriorPower, ZZ, HomogeneousVectorBundle), (exteriorPower, ZZ, FiltrationBundle)},
    Headline => "exterior powers of a homogeneous bundle",
    Usage => "exteriorPower(n,E)",
    Inputs => {
	"n" => ZZ => {"a nonnegative integer"},
	"E" => HomogeneousVectorBundle => {"or a ", TO FiltrationBundle}
	},
    Outputs => {
	{"the ", TEX///$n$///, "-th exterior power of ", TEX///$E$///,
	    " (an object of class ", TO HomogeneousVectorBundle, " or ", TO FiltrationBundle, ")"}
	},
    PARA {"The decomposition is computed with the Adams operations, following the same strategy as LiE.",
        " It is also possible to compute exteriorPowers of ", TO FiltrationBundle, "."},
    EXAMPLE {
	"X = Gr{2,5};",
	"Cot = homogeneousCotangentBundle X;",
	"rank exteriorPower(2,Cot)"
	},
    SeeAlso => {(symmetricPower, ZZ, HomogeneousVectorBundle), hodgeNumbers}
    }

document {
    Key => {(dual, HomogeneousVectorBundle), (dual, FiltrationBundle)},
    Headline => "the dual of a homogeneous vector bundle",
    Usage => "dual E",
    Inputs => {"E" => HomogeneousVectorBundle => {"or a ", TO FiltrationBundle}},
    Outputs => {{"the dual bundle (an object of class ", TO HomogeneousVectorBundle, " or ", TO FiltrationBundle, ")"}},
    PARA {"On the unmarked nodes the weights are dualized by the action of the longest element of ",
	"the Weyl group of the semisimple part of ", TEX///$P$///,
	", while on the marked nodes they simply change sign. The factors of a filtration bundle are ",
	"dualized and their order is reversed."},
    EXAMPLE {
	"X = Gr{2,5};",
	"R = X#\"rootSystem\";",
	"E = homogeneousVectorBundle({weight(R,{0,1,0,0})},{1},X);",
	"chern(1,dual E) == - chern(1,E)"
	},
    SeeAlso => {(determinant, HomogeneousVectorBundle)}
    }

document {
    Key => {(determinant, HomogeneousVectorBundle), (determinant, FiltrationBundle)},
    Headline => "the determinant of a homogeneous vector bundle",
    Usage => "determinant E",
    Inputs => {"E" => HomogeneousVectorBundle => {"or a ", TO FiltrationBundle}},
    Outputs => {{"the line bundle ", TEX///$\det E$///, " (an object of class ", TO HomogeneousVectorBundle, ")"}},
    EXAMPLE {
	"X = Gr{2,5};",
	"D = determinant homogeneousTangentBundle X;",
	"rank D",
	"chern(1,D) == chern(1,X)"
	},
    SeeAlso => {(chern, ZZ, HomogeneousVariety)}
    }

document {
    Key => (koszul, HomogeneousVariety, HomogeneousVectorBundle),
    Headline => "the Koszul complex of the zero locus of a homogeneous bundle",
    Usage => "K = koszul(X,F)",
    Inputs => {
	"X" => HomogeneousVariety,
	"F" => HomogeneousVectorBundle => {"a globally generated bundle on ", TEX///$X$///}
	},
    Outputs => {
	{"the list ", TEX///$\{\mathcal{O}_X, F^\vee, \wedge^2 F^\vee, \dots, \det F^\vee\}$///,
	    " of the terms of the Koszul resolution of ", TEX///$\mathcal{O}_Y$///, ", where ",
	    TEX///$Y$///, " is the zero locus of a general global section of ", TEX///$F$///,
	    " (a ", TO List, ")"}
	},
    PARA {"This resolution allows to compute the cohomology of the restrictions to ",
	TEX///$Y$///, " of bundles on ", TEX///$X$///, ". The result is cached inside ", TEX///$F$///, "."},
    EXAMPLE {
	"X = Gr{2,6};",
        "(U,Q) = bundles X;",
	"F = symmetricPower(3, dual U);",
	"K = koszul(X,F);",
	"apply(K, rank)"
	},
    SeeAlso => {cohomologyRestriction, embeddedVariety}
    }

--------------------------------------------------------------------------------
-- weights
--------------------------------------------------------------------------------

document {
    Key => {isDominant, (isDominant, Weight, RootSystem)},
    Headline => "whether a weight is dominant",
    Usage => "isDominant(l,R)",
    Inputs => {"l" => Weight, "R" => RootSystem},
    Outputs => {
	Boolean => {"whether all the coordinates of ", TEX///$l$///,
	    " in the basis of the fundamental weights are nonnegative"}
	},
    EXAMPLE {
	"R = rootSystemA(4);",
	"isDominant(weight(R,{1,0,2,1}),R)",
	"isDominant(weight(R,{1,0,-2,1}),R)"
	},
    SeeAlso => {dominantConjugate, isSingular}
    }

document {
    Key => {isSingular, (isSingular, Weight, RootSystem)},
    Headline => "whether a weight lies on a wall of the Weyl chambers",
    Usage => "isSingular(l,R)",
    Inputs => {"l" => Weight, "R" => RootSystem},
    Outputs => {Boolean => {"whether ", TEX///$l$///, " is orthogonal to some simple root"}},
    PARA {"In Bott's theorem the cohomology of the irreducible bundle of weight ", TEX///$l$///,
	" vanishes in every degree exactly when ", TEX///$l + \rho$///,
	" is singular, where ", TEX///$\rho$///, " is half the sum of the positive roots."},
    EXAMPLE {
	"R = rootSystemA(4);",
	"isSingular(weight(R,{2,1,0,1}),R)",
	"isSingular(weight(R,{1,1,1,1}),R)"
	},
    SeeAlso => {dominantConjugate, isDominant}
    }

document {
    Key => {dominantConjugate, (dominantConjugate, Weight, RootSystem)},
    Headline => "the dominant weight conjugate to a given weight, and its index",
    Usage => "(v,k) = dominantConjugate(l,R)",
    Inputs => {"l" => Weight, "R" => RootSystem},
    Outputs => {
	Sequence => {"a pair ", TEX///$(v,k)$///, ", where ", TEX///$v$///,
	    " is the unique dominant weight in the orbit of ", TEX///$l$///,
	    " under the Weyl group and ", TEX///$k$///,
	    " is the length of the element carrying ", TEX///$l$///, " to ", TEX///$v$///}
	},
    PARA {"The integer ", TEX///$k$///, " is the index of ", TEX///$l$///,
	"; in Bott's theorem it is the degree in which the cohomology of the corresponding bundle ",
	"is concentrated."},
    EXAMPLE {
	"R = rootSystemA(4);",
	"(v,k) = dominantConjugate(weight(R,{-1,2,-3,0}),R);",
	"v",
	"k"
	},
    SeeAlso => {isDominant, isSingular, weylFormula}
    }

document {
    Key => {weylFormula, (weylFormula, Weight, RootSystem)},
    Headline => "the dimension of an irreducible representation, by Weyl's formula",
    Usage => "weylFormula(l,R)",
    Inputs => {"l" => Weight => {"a dominant weight"}, "R" => RootSystem},
    Outputs => {ZZ => {"the dimension of the irreducible representation with highest weight ", TEX///$l$///}},
    PARA {"The dimension is the product over the positive roots ", TEX///$\alpha$///, " of ",
	TEX///$\langle l+\rho,\alpha\rangle / \langle \rho,\alpha\rangle$///, ", where ",
	TEX///$\rho$///, " is half the sum of the positive roots. This is what computes the rank of a ",
	"homogeneous vector bundle, see ", TO (rank, HomogeneousVectorBundle), "."},
    EXAMPLE {
	"R = rootSystemA(4);",
	"weylFormula(weight(R,{1,0,0,0}),R)",
	"R = rootSystemG2;",
	"weylFormula(weight(R,{1,0}),R)"
	},
    SeeAlso => {(rank, HomogeneousVectorBundle)}
    }

document {
    Key => {adjointRepresentation, (adjointRepresentation, RootSystem)},
    Headline => "the highest weight of the adjoint representation",
    Usage => "adjointRepresentation R",
    Inputs => {"R" => RootSystem},
    Outputs => {
	Weight => {"the highest root of ", TEX///$R$///,
	    ", that is, the highest weight of the adjoint representation"}
	},
    EXAMPLE {
	"R = rootSystemA(3);",
	"adjointRepresentation R",
	"R = rootSystemF4;",
	"adjointRepresentation R"
	}
    }

--------------------------------------------------------------------------------
-- zero loci
--------------------------------------------------------------------------------

document {
    Key => EmbeddedVariety,
    Headline => "the class of all zero loci of homogeneous vector bundles",
    PARA {"An object of this class represents the zero locus ", TEX///$Y \subset X$///,
	" of a general global section of a globally generated homogeneous vector bundle ",
	TEX///$F$///, " on the homogeneous variety ", TEX///$X$///,
	". By Bertini's theorem such a ", TEX///$Y$///, " is smooth of dimension ",
	TEX///$\dim X - \mathrm{rk}\,F$///,
	", and its normal bundle is the restriction of ", TEX///$F$///,
	". The relevant entries of the hash table are ", TT ///"ambientSpace"///, " and ",
	TT ///"normalBundle"///, "."},
    EXAMPLE {
	"X = Gr{1,4};",
	"F = homogeneousVectorBundle({weight(X#\"rootSystem\",{2,0,0})},{1},X);",
	"Y = embeddedVariety F;",
	"dim Y",
	"chern(1,Y)"
	},
    SeeAlso => {embeddedVariety, invariants}
    }

document {
    Key => {embeddedVariety, (embeddedVariety, HomogeneousVectorBundle)},
    Headline => "the zero locus of a general global section of a homogeneous bundle",
    Usage => "Y = embeddedVariety F",
    Inputs => {"F" => HomogeneousVectorBundle => {"a globally generated bundle on a homogeneous variety"}},
    Outputs => {EmbeddedVariety},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"isGloballyGenerated(F,X)",
	"Y = embeddedVariety F;",
	"dim Y",
	"info Y"
	},
    SeeAlso => {EmbeddedVariety, isGloballyGenerated, invariants}
    }

document {
    Key => (dim, EmbeddedVariety),
    Headline => "the dimension of the zero locus of a homogeneous bundle",
    Usage => "dim Y",
    Inputs => {"Y" => EmbeddedVariety},
    Outputs => {
	ZZ => {"the difference between the dimension of the ambient homogeneous variety and the rank ",
	    "of the normal bundle"}
	},
    EXAMPLE {
	"R = rootSystemD(5);",
	"X = homogeneousVariety(R,set{2,3,4,5});",
	"F = homogeneousVectorBundle({weight(R,{0,0,0,0,1})},{2},X);",
	"dim embeddedVariety F"
	},
    SeeAlso => {(dim, HomogeneousVariety)}
    }

--------------------------------------------------------------------------------
-- cohomology
--------------------------------------------------------------------------------

document {
    Key => {(cohomology, FiltrationBundle), (homology, FiltrationBundle)},
    Headline => "the cohomology of a homogeneous vector bundle",
    Usage => "(H,b) = cohomology E",
    Inputs => {"E" => FiltrationBundle => {"or a ", TO HomogeneousVectorBundle}},
    Outputs => {
	{"a pair ", TEX///$(H,b)$///, ", where ", TEX///$H$///,
	    " is the list of the dimensions of the cohomology groups ", TEX///$H^i(X,E)$///,
	    ", for ", TEX///$i$///, " from 0 to ", TEX///$\dim X$///, ", and ", TEX///$b$///,
	    " is a hash table of bounds for the unknowns possibly appearing in ", TEX///$H$///}
	},
    PARA {"For a completely reducible bundle the answer is given by Bott's theorem."},
    EXAMPLE {
	"X = Gr{2,5};",
        "(U,Q) = bundles X;", 
	"cohomology Q"
	},
    PARA {"For a bundle given by a filtration the cohomology is computed from the long exact ",
	"sequences of the filtration. These sequences do not always determine the answer, and in ",
	"that case the entries of ", TEX///$H$///, " are polynomials in some auxiliary variables, ",
	"whose range is recorded in the second output."},
    EXAMPLE {
        "R = rootSystemB(3);",
	"X = homogeneousVariety(R,set{1,3});",
        "L = homogeneousVectorBundle({weight(R,{0,-3,0})},{1},X);",
	"(H,b) = cohomology (L*homogeneousCotangentBundle X);",
	"H",
	"peek b"
	},
    SeeAlso => {eulerCharacteristic, hodgeNumbers}
    }

document {
    Key => {eulerCharacteristic, (eulerCharacteristic, FiltrationBundle), (chi, FiltrationBundle)},
    Headline => "the Euler characteristic of a homogeneous vector bundle",
    Usage => "eulerCharacteristic E",
    Inputs => {
	"E" => FiltrationBundle => {"a bundle on a homogeneous variety"}
	},
    Outputs => {
	{"the Euler characteristic ", TEX///$\chi(X,E)$///, " (always a ", TO ZZ, ")"}
	},
    PARA {"The Euler characteristic is the alternating sum of the cohomology dimensions, so it is ",
	"always an integer even when the individual cohomology groups are not determined."},
    EXAMPLE {
	"X = Gr{2,5};",
	"eulerCharacteristic structureSheaf X"
	},
    PARA {"The function ", TT "chi", " is a synonym."},
    SeeAlso => {(cohomology, FiltrationBundle), (eulerCharacteristic, EmbeddedVariety, FiltrationBundle),
	(eulerCharacteristic, HomogeneousVariety, HomogeneousVectorBundle, FiltrationBundle)}
    }

document {
    Key => {(eulerCharacteristic, EmbeddedVariety, FiltrationBundle), (chi, EmbeddedVariety, FiltrationBundle)},
    Headline => "the Euler characteristic of a homogeneous bundle restricted to a zero locus",
    Usage => "eulerCharacteristic(Y,G)",
    Inputs => {
	"Y" => EmbeddedVariety => {"the zero locus of a globally generated homogeneous vector bundle"},
	"G" => FiltrationBundle => {"a bundle on the ambient variety, to be restricted to ", TEX///$Y$///}
	},
    Outputs => {
	{"the Euler characteristic ", TEX///$\chi(Y,G|_Y)$///, " of the restriction of ", TEX///$G$///,
	    " to ", TEX///$Y$///, " (always a ", TO ZZ, ")"}
	},
    PARA {"It is computed as the alternating sum of the Euler characteristics of the terms of the ",
	"Koszul complex of ", TEX///$Y$///, " twisted by ", TEX///$G$///, "."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"G = filtrationBundle({structureSheaf X, F},X);",
	"Y = embeddedVariety F;",
	"eulerCharacteristic(Y,G)"
	},
    PARA {"The function ", TT "chi", " is a synonym."},
    SeeAlso => {eulerCharacteristic, cohomologyRestriction, eulerCharacteristicStructure}
    }

document {
    Key => {(eulerCharacteristic, HomogeneousVariety, HomogeneousVectorBundle, FiltrationBundle),
	(chi, HomogeneousVariety, HomogeneousVectorBundle, FiltrationBundle)},
    Headline => "the Euler characteristic of a homogeneous bundle restricted to a zero locus",
    Usage => "eulerCharacteristic(X,F,G)",
    Inputs => {
	"X" => HomogeneousVariety,
	"F" => HomogeneousVectorBundle => {"a globally generated bundle on ", TEX///$X$///,
	    ", whose zero locus is ", TEX///$Y$///},
	"G" => FiltrationBundle => {"a bundle on ", TEX///$X$///, ", to be restricted to ", TEX///$Y$///}
	},
    Outputs => {
	{"the Euler characteristic ", TEX///$\chi(Y,G|_Y)$///, " (always a ", TO ZZ, ")"}
	},
    PARA {"Equivalent to ", TO (eulerCharacteristic, EmbeddedVariety, FiltrationBundle),
	" applied to the zero locus of ", TEX///$F$///, ", but avoids building the ",
	TO EmbeddedVariety, " object explicitly."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"G = filtrationBundle({structureSheaf X, F},X);",
	"eulerCharacteristic(X,F,G)"
	},
    PARA {"The function ", TT "chi", " is a synonym."},
    SeeAlso => {eulerCharacteristic, (eulerCharacteristic, EmbeddedVariety, FiltrationBundle)}
    }

document {
    Key => {eulerCharacteristicStructure, (eulerCharacteristicStructure, EmbeddedVariety),
	chiStructure, (chiStructure, EmbeddedVariety)},
    Headline => "the Euler characteristic of the structure sheaf of a zero locus",
    Usage => "eulerCharacteristicStructure Y\nchiStructure Y",
    Inputs => {"Y" => EmbeddedVariety},
    Outputs => {ZZ => {"the Euler characteristic ", TEX///$\chi(Y,\mathcal{O}_Y)$///}},
    PARA {"It is computed as the alternating sum of the Euler characteristics of the terms of the ",
	"Koszul complex of ", TEX///$Y$///, " in ", TEX///$X$///,
	". The two names denote the same function."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"eulerCharacteristicStructure Y",
	"chiStructure Y"
	},
    SeeAlso => {eulerCharacteristicCotangent, eulerCharacteristic}
    }

document {
    Key => {eulerCharacteristicCotangent, (eulerCharacteristicCotangent, ZZ, EmbeddedVariety),
	chiCotangent, (chiCotangent, ZZ, EmbeddedVariety)},
    Headline => "the Euler characteristic of the exterior powers of the cotangent bundle",
    Usage => "eulerCharacteristicCotangent(p,Y)\nchiCotangent(p,Y)",
    Inputs => {"p" => ZZ => {"a nonnegative integer"}, "Y" => EmbeddedVariety},
    Outputs => {ZZ => {"the Euler characteristic ", TEX///$\chi(Y,\Omega^p_Y)$///}},
    PARA {"The conormal sequence of ", TEX///$Y$///, " in ", TEX///$X$///,
	" expresses ", TEX///$\Omega^p_Y$///, " through the exterior powers of ", TEX///$\Omega_X$///,
	" and the symmetric powers of the dual of the normal bundle; the alternating sum of the ",
	"corresponding Euler characteristics gives the answer. The alternating sum over ", TEX///$p$///,
	" of these numbers is the topological Euler characteristic of ", TEX///$Y$///,
	". The two names denote the same function."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"eulerCharacteristicCotangent(0,Y)",
	"eulerCharacteristicCotangent(1,Y)"
	},
    SeeAlso => {eulerCharacteristicTangent, hodgeNumbers}
    }

document {
    Key => {eulerCharacteristicTangent, (eulerCharacteristicTangent, ZZ, EmbeddedVariety),
	chiTangent, (chiTangent, ZZ, EmbeddedVariety)},
    Headline => "the Euler characteristic of the exterior powers of the tangent bundle",
    Usage => "eulerCharacteristicTangent(p,Y)\nchiTangent(p,Y)",
    Inputs => {"p" => ZZ => {"a nonnegative integer"}, "Y" => EmbeddedVariety},
    Outputs => {ZZ => {"the Euler characteristic ", TEX///$\chi(Y,\wedge^p T_Y)$///}},
    PARA {"For ", TEX///$p=1$///, " this is the Euler characteristic controlling the deformations of ",
	TEX///$Y$///, "; the numbers ", TEX///$\chi(Y,\wedge^p T_Y)$///,
	" are the alternating sums of the Hochschild numbers computed by ", TO hochschildNumbers,
	". The two names denote the same function."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"eulerCharacteristicTangent(1,Y)"
	},
    SeeAlso => {eulerCharacteristicTangentTwisted, hochschildNumbers}
    }

document {
    Key => {eulerCharacteristicTangentTwisted, (eulerCharacteristicTangentTwisted, ZZ, EmbeddedVariety, HomogeneousVectorBundle)},
    Headline => "the Euler characteristic of the exterior powers of the tangent bundle, twisted by a vector bundle",
    Usage => "eulerCharacteristicTangentTwisted(p,Y,E)",
    Inputs => {
	"p" => ZZ => {"a nonnegative integer"},
	"Y" => EmbeddedVariety,
	"E" => HomogeneousVectorBundle => {"a bundle on the ambient variety of ", TEX///$Y$///}
	},
    Outputs => {ZZ => {"the Euler characteristic ", TEX///$\chi(Y,\wedge^p T_Y \otimes E|_Y)$///}},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"E = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);",
	"eulerCharacteristicTangentTwisted(1,Y,E)"
	},
    SeeAlso => {eulerCharacteristicTangent, hochschildNumbersTwisted}
    }

document {
    Key => {cohomologyRestriction, (cohomologyRestriction, HomogeneousVariety, HomogeneousVectorBundle, FiltrationBundle)},
    Headline => "the cohomology of a homogeneous bundle restricted to a zero locus",
    Usage => "cohomologyRestriction(X,F,G)",
    Inputs => {
	"X" => HomogeneousVariety,
	"F" => HomogeneousVectorBundle => {"a globally generated bundle on ", TEX///$X$///,
	    ", whose zero locus is ", TEX///$Y$///},
	"G" => FiltrationBundle => {"the bundle to be restricted"}
	},
    Outputs => {List => {"the dimensions of the cohomology groups ", TEX///$H^i(Y,G|_Y)$///}},
    PARA {"The Koszul complex of ", TEX///$Y$///, " in ", TEX///$X$///,
	" is twisted by ", TEX///$G$///,
	" and broken up into short exact sequences; the cohomology of ", TEX///$G|_Y$///,
	" is then obtained by walking through the associated long exact sequences. As for ",
	TO (cohomology, FiltrationBundle), ", the answer may contain unknowns which the exact ",
	"sequences do not determine."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"G = filtrationBundle({structureSheaf X,F},X);",
	"cohomologyRestriction(X,F,G)"
	},
    SeeAlso => {(cohomology, FiltrationBundle), (cohomologyRestriction, EmbeddedVariety, FiltrationBundle), eulerCharacteristic}
    }

document {
    Key => (cohomologyRestriction, EmbeddedVariety, FiltrationBundle),
    Headline => "the cohomology of a homogeneous bundle restricted to a zero locus",
    Usage => "cohomologyRestriction(Y,G)",
    Inputs => {
	"Y" => EmbeddedVariety,
	"G" => FiltrationBundle => {"the bundle to be restricted"}
	},
    Outputs => {List => {"the dimensions of the cohomology groups ", TEX///$H^i(Y,G|_Y)$///}},
    PARA {"Equivalent to ", TO (cohomologyRestriction, HomogeneousVariety, HomogeneousVectorBundle, FiltrationBundle),
	", but starting directly from the zero locus ", TEX///$Y$///, " rather than from its defining bundle."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"G = filtrationBundle({structureSheaf X,F},X);",
	"Y = embeddedVariety F;",
	"cohomologyRestriction(Y,G)"
	},
    SeeAlso => {(cohomology, FiltrationBundle), (cohomologyRestriction, HomogeneousVariety, HomogeneousVectorBundle, FiltrationBundle)}
    }

document {
    Key => {hodgeNumbers, (hodgeNumbers, HomogeneousVariety), (hodgeNumbers, EmbeddedVariety)},
    Headline => "the Hodge numbers of a homogeneous variety or of a zero locus",
    Usage => "hodgeNumbers X\nhodgeNumbers Y",
    Inputs => {
	"X" => HomogeneousVariety => {"or, alternatively, an ", TO EmbeddedVariety}
	},
	  PARA {" holds the list of the numbers ", TEX///$h^{p,q}$///, " for ", TEX///$q$///,
	    " from 0 to ", TEX///$\dim X$///, "; for a zero locus, a pair made of such a hash table ",
	    "and of a hash table of bounds for the unknowns possibly appearing in it"},	
    PARA {"For a homogeneous variety the numbers are read off from the cohomology of the exterior ",
	"powers of the cotangent bundle; for a zero locus one combines the conormal sequence with the ",
	"Koszul complex, and the long exact sequences involved may leave some of the numbers ",
	"undetermined, in which case they are returned as polynomials in some auxiliary variables ",
	"whose range is recorded in the second output. Only the rows up to the middle one are ",
	"computed, the remaining ones being determined by Serre duality; use ",
	TO (hodgeNumbers, ZZ, HomogeneousVariety), " to limit the computation to fewer rows."},
    EXAMPLE {
	"hn = hodgeNumbers Gr{1,3};",
	"peek hn"
	},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"hn = hodgeNumbers Y;",
	"peek first hn"
	},
    SeeAlso => {(hodgeNumbers, ZZ, HomogeneousVariety), displayHN, hochschildNumbers, invariants}
    }

document {
    Key => {(hodgeNumbers, ZZ, HomogeneousVariety), (hodgeNumbers, ZZ, EmbeddedVariety)},
    Headline => "the Hodge numbers of a homogeneous variety or of a zero locus, up to a given row",
    Usage => "hodgeNumbers(k,X)\nhodgeNumbers(k,Y)",
    Inputs => {
	"k" => ZZ => {"compute only the rows up to the ", TEX///$k$///, "-th one of the Hodge diamond"},
	"X" => HomogeneousVariety => {"or, alternatively, an ", TO EmbeddedVariety}
	},
    EXAMPLE {
	"hn = hodgeNumbers(1,Gr{1,4});",
	"peek hn"
	},
    SeeAlso => {hodgeNumbers, displayHN}
    }

document {
    Key => {displayHN, (displayHN, HomogeneousVariety), (displayHN, EmbeddedVariety)},
    Headline => "the Hodge diamond of a homogeneous variety or of a zero locus",
    Usage => "displayHN X\ndisplayHN Y",
    Inputs => {
	"X" => HomogeneousVariety => {"or, alternatively, an ", TO EmbeddedVariety}
	},
    PARA {"The rows below the middle one are filled in by Serre duality; use ",
	TO (displayHN, ZZ, HomogeneousVariety), " to limit the computation to fewer rows, which are ",
	"then marked with an asterisk."},
    EXAMPLE {
	"displayHN Gr{1,3}"
	},
    SeeAlso => {hodgeNumbers, (displayHN, ZZ, HomogeneousVariety), displayHochN}
    }

document {
    Key => {(displayHN, ZZ, HomogeneousVariety), (displayHN, ZZ, EmbeddedVariety)},
    Headline => "the Hodge diamond of a homogeneous variety or of a zero locus, up to a given row",
    Usage => "displayHN(k,X)\ndisplayHN(k,Y)",
    Inputs => {
	"k" => ZZ => {"display only the rows up to the ", TEX///$k$///, "-th one"},
	"X" => HomogeneousVariety => {"or, alternatively, an ", TO EmbeddedVariety}
	},
    Outputs => {
	Net => {"the same diamond as ", TO displayHN, ", truncated to the first ", TEX///$k$///, " rows"}
	},
    EXAMPLE {
	"displayHN(1,Gr{1,4})"
	},
    SeeAlso => {displayHN, hodgeNumbers}
    }

document {
    Key => {[hodgeNumbers, Verbose], [hochschildNumbers, Verbose], [hochschildNumbersTwisted, Verbose]},
    Headline => "whether to print progress messages and timing information",
    Usage => "hodgeNumbers(X, Verbose => b)\nhochschildNumbers(Y, Verbose => b)\nhochschildNumbersTwisted(Y,E, Verbose => b)",
    PARA {"When set to ", TT "true", ", the function prints a short message before each of its main ",
	"computational steps (building the Koszul complex, taking exterior or symmetric powers, ",
	"computing the cohomologies, determining the bounds on the unknowns), together with the time ",
	"each step takes. This is useful to monitor long computations on varieties of large dimension. ",
	"Its default value is ", TT "false", "."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"hn = hochschildNumbers(Y, Verbose => true);"
	},
    SeeAlso => {hodgeNumbers, hochschildNumbers, hochschildNumbersTwisted}
    }

document {
    Key => [displayHN, DisplayBounds],
    Headline => "whether to also return the bounds on the undetermined Hodge numbers",
    Usage => "displayHN(Y, DisplayBounds => true)\ndisplayHN(k,Y, DisplayBounds => true)",
    PARA {"For a zero locus (an ", TO EmbeddedVariety, ") the Hodge diamond computed via ", TO hodgeNumbers,
	" may contain undetermined entries, marked with an asterisk. Setting this option to ", TT "true",
	" makes ", TT "displayHN", " return, alongside the diamond, the hash table of bounds for those ",
	"unknowns, exactly as produced by ", TO hodgeNumbers, ". Its default value is ", TT "false", "."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"(diamond,bounds) = displayHN(Y, DisplayBounds => true);",
	"diamond",
	"peek bounds"
	},
    SeeAlso => {displayHN, hodgeNumbers}
    }

document {
    Key => {hochschildNumbers, (hochschildNumbers, EmbeddedVariety)},
    Headline => "the Hochschild numbers of a zero locus",
    Usage => "(h,b) = hochschildNumbers Y",
    Inputs => {"Y" => EmbeddedVariety},
    Outputs => {
	Sequence => {"a pair ", TEX///$(h,b)$///, ", where ", TEX///$h$///,
	    " is a hash table whose key ", TEX///$p$///, " holds the list of the dimensions of ",
	    TEX///$H^q(Y,\wedge^p T_Y)$///, ", and ", TEX///$b$///,
	    " is a hash table of bounds for the unknowns possibly appearing in them"}
	},
    PARA {"These are the numbers appearing in the Hochschild-Kostant-Rosenberg decomposition of the ",
	"Hochschild cohomology of ", TEX///$Y$///, "; the group ", TEX///$H^1(Y,T_Y)$///,
	" governs the deformations of ", TEX///$Y$///, " and ", TEX///$H^0(Y,T_Y)$///,
	" is the Lie algebra of its automorphism group. They are computed from the exterior powers ",
	"of the tangent bundle of the ambient variety and the symmetric powers of the normal bundle, ",
	"combined through the Koszul complex."},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"hn = hochschildNumbers Y;",
	"peek first hn"
	},
    SeeAlso => {displayHochN, hochschildNumbersTwisted, eulerCharacteristicTangent}
    }

document {
    Key => {hochschildNumbersTwisted, (hochschildNumbersTwisted, EmbeddedVariety, HomogeneousVectorBundle)},
    Headline => "the Hochschild numbers of a zero locus, twisted by a homogeneous bundle",
    Usage => "(h,b) = hochschildNumbersTwisted(Y,E)",
    Inputs => {
	"Y" => EmbeddedVariety,
	"E" => HomogeneousVectorBundle => {"a bundle on the ambient variety of ", TEX///$Y$///}
	},
    Outputs => {
	Sequence => {"a pair whose first entry is a hash table holding the dimensions of ",
	    TEX///$H^q(Y,\wedge^p T_Y \otimes E|_Y)$///,
	    " and whose second entry records the bounds for the unknowns possibly appearing in them"}
	},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"E = homogeneousVectorBundle({weight(R,{0,1,0})},{1},X);",
	"hn = hochschildNumbersTwisted(Y,E);",
	"peek first hn"
	},
    SeeAlso => {hochschildNumbers, eulerCharacteristicTangentTwisted}
    }

document {
    Key => {displayHochN, (displayHochN, EmbeddedVariety)},
    Headline => "the Hochschild parallelogram of a zero locus",
    Usage => "displayHochN Y",
    Inputs => {"Y" => EmbeddedVariety},
    Outputs => {
	Net => {"the numbers ", TEX///$h^q(Y,\wedge^p T_Y)$///,
	    " arranged in a parallelogram, with ", TEX///$p$///, " running along the columns and ",
	    TEX///$q$///, " along the rows"}
	},
    EXAMPLE {
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"Y = embeddedVariety F;",
	"displayHochN Y"
	},
    SeeAlso => {hochschildNumbers, displayHN}
    }

document {
    Key => (hilbertPolynomial, HomogeneousVectorBundle, HomogeneousVectorBundle),
    Headline => "the Hilbert polynomial of a homogeneous bundle with respect to an ample line bundle",
    Usage => "hilbertPolynomial(E,L)",
    Inputs => {
	"E" => HomogeneousVectorBundle,
	"L" => HomogeneousVectorBundle => {"an ample line bundle on the same variety"}
	},
    Outputs => {
	{"the polynomial ", TEX///$k \mapsto \chi(E \otimes L^{k})$///,
	    " (a ", TO RingElement, ")"}
	},
    PARA {"The polynomial is obtained by interpolation from enough Euler characteristics, computed ",
	"by Bott's theorem. It is used by ", TO volumeFano, "."},
    EXAMPLE {
	"X = Gr{1,4};",
	"R = X#\"rootSystem\";",
	"L = homogeneousVectorBundle({weight(R,{1,0,0})},{1},X);",
	"hilbertPolynomial(structureSheaf X, L)"
	},
    PARA {EM "Caveat: ", "an error is raised if ", TEX///$L$///, " does not have rank one or is not ample."},
    SeeAlso => {volumeFano, isAmple, (hilbertPolynomial, HomogeneousVectorBundle, HomogeneousVectorBundle, EmbeddedVariety)}
    }

document {
    Key => (hilbertPolynomial, HomogeneousVectorBundle, HomogeneousVectorBundle, EmbeddedVariety),
    Headline => "the Hilbert polynomial of a homogeneous bundle restricted to a zero locus",
    Usage => "hilbertPolynomial(E,L,Y)",
    Inputs => {
	"E" => HomogeneousVectorBundle,
	"L" => HomogeneousVectorBundle => {"an ample line bundle on the ambient variety of ", TEX///$Y$///},
	"Y" => EmbeddedVariety => {"a zero locus contained in that variety"}
	},
    Outputs => {
	{"the polynomial ", TEX///$k \mapsto \chi(E|_Y \otimes L|_Y^{k})$///,
	    ", computed on ", TEX///$Y$///, " (a ", TO RingElement, ")"}
	},
    PARA {"The polynomial is obtained by interpolation from enough Euler characteristics, computed ",
	"through the Koszul complex of ", TEX///$Y$///, ". It is used by ", TO volumeFano, "."},
    PARA {EM "Caveat: ", "an error is raised if ", TEX///$L$///, " does not have rank one or is not ample."},
    SeeAlso => {volumeFano, isAmple, (hilbertPolynomial, HomogeneousVectorBundle, HomogeneousVectorBundle)}
    }

document {
    Key => {volumeFano, (volumeFano, HomogeneousVariety), (volumeFano, EmbeddedVariety)},
    Headline => "the anticanonical volume of a Fano variety",
    Usage => "volumeFano X\nvolumeFano Y",
    Inputs => {
	"X" => HomogeneousVariety => {"or, alternatively, an ", TO EmbeddedVariety,
	    " whose anticanonical bundle is ample"}
	},
    Outputs => {
	ZZ => {"the volume ", TEX///$(-K)^{\dim}$///, ", that is, ", TEX///$\dim!$///,
	    " times the leading coefficient of the Hilbert polynomial of the structure sheaf with ",
	    "respect to the anticanonical bundle"}
	},
    PARA {"Every rational homogeneous variety is Fano, so the volume is always defined in the first ",
	"form. In the second form the class ", TEX///$-K_Y = c_1(X) - c_1(F)$///,
	" is required to be the restriction of an ample class on the ambient variety, and an error is ",
	"raised otherwise. This property is stronger than the property of ", TEX///$Y$///, " being actually Fano."},
    EXAMPLE {
	"volumeFano Gr{1,4}",
	"R = rootSystemA(3);",
	"X = homogeneousVariety(R,set{2,3});",
	"F = homogeneousVectorBundle({weight(R,{1,0,0})},{2},X);",
	"volumeFano embeddedVariety F"
	},
    SeeAlso => {(hilbertPolynomial, HomogeneousVectorBundle, HomogeneousVectorBundle), invariants}
    }

document {
    Key => {invariants, (invariants, HomogeneousVariety), (invariants, EmbeddedVariety)},
    Headline => "a list of invariants of a homogeneous variety or of a zero locus",
    Usage => "(vol,chiT,chiCot,hodge) = invariants X\n(vol,chiT,chiCot,hodge) = invariants Y",
    Inputs => {"X" => HomogeneousVariety => {"or, alternatively, an ", TO EmbeddedVariety}},
    Outputs => {
	Sequence => {"a quadruple made of the anticanonical volume, the list of the Euler ",
	    "characteristics ", TEX///$\chi(\wedge^k T)$///, " for ", TEX///$k$///,
	    " from 1 to the value of ", TO maxChiT, ", the list of the Euler characteristics ",
	    TEX///$\chi(\Omega^k)$///, " for ", TEX///$k$///, " from 0 to half the dimension, and the ",
	    "pair of hash tables returned by ", TO hodgeNumbers}
	},
    PARA {"This is a convenience function collecting in one call the invariants one usually wants ",
	"when classifying zero loci of homogeneous bundles. For a zero locus whose anticanonical class ",
	"is not ample the volume is returned as -1 rather than raising an error."},
    EXAMPLE {
	"(vol,chiT,chiCot,hodge) = invariants(Gr{1,3}, doHodge => false);",
	"vol",
	"chiT",
	"chiCot"
	},
    SeeAlso => {volumeFano, hodgeNumbers, eulerCharacteristicTangent, doHodge, maxChiT}
    }

document {
    Key => [invariants, doHodge],
    Headline => "whether to compute the Hodge numbers",
    Usage => "invariants(X, doHodge => b)",
    PARA {"The Hodge numbers are by far the most expensive part of ", TO invariants,
	"; setting this option to ", TT "false", " skips them, and the last entry of the output is ",
	"then a pair of empty hash tables. Its default value is ", TT "true", "."},
    EXAMPLE {
	"(vol,chiT,chiCot,hodge) = invariants(Gr{1,4}, doHodge => false, maxChiT => 2);",
	"chiT",
	"peek first hodge"
	},
    SeeAlso => {invariants, maxChiT}
    }

document {
    Key => [invariants, maxChiT],
    Headline => "how many exterior powers of the tangent bundle to use",
    Usage => "invariants(X, maxChiT => k)",
    PARA {"The second entry of the output of ", TO invariants, " is the list of the Euler ",
	"characteristics ", TEX///$\chi(\wedge^k T)$///, " for ", TEX///$k$///,
	" from 1 to this value. Its default value is 1, which computes only ", TEX///$\chi(T)$///,
	", the one governing deformations."},
    EXAMPLE {
	"(vol,chiT,chiCot,hodge) = invariants(Gr{1,3}, doHodge => false, maxChiT => 2);",
	"chiT"
	},
    SeeAlso => {invariants, doHodge}
    }
