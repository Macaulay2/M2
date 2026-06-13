
------------------------------------------------------------------------
---------------------------- Documentation -----------------------------
------------------------------------------------------------------------

beginDocumentation()

document {Key => SpecialFanoFourfolds,
Headline => "A package for working with Hodge-special fourfolds",
PARA {"This package contains several tools related to the rationality problem for cubic fourfolds, Gushel-Mukai fourfolds, and some other special Fano fourfolds. See ",HREF{"https://arxiv.org/abs/2204.11518","arXiv:2204.11518"}," for some applications."},

SUBSECTION "Version History", UL {
{"v. 1.0 (Oct 2019) - Initial version."},
{"v. 2.7.1 (May 2023) - Consolidated stable version."},
{"v. 2.8 (Apr 2026) - Updates and new features."}},
PARA {"The following papers have made use of this package."},
References => UL{
{"F. Russo and G. Staglianò, ",EM"On complete intersections of three quadrics in P^7",", available at ",HREF{"https://arxiv.org/abs/2312.01773","arXiv:2312.01773"}," (2023)."},
{"M. Hoff and G. Staglianò, ",EM"Explicit constructions of K3 surfaces and unirational Noether-Lefschetz divisors",", available at ",HREF{"https://arxiv.org/abs/2110.15819","arXiv:2110.15819"}," (2021)."},
{"G. Staglianò, ",EM"Some new rational Gushel fourfolds",", available at ",HREF{"https://arxiv.org/abs/2003.07809","arXiv:2003.07809"}," (2020)."},
{"G. Staglianò, ",EM"On some families of Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/2002.07026","arXiv:2002.07026"}," (2020)."},
{"M. Hoff and G. Staglianò, ",EM"New examples of rational Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/1910.12838","arXiv:1910.12838"}," (2020)."},
{"F. Russo and G. Staglianò, ",EM"Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds",", available at ",HREF{"https://arxiv.org/abs/1909.01263","arXiv:1909.01263"}," (2020)."},
{"F. Russo and G. Staglianò, ",EM"Explicit rationality of some cubic fourfolds",", available at ",HREF{"https://arxiv.org/abs/1811.03502","arXiv:1811.03502"}," (2019)."},
{"F. Russo and G. Staglianò, ",EM"Congruences of 5-secant conics and the rationality of some admissible cubic fourfolds",", available at ",HREF{"https://arxiv.org/abs/1707.00999","arXiv:1707.00999"}," (2018)."}}}

document {Key => {GushelMukaiFourfold},
Headline => "the class of all special Gushel-Mukai fourfolds",
PARA{"The general type of Gushel-Mukai fourfold (called ",EM "ordinary",") can be realized as the intersection of a smooth del Pezzo fivefold ", TEX///$\mathbb{G}(1,4)\cap\mathbb{P}^8\subset \mathbb{P}^8$///, " with a quadric hypersurface in ", TEX///$\mathbb{P}^8$///, ". A Gushel-Mukai fourfold is said to be ", EM"special", " if it contains a surface whose cohomology class ", EM "does not come", " from the Grassmannian ", TEX///$\mathbb{G}(1,4)$///, ". The special Gushel-Mukai fourfolds are parametrized by a countable union of (not necessarily irreducible) hypersurfaces in the corresponding moduli space, labelled by the integers ", TEX///$d \geq 10$///, " with ", TEX///$d = 0, 2, 4\ ({mod}\ 8)$///, "; the number ",TEX///$d$///," is called the discriminant of the fourfold. For precise definition and results, we refer mainly to the paper ", HREF{"https://arxiv.org/abs/1302.1398", "Special prime Fano fourfolds of degree 10 and index 2"}, ", by O. Debarre, A. Iliev, and L. Manivel."},
PARA{"An object of the class ", TO GushelMukaiFourfold, " is basically represented by a pair ", TEX///(S,X)///, ", where ", TEX///$X$///, " is a Gushel-Mukai fourfold and ", TEX///$S$///, " is a surface contained in ", TEX///$X$///, ".  The main constructor for the objects of the class is the function ", TO gushelMukaiFourfold,"."},
SeeAlso => {(discriminant,GushelMukaiFourfold)}}

document {Key => {(discriminant, CubicFourfold), (discriminant, HodgeSpecialFourfold)},
Headline => "discriminant of a special cubic fourfold",
Usage => "discriminant X",
Inputs => {"X" => CubicFourfold},
Outputs => {ZZ => {"the discriminant of ", TEX///$X$///}},
PARA{"This calculation passes through the determination of the topological Euler characteristic of the surface contained in the fourfold, which is obtained thanks to the functions ", TO EulerCharacteristic, " and ", TO Euler, " (the option ", TT "Algorithm", " allows you to select the method)."},
EXAMPLE {"X = cubicFourfold \"quintic del Pezzo surface\";", "discriminant X"},
SeeAlso => {(discriminant, GushelMukaiFourfold)}}

document {Key => {(discriminant, GushelMukaiFourfold)},
Headline => "discriminant of a special Gushel-Mukai fourfold",
Usage => "discriminant X",
Inputs => {"X" => GushelMukaiFourfold},
Outputs => {ZZ => {"the discriminant of ", TEX///$X$///}},
PARA{"This function applies a formula given in Section 7 of the paper ", HREF{"https://arxiv.org/abs/1302.1398", "Special prime Fano fourfolds of degree 10 and index 2"}, ", obtaining the data required through the functions ", TO cycleClass, ", ", TO EulerCharacteristic, " and ", TO Euler, " (the option ", TT "Algorithm", " allows you to select the method)."},
EXAMPLE {"X = gushelMukaiFourfold \"tau-quadric\";", "discriminant X"},
SeeAlso => {(discriminant, CubicFourfold)}}

undocumented{(expression, GushelMukaiFourfold), (describe, GushelMukaiFourfold)}

document {Key => {Verbose, [cubicFourfold, Verbose], [gushelMukaiFourfold, Verbose], [mirrorFourfold, Verbose], [specialFourfold, Verbose], [parameterCount, Verbose],  [associatedK3surface, Verbose], [associatedCastelnuovoSurface, Verbose], [polarizedK3surface, Verbose], [detectCongruence, Verbose], [trisecantFlop, Verbose]},
Headline => "request verbose feedback"}

document {Key => {gushelMukaiFourfold, (gushelMukaiFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (gushelMukaiFourfold, Ideal, Ideal), [gushelMukaiFourfold, InputCheck]},
Headline => "make a special Gushel-Mukai fourfold",
Usage => "gushelMukaiFourfold(S,X)",
Inputs => {"S" => EmbeddedProjectiveVariety => {"a smooth irreducible surface ", TEX///$S\subset\mathbb{P}^8$///}, "X" => EmbeddedProjectiveVariety => {"a smooth prime Fano fourfold ", TEX///$X\subset \mathbb{P}^8$///, " of degree 10 and sectional genus 6, which contains the surface ", TEX///$S$///}},
Outputs => {GushelMukaiFourfold => {"the special Gushel-Mukai fourfold corresponding to the pair ", TEX///$(S,X)$///}},
PARA{"In the following example, we define a Gushel-Mukai fourfold containing a so-called ", TEX///$\tau$///, "-quadric."},
EXAMPLE {"K = ZZ/33331; x := gens ring PP_K^8;", "S = projectiveVariety ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8);", "X = projectiveVariety ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8);", "F = gushelMukaiFourfold(S,X);", "describe F", "assert(F == X)"},
SeeAlso => {(gushelMukaiFourfold, EmbeddedProjectiveVariety), (gushelMukaiFourfold, String, Ring), specialFourfold}}

document {Key => {(gushelMukaiFourfold, EmbeddedProjectiveVariety),(gushelMukaiFourfold, Ideal)},
Headline => "random special Gushel-Mukai fourfold",
Usage => "gushelMukaiFourfold S
gushelMukaiFourfold (S%Y)",
Inputs => {"S" => EmbeddedProjectiveVariety => {"a smooth irreducible surface ",TEX///$S$///," which is a ",TO2{(symbol %,MultiprojectiveVariety,MultiprojectiveVariety),"subvariety"}," of a del Pezzo fivefold/sixfold ",TEX///$Y$///,"; alternatively, you can pass the ideal of ",TEX///$S$///," in ",TEX///$Y$///," (e.g., an ideal in the ring ", TO Grass, TEX///$(1,4)$///, ")"}},
Outputs => {GushelMukaiFourfold => {"a random special Gushel-Mukai fourfold containing the given surface"}},
EXAMPLE {"Y = GG(ZZ/33331,1,4);", "-- cubic scroll in G(1,4)"|newline|"S = schubertCycle({2,0},Y) * schubertCycle({1,0},Y) * schubertCycle({1,0},Y);", "X = gushelMukaiFourfold S;", "discriminant X"},
SeeAlso => {(gushelMukaiFourfold, String, Ring),(symbol %,MultiprojectiveVariety,MultiprojectiveVariety)}}

document {Key => {(gushelMukaiFourfold, String, Ring), (gushelMukaiFourfold, String)},
Headline => "random special Gushel-Mukai fourfold of a given type",
Usage => "gushelMukaiFourfold(n,K)
gushelMukaiFourfold n",
Inputs => {"n" => String => {"the name of some known type of Gushel-Mukai fourfolds"}, "K" => {"the coefficient ring"}},
Outputs => {GushelMukaiFourfold => {"a random special Gushel-Mukai fourfold of the indicated type over ",TT"K"}},
EXAMPLE {"X = gushelMukaiFourfold(\"cubic scroll\",ZZ/65521);", "describe X"},
References => UL{
{"O. Debarre, A. Iliev, and L. Manivel, ",EM"Special prime Fano fourfolds of degree 10 and index 2",", available at ",HREF{"https://arxiv.org/abs/1302.1398","arXiv:1302.1398"}," (2014)."},
{"G. Staglianò, ",EM"On some families of Gushel-Mukai fourfolds",", available at ",HREF{"https://arxiv.org/abs/2002.07026","arXiv:2002.07026"}," (2020)."}},
SeeAlso => {(gushelMukaiFourfold, EmbeddedProjectiveVariety), GMtables}}

document {Key => {toGrass, (toGrass, GushelMukaiFourfold)},
Headline => "Gushel morphism from a GM fourfold to GG(1,4)",
Usage => "toGrass X",
Inputs => {"X" => GushelMukaiFourfold},
Outputs => {MultirationalMap => {"a linear morphism from ", TEX///$X$///, " into the ",TO2{GrassmannianVariety,"Grassmannian"}," ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plücker embedded, which is an embedding when ",TEX///$X$///," is of ordinary type"}},
EXAMPLE {"x := gens ring PP_(ZZ/33331)^8;", "X = gushelMukaiFourfold(ideal(x_6-x_7, x_5, x_3-x_4, x_1, x_0-x_4, x_2*x_7-x_4*x_8), ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8, x_0^2+x_0*x_1+x_1^2+x_0*x_2+2*x_0*x_3+x_1*x_3+x_2*x_3+x_3^2-x_0*x_4-x_1*x_4-2*x_2*x_4-x_3*x_4-2*x_4^2+x_0*x_5+x_2*x_5+x_5^2+2*x_0*x_6+x_1*x_6+2*x_2*x_6+x_3*x_6+x_5*x_6+x_6^2-3*x_4*x_7+2*x_5*x_7-x_7^2+x_1*x_8+x_3*x_8-3*x_4*x_8+2*x_5*x_8+x_6*x_8-x_7*x_8));", "time toGrass X", "show oo"},
SeeAlso => {(toGrass, EmbeddedProjectiveVariety), (symbol ===>, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)}}

document {Key => {(toGrass, EmbeddedProjectiveVariety)},
Headline => "embedding of an ordinary Gushel-Mukai fourfold or a del Pezzo variety into GG(1,4)",
Usage => "toGrass X",
Inputs => {"X" => EmbeddedProjectiveVariety => {"an ordinary Gushel-Mukai fourfold, or a del Pezzo variety of dimension at least 4 (e.g., a sixfold projectively equivalent to ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///,")"}},
Outputs => {MultirationalMap => {"an embedding of ", TEX///$X$///, " into the ",TO2{GrassmannianVariety,"Grassmannian"}," ", TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///, ", Plücker embedded"}},
EXAMPLE {"x = gens ring PP_(ZZ/33331)^8;", "X = projectiveVariety ideal(x_4*x_6-x_3*x_7+x_1*x_8, x_4*x_5-x_2*x_7+x_0*x_8, x_3*x_5-x_2*x_6+x_0*x_8+x_1*x_8-x_5*x_8, x_1*x_5-x_0*x_6+x_0*x_7+x_1*x_7-x_5*x_7, x_1*x_2-x_0*x_3+x_0*x_4+x_1*x_4-x_2*x_7+x_0*x_8);", "time toGrass X", "show oo"},
SeeAlso => {(toGrass,GushelMukaiFourfold), (symbol ===>, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)}}

undocumented{(cycleClass, GushelMukaiFourfold)}

document {Key => {GMtables, (GMtables, ZZ, Ring), (GMtables, ZZ), [GMtables, Verify]},
Headline => "make examples of reducible subschemes of P^5",
Usage => "GMtables(i,K)",
Inputs => {"i" => ZZ => {"an integer between 1 and 21"}, "K" => Ring => {"the coefficient ring"}},
Outputs => {{"a triple of ",TO2{EmbeddedProjectiveVariety,"varieties"},", ",TEX///$(B,V,C)$///, ", which represents a reducible subscheme of ", TEX///$\mathbb{P}^5$///, ", in accordance with the 21 examples listed in Table 2 of the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."}},
EXAMPLE {"(B,V,C) := GMtables(1,ZZ/33331)", "B * V == C"},
PARA{"The corresponding example of fourfold reported in Table 1 of the aforementioned paper can be obtained as follows."},
EXAMPLE {"psi = rationalMap(ideal B,Dominant=>2);", "X = gushelMukaiFourfold psi ideal V;"},
PARA{"This is basically the same as doing this:"},
EXAMPLE {"gushelMukaiFourfold(\"1\",ZZ/33331);"},
SeeAlso => {(gushelMukaiFourfold,String,Ring),(gushelMukaiFourfold,Array,Array)}}

undocumented {(GMtables, Ring, String), (GMtables,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety,EmbeddedProjectiveVariety)};

document {Key => {parameterCount, (parameterCount, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (parameterCount, HodgeSpecialFourfold)},
Headline => "count of parameters",
Usage => "parameterCount(S,X)",
Inputs => {"S" => EmbeddedProjectiveVariety, "X" => EmbeddedProjectiveVariety => {"such that ", TEX///$S\subseteq X$///}},
Outputs => {{"a count of parameters to estimate the dimensions of the corresponding Hilbert schemes"}},
PARA{"See ",TO (parameterCount, CubicFourfold)," and ", TO (parameterCount, GushelMukaiFourfold)," for more precise applications of this function."},
PARA{"The following calculation shows that the family of complete intersections of 3 quadrics in ",TEX///$\mathbb{P}^5$///," containing a rational normal quintic curve has codimension 1 in the space of all such complete intersections."},
EXAMPLE {"K = ZZ/33331; S = PP_K^(1,5);", "X = random({{2},{2},{2}},S);", "time parameterCount(S,X,Verbose=>true)"},
SeeAlso => {(parameterCount, CubicFourfold), (parameterCount, GushelMukaiFourfold), normalSheaf}}

document {Key => {(parameterCount, CubicFourfold)},
Headline => "count of parameters in the moduli space of GM fourfolds",
Usage => "parameterCount X",
Inputs => {"X" => CubicFourfold => {"a special cubic fourfold containing a surface ", TEX///$S$///}},
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of cubic fourfolds of the locus of cubic fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme containing ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/P^5}(3)), h^0(N_{S/P^5}), h^0(N_{S/X}))$///}},
PARA{"This function implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/1503.05256", "Unirationality of moduli spaces of special cubic fourfolds and K3 surfaces"}, ", by H. Nuer."},
PARA{"Below, we show that the closure of the locus of cubic fourfolds containing a Veronese surface has codimension at most one (hence exactly one) in the moduli space of cubic fourfolds. Then, by the computation of the discriminant, we deduce that the cubic fourfolds containing a Veronese surface describe the Hassett's divisor ", TEX///$\mathcal{C}_{20}$///},
EXAMPLE {"K = ZZ/33331; V = PP_K^(2,2);", "X = cubicFourfold V;", "time parameterCount(X,Verbose=>true)", "discriminant X"},
SeeAlso => {(parameterCount, GushelMukaiFourfold), normalSheaf}}

document {Key => {(parameterCount, GushelMukaiFourfold)},
Headline => "count of parameters in the moduli space of GM fourfolds",
Usage => "parameterCount X",
Inputs => {"X" => GushelMukaiFourfold => {"a special GM fourfold containing a surface ", TEX///$S$///, " and contained in a del Pezzo fivefold ", TEX///$Y$///}},
Outputs => {ZZ => {"an upper bound for the codimension in the moduli space of GM fourfolds of the locus of GM fourfolds that contain a surface belonging to the same irreducible component of the Hilbert scheme of ", TEX///$Y$///, " that contains ", TEX///$[S]$///}, Sequence => {"the triple of integers: ", TEX///$(h^0(I_{S/Y}(2)), h^0(N_{S/Y}), h^0(N_{S/X}))$///}},
PARA{"This function implements a parameter count explained in the paper ", HREF{"https://arxiv.org/abs/2002.07026", "On some families of Gushel-Mukai fourfolds"}, "."},
PARA{"Below, we show that the closure of the locus of GM fourfolds containing a cubic scroll has codimension at most one (hence exactly one) in the moduli space of GM fourfolds."},
EXAMPLE {"G = GG(ZZ/33331,1,4);", "S = (schubertCycle({2,0},G) * random({{1},{1}},0_G))%G", "X = gushelMukaiFourfold S;", "time parameterCount(X,Verbose=>true)", "discriminant X"},
SeeAlso => {(parameterCount, CubicFourfold), normalSheaf}}

document {Key => {normalSheaf, (normalSheaf, EmbeddedProjectiveVariety), (normalSheaf, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)},
Headline => "normal sheaf",
Usage => "normalSheaf X"|newline|"normalSheaf(X % Y)"|newline|"normalSheaf(X,Y)",
Inputs => {"X" => EmbeddedProjectiveVariety, "Y" => EmbeddedProjectiveVariety => {" such that ",TEX///$X\subset Y$///," (if not given, it is taken to be the ",TO2{ambientVariety,"ambient variety"}," of ",TEX///$X$///,")"}},
Outputs => {CoherentSheaf => {"the normal sheaf ", TEX///$\mathcal{N}_{X, Y}$///, " of ", TEX///$X$///, " in ", TEX///$Y$///}},
EXAMPLE {"X = PP_(ZZ/65521)^(2,2);", "Y = random(2,X);", "N = normalSheaf X;", "N' = normalSheaf(X,Y);", "rank HH^0 N", "rank HH^0 N'"}}

undocumented {(normalSheaf, MultiprojectiveVariety), (normalSheaf, MultiprojectiveVariety, MultiprojectiveVariety)}

document {Key => {isAdmissible, (isAdmissible, ZZ), (isAdmissible, CubicFourfold)},
Headline => "whether an integer is admissible (in the sense of the theory of cubic fourfolds)",
Usage => "isAdmissible d",
Inputs => {"d" => ZZ},
Outputs => {Boolean => {"whether ", TT"d", " is admissible, i.e., it is an even integer ", TT"d>6", " which is not divisible by 4, 9 or any odd prime congruent to 2 modulo 3"}},
EXAMPLE{"select(150,isAdmissible)"},
SeeAlso => {isAdmissibleGM}}

document {Key => {isAdmissibleGM, (isAdmissibleGM, ZZ), (isAdmissibleGM, GushelMukaiFourfold)},
Headline => "whether an integer is admissible (in the sense of the theory of GM fourfolds)",
Usage => "isAdmissibleGM d",
Inputs => {"d" => ZZ},
Outputs => {Boolean => {"whether ",TEX///$d$///," is an integer ",TEX///$>$///," 8 and ",TEX///$\equiv$///," 2 or 4 (mod 8) such that the only odd primes that divide ",TEX///$d$///," are ",TEX///$\equiv$///," 1 (mod 4). In other words, whether a GM fourfold of discriminant ", TT"d", " has an associated K3 surface."}},
EXAMPLE{"select(140,isAdmissibleGM)"},
SeeAlso => {isAdmissible}}

document {Key => {CongruenceOfCurves},
Headline => "the class of all congruences of secant curves to surfaces",
PARA{"Objects of this type are created by ",TO detectCongruence,"."}}

document {Key => {(symbol SPACE, CongruenceOfCurves, EmbeddedProjectiveVariety), (symbol SPACE, CongruenceOfCurves, Ideal)},
Headline => "get the curve of a congruence passing through a point",
Usage => "f(p)",
Inputs => {"f" => CongruenceOfCurves => {"a congruence of curves to a surface inside a variety ", TEX///$Y$///}, "p" => EmbeddedProjectiveVariety => {"a general point on ",TEX///$Y$///," (that is, a point on ",TEX///$Y$///," outside a certain proper Zariski closed subset)"}},
Outputs => {EmbeddedProjectiveVariety => {"the unique curve of the congruence ", TEX///$f$///, " that passes through ", TEX///$p$///}},
EXAMPLE {"X = cubicFourfold surface {3,4};", "f = detectCongruence(X,1);","C = f point ambient X;","member(C,f)","assert oo"},
SeeAlso => {detectCongruence, (member, EmbeddedProjectiveVariety, CongruenceOfCurves)}}

document {Key => {(map, CongruenceOfCurves)},
Headline => "compute the parameter space of a congruence",
Usage => "map f",
Inputs => {"f" => CongruenceOfCurves => {"a congruence of curves to a surface inside a variety ", TEX///$Y$///}},
Outputs => {MultirationalMap => {"a dominant map from ",TEX///$Y$///," to the parameter space of ",TEX///$f$///," whose general fibers are the curves of the congruence"}},
EXAMPLE {"S = PP_(ZZ/65521)[2,2];","Y = ambient S;","X = cubicFourfold S;","f = detectCongruence(X,1);","F = map f;","Q = target F","f;","p = point Y;","assert(f p == F^* F p)"},
SeeAlso => {detectCongruence}}

document {Key => {(member, EmbeddedProjectiveVariety, CongruenceOfCurves)},
Headline => "test membership in a congruence of curves",
Usage => "member(C,f)",
Inputs => {"C" => EmbeddedProjectiveVariety => {"a curve"}, "f" => CongruenceOfCurves},
Outputs => {Boolean => {"whether the curve ",TEX///$C$///," belongs to the congruence ", TEX///$f$///}},
SeeAlso => {(symbol SPACE, CongruenceOfCurves, EmbeddedProjectiveVariety)}}

undocumented{(toString, CongruenceOfCurves), (net, CongruenceOfCurves), (texMath, CongruenceOfCurves)}

document {Key => {detectCongruence, (detectCongruence, HodgeSpecialFourfold, ZZ), (detectCongruence, HodgeSpecialFourfold)},
Headline => "detect and return a congruence of secant curves to a surface",
PARA{"See ",TO (detectCongruence, CubicFourfold)," and ",TO (detectCongruence, GushelMukaiFourfold),"."}}

document {Key => {(detectCongruence, CubicFourfold, ZZ), (detectCongruence, CubicFourfold)},
Headline => "detect and return a congruence of (3e-1)-secant curves of degree e",
Usage => "detectCongruence X"|newline|"detectCongruence(X,e)",
Inputs => {"X" => CubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///}, "e" => ZZ => {"a positive integer (optional but recommended)"}},
Outputs => {CongruenceOfCurves => {"that is a function which takes a general point ", TEX///$p\in\mathbb{P}^5$///, " (that is, outside a certain proper Zariski closed subset of ",TEX///$\mathbb{P}^5$///,") and returns the unique rational curve of degree ", TEX///$e$///, ", ", TEX///$(3e-1)$///, "-secant to ", TEX///$S$///, ", and passing through ", TEX///$p$///, " (an error is thrown if such a curve does not exist or is not unique)"}},
EXAMPLE {"-- A general cubic fourfold of discriminant 26"|newline|"X = cubicFourfold(\"3-nodal septic scroll\",ZZ/33331);", "describe X", "time f = detectCongruence(X,Verbose=>true);", "p := point ambient X -- random point on P^5", "time C = f p; -- 5-secant conic to the surface", "assert(dim C == 1 and degree C == 2 and dim(C * surface X) == 0 and degree(C * surface X) == 5 and isSubset(p, C))"},
SeeAlso => {(detectCongruence, GushelMukaiFourfold, ZZ), coneOfLines}}

document {Key => {(detectCongruence, GushelMukaiFourfold, ZZ), (detectCongruence, GushelMukaiFourfold)},
Headline => "detect and return a congruence of (2e-1)-secant curves of degree e inside a del Pezzo fivefold",
Usage => "detectCongruence X"|newline|"detectCongruence(X,e)",
Inputs => {"X" => GushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///,", where ",TEX///$Y$///," denotes the unique del Pezzo fivefold containing the fourfold ",TEX///$X$///}, "e" => ZZ => {"a positive integer (optional but recommended)"}},
Outputs => {CongruenceOfCurves => {"that is a function which takes a general point ", TEX///$p\in Y$///, "(that is, a point on ",TEX///$Y$///," outside a certain proper Zariski closed subset) and returns the unique rational curve of degree ", TEX///$e$///, ", ", TEX///$(2e-1)$///, "-secant to ", TEX///$S$///, ", contained in ",TEX///$Y$///," and passing through ", TEX///$p$///, " (an error is thrown if such a curve does not exist or is not unique)"}},
EXAMPLE{"-- A GM fourfold of discriminant 20"|newline|"X = gushelMukaiFourfold(\"17\",ZZ/33331);", "describe X", "time f = detectCongruence(X,Verbose=>true);", "Y = ambientFivefold X; -- del Pezzo fivefold containing X", "p := point Y -- random point on Y", "time C = f p; -- 3-secant conic to the surface", "S = surface X;", "assert(dim C == 1 and degree C == 2 and dim(C*S) == 0 and degree(C*S) == 3 and isSubset(p,C) and isSubset(C,Y))"},
SeeAlso => {(detectCongruence, CubicFourfold, ZZ), coneOfLines}}

document {Key => {CubicFourfold},
Headline => "the class of all special cubic fourfolds",
PARA{"A cubic fourfold is a smooth cubic hypersurface in ", TEX///$\mathbb{P}^5$///, ". A cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " is ", EM "special", " of discriminant ", TEX///$d>6$///, " if it contains an algebraic surface ", TEX///$S$///, ", and the discriminant of the saturated lattice spanned by ", TEX///$h^2$///, " and ", TEX///$[S]$///, " in ", TEX///$H^{2,2}(X,\mathbb{Z}):=H^4(X,\mathbb{Z})\cap H^2(\Omega_X^2)$///, " is ", TEX///$d$///, ", where ", TEX///$h$///, " denotes the class of a hyperplane section of ", TEX///$X$///, ". The set ", TEX///$\mathcal{C}_d$///, " of special cubic fourfolds of discriminant ", TEX///$d$///, " is either empty or an irreducible divisor inside the moduli space of cubic fourfolds ", TEX///$\mathcal{C}$///, ". Moreover, ", TEX///$\mathcal{C}_d\neq \emptyset$///, " if and only if ", TEX///$d>6$///, " and ", TEX///$d=$///, "0 or 2 (mod 6). For the general theory, see the papers ", HREF{"https://link.springer.com/article/10.1023/A:1001706324425", "Special cubic fourfolds"}, " and ", HREF{"http://imperium.lenin.ru/~kaledin/math/hasset.pdf", "Some rational cubic fourfolds"}, ", by B. Hassett."},
PARA{"An object of the class ", TO CubicFourfold, " is basically represented by a pair ", TEX///(S,X)///, ", where ", TEX///$X$///, " is a cubic fourfold and ", TEX///$S$///, " is a surface contained in ", TEX///$X$///, ". The surface ", TEX///$S$///, " is required to be smooth or with at most a finite number ", TEX///$n$///, " of non-normal nodes. This number ", TEX///$n$///, " (if known) can be specified manually using the option ", TT "NumNodes", ". The main constructor for the objects of the class is the function ", TO cubicFourfold,"."},
SeeAlso => {(discriminant,CubicFourfold)}}

undocumented{(expression, CubicFourfold), (describe, CubicFourfold)}

undocumented{InputCheck, NumNodes}

document {Key => {cubicFourfold, (cubicFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (cubicFourfold, Ideal, Ideal), (cubicFourfold, Ideal, RingElement), [cubicFourfold, NumNodes], [cubicFourfold, InputCheck]},
Headline => "make a special cubic fourfold",
Usage => "cubicFourfold(S,X)"|newline|"cubicFourfold(S,X,NumNodes=>n)",
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible surface ", TEX///$S\subset\mathbb{P}^5$///, ", which has as singularities only a finite number ",TEX///$n\geq 0$///," of non-normal nodes (this number ",TEX///$n$///," should be passed with the option ", TT "NumNodes",", otherwise it is obtained using a probabilistic method)"}, "X" => EmbeddedProjectiveVariety => {"a smooth cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " containing the surface ", TEX///$S$///}},
Outputs => {CubicFourfold => {"the special cubic fourfold corresponding to the pair ", TEX///$(S,X)$///}},
PARA{"In the example below, we define a cubic fourfold containing a rational scroll of degree 7 with 3 nodes."},
EXAMPLE {"K = ZZ/33331; x := gens ring PP_K^5;", "S = projectiveVariety ideal(x_0*x_2*x_3-2*x_1*x_2*x_3-x_1*x_3^2-x_2*x_3^2-x_0*x_1*x_4+2*x_1^2*x_4-x_1*x_2*x_4+x_2^2*x_4+2*x_0*x_3*x_4-x_1*x_3*x_4-x_1*x_4^2+x_1*x_3*x_5, x_1^2*x_3-4*x_1*x_2*x_3-x_0*x_3^2-3*x_1*x_3^2-2*x_2*x_3^2+2*x_0^2*x_4-9*x_0*x_1*x_4+11*x_1^2*x_4-x_0*x_2*x_4-2*x_1*x_2*x_4+2*x_2^2*x_4+12*x_0*x_3*x_4-7*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-6*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+2*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2, x_0*x_1*x_3-7*x_1*x_2*x_3-3*x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+x_3^3+3*x_0^2*x_4-14*x_0*x_1*x_4+17*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+19*x_0*x_3*x_4-9*x_1*x_3*x_4-x_2*x_3*x_4-6*x_3^2*x_4+x_0*x_4^2-9*x_1*x_4^2+6*x_2*x_4^2-3*x_3*x_4^2-3*x_4^3-2*x_0*x_1*x_5+2*x_1^2*x_5+4*x_1*x_2*x_5+5*x_0*x_3*x_5+4*x_1*x_3*x_5-2*x_3^2*x_5-2*x_0*x_4*x_5-7*x_1*x_4*x_5+5*x_2*x_4*x_5+3*x_3*x_4*x_5-2*x_1*x_5^2, x_0^2*x_3-12*x_1*x_2*x_3-6*x_0*x_3^2-6*x_1*x_3^2-5*x_2*x_3^2+2*x_3^3+5*x_0^2*x_4-24*x_0*x_1*x_4+29*x_1^2*x_4-x_0*x_2*x_4-5*x_1*x_2*x_4+5*x_2^2*x_4+32*x_0*x_3*x_4-14*x_1*x_3*x_4-2*x_2*x_3*x_4-10*x_3^2*x_4+x_0*x_4^2-15*x_1*x_4^2+10*x_2*x_4^2-5*x_3*x_4^2-5*x_4^3-3*x_0*x_1*x_5+3*x_1^2*x_5+6*x_1*x_2*x_5+8*x_0*x_3*x_5+7*x_1*x_3*x_5-3*x_3^2*x_5-3*x_0*x_4*x_5-11*x_1*x_4*x_5+8*x_2*x_4*x_5+5*x_3*x_4*x_5-3*x_1*x_5^2, x_1*x_2^2+6*x_1*x_2*x_3+2*x_0*x_3^2+3*x_1*x_3^2+2*x_2*x_3^2-x_3^3-3*x_0^2*x_4+12*x_0*x_1*x_4-14*x_1^2*x_4-2*x_2^2*x_4-15*x_0*x_3*x_4+6*x_1*x_3*x_4+x_2*x_3*x_4+5*x_3^2*x_4+x_0*x_4^2+8*x_1*x_4^2-5*x_2*x_4^2+2*x_3*x_4^2+2*x_4^3+x_0*x_1*x_5-2*x_1^2*x_5-4*x_1*x_2*x_5-4*x_0*x_3*x_5-3*x_1*x_3*x_5+2*x_3^2*x_5+2*x_0*x_4*x_5+7*x_1*x_4*x_5-4*x_2*x_4*x_5-2*x_3*x_4*x_5+2*x_1*x_5^2, x_0*x_2^2+10*x_1*x_2*x_3+3*x_0*x_3^2+5*x_1*x_3^2+4*x_2*x_3^2-x_3^3-5*x_0^2*x_4+19*x_0*x_1*x_4-22*x_1^2*x_4-x_0*x_2*x_4+3*x_1*x_2*x_4-4*x_2^2*x_4-24*x_0*x_3*x_4+9*x_1*x_3*x_4+x_2*x_3*x_4+8*x_3^2*x_4+2*x_0*x_4^2+11*x_1*x_4^2-7*x_2*x_4^2+4*x_3*x_4^2+3*x_4^3+2*x_0*x_1*x_5-4*x_1^2*x_5-7*x_1*x_2*x_5-7*x_0*x_3*x_5-5*x_1*x_3*x_5-x_2*x_3*x_5+3*x_3^2*x_5+4*x_0*x_4*x_5+12*x_1*x_4*x_5-7*x_2*x_4*x_5-3*x_3*x_4*x_5+4*x_1*x_5^2, x_1^2*x_2+17*x_1*x_2*x_3+6*x_0*x_3^2+9*x_1*x_3^2+7*x_2*x_3^2-2*x_3^3-9*x_0^2*x_4+36*x_0*x_1*x_4-44*x_1^2*x_4+3*x_0*x_2*x_4+5*x_1*x_2*x_4-7*x_2^2*x_4-47*x_0*x_3*x_4+21*x_1*x_3*x_4+2*x_2*x_3*x_4+16*x_3^2*x_4+24*x_1*x_4^2-16*x_2*x_4^2+7*x_3*x_4^2+7*x_4^3+3*x_0*x_1*x_5-6*x_1^2*x_5-9*x_1*x_2*x_5-12*x_0*x_3*x_5-8*x_1*x_3*x_5+5*x_3^2*x_5+5*x_0*x_4*x_5+19*x_1*x_4*x_5-12*x_2*x_4*x_5-7*x_3*x_4*x_5+5*x_1*x_5^2, x_0*x_1*x_2+29*x_1*x_2*x_3+11*x_0*x_3^2+15*x_1*x_3^2+12*x_2*x_3^2-4*x_3^3-16*x_0^2*x_4+62*x_0*x_1*x_4-74*x_1^2*x_4+5*x_0*x_2*x_4+9*x_1*x_2*x_4-12*x_2^2*x_4-80*x_0*x_3*x_4+35*x_1*x_3*x_4+4*x_2*x_3*x_4+27*x_3^2*x_4+40*x_1*x_4^2-27*x_2*x_4^2+12*x_3*x_4^2+12*x_4^3+5*x_0*x_1*x_5-10*x_1^2*x_5-16*x_1*x_2*x_5-21*x_0*x_3*x_5-14*x_1*x_3*x_5+9*x_3^2*x_5+9*x_0*x_4*x_5+33*x_1*x_4*x_5-21*x_2*x_4*x_5-12*x_3*x_4*x_5+9*x_1*x_5^2, x_0^2*x_2+49*x_1*x_2*x_3+19*x_0*x_3^2+25*x_1*x_3^2+20*x_2*x_3^2-7*x_3^3-28*x_0^2*x_4+106*x_0*x_1*x_4-124*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-20*x_2^2*x_4-134*x_0*x_3*x_4+58*x_1*x_3*x_4+7*x_2*x_3*x_4+45*x_3^2*x_4+66*x_1*x_4^2-45*x_2*x_4^2+20*x_3*x_4^2+20*x_4^3+9*x_0*x_1*x_5-18*x_1^2*x_5-28*x_1*x_2*x_5-37*x_0*x_3*x_5-23*x_1*x_3*x_5+16*x_3^2*x_5+16*x_0*x_4*x_5+57*x_1*x_4*x_5-36*x_2*x_4*x_5-20*x_3*x_4*x_5+16*x_1*x_5^2, x_1^3+47*x_1*x_2*x_3+18*x_0*x_3^2+23*x_1*x_3^2+19*x_2*x_3^2-7*x_3^3-24*x_0^2*x_4+97*x_0*x_1*x_4-117*x_1^2*x_4+8*x_0*x_2*x_4+16*x_1*x_2*x_4-19*x_2^2*x_4-127*x_0*x_3*x_4+54*x_1*x_3*x_4+7*x_2*x_3*x_4+42*x_3^2*x_4-x_0*x_4^2+62*x_1*x_4^2-42*x_2*x_4^2+19*x_3*x_4^2+19*x_4^3+9*x_0*x_1*x_5-16*x_1^2*x_5-25*x_1*x_2*x_5-33*x_0*x_3*x_5-23*x_1*x_3*x_5+14*x_3^2*x_5+14*x_0*x_4*x_5+51*x_1*x_4*x_5-33*x_2*x_4*x_5-19*x_3*x_4*x_5+14*x_1*x_5^2, x_0*x_1^2+79*x_1*x_2*x_3+29*x_0*x_3^2+40*x_1*x_3^2+32*x_2*x_3^2-11*x_3^3-41*x_0^2*x_4+164*x_0*x_1*x_4-196*x_1^2*x_4+14*x_0*x_2*x_4+26*x_1*x_2*x_4-32*x_2^2*x_4-214*x_0*x_3*x_4+92*x_1*x_3*x_4+11*x_2*x_3*x_4+71*x_3^2*x_4-2*x_0*x_4^2+105*x_1*x_4^2-71*x_2*x_4^2+32*x_3*x_4^2+32*x_4^3+14*x_0*x_1*x_5-26*x_1^2*x_5-41*x_1*x_2*x_5-55*x_0*x_3*x_5-38*x_1*x_3*x_5+23*x_3^2*x_5+23*x_0*x_4*x_5+85*x_1*x_4*x_5-55*x_2*x_4*x_5-32*x_3*x_4*x_5+23*x_1*x_5^2, x_0^2*x_1+133*x_1*x_2*x_3+48*x_0*x_3^2+68*x_1*x_3^2+54*x_2*x_3^2-18*x_3^3-70*x_0^2*x_4+278*x_0*x_1*x_4-330*x_1^2*x_4+24*x_0*x_2*x_4+44*x_1*x_2*x_4-54*x_2^2*x_4-361*x_0*x_3*x_4+156*x_1*x_3*x_4+18*x_2*x_3*x_4+120*x_3^2*x_4-4*x_0*x_4^2+177*x_1*x_4^2-120*x_2*x_4^2+54*x_3*x_4^2+54*x_4^3+23*x_0*x_1*x_5-44*x_1^2*x_5-69*x_1*x_2*x_5-93*x_0*x_3*x_5-63*x_1*x_3*x_5+39*x_3^2*x_5+39*x_0*x_4*x_5+144*x_1*x_4*x_5-93*x_2*x_4*x_5-54*x_3*x_4*x_5+39*x_1*x_5^2, x_0^3+224*x_1*x_2*x_3+80*x_0*x_3^2+115*x_1*x_3^2+91*x_2*x_3^2-30*x_3^3-119*x_0^2*x_4+470*x_0*x_1*x_4-555*x_1^2*x_4+41*x_0*x_2*x_4+75*x_1*x_2*x_4-91*x_2^2*x_4-608*x_0*x_3*x_4+263*x_1*x_3*x_4+30*x_2*x_3*x_4+202*x_3^2*x_4-8*x_0*x_4^2+297*x_1*x_4^2-202*x_2*x_4^2+91*x_3*x_4^2+91*x_4^3+39*x_0*x_1*x_5-76*x_1^2*x_5-118*x_1*x_2*x_5-158*x_0*x_3*x_5-105*x_1*x_3*x_5+67*x_3^2*x_5+68*x_0*x_4*x_5+245*x_1*x_4*x_5-158*x_2*x_4*x_5-91*x_3*x_4*x_5+67*x_1*x_5^2);", "X = projectiveVariety ideal(x_1^2*x_3+x_0*x_2*x_3-6*x_1*x_2*x_3-x_0*x_3^2-4*x_1*x_3^2-3*x_2*x_3^2+2*x_0^2*x_4-10*x_0*x_1*x_4+13*x_1^2*x_4-x_0*x_2*x_4-3*x_1*x_2*x_4+3*x_2^2*x_4+14*x_0*x_3*x_4-8*x_1*x_3*x_4-4*x_3^2*x_4+x_0*x_4^2-7*x_1*x_4^2+4*x_2*x_4^2-2*x_3*x_4^2-2*x_4^3-x_0*x_1*x_5+x_1^2*x_5+2*x_1*x_2*x_5+3*x_0*x_3*x_5+3*x_1*x_3*x_5-x_3^2*x_5-x_0*x_4*x_5-4*x_1*x_4*x_5+3*x_2*x_4*x_5+2*x_3*x_4*x_5-x_1*x_5^2);", "F = cubicFourfold(S,X,NumNodes=>3);", "describe F", "assert(F == X)"},
SeeAlso => {(cubicFourfold, EmbeddedProjectiveVariety), (cubicFourfold, String, Ring), specialFourfold}}

document {Key => {(cubicFourfold, EmbeddedProjectiveVariety), (cubicFourfold, Ideal)},
Headline => "random special cubic fourfold",
Usage => "cubicFourfold S"|newline|"cubicFourfold(S,NumNodes=>n)",
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible surface in ", TEX///$\mathbb{P}^5$///}},
Outputs => {CubicFourfold => {"a random cubic fourfold containing the given surface"}},
EXAMPLE {"-- quintic del Pezzo surface"|newline|"S = surface({3,4},ZZ/33331);", "X = cubicFourfold S;", "discriminant X"},
SeeAlso => {(cubicFourfold, String, Ring)}}

document {Key => {(cubicFourfold, String, Ring), (cubicFourfold, String)},
Headline => "random special cubic fourfold of a given type",
Usage => "cubicFourfold(n,K)
cubicFourfold n",
Inputs => {"n" => String => {"the name of some known type of cubic fourfolds"}, "K" => {"the coefficient ring"}},
Outputs => {CubicFourfold => {"a random special cubic fourfold of the indicated type over ",TT"K"}},
EXAMPLE {"X = cubicFourfold(\"3-nodal septic scroll\",ZZ/65521);", "describe X"},
SeeAlso => (cubicFourfold, EmbeddedProjectiveVariety)}

document {Key => {ambientFivefold, (ambientFivefold, HodgeSpecialFourfold)},
Headline => "get the ambient fivefold of the Hodge-special fourfold",
Usage => "ambientFivefold X",
Inputs => {"X" => HodgeSpecialFourfold},
Outputs => {EmbeddedProjectiveVariety => {"the ambient fivefold of ",TT"X"}},
EXAMPLE {"S = surface {4,5,1};", "V = random(3,S);", "X = V * random(2,S);", "F = specialFourfold(S,X,V);", "ambientFivefold F"},
PARA {"When ",TEX///$X$///," is a ",TO2{GushelMukaiFourfold,"GM fourfold"},", the ambient fivefold of ",TEX///$X$///," is a fivefold ",TEX///$Y\subset\mathbb{P}^8$///," of degree 5 such that ",TEX///$X\subset Y$///," is a quadric hypersurface. We have that the fourfold ",TEX///$X$///," is of ordinary type if and only if ",TEX///$Y$///," is smooth."},
EXAMPLE {
"X = specialFourfold(\"21\",ZZ/33331);",
"describe X",
"Y = ambientFivefold X;",
"isSubset(X,Y)",
"Y!"}}

document {Key => {(map, CubicFourfold)},
Headline => "associated cubic map",
Usage => "map X",
Inputs => {"X" => CubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///}},
Outputs => {RationalMap => {"the rational map from ", TEX///$\mathbb{P}^5$///, " defined by the linear system of cubics through ", TEX///$S$///}}}

document {Key => {(map, GushelMukaiFourfold)},
Headline => "associated quadratic map",
Usage => "map X",
Inputs => {"X" => GushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///, ", where ", TEX///$Y\subset\mathbb{P}^8$///, " is the unique del Pezzo fivefold containing ", TEX///$X$///}},
Outputs => {RationalMap => {"the rational map from ", TEX///$Y$///, " defined by the linear system of quadrics through ", TEX///$S$///}}}

document {Key => {surface, (surface, HodgeSpecialFourfold)},
Headline => "get the special surface contained in the fourfold",
Usage => "surface X",
Inputs => {"X" => HodgeSpecialFourfold},
Outputs => {EmbeddedProjectiveVariety => {"the special surface contained in the fourfold ",TT"X"}},
EXAMPLE {"X = specialFourfold \"quintic del Pezzo surface\";", "V = ambientFivefold X;", "S = surface X;", "assert isSubset(S,X)"},
SeeAlso => {(surface,List)}}

document {
Key => {(surface, List), (surface, VisibleList, Ring), (surface, VisibleList, Option), (surface, VisibleList, Option), (surface, VisibleList, Ring, Option), (surface, VisibleList, Option, Option), (surface, VisibleList, Ring, Option, Option)},
Headline => "get a rational surface",
Usage => "surface {a,i,j,k,...}
surface({a,i,j,k,...),K)
surface({a,i,j,k,...},K,NumNodes=>n,ambient=>m)",
Inputs => {List => {"a list ",TEX///$\{a,i,j,k,\ldots\}$///," of nonnegative integers"}},
Outputs => {EmbeddedProjectiveVariety => {"the image of the rational map defined by the linear system of curves of degree ",TEX///$a$///," in ",TEX///$\mathbb{P}_{K}^2$///," having ",TEX///$i$///," random base points of multiplicity 1, ",TEX///$j$///," random base points of multiplicity 2, ",TEX///$k$///," random base points of multiplicity 3, and so on until the last integer in the given list."}},
PARA{"In the example below, we take the image of the rational map defined by the linear system of septic plane curves with 3 random simple base points and 9 random double points."},
EXAMPLE {
"S = surface {7,3,9};",
"coefficientRing S",
"T = surface({7,3,9},ZZ/33331);",
"X = cubicFourfold T;",
"coefficientRing X",
"describe X"},
SeeAlso => {(rationalMap,PolynomialRing,List),(gushelMukaiFourfold,Array,Array)}}

typValSurf := typicalValues#surface;
typicalValues#surface = Nothing;
document {Key => {(surface, MultiprojectiveVariety, MultiprojectiveVariety)},
Headline => "make a Hodge-special surface",
Usage => "surface(C,S)",
Inputs => {"C" => MultiprojectiveVariety => {"an irreducible curve"}, "S" => MultiprojectiveVariety => {"a smooth surface ", TEX///$S$///, " containing the curve ", TEX///$C$///}},
Outputs => {{"the Hodge special surface corresponding to the pair ", TEX///$(C,S)$///}},
PARA{"The curve ",TEX///$C$///," can be recovered using the function ",TT "curve","."},
EXAMPLE lines ///K = ZZ/65521;
C = random PP_K^(1,3); -- random twisted cubic in P^3
j = parametrize PP_K(1,1,1,4);
C = (rationalMap(ambient C,source j) * j) C;
describe C
S = random(8,C);
describe S
S = surface(C,S);
discriminant S
parameterCount(S,Verbose=>true)
f := map(S,1,0)
f = quadricFibration f
discriminant f///,
Caveat => {"This feature is currently under development."}}
typicalValues#surface = typValSurf;

document {Key => {unirationalParametrization, (unirationalParametrization, CubicFourfold), (unirationalParametrization, CubicFourfold, EmbeddedProjectiveVariety), (unirationalParametrization, GushelMukaiFourfold), (unirationalParametrization, HodgeSpecialFourfold)},
Headline => "unirational parametrization",
Usage => "unirationalParametrization X",
Inputs => {"X" => CubicFourfold => {"or ", ofClass GushelMukaiFourfold}},
Outputs => {MultirationalMap => {"a rational map of degree 2 from ",TEX///$\mathbb{P}^4$///," to ",TEX///$X$///}},
PARA{"The degree of the forms defining the returned map is 10 in the case of cubic fourfolds, and 26 in the case of GM fourfolds."},
EXAMPLE {"K = ZZ/10000019; S = PP_K^(2,2); -- Veronese surface;", "X = cubicFourfold S;", "time f = unirationalParametrization X;", "degreeSequence f", "degree(f,Strategy=>\"random point\")"},
SeeAlso => {(parametrize, HodgeSpecialFourfold), (parametrize, MultiprojectiveVariety)}}

document {Key => {(parametrize, HodgeSpecialFourfold)},
Headline => "rational parametrization",
Usage => "parametrize X",
Inputs => {"X" => HodgeSpecialFourfold},
Outputs => {MultirationalMap => {"a birational map from a rational fourfold to ", TT "X"}},
PARA{"Some Hodge-special fourfolds are known to be rational. In this case, the function tries to obtain a birational map from ", TEX///$\mathbb{P}^4$///, " (or, e.g., from a quadric hypersurface in ", TEX///$\mathbb{P}^5$///, ") to the fourfold."},
EXAMPLE {"X = specialFourfold surface {3,4};", "phi = parametrize X;", "describe phi"},
EXAMPLE {"Y = specialFourfold \"tau-quadric\";", "psi = parametrize Y;", "describe psi"},
EXAMPLE {"Z = specialFourfold \"plane in PP^7\";", "eta = parametrize Z;", "describe eta"},
SeeAlso => {unirationalParametrization, (parametrize,MultiprojectiveVariety)}}

document {Key => {fromOrdinaryToGushel, (fromOrdinaryToGushel, GushelMukaiFourfold)},
Headline => "try to deform to a fourfold of Gushel type",
Usage => "fromOrdinaryToGushel X",
Inputs => {"X" => GushelMukaiFourfold => {"a fourfold of ordinary type"}},
Outputs => {GushelMukaiFourfold => {"a fourfold of Gushel type, a deformation of ",TT"X"}},
EXAMPLE {"X = gushelMukaiFourfold \"quintic del Pezzo surface\";", "singularLocus ambientFivefold X", "X' = fromOrdinaryToGushel X;", "support singularLocus ambientFivefold X'"}}

document {Key => {associatedK3surface, [associatedK3surface, Strategy], building},
Headline => "K3 surface associated to a rational fourfold",
PARA{"See ",TO (associatedK3surface, CubicFourfold)," and ",TO (associatedK3surface, GushelMukaiFourfold),"."}}

document {Key => {(associatedK3surface, CubicFourfold)},
Headline => "K3 surface associated to a rational cubic fourfold",
Usage => "E = associatedK3surface X",
Inputs => {"X" => CubicFourfold => {"containing a surface ", TEX///$S\subset\mathbb{P}^5$///," that admits a ",TO2{CongruenceOfCurves,"congruence"}," of ",TEX///$(3e-1)$///,"-secant curves of degree ",TEX///$e$///}},
Outputs => {"E" => {"the (minimal) K3 surface associated to ",TEX///$X$///}},
Consequences => {{{TT"building E"," will return the following four objects:"}, UL{{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:\mathbb{P}^5 \dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$3e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to the minimal K3 surface ",TEX///$\widetilde{U}$///,"."}}}},
PARA {"For more details and notation, see the papers ",HREF{"https://arxiv.org/abs/1909.01263","Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds"}," and ",HREF{"https://arxiv.org/abs/2204.11518","Explicit computations with cubic fourfolds, Gushel-Mukai fourfolds, and their associated K3 surfaces"},"."},
EXAMPLE {"X = cubicFourfold \"quartic scroll\";", "describe X", "E = associatedK3surface(X, Verbose=>true);", "describe X", "(mu,U,exCurves,f) = building E; ? mu", "assert(image f == E)"},
SeeAlso => {(associatedK3surface, GushelMukaiFourfold), detectCongruence, mirrorFourfold, (polarizedK3surface, DoublySpecialCubicFourfold)}}

document {Key => {(associatedK3surface, GushelMukaiFourfold)},
Headline => "K3 surface associated to a rational Gushel-Mukai fourfold",
Usage => "E = associatedK3surface X",
Inputs => {"X" => GushelMukaiFourfold => {"containing a surface ", TEX///$S\subset Y$///," that admits a ",TO2{CongruenceOfCurves,"congruence"}," of ",TEX///$(2e-1)$///,"-secant curves of degree ",TEX///$e$///," inside the ",TO2{ambientFivefold,"ambient fivefold"}," ",TEX///$Y$///," of the fourfold ",TEX///$X$///}},
Outputs => {"E" => {"the (minimal) K3 surface associated to ",TEX///$X$///}},
Consequences => {{{TT"building E"," will return the following four objects:"}, UL{{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:Y\dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$2e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to the minimal K3 surface ",TEX///$\widetilde{U}$///,"."}}}},
PARA {"For more details and notation, see the paper ",HREF{"https://arxiv.org/abs/2204.11518","Explicit computations with cubic fourfolds, Gushel-Mukai fourfolds, and their associated K3 surfaces"},"."},
EXAMPLE {"X = gushelMukaiFourfold \"tau-quadric\";", "describe X", "E = associatedK3surface(X, Verbose=>true);", "describe X", "(mu,U,exCurves,f) = building E; ? mu", "assert(image f == E)"},
SeeAlso => {(associatedK3surface, CubicFourfold), detectCongruence, mirrorFourfold, (polarizedK3surface, DoublySpecialCubicFourfold)}}

document {Key => {parametrizeFanoFourfold, (parametrizeFanoFourfold, EmbeddedProjectiveVariety), [parametrizeFanoFourfold,Strategy]},
Headline => "rational parametrization of a prime Fano fourfold of coindex at most 3",
Usage => "parametrize X
parametrizeFanoFourfold(X,Strategy=>...)",
Inputs => {"X" => EmbeddedProjectiveVariety => {"a prime Fano fourfold ",TEX///$X$///," of coindex at most 3 having degree ",TEX///$d$///," and genus ",TEX///$g$///," with ",TEX///$(d,g)\in\{(2,0),(4,1),(5,1),(12,7),(14,8),(16,9),(18,10)\}$///}},
Outputs => {MultirationalMap => {"a birational map from ",TEX///$\mathbb{P}^4$///," to ", TEX///$X$///}},
PARA{"This function is mainly based on results contained in the classical paper ",HREF{"https://link.springer.com/article/10.1007/BF02413916","Algebraic varieties with canonical curve sections"},", by L. Roth. In some examples, more strategies are available. For instance, if ",TEX///$X\subset\mathbb{P}^7$///," is a 4-dimensional linear section of ",TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9$///,", then by passing ",TT"Strategy=>1"," (which is the default choice) we get the inverse of the projection from the plane spanned by a conic contained in ",TEX///$X$///,"; while with ",TT"Strategy=>2"," we get the projection from the unique ",TEX///$\sigma_{2,2}$///,"-plane contained in ",TEX///$X$///," (Todd's result)."},
EXAMPLE {"K = ZZ/65521; X = GG_K(1,4) * random({{1},{1}},0_(GG_K(1,4)));","? X", "time parametrizeFanoFourfold X"},
SeeAlso => {fanoFourfold,(parametrize,HodgeSpecialFourfold),unirationalParametrization,(parametrize,MultiprojectiveVariety)}}

document {Key => {fanoFourfold, (fanoFourfold,ZZ,ZZ), [fanoFourfold,CoefficientRing]},
Headline => "random prime Fano fourfold of coindex at most 3",
Usage => "fanoFourfold(d,g)
fanoFourfold(d,g,CoefficientRing=>K)",
Inputs => {{TT"(d,g)"," a pair of integers belonging to the set ",TEX///$\{(2,0),(3,1),(4,1),(5,1),(4,3),(6,4),(8,5),(10,6),(12,7),(14,8),(16,9),(18,10)\}$///}},
Outputs => {EmbeddedProjectiveVariety => {"a random prime Fano fourfold of coindex at most 3 having degree ",TEX///$d$///," and genus ",TEX///$g$///}},
EXAMPLE {"X = fanoFourfold(4,1);", "describe X", "parametrize X"},
SeeAlso => {parametrizeFanoFourfold}}

document {
Key => {(clean,HodgeSpecialFourfold)},
Headline => "clean the internal information of a fourfold",
Usage => "clean X",
Inputs => {"X" => HodgeSpecialFourfold},
Outputs => {HodgeSpecialFourfold => {"which is mathematically identical to ",TT"X",", but new to the system"}},
PARA{"This function is only useful for testing."},
EXAMPLE {"X = specialFourfold \"quartic scroll\"", "X' = clean X", "X === X'"}}

document {Key => {trisecantFlop},
Headline => "examples of trisecant flops",
Usage => "trisecantFlop i",
Inputs => {"i" => ZZ => {"an integer between 0 and 17"}},
Outputs => {{"the i-th example of birational map ",TEX///$X\dashrightarrow W$///," in accordance to the Table 1 in the paper ",HREF{"https://arxiv.org/abs/1909.01263","Trisecant Flops, their associated K3 surfaces and the rationality of some Fano fourfolds"},"."}},
PARA{"This function requires the package ",HREF{"https://github.com/giovannistagliano/TrisecantFlops","TrisecantFlops"},". If not present the user will be asked to automatically install the package."},
SeeAlso => {(specialFourfold, String, ZZ)}}
undocumented {(trisecantFlop,ZZ)}

document {Key => {(specialFourfold, String, ZZ)},
Headline => "load a prebuilt example of fourfold",
Usage => "specialFourfold(str,i)",
Inputs => {"str" => String => {"such as \"",TT"prebuilt-example-in-P5","\" or \"",TT"prebuilt-example-in-P7","\"."}, "i" => ZZ},
Outputs => {HodgeSpecialFourfold => {"the i-th example of fourfold in accordance with some classification (e.g., ",TT"specialFourfold(\"prebuilt-example-in-P5\",i)"," is the same as ",TO2{(source,MultirationalMap),"source"}," ",TO trisecantFlop,TT"(i)","."}},
PARA{"This function requires the package ",HREF{"https://github.com/giovannistagliano/TrisecantFlops","TrisecantFlops"},". If not present the user will be asked to automatically install the package."},
SeeAlso => trisecantFlop}

undocumented {(random, HodgeSpecialFourfold), (symbol **, HodgeSpecialFourfold,Ring), (map, HodgeSpecialFourfold), (describe, HodgeSpecialFourfold)}

document {Key => {(gushelMukaiFourfold, Array, Array, String, Thing), (gushelMukaiFourfold, Array, Array), (gushelMukaiFourfold, Array, Array, String), (gushelMukaiFourfold, Array, Array, Thing)},
Headline => "construct GM fourfolds by gluing cubic or quartic scrolls to surfaces in PP^6",
Usage => "gushelMukaiFourfold(surface,curve)
gushelMukaiFourfold(surface,curve,scroll)
gushelMukaiFourfold(surface,curve,K)
gushelMukaiFourfold(surface,curve,scroll,K)",
Inputs => {"surface" => Array => {"an array of integers ",TT"[a,i,j,k,...]"," to indicate the rational surface ",TEX///$S\subset\mathbb{P}^6$///," constructed by ",TO2{(surface,List),"surface"},TT"({a,i,j,k,...},K,ambient=>6)"},
           "curve" => Array => {"an array of integers ",TT"[d,l,m,n,...]"," to indicate the plane representation of a curve ",TEX///$C$///," on the surface ",TEX///$S$///," (the command that constructs ",TEX///$C$///," is ",TT///S.cache#"takeCurve"(d,{l,m,n,...})///,")"},
           "scroll" => String => {"which can be either \"cubic scroll\" (the default value) or \"quartic scroll\", to indicate the type of scroll ",TEX///$B\subset\mathbb{P}^6$///," to be used; in the former case ",TEX///$B\simeq\mathbb{P}^1\times\mathbb{P}^2\subset\mathbb{P}^5\subset\mathbb{P}^6$///," while in the latter case ",TEX///$B\subset\mathbb{P}^6$///," is a generic projection of a rational normal quartic scroll of dimension 4 in ",TEX///$\mathbb{P}^7$///},
           "K" => {{"the coefficient ring (",TT"ZZ/65521"," is used by default)"}}},
Outputs => {GushelMukaiFourfold => {"a GM fourfold ",TEX///$X$///," containing the surface ",TEX///$\overline{\psi_{B}(S)}\subset\mathbb{G}(1,4)\subset\mathbb{P}^9$///,", where ",TEX///$B$///," is a scroll of the indicated type such that ",TEX///$C\subseteq S\cap B$///," and ",TEX///$\psi_{B}:\mathbb{P}^6\dashrightarrow\mathbb{G}(1,4)$///," is the birational map defined by ",TEX///$B$///}},
PARA {"From the returned fourfold ",TEX///$X$///,", with the following commands we obtain the surface ",TEX///$S$///,", the curve ",TEX///$C$///,", and the scroll ",TEX///$B$///," used in the construction: "},PARA{TT///(B,C) = X.cache#"Construction"; S = ambientVariety C;///},PARA{"Then the surface ",TEX///$\overline{\psi_{B}(S)}\subset\mathbb{G}(1,4)$///," can be constructed with "},PARA{TT///psi = rationalMap B; (psi S)%(image psi);///},
PARA {"In the following example we construct a GM fourfold containing the image via ",TEX///$\psi_B:\mathbb{P}^6\dashrightarrow\mathbb{G}(1,4)$///," of a quintic del Pezzo surface ",TEX///$S\subset\mathbb{P}^5\subset\mathbb{P}^6$///,", obtained as the image of the plane via the linear system of quartic curves with three general simple base points and two general double points, which cuts ",TEX///$B\simeq\mathbb{P}^1\times\mathbb{P}^2\subset\mathbb{P}^5\subset\mathbb{P}^6$///," along a rational normal quartic curve obtained as the image of a general conic passing through the two double points."},
EXAMPLE lines ///X = gushelMukaiFourfold([4, 3, 2],[2, 0, 2]);
describe X
(B,C) = X.cache#"Construction";
S = ambientVariety C;
C;
B;
assert(C == S * B)///,
References => UL{{"G. Staglianò, ",EM"Explicit computations with cubic fourfolds, Gushel-Mukai fourfolds, and their associated K3 surfaces",", available at ",HREF{"https://arxiv.org/abs/2204.11518","arXiv:2204.11518"}," (2022)."}},
SeeAlso => {(surface,VisibleList,Ring), GMtables}}

document {Key => {specialFourfold, (specialFourfold, EmbeddedProjectiveVariety), (specialFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialFourfold, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety), (specialFourfold, Ideal), (specialFourfold, Ideal, Ideal), (specialFourfold, Ideal, RingElement), (specialFourfold, String), (specialFourfold, String, Ring), [specialFourfold, NumNodes], [specialFourfold, InputCheck]},
Headline => "make a Hodge-special fourfold",
Usage => "specialFourfold(S,X,V)"|newline|"specialFourfold(S,X)"|newline|"specialFourfold S",
Inputs => {"S" => EmbeddedProjectiveVariety => {"an irreducible ",TO2{(surface,HodgeSpecialFourfold),"surface"}}, "X" => EmbeddedProjectiveVariety => {"a smooth fourfold ", TEX///$X$///, " containing the surface ", TEX///$S$///}, "V" => EmbeddedProjectiveVariety => {"optional, a ",TO2{ambientFivefold,"fivefold"}," where ",TT"X"," is a hypersurface"}},
Outputs => {HodgeSpecialFourfold => {"which corresponds to the pair ", TEX///$(S,X)$///}},
PARA{"This can also be used as a shortcut for both ",TO cubicFourfold," and ",TO gushelMukaiFourfold,"."},
EXAMPLE {"S = surface {3,4};", "X = specialFourfold S -- a random cubic fourfold through S", "describe X", "Y = specialFourfold \"tau-quadric\" -- a random GM fourfold through a tau-quadric", "describe Y", "T = surface {3,2};", "Z = specialFourfold T -- a random c. i. of 3 quadrics through T", "describe Z"},
SeeAlso => {cubicFourfold, gushelMukaiFourfold}}

document {
Key => {Singular, [associatedK3surface, Singular], [associatedCastelnuovoSurface, Singular], [mirrorFourfold, Singular]},
Headline => "whether to transfer computation to Singular",
Usage => "Singular => true or false",
PARA{"This option allows you to transfer part of the computation to Singular."},
SeeAlso => {associatedK3surface}}

document {Key => {mirrorFourfold, (mirrorFourfold, CubicFourfold), (mirrorFourfold, GushelMukaiFourfold), (mirrorFourfold, IntersectionOfThreeQuadricsInP7), (mirrorFourfold, HodgeSpecialFourfold), [mirrorFourfold, Strategy]},
Headline => "associated fourfold to a rational cubic or GM fourfold",
Usage => "mirrorFourfold X",
Inputs => {"X" => CubicFourfold => {"or ", ofClass GushelMukaiFourfold}},
Outputs => {HodgeSpecialFourfold => {"the fourfold ",TT"W"," containing the surface ",TT"U",", with the same notation as in the function ",TO associatedK3surface,"."}},
EXAMPLE {"X = specialFourfold(PP_(ZZ/65521)[2,2]);", "W = mirrorFourfold X;", "U = surface W;", "mirrorFourfold W", "(building associatedK3surface X)_1", "assert(oo === U)"},
EXAMPLE {"X' = specialFourfold \"tau-quadric\";", "W' = mirrorFourfold X';", "U' = surface W';", "mirrorFourfold W'", "(building associatedK3surface X')_1", "assert(oo === U')"},
SeeAlso => {associatedK3surface}}

document {
Key => {(toExternalString, HodgeSpecialFourfold)},
Headline => "convert to a readable string",
Usage => "toExternalString X",
Inputs => {"X" => HodgeSpecialFourfold},
Outputs => {String => {"a string representation of ",TT "X",", which can be used, in conjunction with ",TO "value",", to read the object back into the program later"}},
PARA{"Some of the internal data of the input ",TT"X"," are included in the returned string."},
EXAMPLE {"describe (X = specialFourfold \"tau-quadric\")", "str = toExternalString X;", "describe (value str)"}}

undocumented {(expression, HodgeSpecialFourfold)}

document {Key => {HodgeSpecialFourfold},
Headline => "the class of all Hodge-special fourfolds",
PARA{"An object of the class ", TO HodgeSpecialFourfold, " is basically represented by a pair ", TEX///(S,X)///, ", where ", TEX///$X$///, " is a fourfold and ", TEX///$S$///, " is a surface contained in ", TEX///$X$///,". Such objects are created by the function ",TO specialFourfold,"."},
SeeAlso => {specialFourfold, CubicFourfold, GushelMukaiFourfold, IntersectionOfThreeQuadricsInP7}}

document {Key => {(check, ZZ, CongruenceOfCurves), (check, CongruenceOfCurves)},
Headline => "check that a congruence of curves is well-defined",
Usage => "check f"|newline|"check(n,f)",
Inputs => {"n" => ZZ => {"(optional with default value 5)"}, "f" => CongruenceOfCurves},
Outputs => {CongruenceOfCurves => {"the same object passed as input, but an error is thrown if the congruence fails when ",TO2{(symbol SPACE, CongruenceOfCurves, EmbeddedProjectiveVariety),"evaluated"}," on ",TT"n"," random points."}}}

undocumented {(symbol ?, HodgeSpecialFourfold, HodgeSpecialFourfold)}

document {Key => {IntersectionOfThreeQuadricsInP7},
Headline => "the class of all special intersection of three quadrics in P^7",
PARA {"This is a subtype of the ",TO HodgeSpecialFourfold," type."},
SeeAlso => {specialFourfold}}

undocumented {(expression, IntersectionOfThreeQuadricsInP7)}

document {Key => {associatedCastelnuovoSurface, (associatedCastelnuovoSurface, IntersectionOfThreeQuadricsInP7), [associatedCastelnuovoSurface, Strategy]},
Headline => "Castelnuovo surface associated to a rational complete intersection of three quadrics in P^7",
Usage => "E = associatedCastelnuovoSurface X",
Inputs => {"X" => IntersectionOfThreeQuadricsInP7 => {"containing a surface ", TEX///$S\subset Y$///," that admits a ",TO2{CongruenceOfCurves,"congruence"}," of ",TEX///$(2e-1)$///,"-secant curves of degree ",TEX///$e$///," inside the ",TO2{ambientFivefold,"ambient fivefold"}," ",TEX///$Y$///," of the fourfold ",TEX///$X$///}},
Outputs => {"E" => {"the (minimal) Castelnuovo surface associated to ",TEX///$X$///}},
Consequences => {{{TT"building E"," will return the following four objects:"}, UL{{"the dominant ",TO2{MultirationalMap,"rational map"}," ",TEX///$\mu:Y\dashrightarrow W$///," defined by the linear system of hypersurfaces of degree ",TEX///$2e-1$///," having points of multiplicity ",TEX///$e$///," along ",TEX///$S$///,";"}, {"the ",TO2{EmbeddedProjectiveVariety,"surface"}," ",TEX///$U\subset W$///," determining the inverse map of the restriction of ",TEX///$\mu$///," to ",TEX///$X$///,";"}, {"the ",TO2{List,"list"}," of the exceptional curves on the surface ",TEX///$U$///,";"}, {"a ",TO2{MultirationalMap,"rational map"}," of degree 1 from the surface ",TEX///$U$///," to the minimal Castelnuovo surface ",TEX///$\widetilde{U}$///,"."}}}},
PARA {"For more details, see the paper ",HREF{"https://arxiv.org/abs/2312.01773","On complete intersections of three quadrics in P^7"},"."},
EXAMPLE {"X = specialFourfold random({5:{1}},0_(PP_(ZZ/33331)^7));", "describe X", "E = associatedCastelnuovoSurface(X, Verbose=>true);", "describe X", "(mu,U,exCurves,f) = building E; ? mu", "assert(image f == E)"},
SeeAlso => {associatedK3surface, detectCongruence}}

document {Key => {beauvilleMap, (beauvilleMap, IntersectionOfThreeQuadricsInP7)},
Headline => "construction of Beauville for complete intersections of three quadrics in P^7",
Usage => "f = beauvilleMap X",
Inputs => {"X" => IntersectionOfThreeQuadricsInP7},
Outputs => {"f" => MultirationalMap => {"a birational map from ",TEX///$X$///," to a fourfold ",TEX///$Y\subset\mathbb{P}^2\times\mathbb{P}^5$///," whose first projection ",TEX///$Y\to\mathbb{P}^2$///," is a quadric surface bundle"}},
PARA{"Smooth intersections of three quadrics in ",TEX///$\mathbb{P}^7$///," are birational to quadric surface bundles over ",TEX///$\mathbb{P}^2$///," with discriminant curve of degree 8. This is a construction of Beauville; see e.g. Proposition 6 in the paper ",HREF{"https://www.intlpress.com/site/pub/files/_fulltext/journals/sdg/2017/0022/0001/SDG-2017-0022-0001-a009.pdf", "Intersections of three quadrics in P7"}, ", by B. Hassett, A. Pirutka, and Y. Tschinkel."},
EXAMPLE {"X = specialFourfold random({5:{1}},0_(PP_(ZZ/33331)^7));", "f = beauvilleMap X;", "Y = target f;", "inverse f;", "first projectionMaps target f;"}}

document {Key => {DoublySpecialCubicFourfold, (symbol &, EmbeddedProjectiveVariety, EmbeddedProjectiveVariety)},
Headline => "the class of all cubic fourfolds belonging to the intersection of two Hassett divisors",
PARA{"A cubic fourfold ", TEX///$X\subset \mathbb{P}^5$///, " is called ", EM "doubly special", " if it belongs to the intersection of two Hassett divisors ", TEX///$\mathcal{C}_{d_1} \cap \mathcal{C}_{d_2}$///, ". This means that the algebraic part of its middle cohomology, ", TEX///$H^{2,2}(X, \mathbb{Z})$///, ", has rank at least 3, containing the classes of two surfaces ", TEX///$S$///, " and ", TEX///$T$///, " which, together with the square of the hyperplane class ", TEX///$h^2$///, ", span a saturated lattice of rank 3."},
PARA{"An object of the class ", TO DoublySpecialCubicFourfold, " can be thought of as a triple ", TEX///$(S,T,X)$///, ", where ", TEX///$X$///, " is a cubic fourfold and ", TEX///$S, T$///, " are two surfaces contained in ", TEX///$X$///, ". Internally, it is represented as a nested structure, where the ", TO2{CubicFourfold, "cubic fourfold"}, " containing ", TEX///$S$///, " is built upon the ", TO2{CubicFourfold, "cubic fourfold"}, " containing ", TEX///$T$///, ". This nesting allows the object to inherit the functions and properties of the cubic fourfold containing ", TEX///$S$///, "."},
PARA{"Such an object is constructed using the operator ", TT "&", " to combine the two surfaces. Specifically, if ", TEX///$S$///, " and ", TEX///$T$///, " are two surfaces and ", TEX///$X$///, " is a cubic fourfold containing both, an object is created via the function ", TO specialFourfold, " (or equivalently ", TO cubicFourfold, ") as follows:"},
EXAMPLE {"K = ZZ/33331;", "S = PP_K^(2,2); -- the Veronese surface", "T = random({{2},{1},{1}}, sum(3, i -> point S)); -- a 3-secant quadric to S", "X = random(3, S + T);", "X = specialFourfold(S & T, X)", "surface X", "discriminant X"},
PARA{"If the second argument (the cubic fourfold) is omitted, the constructor will automatically provide a random cubic fourfold containing the union ", TEX///$S \cup T$///, ". A summary of the created fourfold ",TEX///$X$///," is provided by the function ", TT "describe", ", while the pair of surfaces ", TEX///$(S,T)$///, " defining the doubly special structure can be retrieved with the function ", TT "surfaces", "."},
EXAMPLE {"X = specialFourfold(S & T);", "describe X", "surfaces X"},
SeeAlso => {CubicFourfold, (polarizedK3surface, DoublySpecialCubicFourfold), surfaces}}

document {Key => {polarizedK3surface, (polarizedK3surface, DoublySpecialCubicFourfold), FanoMapType, [polarizedK3surface, FanoMapType], [polarizedK3surface, Strategy], latticePolarization},
Headline => "polarized K3 surface associated to a doubly special cubic fourfold",
Usage => "E = polarizedK3surface X",
Inputs => {"X" => DoublySpecialCubicFourfold => {"whose defining union of surfaces admits a ", TO2{CongruenceOfCurves, "congruence"}, " of curves"}},
Outputs => {"E" => {"the polarized K3 surface associated to ", TEX///$X$///}},
PARA {"At the first call, this function initializes a ", EM "container", " object: it computes the underlying ", TO2{EmbeddedProjectiveVariety, "projective surface"}, " but leaves the lattice polarization data empty to save time."},
PARA {"The full lattice data is computed and stored within the object only upon a second call. Once initialized, ", TO polarize, " can be used as a synonym (using default options)."},
PARA {"The following methods can be used to access the construction data of ", TEX///$E$///, ":"},
UL {{TT "building E", " -- returns the four construction objects as in ", TO (associatedK3surface, CubicFourfold), " (the Fano rational map ", TEX///$\mu$///, ", the non-minimal K3 surface ", TEX///$U$///, ", the exceptional curves on ", TEX///$U$///, " and the morphism ", TEX///$f: U \to E$///, " contracting them);"}, {TT "projectiveVariety E", " -- returns the underlying ", TO2{EmbeddedProjectiveVariety, "projective surface"}, ";"}, {TT "latticePolarization E", " -- returns the lattice data induced by the doubly special structure of ", TEX///$X$///, "."}},
PARA {"Optional inputs:"},
UL {{TO "Strategy", " -- provides instructions for each step. The first stage handles the construction of the non-minimal K3 surface ", TEX///$U$///, " by inverting the Fano map restricted to the cubic (via ", TT "\"Inverse\"", " or ", TT "\"Approximate\"", "), while the second computes the lattice data (common options include ", TT "\"MapFromW\"", ", ", TT "\"MapFromU\"", ", or ", TT "\"SpecialCurve\"", "). Both can be passed at once as a sequence, e.g., ", TT "Strategy => (\"Inverse\", \"MapFromW\")", ", or provided individually through nested calls."}, {TO "FanoMapType", " -- sets the Fano map to ", TT "\"Standard\"", " (default) or ", TT "\"P2xP2\"", ". Note that switching this type updates the global behavior of ", TEX///$X$///, " (e.g., affecting methods like ", TO mirrorFourfold, "), though previously computed data is preserved;"}, {TO "Verbose", " -- if set to ", TO true, ", provides feedback during the construction."}},
EXAMPLE {"S = random({3:{1}},0_(PP_(ZZ/65521)^5)); T = random S;", "X = specialFourfold(S & T);", "describe X", "polarizedK3surface(X, Verbose=>true)", "polarizedK3surface(X, Verbose=>true)", "describe X"},
PARA{"Here is another example."},
EXAMPLE {"X = specialFourfold surface((2,0,0),(1,0,0));", "describe X", "polarizedK3surface(X, Verbose=>true)", "polarizedK3surface(oo, Verbose=>true)", "latticePolarization oo", "describe X"},
SeeAlso => {DoublySpecialCubicFourfold, (associatedK3surface, CubicFourfold)}}

undocumented {(expression, DoublySpecialCubicFourfold), (describe, DoublySpecialCubicFourfold), (check, DoublySpecialCubicFourfold), (clean, DoublySpecialCubicFourfold), (random, DoublySpecialCubicFourfold), (mirrorFourfold, DoublySpecialCubicFourfold), (quadricFibration, DoublySpecialCubicFourfold), (associatedK3surface, DoublySpecialCubicFourfold)}

undocumented {(surface, VisibleList, VisibleList, Ring, Option), (surface, VisibleList, VisibleList, Option), (surface, VisibleList, VisibleList, Ring), (surface, VisibleList, VisibleList)}

document {Key => {surfaces, (surfaces, DoublySpecialCubicFourfold)},
Headline => "get the special surfaces contained in the cubic fourfold",
Usage => "surfaces X",
Inputs => {"X" => DoublySpecialCubicFourfold},
Outputs => {{"the two surfaces contained in the fourfold ",TT"X"}},
EXAMPLE {"X = specialFourfold \"DSCF-6\";", "surfaces X"},
SeeAlso => {(surface,HodgeSpecialFourfold)}}

document {Key => {swap, (swap, DoublySpecialCubicFourfold)},
Headline => "swap the order of the two special surfaces",
Usage => "swap X",
Inputs => {"X" => DoublySpecialCubicFourfold},
Outputs => {DoublySpecialCubicFourfold => {"the same fourfold with the order of the two surfaces reversed."}},
EXAMPLE {"S = random({3:{1}},0_(PP_(ZZ/65521)^5));", "T = random S;", "X = specialFourfold (S & T);", "surfaces X", "Y = swap X;", "surfaces Y", "assert(X == Y)"},
SeeAlso => {surfaces}}
