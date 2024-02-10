beginDocumentation()
document { 
Key => CoincidentRootLoci, 
Headline => "A package for computations with coincident root loci",
PARA{"This package accompanies the paper ",HREF{"https://arxiv.org/abs/1804.08309","On the algebraic boundaries among typical ranks for real binary forms"}," (see also ",HREF{"https://arxiv.org/abs/1911.01958","Algebraic boundaries among typical ranks for real binary forms of arbitrary degree"},"). It provides some tools that can be useful for working with symmetric tensors of dimension 2. Such tensors are bijectively associated with homogeneous polynomials in two variables, which are also called binary forms. One of the main methods is ",TO realrank,", which uses ",HREF{"https://www.usna.edu/CS/qepcadweb/B/QEPCAD.html","QEPCAD"}," to compute the real rank of binary forms defined over ",TEX///$\mathbb{Q}$///,"."},
Contributors => {UL {{"Maria Chiara Brambilla <",HREF{"mailto: brambilla@dipmat.univpm.it","brambilla@dipmat.univpm.it"},">"}}}}
document { 
Key => {CoincidentRootLocus}, 
Headline => "the class of all coincident root loci",
PARA{"The coincident root locus associated with a partition ",TEX///$\lambda=(\lambda_1,\ldots,\lambda_d)$///," of a number ",TEX///$n$///," is the closed subvariety of the projective space ",TEX///$\mathbb{P}^n = \mathbb{P}(Sym^n(K^2))$///," given by binary forms that factor as ",TEX///${L_1}^{\lambda_1}\cdots{L_d}^{\lambda_d}$///, " for some linear forms ",TEX///$L_1,\ldots,L_d$///," over ",TEX///$\bar{K}$///,". For instance, if ",TEX///$\lambda=(2,1,\ldots,1)$///,", then we have the classical discriminant hypersurface; in the opposite case, if ",TEX///$\lambda=(n)$///," we have the rational normal curve of degree ",TEX///$n$///,"."}, 
SeeAlso => {coincidentRootLocus}}
document { 
Key => {coincidentRootLocus,(coincidentRootLocus,VisibleList,Ring),(coincidentRootLocus,List)},
Headline => "makes a coincident root locus", 
Usage => "coincidentRootLocus(l,K) 
coincidentRootLocus l", 
Inputs => {"l" => VisibleList => {"a partition of a number ",TEX///$n$///,", i.e., a list of ",TEX///$d$///," positive integers ",TEX///$l_1,\ldots,l_d$///," satisfying ",TEX///${\sum}_{i} l_i = n$///}, "K" => Ring => {"the coefficient ring (optional with default value ",TO QQ,")"}}, 
Outputs => {CoincidentRootLocus => {"the coincident root locus associated with the partition ",TEX///$l$///," over ",TEX///$K$///,", i.e., the subvariety of the projective ",TEX///$n$///,"-space ",TEX///$\mathbb{P}(Sym^n(K^2))$///," of all binary forms whose linear factors are distributed according to ",TEX///$l$///}}, 
EXAMPLE {"X = coincidentRootLocus {6,4,3,3,2}","describe X","describe dual X"}}
undocumented {(coincidentRootLocus,Thing)}
document { 
Key => {(dim, CoincidentRootLocus)},
Headline => "compute the dimension", 
Usage => "dim X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {ZZ => {"the dimension of ",TT"X"," as a projective variety"}},
PARA{"The dimension of a coincident root locus ",TEX///$X$///," associated with a partition ",TEX///$\lambda=(\lambda_1,\ldots,\lambda_d)$///," is ",TEX///$d$///,", the number of parts of ",TEX///$\lambda$///,"."},
EXAMPLE {"X = coincidentRootLocus {3,2,2,1,1,1,1}","dim X"},
SeeAlso => {(codim,CoincidentRootLocus)}}
document { 
Key => {(codim, CoincidentRootLocus)},
Headline => "compute the codimension", 
Usage => "codim X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {ZZ => {"the codimension of ",TT"X"," as a projective variety"}},
EXAMPLE {"X = coincidentRootLocus {3,2,2,1,1,1,1}","codim X"},
SeeAlso => {(dim,CoincidentRootLocus)}}
document { 
Key => {(degree, CoincidentRootLocus)},
Headline => "compute the degree", 
Usage => "degree X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {ZZ => {"the degree of ",TT"X"," as a projective variety"}},
PARA{"The formula for the degree of a coincident root locus ",TEX///$X$///," associated with a partition ",TEX///$\lambda=(\lambda_1,\ldots,\lambda_d)$///," was determined in the paper by D. Hilbert - Singularitaten der Diskriminantenflache - Math. Ann. 30, 437-441, 1887."},
EXAMPLE {"X = coincidentRootLocus {3,2,2,1,1,1,1}","degree X"}}
undocumented{(texMath,CoincidentRootLocus),(net,CoincidentRootLocus),(expression,CoincidentRootLocus),(symbol ==,CoincidentRootLocus,CoincidentRootLocus),(toString,CoincidentRootLocus)}
document { 
Key => {(map, CoincidentRootLocus)},
Headline => "the map associated to a coincident root locus", 
Usage => "map X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {RationalMap => {"a unirational parameterization of ",TT"X"}},
PARA{"If ",TEX///$X\subset\mathbb{P}^n$///," is the coincident root locus associated with the partition ",TEX///$(\lambda_1,\ldots,\lambda_d)$///,", then we have a map ",TEX///$\mathbb{P}^1\times\cdots\times\mathbb{P}^1\to\mathbb{P}^n$///," which sends a ",TEX///$d$///,"-tuple of linear forms ",TEX///$(L_1,\ldots,L_d)$///," to ",TEX///${L_1}^{\lambda_1}\cdots{L_d}^{\lambda_d}$///,". It is this map that is returned."},
EXAMPLE {"X = coincidentRootLocus {3,2,2}","f = map X","describe f"},
SeeAlso => {(ideal,CoincidentRootLocus)}}
document { 
Key => {(ideal, CoincidentRootLocus)},
Headline => "the defining ideal of a coincident root locus", 
Usage => "ideal X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {Ideal => {"the defining homogeneous ideal of ",TT"X"}},
PARA{"This is the same as ",TT "image map X","."},
EXAMPLE {"X = coincidentRootLocus {3,2}","ideal X"},
SeeAlso => {(map,CoincidentRootLocus)}}
document { 
Key => {(coefficientRing, CoincidentRootLocus)},
Headline => "get the coefficient ring", 
Usage => "coefficientRing X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {Ring => {"the coefficient ring of the ring of ",TT"X"}},
EXAMPLE {"X = coincidentRootLocus {3,2}","coefficientRing X","X' = coincidentRootLocus({3,2},ZZ/3331)","coefficientRing X'"},
SeeAlso => {(ring,CoincidentRootLocus)}}
document { 
Key => {(ring, CoincidentRootLocus)},
Headline => "get the ring of a coincident root locus", 
Usage => "ring X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {Ring => {"the coordinate ring of the target of the map of ",TT"X"}},
EXAMPLE {"X = coincidentRootLocus {3,2}","ring X","X' = coincidentRootLocus({3,2},ZZ/3331,Variable=>x)","ring X'"},
SeeAlso => {(coefficientRing,CoincidentRootLocus),(map,CoincidentRootLocus),(target,RationalMap)}}
document { 
Key => {(partition, CoincidentRootLocus)},
Headline => "the partition associated to a coincident root locus", 
Usage => "partition X", 
Inputs => {"X" => CoincidentRootLocus => {"associated with the partition ",TEX///$(\lambda_1,\ldots,\lambda_d)$///}},
Outputs => {List => {"the partition ",TEX///$(\lambda_1,\ldots,\lambda_d)$///}},
EXAMPLE {"X = coincidentRootLocus {3,2,2}","partition X"}}
document { 
Key => {(singularLocus, CoincidentRootLocus)},
Headline => "the singular locus of a coincident root locus", 
Usage => "singularLocus X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {List => {"a list of coincident root loci such that their union coincides with the singular locus of ",TT "X"}},
PARA{"The singular locus of a coincident root locus is the union of certain subloci, as described in the paper by J. Chipalkatti - On equations defining coincident root loci - J. Algebra 267, 246-271, 2003; see also the paper by S. Kurmann - Some remarks on equations defining coincident root loci - J. Algebra 352, 223-231, 2012."},
EXAMPLE {"X = coincidentRootLocus {3,2,2,1,1,1,1}","singularLocus X"},
SeeAlso => {(subsets,CoincidentRootLocus)}}
document { 
Key => {(subsets, CoincidentRootLocus)},
Headline => "produce all the subloci", 
Usage => "subsets X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {List => {"the list of the coincident root loci that are contained in ",TT "X"}},
EXAMPLE {"X = coincidentRootLocus {5,2,2,1}","subsets X"},
SeeAlso => {(isSubset,CoincidentRootLocus,CoincidentRootLocus),(supsets,CoincidentRootLocus)}}
document { 
Key => {(isSubset, CoincidentRootLocus, CoincidentRootLocus)},
Headline => "whether one object is a subset of another", 
Usage => "isSubset(X,Y)", 
Inputs => {"X" => CoincidentRootLocus => {"associated with a partition ",TEX///$\lambda$///},"Y" => CoincidentRootLocus => {"associated with a partition ",TEX///$\mu$///}},
Outputs => {Boolean => {"whether ",TEX///$X$///," is contained in ",TEX///$Y$///,", i.e., whether the partition ",TEX///$\mu$///," is a refinement of the partition ",TEX///$\lambda$///}},
EXAMPLE {"X = coincidentRootLocus {6,4}","Y = coincidentRootLocus {5,2,2,1}","isSubset(X,Y)","X' = coincidentRootLocus {6,3,1}","isSubset(X',Y)"},
SeeAlso => {(subsets,CoincidentRootLocus),(supsets,CoincidentRootLocus)}}
undocumented {(symbol ?,CoincidentRootLocus,CoincidentRootLocus)}
document { 
Key => {(supsets, CoincidentRootLocus),supsets},
Headline => "produce all the suploci", 
Usage => "supsets X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {List => {"the list of the coincident root loci that contain ",TT "X"}},
EXAMPLE {"X = coincidentRootLocus {4,2}","supsets X"},
SeeAlso => {(isSubset,CoincidentRootLocus,CoincidentRootLocus),(subsets,CoincidentRootLocus)}}
document { 
Key => {(generic, CoincidentRootLocus),generic},
Headline => "get the generic element", 
Usage => "generic X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {RingElement => {"the generic binary form which belongs to ",TT "X"}},
EXAMPLE {"X = coincidentRootLocus {3,1,1}","F = generic X","member(F,X)","factor F","G = generic(X,Reduce=>true)","member(G,X)","factor G"},
SeeAlso => {(random,CoincidentRootLocus),(member,RingElement,CoincidentRootLocus)}}
undocumented {(generic,PolynomialRing)}
document { 
Key => {[generic,Reduce]}, 
Headline => "reduce the number of variables", 
Usage => "generic(X,Reduce=>b)",
Inputs => {"b" => Boolean},
SeeAlso => {(generic,CoincidentRootLocus)}}
document { 
Key => {(random, CoincidentRootLocus)},
Headline => "get a random element", 
Usage => "random X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {RingElement => {"a random binary form which belongs to ",TT "X"}},
EXAMPLE {"X = coincidentRootLocus {3,3,2}","F = random X","member(F,X)","factor F"},
SeeAlso => {(member,RingElement,CoincidentRootLocus)}}
document { 
Key => {(member, RingElement, CoincidentRootLocus)},
Headline => "test membership in a coincident root locus", 
Usage => "member(F,X)", 
Inputs => {"F" => RingElement => {"a binary form"},"X" => CoincidentRootLocus},
Outputs => {Boolean => {"whether ",TEX///$F$///," belongs to ",TEX///$X$///}},
EXAMPLE {"R := QQ[x,y];","F = x^7-3*x^6*y+5*x^5*y^2-7*x^4*y^3+7*x^3*y^4-5*x^2*y^5+3*x*y^6-y^7","member(F,coincidentRootLocus {3,2,2})","member(F,coincidentRootLocus {5,2})"},
SeeAlso => {(random,CoincidentRootLocus)}}
document { 
Key => {(isInCoisotropic,Ideal,CoincidentRootLocus)},
Headline => "test membership in a coisotropic hypersurface", 
Usage => "isInCoisotropic(I,X)", 
Inputs => {"I" => Ideal => {"generated by ",TEX///$r+1$///," linearly independent binary forms of the same degree ",TEX///$n$///,", which corresponds to a point ",TEX///$p_I\in Grass(r,\mathbb{P}(Sym^n(K^2)))=Grass(r,n)$///},"X" => CoincidentRootLocus => {"of dimension ",TEX///$d$///," and codimension ",TEX///$n-d$///}},
Outputs => {Boolean => {"whether ",TEX///$p_I$///," belongs to the ",TEX///$(r+d+1-n)$///,"-th coisotropic hypersurface of ",TEX///$X$///}},
EXAMPLE {"X = coincidentRootLocus({2,2,1},ZZ/101)","I = randomInCoisotropic(X,1)","isInCoisotropic(I,X)"},
SeeAlso => {(randomInCoisotropic,CoincidentRootLocus,ZZ),isInCoisotropic}}
document { 
Key => {(randomInCoisotropic,CoincidentRootLocus,ZZ),randomInCoisotropic},
Headline => "get a random element", 
Usage => "randomInCoisotropic(X,i)", 
Inputs => {"X" => CoincidentRootLocus => {"of dimension ",TEX///$d$///," and codimension ",TEX///$n-d$///},"i" => ZZ},
Outputs => {Ideal => {"generated by ",TEX///$n-d+i$///," linearly independent binary forms of the same degree ",TEX///$n$///,", which corresponds to a random point of the ",TEX///$i$///,"-th coisotropic hypersurface ",TEX///$CH_i(X)\subset Grass(n-d-1+i,\mathbb{P}(Sym^n(K^2)))$///}},
EXAMPLE {"X = coincidentRootLocus({2,2,1},ZZ/101)","I = randomInCoisotropic(X,1)","isInCoisotropic(I,X)"},
SeeAlso => {(isInCoisotropic,Ideal,CoincidentRootLocus)}}
undocumented {(randomInCoisotropic,Ideal,ZZ),(randomInCoisotropic,Ideal,ZZ,Ideal)}
document { 
Key => {(dual, CoincidentRootLocus)},
Headline => "the projectively dual to a coincident root locus", 
Usage => "dual X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {{"the dual variety to ",TEX///$X$///}},
PARA{"The dual variety to a coincident root locus is the join of certain coincident root loci, as described in the paper by H. Lee and B. Sturmfels - Duality of multiple root loci - J. Algebra 446, 499-526, 2016."},
EXAMPLE {"X = coincidentRootLocus {5,3,2,2,1,1}","dual X"},
PARA{"In the example below, we apply some of the functions that are available for the returned objects."},
EXAMPLE {"Y = dual coincidentRootLocus {4,2}","ring Y","coefficientRing Y","dim Y","codim Y","degree Y","dual Y","G = random Y","member(G,Y)","ideal Y;","describe Y"},
SeeAlso => {(symbol *,CoincidentRootLocus,CoincidentRootLocus),dualVariety}}
undocumented {(describe,CoincidentRootLocus)}
document { 
Key => {apolar,(apolar,ZZ,ZZ,Ring),(apolar,ZZ,ZZ)},
Headline => "the apolar map", 
Usage => "apolar(n,s,K) 
apolar(n,s)", 
Inputs => {"n" => ZZ,"s" => ZZ,"K" => Ring => {"the coefficient ring (optional with default value ",TO QQ,")"}},
Outputs => {RationalMap => {"the map from ",TEX///$\mathbb{P}^n$///," to ",TEX///$Grass(2s-n-1,s)$///," that sends a generic binary form ",TEX///$F\in K[x,y]$///," of degree ",TEX///$n$///," to the set of binary forms of degree ",TEX///$s$///," that annihilate ",TEX///$F$///,", which can be identified with a ",TEX///$(2s-n-1)$///,"-dimensional linear subspace of ",TEX///$\mathbb{P}^s$///,"."}},
PARA{"We illustrate two ways of using this method."},
PARA{"1. Given a binary form ",TEX///$F$///," of degree ",TEX///$n$///," we can obtain a basis for the space of forms of degree ",TEX///$s$///," that annihilate ",TEX///$F$///,", say for example ",TEX///$(n,s)=(6,4)$///,"."},
EXAMPLE {"(n,s) = (6,4)","F = randomBinaryForm n","phi = apolar(n,s)","P = switch plucker phi switch switch F","diff(gens P,F) == 0"},
PARA{"2. We can recover the form ",TEX///$F$///," from the above output."},
EXAMPLE {"switch phi^* plucker switch P","oo == F"},
SeeAlso => {(apolar,RingElement),(apolar,RingElement,ZZ),(recover,Ideal),(switch,RingElement),plucker}}
document { 
Key => {(apolar,RingElement,ZZ)},
Headline => "homogeneous components of the apolar ideal", 
Usage => "apolar(F,s)", 
Inputs => {"F" => RingElement => {"a binary form"},"s" => ZZ},
Outputs => {Ideal => {"the ideal generated by the binary forms of degree ",TEX///$s$///," that annihilate ",TEX///$F$///}},
EXAMPLE {"F = randomBinaryForm 7","apolar(F,4)","apolar(F,5)"},
SeeAlso => {(apolar,RingElement),(apolar,ZZ,ZZ)}}
document { 
Key => {(apolar,RingElement)},
Headline => "the apolar ideal", 
Usage => "apolar F", 
Inputs => {"F" => RingElement => {"a binary form"}},
Outputs => {Ideal => {"the ideal generated by all binary forms that annihilate ",TEX///$F$///}},
EXAMPLE {"F = randomBinaryForm 7","apolar F"},
SeeAlso => {(apolar,RingElement,ZZ),(apolar,ZZ,ZZ)}}
document { 
Key => {recover,(recover,Ideal),(recover,RingElement,RingElement)},
Headline => "recover the binary form from its apolar ideal", 
Usage => "recover I", 
Inputs => {"I" => Ideal => {"the apolar ideal of a binary form ",TEX///$F\in K[x,y]$///,", or one of its homogeneous components of sufficiently large degree"}},
Outputs => {RingElement => {"the binary form ",TEX///$F$///," (up to a multiplicative constant)"}},
EXAMPLE {"F = randomBinaryForm 7","I = apolar F","I5 = apolar(F,5)","recover I","recover I5"},
SeeAlso => {(apolar,ZZ,ZZ),(apolar,RingElement),(apolar,RingElement,ZZ)}}
document {
Key => {(switch,RingElement),(switch,List),(switch,Ideal)},
PARA{"A binary form ",TEX///$F(t_0,t_1) = a_0\,t_0^n+n\,a_1\,t_0^{n-1}\,t_1+1/2\,n\,(n-1)\,a_2\,t_0^{n-2}\,t_1^2+\,\cdots\,+n\,a_{n-1}\,t_0\,t_1^{n-1}+a_n\,t_1^n$///," can be identified with the list ",TEX///$\{a_0,a_1,\ldots,a_n\}$///," of its coefficients, and also with the ideal of the corresponding point of ",TEX///$\mathbb{P}^n$///,". The method ",TT "switch"," when applied to a binary form returns the list of its coefficients; when applied to a list of coefficients returns the ideal of the corresponding point; when applied to the ideal of a point returns the corresponding binary form."},
EXAMPLE {"F = randomBinaryForm 9","L = switch F","I = switch L","switch I","oo == F"}}
document { 
Key => {realrank,(realrank,RingElement)},
Headline => "compute the real rank", 
Usage => "realrank F", 
Inputs => {"F" => RingElement => {"a binary form ",TEX///$F\in K[x,y]$///," of degree ",TEX///$d$///,", where ",TEX///$K$///," is either ",TEX///$\mathbb{Q}$///, " or a polynomial ring over ",TEX///$\mathbb{Q}$///," (currently, with only one variable)"}},
Outputs => {ZZ => {"the real rank of ",TEX///$F$///,", i.e., the minimum integer ",TEX///$r$///," such that there is a decomposition ",TEX///$F = c_1\,(l_1)^d+\cdots+c_r\,(l_r)^d$///," where ",TEX///$l_1,\ldots,l_r$///," are real linear forms and ",TEX///$c_1,\ldots,c_r\in\mathbb{R}$///}},
PARA{"This method requires generally the program ",HREF{"https://www.usna.edu/CS/qepcadweb/B/QEPCAD.html","QEPCAD"}," to be installed. Source code and installation instructions for it are available at ",HREF{"https://www.usna.edu/CS/qepcadweb/INSTALL/IQ.html","Downloading and Installing QEPCAD"},"."},
PARA{"Below we compute the real rank of a binary form of degree 7."},
if CoincidentRootLoci.Options.OptionalComponentsPresent 
then EXAMPLE {"R := QQ[x,y];","F = 2*x^7+7*x^6*y+168*x^5*y^2+140*x^4*y^3+70*x^3*y^4+21*x^2*y^5+56*x*y^6+4*y^7","realrank F"}
else PRE ///i1 : R := QQ[x,y];

i2 : F = 2*x^7+7*x^6*y+168*x^5*y^2+140*x^4*y^3+70*x^3*y^4+21*x^2*y^5+56*x*y^6+4*y^7

       7     6        5 2       4 3      3 4      2 5        6     7
o2 = 2x  + 7x y + 168x y  + 140x y  + 70x y  + 21x y  + 56x*y  + 4y

o2 : QQ[x..y]

i3 : realrank F

o3 = 5
///,
PARA{"In the case when the coefficient ring ",TEX///$K$///," contains a variable, say ",TEX///$u$///,", then the method returns a value ",TEX///$r$///," if the real rank of ",TEX///$F$///," is ",TEX///$r$///," for all the real values of ",TEX///$u$///," in the range specified by the option ",TO [realrank, Range],". An error is thrown if the answer is not uniform."},
if CoincidentRootLoci.Options.OptionalComponentsPresent 
then EXAMPLE {"Ru := QQ[u][x,y];","F = u*x^4*y+2*x^2*y^3","realrank(F,Range=>(0,infinity))","realrank(F,Range=>[-1,0])","realrank(F,Range=>(-infinity,-1))"}
else PRE ///
i4 : Ru := QQ[u][x,y];

i5 : F = u*x^4*y+2*x^2*y^3

        4      2 3
o5 = u*x y + 2x y

o5 : QQ[u][x..y]

i6 : realrank(F,Range=>(0,infinity))

o6 = 3

i7 : realrank(F,Range=>[-1,0])

o7 = 3

i8 : realrank(F,Range=>(-infinity,-1))

o8 = 3
///,
SeeAlso => {complexrank}}
document { 
Key => {complexrank,(complexrank,RingElement)},
Headline => "compute the complex rank", 
Usage => "complexrank F", 
Inputs => {"F" => RingElement => {"a binary form ",TEX///$F\in K[x,y]$///," of degree ",TEX///$d$///,", where ",TEX///$K$///," is either ",TEX///$\mathbb{Q}$///,", or more generally a field"}},
Outputs => {ZZ => {"the rank of ",TEX///$F$///,", i.e., the minimum integer ",TEX///$r$///," such that there is a decomposition ",TEX///$F = c_1\,(l_1)^d+\cdots+c_r\,(l_r)^d$///," where ",TEX///$l_1,\ldots,l_r\in\bar{K}[x,y]$///," are linear forms and ",TEX///$c_1,\ldots,c_r\in\bar{K}$///}},
PARA{"This method provides a quick way to calculate the complex rank of a binary form as an application of the methods ",TO (apolar,RingElement,ZZ)," and ",TO (discriminant,RingElement),"."},
EXAMPLE {"R := QQ[x,y];","F = 325699392019820093805938500473136959995883*x^11-5810907570924644857232186920803498012892938*x^10*y+65819917752061707843768328400359649501719860*x^9*y^2-519457154316395169830396776661486079064173600*x^8*y^3+1705429425321816258526777767700378341505324800*x^7*y^4-3810190868583760635545828188931628645390528000*x^6*y^5+9250941324308079844692884039573393626015320480*x^5*y^6-9323164714263069666482962682446368124512793200*x^4*y^7+1072684515031339121680779290598231336889158000*x^3*y^8-66208958025372412656331871291180685863962950*x^2*y^9-3357470237827984950448384820635661305324565*x*y^10+2036327846200712576945384935680953020530520*y^11","complexrank F"},
SeeAlso => {realrank}}
document { 
Key => {[realrank,Range]}, 
Headline => "can be assigned an interval", 
Usage => "realrank(F,Range=>(a,b)) 
realrank(F,Range=>[a,b])",
Inputs => {"F" => RingElement => {"a binary form ",TEX///$F_u(x,y)\in \mathbb{Q}[u][x,y]$///},"a" => QQ => {"or an ",TO InfiniteNumber},"b" => QQ => {"or an ",TO InfiniteNumber}},
Outputs => {"r" => ZZ => {"if the real rank of ",TEX///$F_u(x,y)$///," is ",TEX///$r$///," for all the values ",TEX///$u\in(a,b)$///," or ",TEX///$u\in[a,b]$///}},
PARA{"If the real rank of ",TEX///$F_u(x,y)$///," is not constant over the interval, then an error message will be returned. The default value is ",TT"(-infinity,infinity)","."}} 
document { 
Key => {QepcadOptions,[realrank,QepcadOptions]}, 
Headline => "set the number of cells in the garbage collected space", 
Usage => "realrank(F,QepcadOptions=>(a,b))",
Inputs => {"a" => ZZ,"b" => ZZ},
PARA{"This specifies how the program QEPCAD is executed: ",TT"$qe/bin/qepcad +Na +Lb",". For details, we refer to the documentation ",HREF{"https://www.usna.edu/CS/qepcadweb/B/user/UsingQEPCAD.html","QEPCAD - Basic User Information"},"."}} 
document { 
Key => {[realrank,Verbose],[realroots,Verbose]}, 
Headline => "request verbose feedback", 
Usage => "realrank(F,Verbose=>b) 
realroots(F,Verbose=>b)",
Inputs => {"b" => Boolean}}
document { 
Key => {(chowForm,CoincidentRootLocus)}, 
Headline => "Chow form of a coincident root locus", 
Usage => "chowForm X", 
Inputs => {"X" => CoincidentRootLocus}, 
Outputs => {{"the Chow form of ",TEX///$X$///}},
PARA{"This is equivalent to ",TT "chowForm ideal X",", but should be faster. See ",TO (chowForm,Ideal),"."},
EXAMPLE {"X = coincidentRootLocus {3,1}","chowForm X"},
SeeAlso => {(chowForm,Ideal)}}
document { 
Key => {[coincidentRootLocus,Variable], [apolar,Variable], [randomBinaryForm,Variable], [generic,Variable], [realRankBoundary,Variable]}, 
Headline => "specify a name for a variable", 
Usage => "Variable => x",
Inputs => {"x" => Symbol},
PARA{"This is an option used to specify a symbol to be used as a name for the generator of the ambient polynomial ring."}}
document { 
Key => {[realrank,Limit],[complexrank,Limit]}, 
Headline => "set a bound for the rank", 
Usage => "realrank(F,Limit=>r) 
complexrank(F,Limit=>r)",
Inputs => {"r" => ZZ},
PARA{"This tells the method to search the rank in the range between 1 and ",TEX///$r-1$///,". If the output is not an integer then the rank is greater than or equal to ",TEX///$r$///,"."},
SeeAlso => {realrank,complexrank}}
document { 
Key => {randomBinaryForm,(randomBinaryForm,ZZ),(randomBinaryForm,ZZ,Ring),(randomBinaryForm,ZZ,Thing,Thing),(randomBinaryForm,ZZ,Thing,Thing,Ring)}, 
Headline => "random homogeneous polynomial in two variables", 
Usage => "randomBinaryForm d 
randomBinaryForm(d,r,c) 
randomBinaryForm(d,r,)
randomBinaryForm(d,,c)", 
Inputs => {"d" => ZZ,"r" => ZZ,"c" => ZZ}, 
Outputs => {RingElement => {"a random binary form of degree ",TT"d",", real rank ",TT "r"," and complex rank ",TT "c"}},
EXAMPLE {"F = randomBinaryForm 5","F = randomBinaryForm(5,4,3)","(realrank F,complexrank F)","F = randomBinaryForm(6,4,4)","(realrank F,complexrank F)",},
SeeAlso => {realrank,complexrank}}
document { 
Key => {realroots,(realroots,RingElement)}, 
Headline => "real roots of a binary form", 
Usage => "realroots F", 
Inputs => {"F" => RingElement => {"a binary form over ",TEX///$\mathbb{Q}$///}},
Outputs => {List => {"a list of pairs of elements of ",TO RR,", the real roots of ",TEX///$F$///," (multiple roots appear multiple times)"}},
PARA{"This is a simple application of the method ",TO roots,"."},
EXAMPLE {"F = randomBinaryForm 5","realroots F"},
SeeAlso => {roots}}
document {
Key => {realRankBoundary,(realRankBoundary,ZZ,ZZ),(realRankBoundary,ZZ,ZZ,Ring)},
Headline => "algebraic boundaries among typical ranks for real binary forms",
Usage => "realRankBoundary(n,i)",
Inputs => {"n" => ZZ, "i" => ZZ => {"a typical rank"}},
Outputs => {{"the irreducible components of the real algebraic boundary that separates real rank ",TEX///$i$///," from the other typical ranks in the real projective space ",TEX///$\mathbb{P}^n=\mathbb{P}(Sym^n(\mathbb{R}^2))$///," of binary forms of degree ",TEX///$n$///}},
PARA{"Define ",TEX///${\mathcal{R}}_{n,i}$///," as the interior of the set ",TEX///$\{F\in Sym^n(\mathbb{R}^2) : realrank(F) = i\}$///,". Then ",TEX///${\mathcal{R}}_{n,i}$///," is a semi-algebraic set which is non-empty exactly when ",TEX///$(n+1)/2 \leq i\leq n$///," (in this case we say that ",TEX///$i$///," is a typical rank); see the paper by G. Blekherman - Typical real ranks of binary forms - Found. Comput. Math. 15, 793-798, 2015. The topological boundary ",TEX///$\partial({\mathcal{R}}_{n,i})$///," is the set-theoretic difference of the closure of ",TEX///${\mathcal{R}}_{n,i}$///," minus the interior of the closure of ",TEX///${\mathcal{R}}_{n,i}$///,". In the range ",TEX///$(n+1)/2 \leq i\leq n-1$///,", it is a semi-algebraic set of pure codimension one. The (real) algebraic boundary ",TEX///$\partial_{alg}({\mathcal{R}}_{n,i})$///," is defined as the Zariski closure of the topological boundary ",TEX///$\partial({\mathcal{R}}_{n,i})$///,". This is viewed as a hypersurface in ",TEX///$\mathbb{P}(Sym^n(\mathbb{R}^2))$///," and the method returns its irreducible components over ",TEX///$\mathbb{C}$///,"."},
PARA{"In the case ",TEX///$i = n$///,", the algebraic boundary ",TEX///$\partial_{alg}({\mathcal{R}}_{n,n})$///," is the discriminant hypersurface; see the paper by A. Causa and R. Re - On the maximum rank of a real binary form - Ann. Mat. Pura Appl. 190, 55-59, 2011; see also the paper by P. Comon, G. Ottaviani - On the typical rank of real binary forms - Linear Multilinear Algebra 60, 657-667, 2012."},
EXAMPLE {"D77 = realRankBoundary(7,7)","describe D77"},
PARA{"In the opposite extreme case, ",TEX///$i = ceiling((n+1)/2)$///,", the algebraic boundary ",TEX///$\partial_{alg}({\mathcal{R}}_{n,i})$///," has been described in the paper by H. Lee and B. Sturmfels - Duality of multiple root loci - J. Algebra 446, 499-526, 2016. It is irreducible if ",TEX///$n$///," is odd, and has two irreducible components if ",TEX///$n$///," is even."},  
EXAMPLE {"D64 = realRankBoundary(6,4)","describe first D64","describe last D64","D74 = realRankBoundary(7,4)","describe D74"},
PARA{"In the next example, we compute the irreducible components of the algebraic boundaries ",TEX///$\partial_{alg}({\mathcal{R}}_{7,5})$///," and ",TEX///$\partial_{alg}({\mathcal{R}}_{7,6})$///,"."},
EXAMPLE {"D75 = realRankBoundary(7,5)","describe D75_0","describe D75_1","describe D75_2","D76 = realRankBoundary(7,6)","describe D76_0","describe D76_1","describe D76_2"},
SeeAlso => {CoincidentRootLocus,(dual,CoincidentRootLocus),realrank}}
document { 
Key => {(symbol *,CoincidentRootLocus, CoincidentRootLocus),projectiveJoin},
Headline => "projective join of coincident root loci", 
Usage => "X * Y 
projectiveJoin(X,Y)", 
Inputs => {"X" => CoincidentRootLocus,"Y" => CoincidentRootLocus},
Outputs => {{"the projective join of ",TEX///$X$///," and ",TEX///$Y$///}},
PARA{"A partition of a number ",TEX///$n$///," is a hook if at most one part is not 1. The inputs of this method are required to be coincident root loci associated with hook partitions of ",TEX///$n$///,". In this case, the returned object is the dual of a certain coincident root locus; see the paper by H. Lee and B. Sturmfels - Duality of multiple root loci - J. Algebra 446, 499-526, 2016."},
EXAMPLE {"X = coincidentRootLocus {11,1,1,1,1}","Y = coincidentRootLocus {13,1,1}","X * Y","X * Y * Y"},
PARA{"More generally, if ",TEX///$I_1,I_2,\ldots$///," is a sequence of homogeneous ideals (resp. parameterizations) of projective varieties ",TEX///$X_1,X_2,\ldots \subset \mathbb{P}^n$///,", then ",TT "projectiveJoin(I_1,I_2,...)"," is the ideal of the projective join ",TEX///$X_1\,*\,X_2\,*\,\cdots \subset \mathbb{P}^n$///,"."},
EXAMPLE {"I = ideal coincidentRootLocus {4}", "projectiveJoin(I,I)"},
SeeAlso => {(dual,CoincidentRootLocus)}}
undocumented {(projectiveJoin,Thing)}
undocumented {(symbol +,CoincidentRootLocus, CoincidentRootLocus)}
document { 
Key => {projectiveTangentSpace,(projectiveTangentSpace,CoincidentRootLocus,RingElement)},
Headline => "projective tangent space", 
Usage => "projectiveTangentSpace(X,F)", 
Inputs => {"X" => CoincidentRootLocus,"F" => RingElement => {"a binary form that belongs to ",TT"X"}},
Outputs => {Ideal => {"generated by binary forms of the same degree as that of ",TT"F",", which corresponds to the projective tangent space to ",TT"X"," at ",TT"F"}},
EXAMPLE {"R = QQ[x,y];", "F = x^7+2*x^6*y-x^5*y^2-4*x^4*y^3-x^3*y^4+2*x^2*y^5+x*y^6", "X = coincidentRootLocus(4,2,1)", "projectiveTangentSpace(X,F)"},
SeeAlso => {(member,RingElement,CoincidentRootLocus),(singularLocus,CoincidentRootLocus)}}
document { 
Key => {polarDegrees, (polarDegrees, CoincidentRootLocus)},
Headline => "polar degrees of a coincident root locus", 
Usage => "polarDegrees X", 
Inputs => {"X" => CoincidentRootLocus},
Outputs => {List => {"the polar degrees of ",TT"X"}},
PARA{"This method is based on a conjecture given in ",HREF{"https://arxiv.org/abs/1911.01958","Algebraic boundaries among typical ranks for real binary forms of arbitrary degree"},", which has been verified for all the coincident root loci ",TEX///$X\subset\mathbb{P}^r$///," with ",TEX///$r\leq 7$///,"."}, 
EXAMPLE {"X = coincidentRootLocus(3,2,2)","polarDegrees X"},
SeeAlso => {conormalVariety, multidegree}}

