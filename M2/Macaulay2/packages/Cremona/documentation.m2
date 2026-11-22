beginDocumentation() 

document { 
Key => Cremona, 
Headline => "package for some computations on rational maps between projective varieties", 
EM "Cremona", " is a package to perform some basic computations on rational and birational maps between absolutely irreducible projective varieties over a field ",TEX///$K$///,". For instance, it provides general methods to compute degrees and projective degrees of rational maps (see ", TO"degreeMap", " and ", TO"projectiveDegrees",") and a general method to compute the push-forward to projective space of Segre classes (see ",TO"SegreClass","). Moreover, all the main methods are available both in version probabilistic and in version deterministic, and one can switch from one to the other with the boolean option ",TO "Certify",".",
PARA{BOLD "Legacy status. ","The rational maps of this package are objects of the class ",TO LegacyRationalMap," (constructed with ",TO legacyRationalMap,"). The prefix ",EM "legacy"," means that this class is kept mainly for compatibility: for new code, it is recommended to use the package ",TO "MultiprojectiveVarieties::MultiprojectiveVarieties",", whose class ",TO "MultiprojectiveVarieties::MultirationalMap"," supersedes ",TO LegacyRationalMap," and also handles rational maps between multi-projective varieties. Up to version 5.2.1 of ",EM "Cremona",", the class and its constructor were called ",TT "RationalMap"," and ",TT "rationalMap","; these names are still available as deprecated synonyms (see ",TO rationalMap,")."},
PARA{"Let ",TEX///$\Phi:X \dashrightarrow Y$///," be a rational map from a subvariety ",TEX///$X=V(I)\subseteq\mathbb{P}^n=Proj(K[x_0,\ldots,x_n])$///," to a subvariety ",TEX///$Y=V(J)\subseteq\mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". Then the map ",TEX///$\Phi $///," can be represented, although not uniquely, by a homogeneous ring map ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," of quotients of polynomial rings by homogeneous ideals. These kinds of ring maps, together with the objects of the ",TO LegacyRationalMap," class, are the typical inputs for the methods in this package. The method ", TO toMap," (resp. ",TO legacyRationalMap,") constructs such a ring map (resp. rational map) from a list of ",TEX///$m+1$///," homogeneous elements of the same degree in ",TEX///$K[x_0,...,x_n]/I$///,"."}, 
PARA{"Below is an example using the methods provided by this package, dealing with a birational transformation ",TEX///$\Phi:\mathbb{P}^6 \dashrightarrow \mathbb{G}(2,4)\subset\mathbb{P}^9$///," of bidegree ",TEX///$(3,3)$///,"."},
EXAMPLE { 
"ZZ/300007[t_0..t_6];", 
"time phi = toMap minors(3,matrix{{t_0..t_4},{t_1..t_5},{t_2..t_6}})", 
"time J = kernel(phi,2)", 
"time degreeMap phi", 
"time projectiveDegrees phi", 
"time projectiveDegrees(phi,NumDegrees=>0)", 
"time phi = toMap(phi,Dominant=>J)", 
"time psi = inverseMap phi", 
"time isInverseMap(phi,psi)",
"time degreeMap psi", 
"time projectiveDegrees psi"}, 
PARA{"We repeat the example using the type ",TO LegacyRationalMap, " and using deterministic methods."},
EXAMPLE { 
"time phi = legacyRationalMap minors(3,matrix{{t_0..t_4},{t_1..t_5},{t_2..t_6}})", 
"time phi = legacyRationalMap(phi,Dominant=>2)", 
"time phi^(-1)", 
"time degrees phi^(-1)", 
"time degrees phi", 
"time describe phi",
"time describe phi^(-1)",
"time (f,g) = graph phi^-1; f;",
"time degrees f",
"time degree f",
"time describe f"}, 
PARA{"A rudimentary version of ",EM"Cremona"," has been already used in an essential way in the paper ",HREF{"http://dx.doi.org/10.1016/j.jsc.2015.11.004","doi:10.1016/j.jsc.2015.11.004"}," (it was originally named ", HREF{"http://goo.gl/eT4rCR","bir.m2"},")."}} 

document { 
Key => {degreeMap, (degreeMap,RingMap)}, 
Headline => "degree of a rational map between projective varieties", 
Usage => "degreeMap phi", 
Inputs => { "phi" => RingMap => {"which represents a rational map ",TEX///$\Phi$///," between projective varieties"}}, 
Outputs => {ZZ => {"the degree of ",TEX///$\Phi$///,". So this value is 1 if and only if the map is birational onto its image."}}, 
PARA{"One important case is when ",TEX///$\Phi:\mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) \dashrightarrow \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///," is a rational map between projective spaces, corresponding to a ring map ",TEX///$\phi$///,". If ",TEX///$p$///," is a general point of ",TEX///$\mathbb{P}^n$///,", denote by ",TEX///$F_p(\Phi)$///," the closure of ",TEX///$\Phi^{-1}(\Phi(p))\subseteq \mathbb{P}^n$///,". The degree of ",TEX///$\Phi$///," is defined as the degree of ",TEX///$F_p(\Phi)$///," if ",TEX///$dim F_p(\Phi) = 0$///," and ",TEX///$0$///," otherwise. If ",TEX///$\Phi$///," is defined by forms ",TEX///$F_0(x_0,\ldots,x_n),\ldots,F_m(x_0,\ldots,x_n)$///," and ",TEX///$I_p$///," is the ideal of the point ",TEX///$p$///,", then the ideal of ",TEX///$F_p(\Phi)$///," is nothing but the saturation ",TEX///${(\phi(\phi^{-1}(I_p))):(F_0,....,F_m)}^{\infty}$///,"."}, 
EXAMPLE { 
"-- Take a rational map phi:P^8--->G(1,5) subset P^14 defined by the maximal minors 
-- of a generic 2 x 6 matrix of linear forms on P^8 (thus phi is birational onto its image)
K=ZZ/3331; ringP8=K[x_0..x_8]; ringP14=K[t_0..t_14];",
"phi=map(ringP8,ringP14,gens minors(2,matrix pack(6,for i to 11 list random(1,ringP8))))",
"time degreeMap phi",
"-- Compose phi:P^8--->P^14 with a linear projection P^14--->P^8 from a general subspace of P^14 
-- of dimension 5 (so that the composition phi':P^8--->P^8 must have degree equal to deg(G(1,5))=14)
phi'=phi*map(ringP14,ringP8,for i to 8 list random(1,ringP14))",
"time degreeMap phi'"},
SeeAlso => {(degree,LegacyRationalMap),projectiveDegrees}} 

undocumented{(projectiveDegrees,LegacyRationalMap,ZZ),(projectiveDegrees,RingMap,ZZ)}

document { 
Key => {projectiveDegrees,(projectiveDegrees,RingMap)}, 
Headline => "projective degrees of a rational map between projective varieties", 
Usage => "projectiveDegrees phi", 
Inputs => { "phi" => RingMap => {"which represents a rational map ",TEX///$\Phi$///," between projective varieties"}}, 
Outputs => {List => {"the list of the projective degrees of ",TEX///$\Phi$///}}, 
PARA{"Let ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," be a ring map representing a rational map ",TEX///$\Phi: V(I) \subseteq \mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) \dashrightarrow V(J) \subseteq \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". The ",TEX///$i$///,"-th projective degree of ",TEX///$\Phi$///," is defined in terms of dimension and degree of the closure of ",TEX///$\Phi^{-1}(L)$///,", where ",TEX///$L$///," is a general linear subspace of ",TEX///$\mathbb{P}^m$///," of a certain dimension; for the precise definition, see Harris's book (Algebraic geometry: A first course - Vol. 133 of Grad. Texts in Math., p. 240). If ",TEX///$\Phi$///," is defined by elements ",TEX///$F_0(x_0,\ldots,x_n),\ldots,F_m(x_0,\ldots,x_n)$///," and ",TEX///$I_L$///," denotes the ideal of the subspace ",TEX///$L\subseteq \mathbb{P}^m$///,", then the ideal of the closure of ",TEX///$\Phi^{-1}(L) $///," is nothing but the saturation of the ideal ",TEX///$(\phi(I_L))$///," by ",TEX///$(F_0,....,F_m)$///," in the ring ",TEX///$K[x_0,\ldots,x_n]/I$///,". So, replacing in the definition, ", EM "general linear subspace", " by ", EM "random linear subspace", ", we get a probabilistic algorithm to compute all projective degrees. Furthermore, we can considerably speed up this algorithm by taking into account two simple remarks: 1) the saturation ",TEX///$(\phi(I_L)):{(F_0,\ldots,F_m)}^{\infty}$///," is the same as ",TEX///$(\phi(I_L)):{(\lambda_0 F_0+\cdots+\lambda_m F_m)}^{\infty}$///,", where ",TEX///$\lambda_0,\ldots,\lambda_m\in\mathbb{K}$///," are general scalars; 2) the ",TEX///$i$///,"-th projective degree of ",TEX///$\Phi$///," coincides with the ",TEX///$(i-1)$///,"-th projective degree of the restriction of ",TEX///$\Phi$///," to a general hyperplane section of ",TEX///$X$///," (see ",EM"loc. cit.","). This is what the method uses if ", TO Certify, " is set to ", TT "false",". If instead ", TO Certify, " is set to ", TT "true", ", then the method simply computes the ",TO "multidegree"," of the ",TO "graph","."},
EXAMPLE { 
"-- map from P^4 to G(1,3) given by the quadrics through a rational normal curve of degree 4
GF(331^2)[t_0..t_4]; phi=toMap minors(2,matrix{{t_0..t_3},{t_1..t_4}})", 
"time projectiveDegrees(phi,Certify=>true)",
"psi=inverseMap(toMap(phi,Dominant=>infinity))", 
"time projectiveDegrees(psi,Certify=>true)"}, 
EXAMPLE { 
"-- Cremona transformation of P^6 defined by the quadrics through a rational octic surface
phi = map specialCremonaTransformation(7,ZZ/300007)",
"time projectiveDegrees phi", 
"time projectiveDegrees(phi,NumDegrees=>1)"}, 
PARA{"Another way to use this method is by passing an integer ",TT"i"," as second argument. However, this is equivalent to ",TT"first projectiveDegrees(phi,NumDegrees=>i)", " and generally it is not faster."},
SeeAlso => {(degrees,LegacyRationalMap),degreeMap, SegreClass}} 

document { 
Key => {Certify, [inverseMap,Certify], [projectiveDegrees,Certify],[degreeMap,Certify],[approximateInverseMap,Certify],[isDominant,Certify],[isBirational,Certify],[SegreClass,Certify],[ChernSchwartzMacPherson,Certify],[EulerCharacteristic,Certify],[exceptionalLocus,Certify]}, 
Headline => "whether to ensure correctness of output", 
"This option accepts a ", TO Boolean, " value, default value ",TT "false",".",
PARA{"If turned on in the methods ", TO inverseMap," and ", TO approximateInverseMap, ", then it will be checked whether the maps in input and output are one the inverse of the other, throwing an error if they are not. Actually, ", TO approximateInverseMap, " will first try to fix the error of the approximation. When turned on in the methods ", TO projectiveDegrees,", ", TO degreeMap, ", ", TO isBirational,", ", TO isDominant, ", ", TO SegreClass, ", ", TO EulerCharacteristic, " and ", TO ChernSchwartzMacPherson, ", it means whether to use a non-probabilistic algorithm."}} 

document { 
Key => {Dominant, [toMap,Dominant], [legacyRationalMap,Dominant], [rationalMap,Dominant]}, 
-- Headline => "makes a dominant rational map" , 
"This is an optional argument for ", TO toMap,". When ", TO true," or ",TO infinity," is passed to this option, the kernel of the returned ring map will be zero."} 

document { 
Key => {NumDegrees, [projectiveDegrees,NumDegrees]}, 
"This is an optional argument for ", TO projectiveDegrees, " and accepts a non-negative integer, 1 less than the number of projective degrees to be computed."} 

document {
Key => {CodimBsInv, [approximateInverseMap,CodimBsInv]}, 
"This is a technical option for ", TO approximateInverseMap, ". It accepts an integer which should be a lower bound for the codimension of the base locus of the inverse map. In most cases, one can obtain the optimal value to be passed as in the following example.", 
PARA{},
EXAMPLE { 
"codimBsInv = (m) -> (
   -- input: m, the list of projective degrees of a birational map
   -- output: the codimension of the base locus of the inverse map
   k:=#m -1; z:=m_k; d:=floor(m_(k-1)/z);
   for i from 2 to k do if z*d^i - m_(k-i) > 0 then return i;
);",
"phi = toMap trim minors(2,genericSymmetricMatrix(QQ[x_0..x_5],3))",
"codimBsInv projectiveDegrees phi"},
PARA{"However, sometimes larger values may be preferable."}} 

undocumented{(kernel,ZZ,RingMap)}

document { 
Key => {(kernel,RingMap,ZZ)}, 
Headline => "homogeneous components of the kernel of a homogeneous ring map", 
Usage => "kernel(phi,d)", 
Inputs => { 
"phi" => RingMap => {TEX///$K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///,", defined by homogeneous forms of the same degree and where ",TEX///$J$///," and ",TEX///$I$///," are homogeneous ideals"}, 
"d" => ZZ}, 
Outputs => { {"the ",TO2{Ideal,"ideal"}," generated by all homogeneous elements of degree ", TT "d"," belonging to the kernel of ",TT "phi"}}, 
PARA{"This is equivalent to ",TT "ideal image basis(d,kernel phi)",", but we use a more direct algorithm. We take advantage of the homogeneity and reduce the problem to linear algebra. For small values of ",TT"d", " this method can be very fast, as the following example shows."},
EXAMPLE { 
"phi = toMap map specialQuadraticTransformation 8",
"time kernel(phi,1)",
"time kernel(phi,2)"},
SeeAlso => {(kernel,RingMap)}} 

document { 
Key => {isBirational,(isBirational,LegacyRationalMap),(isBirational,RingMap)}, 
Headline => "whether a rational map is birational", 
Usage => "isBirational phi", 
Inputs => { 
"phi" => LegacyRationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi"," is birational"}},
PARA{"The testing passes through the methods ", TO projectiveDegrees, ", ", TO degreeMap," and ", TO isDominant,"."},
EXAMPLE { 
"GF(331^2)[t_0..t_4]",
"phi = legacyRationalMap(minors(2,matrix{{t_0..t_3},{t_1..t_4}}),Dominant=>infinity)",
"time isBirational phi",
"time isBirational(phi,Certify=>true)"},
SeeAlso => {isDominant}}

document { 
Key => {isDominant,(isDominant,LegacyRationalMap),(isDominant,RingMap)}, 
Headline => "whether a rational map is dominant", 
Usage => "isDominant phi", 
Inputs => { 
"phi" => LegacyRationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi"," is dominant"}},
PARA{"This method is based on the fibre dimension theorem. A more standard way would be to perform the command ", TT "kernel map phi == 0","."},
EXAMPLE { 
"P8 = ZZ/101[x_0..x_8];",
"phi = legacyRationalMap ideal jacobian ideal det matrix{{x_0..x_4},{x_1..x_5},{x_2..x_6},{x_3..x_7},{x_4..x_8}};",
"time isDominant(phi,Certify=>true)",
"P7 = ZZ/101[x_0..x_7];", 
"-- hyperelliptic curve of genus 3
C = ideal(x_4*x_5+23*x_5^2-23*x_0*x_6-18*x_1*x_6+6*x_2*x_6+37*x_3*x_6+23*x_4*x_6-26*x_5*x_6+2*x_6^2-25*x_0*x_7+45*x_1*x_7+30*x_2*x_7-49*x_3*x_7-49*x_4*x_7+50*x_5*x_7,x_3*x_5-24*x_5^2+21*x_0*x_6+x_1*x_6+46*x_3*x_6+27*x_4*x_6+5*x_5*x_6+35*x_6^2+20*x_0*x_7-23*x_1*x_7+8*x_2*x_7-22*x_3*x_7+20*x_4*x_7-15*x_5*x_7,x_2*x_5+47*x_5^2-40*x_0*x_6+37*x_1*x_6-25*x_2*x_6-22*x_3*x_6-8*x_4*x_6+27*x_5*x_6+15*x_6^2-23*x_0*x_7-42*x_1*x_7+27*x_2*x_7+35*x_3*x_7+39*x_4*x_7+24*x_5*x_7,x_1*x_5+15*x_5^2+49*x_0*x_6+8*x_1*x_6-31*x_2*x_6+9*x_3*x_6+38*x_4*x_6-36*x_5*x_6-30*x_6^2-33*x_0*x_7+26*x_1*x_7+32*x_2*x_7+27*x_3*x_7+6*x_4*x_7+36*x_5*x_7,x_0*x_5+30*x_5^2-11*x_0*x_6-38*x_1*x_6+13*x_2*x_6-32*x_3*x_6-30*x_4*x_6+4*x_5*x_6-28*x_6^2-30*x_0*x_7-6*x_1*x_7-45*x_2*x_7+34*x_3*x_7+20*x_4*x_7+48*x_5*x_7,x_3*x_4+46*x_5^2-37*x_0*x_6+27*x_1*x_6+33*x_2*x_6+8*x_3*x_6-32*x_4*x_6+42*x_5*x_6-34*x_6^2-37*x_0*x_7-28*x_1*x_7+10*x_2*x_7-27*x_3*x_7-42*x_4*x_7-8*x_5*x_7,x_2*x_4-25*x_5^2-4*x_0*x_6+2*x_1*x_6-31*x_2*x_6-5*x_3*x_6+16*x_4*x_6-24*x_5*x_6+31*x_6^2-30*x_0*x_7+32*x_1*x_7+12*x_2*x_7-40*x_3*x_7+3*x_4*x_7-28*x_5*x_7,x_0*x_4+15*x_5^2+48*x_0*x_6-50*x_1*x_6+46*x_2*x_6-48*x_3*x_6-23*x_4*x_6-28*x_5*x_6+39*x_6^2+38*x_1*x_7-5*x_3*x_7+5*x_4*x_7-34*x_5*x_7,x_3^2-31*x_5^2+41*x_0*x_6-30*x_1*x_6-4*x_2*x_6+43*x_3*x_6+23*x_4*x_6+7*x_5*x_6+31*x_6^2-19*x_0*x_7+25*x_1*x_7-49*x_2*x_7-16*x_3*x_7-45*x_4*x_7+25*x_5*x_7,x_2*x_3+13*x_5^2-45*x_0*x_6-22*x_1*x_6+33*x_2*x_6-26*x_3*x_6-21*x_4*x_6+34*x_5*x_6-21*x_6^2-47*x_0*x_7-10*x_1*x_7+29*x_2*x_7-46*x_3*x_7-x_4*x_7+20*x_5*x_7,x_1*x_3+22*x_5^2+4*x_0*x_6+3*x_1*x_6+45*x_2*x_6+37*x_3*x_6+17*x_4*x_6+36*x_5*x_6-2*x_6^2-31*x_0*x_7+3*x_1*x_7-12*x_2*x_7+19*x_3*x_7+28*x_4*x_7+30*x_5*x_7,x_0*x_3-47*x_5^2-43*x_0*x_6+6*x_1*x_6-40*x_2*x_6+21*x_3*x_6+26*x_4*x_6-5*x_5*x_6-5*x_6^2+4*x_0*x_7-15*x_1*x_7+18*x_2*x_7-31*x_3*x_7+50*x_4*x_7-46*x_5*x_7,x_2^2+4*x_5^2+31*x_0*x_6+41*x_1*x_6+31*x_2*x_6+28*x_3*x_6+42*x_4*x_6-28*x_5*x_6-4*x_6^2-7*x_0*x_7+15*x_1*x_7-9*x_2*x_7+31*x_3*x_7+3*x_4*x_7+7*x_5*x_7,x_1*x_2-46*x_5^2-6*x_0*x_6-50*x_1*x_6+32*x_2*x_6-10*x_3*x_6+42*x_4*x_6+33*x_5*x_6+18*x_6^2-9*x_0*x_7-20*x_1*x_7+45*x_2*x_7-9*x_3*x_7+10*x_4*x_7-8*x_5*x_7,x_0*x_2-9*x_5^2+34*x_0*x_6-45*x_1*x_6+19*x_2*x_6+24*x_3*x_6+23*x_4*x_6-37*x_5*x_6-44*x_6^2+24*x_0*x_7-33*x_2*x_7+41*x_3*x_7-40*x_4*x_7+4*x_5*x_7,x_1^2+x_1*x_4+x_4^2-28*x_5^2-33*x_0*x_6-17*x_1*x_6+11*x_3*x_6+20*x_4*x_6+25*x_5*x_6-21*x_6^2-22*x_0*x_7+24*x_1*x_7-14*x_2*x_7+5*x_3*x_7-39*x_4*x_7-18*x_5*x_7,x_0*x_1-47*x_5^2-5*x_0*x_6-9*x_1*x_6-45*x_2*x_6+48*x_3*x_6+45*x_4*x_6-29*x_5*x_6+3*x_6^2+29*x_0*x_7+40*x_1*x_7+46*x_2*x_7+27*x_3*x_7-36*x_4*x_7-39*x_5*x_7,x_0^2-31*x_5^2+36*x_0*x_6-30*x_1*x_6-10*x_2*x_6+42*x_3*x_6+9*x_4*x_6+34*x_5*x_6-6*x_6^2+48*x_0*x_7-47*x_1*x_7-19*x_2*x_7+25*x_3*x_7+28*x_4*x_7+34*x_5*x_7);",
"phi = legacyRationalMap(C,3,2);",
"time isDominant(phi,Certify=>true)"},
SeeAlso => {isBirational}} 

document { 
Key => {isInverseMap,(isInverseMap,RingMap,RingMap)}, 
Headline => "checks whether a rational map is the inverse of another", 
Usage => "isInverseMap(phi,psi)", 
Inputs => { 
"phi" => RingMap => {"representing a rational map ",TEX///$\Phi:X \dashrightarrow Y$///,""}, 
"psi" => RingMap => {"representing a rational map ",TEX///$\Psi:Y \dashrightarrow X$///,""}}, 
Outputs => { 
Boolean => {"according to the condition that the composition ",TEX///$\Psi\,\Phi:X \dashrightarrow X$///," coincides with the identity of ",TEX///$X$///," (as a rational map)"}}} 

undocumented{(inverseMap,LegacyRationalMap,Nothing)} 

document { 
Key => {inverseMap,(inverseMap,LegacyRationalMap),(inverseMap,RingMap)}, 
Headline => "inverse of a birational map", 
Usage => "inverseMap phi", 
Inputs => { 
"phi" => LegacyRationalMap => {"a birational map"}}, 
Outputs => { 
LegacyRationalMap => {"the inverse map of ",TT"phi"}},
PARA{"If the source variety is a projective space and if a further technical condition is satisfied, then the algorithm used is that described in the paper by Russo and Simis - On birational maps and Jacobian matrices - Compos. Math. 126 (3), 335-358, 2001. For the general case, the algorithm used is the same as for ", TO "Parametrization::invertBirationalMap", " in the package ", TO "Parametrization", ". Note that in this case, the analogous method ", TO "RationalMaps::inverseOfMap", " in the package ", TO "RationalMaps"," generally turns out to be faster."},
EXAMPLE { 
"-- A Cremona transformation of P^20 
phi = legacyRationalMap map quadroQuadricCremonaTransformation(20,1)", 
"time psi = inverseMap phi",
"assert(phi * psi == 1)"},
PARA{"The method also accepts as input a ",TO2{RingMap,"ring map"}," representing a rational map ",TEX///$\Phi$///, " between projective varieties. In this case, the ",TO2{RingMap,"ring map"}," defining ",TEX///$\Phi^{-1}$///," is returned."},
EXAMPLE { 
"-- A Cremona transformation of P^26 
phi = map quadroQuadricCremonaTransformation(26,1)",
"time psi = inverseMap phi", 
"assert isInverseMap(phi,psi)"},
Caveat => {"If the map passed is not birational and the option ", TO Certify, " is set to ", TT "false", ", you might not get any error message."},
SeeAlso => {approximateInverseMap,(inverse,LegacyRationalMap)}}

document { 
Key => {approximateInverseMap,(approximateInverseMap,LegacyRationalMap,ZZ),(approximateInverseMap,LegacyRationalMap),(approximateInverseMap,RingMap),(approximateInverseMap,RingMap,ZZ)}, 
Headline => "random map related to the inverse of a birational map", 
Usage => "approximateInverseMap phi 
approximateInverseMap(phi,d)", 
Inputs => { 
LegacyRationalMap => "phi" => {"a birational map"},
ZZ => "d" => {"optional, but it should be the degree of the forms defining the inverse of ",TT"phi"}}, 
Outputs => { 
LegacyRationalMap => {"a random rational map which in some sense is related to the inverse of ",TT"phi"," (e.g., they should have the same base locus)"}}, 
PARA{"The algorithm is to try to construct the ideal of the base locus of the inverse by looking for the images via ", TT "phi"," of random linear sections of the source variety. Generally, one can speed up the process by passing through the option ", TO CodimBsInv," a good lower bound for the codimension of this base locus."},
EXAMPLE { 
"P8 = ZZ/97[t_0..t_8];", 
"phi = inverseMap legacyRationalMap(trim(minors(2,genericMatrix(P8,3,3))+random(2,P8)),Dominant=>true)",
"time psi = approximateInverseMap phi",
"assert(phi * psi == 1 and psi * phi == 1)",
"time psi' = approximateInverseMap(phi,CodimBsInv=>5);",
"assert(psi == psi')"}, 
PARA{"A more complicated example is the following (here ", TO inverseMap," takes a lot of time!)."},
EXAMPLE { 
"phi = legacyRationalMap map(P8,ZZ/97[x_0..x_11]/ideal(x_1*x_3-8*x_2*x_3+25*x_3^2-25*x_2*x_4-22*x_3*x_4+x_0*x_5+13*x_2*x_5+41*x_3*x_5-x_0*x_6+12*x_2*x_6+25*x_1*x_7+25*x_3*x_7+23*x_5*x_7-3*x_6*x_7+2*x_0*x_8+11*x_1*x_8-37*x_3*x_8-23*x_4*x_8-33*x_6*x_8+8*x_0*x_9+10*x_1*x_9-25*x_2*x_9-9*x_3*x_9+3*x_4*x_9+24*x_5*x_9-27*x_6*x_9-5*x_0*x_10+28*x_1*x_10+37*x_2*x_10+9*x_4*x_10+27*x_6*x_10-25*x_0*x_11+9*x_2*x_11+27*x_4*x_11-27*x_5*x_11,x_2^2+17*x_2*x_3-14*x_3^2-13*x_2*x_4+34*x_3*x_4+44*x_0*x_5-30*x_2*x_5+27*x_3*x_5+31*x_2*x_6-36*x_3*x_6-x_0*x_7+13*x_1*x_7+8*x_3*x_7+9*x_5*x_7+46*x_6*x_7+41*x_0*x_8-7*x_1*x_8-34*x_3*x_8-9*x_4*x_8-46*x_6*x_8-17*x_0*x_9+32*x_1*x_9-8*x_2*x_9-35*x_3*x_9-46*x_4*x_9+26*x_5*x_9+17*x_6*x_9+15*x_0*x_10+35*x_1*x_10+34*x_2*x_10+20*x_4*x_10+14*x_0*x_11+36*x_1*x_11+35*x_2*x_11-17*x_4*x_11,x_1*x_2-40*x_2*x_3+28*x_3^2-x_0*x_4+5*x_2*x_4-16*x_3*x_4+5*x_0*x_5-36*x_2*x_5+37*x_3*x_5+48*x_2*x_6-5*x_1*x_7-5*x_3*x_7+x_5*x_7+20*x_6*x_7+10*x_0*x_8+34*x_1*x_8+41*x_3*x_8-x_4*x_8+x_6*x_8+40*x_0*x_9-32*x_1*x_9+5*x_2*x_9-11*x_3*x_9-20*x_4*x_9+45*x_5*x_9-14*x_6*x_9-25*x_0*x_10+45*x_1*x_10-41*x_2*x_10-46*x_4*x_10+8*x_6*x_10-28*x_0*x_11+11*x_2*x_11+14*x_4*x_11-8*x_5*x_11),{t_4^2+t_0*t_5+t_1*t_5+35*t_2*t_5+10*t_3*t_5+25*t_4*t_5-5*t_5^2-14*t_0*t_6-14*t_1*t_6-5*t_2*t_6-13*t_4*t_6+37*t_5*t_6+22*t_6^2-31*t_3*t_7+26*t_4*t_7+12*t_5*t_7-45*t_6*t_7-46*t_3*t_8+37*t_4*t_8+28*t_5*t_8+33*t_6*t_8,t_3*t_4+4*t_0*t_5+39*t_1*t_5-40*t_2*t_5+40*t_3*t_5+26*t_4*t_5-20*t_5^2+41*t_0*t_6+36*t_1*t_6-22*t_2*t_6+36*t_4*t_6-30*t_5*t_6-13*t_6^2-25*t_3*t_7+5*t_4*t_7-35*t_5*t_7+10*t_6*t_7+11*t_3*t_8+46*t_4*t_8+29*t_5*t_8+28*t_6*t_8,t_2*t_4-5*t_0*t_5-40*t_1*t_5+12*t_2*t_5+47*t_3*t_5+37*t_4*t_5+25*t_5^2-27*t_0*t_6-22*t_1*t_6+27*t_2*t_6-23*t_4*t_6+5*t_5*t_6-13*t_6^2-39*t_3*t_7-29*t_4*t_7+9*t_5*t_7+39*t_6*t_7+36*t_3*t_8+13*t_4*t_8+26*t_5*t_8+37*t_6*t_8,t_0*t_4-t_0*t_5-8*t_1*t_5-35*t_2*t_5-10*t_3*t_5-33*t_4*t_5+5*t_5^2+15*t_0*t_6+15*t_1*t_6+5*t_2*t_6+15*t_4*t_6-38*t_5*t_6-22*t_6^2+31*t_3*t_7-25*t_4*t_7-19*t_5*t_7+47*t_6*t_7+46*t_3*t_8-36*t_4*t_8-35*t_5*t_8-31*t_6*t_8,t_2*t_3-t_0*t_5-t_1*t_5-35*t_2*t_5-10*t_3*t_5-33*t_4*t_5+5*t_5^2+14*t_0*t_6+14*t_1*t_6+5*t_2*t_6+14*t_4*t_6-31*t_5*t_6-24*t_6^2+32*t_3*t_7-25*t_4*t_7-19*t_5*t_7+47*t_6*t_7+46*t_3*t_8-36*t_4*t_8-35*t_5*t_8-31*t_6*t_8,t_1*t_3-7*t_1*t_5+t_1*t_6+t_4*t_6-7*t_5*t_6+2*t_6^2-t_3*t_7,t_0*t_3-46*t_0*t_5-39*t_1*t_5-43*t_2*t_5-41*t_3*t_5-26*t_4*t_5-28*t_5^2-35*t_0*t_6-36*t_1*t_6+20*t_2*t_6-36*t_4*t_6+9*t_5*t_6+15*t_6^2+26*t_3*t_7-5*t_4*t_7+35*t_5*t_7-10*t_6*t_7-10*t_3*t_8-46*t_4*t_8+47*t_5*t_8-25*t_6*t_8,t_2^2-46*t_1*t_4-33*t_0*t_5-45*t_1*t_5-39*t_2*t_5-39*t_3*t_5-46*t_4*t_5-29*t_5^2-48*t_0*t_6-38*t_1*t_6-30*t_2*t_6+19*t_4*t_6-44*t_5*t_6-47*t_6^2-36*t_0*t_7-46*t_1*t_7+t_2*t_7-44*t_3*t_7+48*t_4*t_7-14*t_5*t_7+4*t_6*t_7-36*t_0*t_8-46*t_1*t_8+47*t_2*t_8-34*t_3*t_8-24*t_4*t_8-12*t_5*t_8-47*t_6*t_8+47*t_7*t_8,t_1*t_2+6*t_1*t_5+5*t_0*t_6-2*t_1*t_6-t_4*t_6-t_5*t_6+5*t_0*t_7+t_1*t_7-2*t_2*t_7-7*t_5*t_7+2*t_6*t_7-2*t_1*t_8+3*t_7*t_8,t_0*t_2+t_1*t_4+5*t_0*t_5+32*t_1*t_5-20*t_2*t_5-47*t_3*t_5-37*t_4*t_5-25*t_5^2+19*t_0*t_6+22*t_1*t_6-25*t_2*t_6+25*t_4*t_6-5*t_5*t_6+13*t_6^2+5*t_0*t_7+t_1*t_7+39*t_3*t_7+28*t_4*t_7-9*t_5*t_7-39*t_6*t_7+4*t_0*t_8+t_1*t_8-36*t_3*t_8-14*t_4*t_8-26*t_5*t_8-37*t_6*t_8,t_0*t_1-39*t_1*t_4+40*t_1*t_5-37*t_0*t_6-39*t_1*t_6+19*t_4*t_6-39*t_5*t_6-38*t_0*t_7+39*t_1*t_7+19*t_2*t_7+18*t_5*t_7-19*t_6*t_7+19*t_1*t_8+20*t_7*t_8,t_0^2+12*t_1*t_4+20*t_0*t_5+27*t_1*t_5-8*t_2*t_5+37*t_3*t_5+28*t_4*t_5+30*t_5^2-46*t_0*t_6+24*t_1*t_6-40*t_2*t_6+25*t_4*t_6+16*t_5*t_6-35*t_6^2+29*t_0*t_7+12*t_1*t_7-35*t_2*t_7-8*t_3*t_7-18*t_4*t_7+42*t_5*t_7-12*t_6*t_7-6*t_0*t_8+12*t_1*t_8-15*t_3*t_8+9*t_4*t_8+20*t_5*t_8-30*t_6*t_8+4*t_7*t_8})", 
"-- without the option 'CodimBsInv=>4', it takes about triple time 
time psi=approximateInverseMap(phi,CodimBsInv=>4)",
"-- but...
phi * psi == 1",
"-- in this case we can remedy enabling the option Certify
time psi = approximateInverseMap(phi,CodimBsInv=>4,Certify=>true)",
"assert(phi * psi == 1)"},
PARA{"The method also accepts as input a ",TO2{RingMap,"ring map"}," representing a rational map between projective varieties. In this case, a ",TO2{RingMap,"ring map"}," is returned as well."},
Caveat => {"For the purpose of this method, the option ", TO Certify, TT"=>true"," is too rigid, especially when the source of the passed map is not a projective space."},
SeeAlso => {inverseMap,(inverse,LegacyRationalMap)}}

document { 
Key => {graph,(graph,LegacyRationalMap)}, 
Headline => "closure of the graph of a rational map", 
Usage => "graph phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
LegacyRationalMap => {"the first projection"},
LegacyRationalMap => {"the second projection"}}, 
EXAMPLE { 
"(ZZ/190181)[x_0..x_4]; phi = legacyRationalMap(minors(2,matrix{{x_0..x_3},{x_1..x_4}}),Dominant=>true)",
"time (p1,p2) = graph phi;",
"p1",
"p2",
"assert(p1 * phi == p2 and p2 * phi^-1 == p1)",
"describe p2",
"projectiveDegrees p2"},
PARA{"When the source of the rational map is a multi-projective variety, the method returns all the projections."},
EXAMPLE {
"time g = graph p2;",
"g_0;",
"g_1;",
"g_2;",
"describe g_0"},
SeeAlso => {(graph,RingMap),graphIdeal}}

document { 
Key => {(graph,RingMap)}, 
Headline => "closure of the graph of a rational map", 
Usage => "graph phi", 
Inputs => { 
RingMap => "phi" => {"representing a rational map ",TEX///$\Phi:X \dashrightarrow Y$///}}, 
Outputs => { 
RingMap => {"representing the projection on the first factor ",TEX///$\mathbf{Graph}(\Phi) \to X$///},
RingMap => {"representing the projection on the second factor ",TEX///$\mathbf{Graph}(\Phi) \to Y$///}}, 
EXAMPLE { 
"phi = map(QQ[x_0..x_3],QQ[y_0..y_2],{-x_1^2+x_0*x_2,-x_1*x_2+x_0*x_3,-x_2^2+x_1*x_3})", 
"graph phi"},
SeeAlso => {(graph,LegacyRationalMap),graphIdeal}}

document { 
Key => {SegreClass,(SegreClass,Ideal),(SegreClass,RingMap),(SegreClass,LegacyRationalMap)}, 
Headline => "Segre class of a closed subscheme of a projective variety", 
Usage => "SegreClass I", 
Inputs => { 
Ideal => "I" => {"a homogeneous ideal of a graded quotient ring ",TEX///$K[x_0,\ldots,x_n]/J$///," defining a subscheme ",TEX///$X=V(I)$///," of ",TEX///$Y=Proj(K[x_0,\ldots,x_n]/J)\subseteq \mathbb{P}^n=Proj(K[x_0,\ldots,x_n])$///}}, 
Outputs => { 
RingElement => {"the push-forward to the Chow ring of ",TEX///$\mathbb{P}^n$///," of the Segre class ",TEX///$s(X,Y)$///," of ",TEX///$X$///," in ",TEX///$Y$///}}, 
PARA{"This is an example of application of the method ", TO "projectiveDegrees","; see Proposition 4.4 in ",HREF{"http://link.springer.com/book/10.1007%2F978-3-662-02421-8","Intersection theory"},", by W. Fulton, and Subsection 2.3 in ",HREF{"http://www.math.lsa.umich.edu/~idolga/cremonalect.pdf","Lectures on Cremona transformations"},", by I. Dolgachev. See also the corresponding methods in the packages ", HREF{"http://www.math.fsu.edu/~aluffi/CSM/CSM.html", "CSM-A"},", by P. Aluffi, and ", TO "CharacteristicClasses::CharacteristicClasses", ", by M. Helmer and C. Jost."},
PARA{"In the example below, we take ", TEX///$Y\subset\mathbb{P}^7$///," to be the dual hypersurface of ",TEX///$\mathbb{P}^1\times\mathbb{P}^1\times\mathbb{P}^1\subset{\mathbb{P}^7}^*$///, " and ",TEX///$X\subset Y$///, " its singular locus. We compute the push-forward to the Chow ring of ",TEX///$\mathbb{P}^7$///, " of the Segre class both of ",TEX///$X$///, " in ",TEX///$Y$///, " and of ",TEX///$X$///, " in ",TEX///$\mathbb{P}^7$///, ", using both a probabilistic and a non-probabilistic approach."},
EXAMPLE { 
"P7 = ZZ/100003[x_0..x_7]",
"Y = ideal(x_3^2*x_4^2-2*x_2*x_3*x_4*x_5+x_2^2*x_5^2-2*x_1*x_3*x_4*x_6-2*x_1*x_2*x_5*x_6+4*x_0*x_3*x_5*x_6+x_1^2*x_6^2+4*x_1*x_2*x_4*x_7-2*x_0*x_3*x_4*x_7-2*x_0*x_2*x_5*x_7-2*x_0*x_1*x_6*x_7+x_0^2*x_7^2)",
"X = sub(ideal jacobian Y,P7/Y)",
"time SegreClass X",
"time SegreClass lift(X,P7)",
"time SegreClass(X,Certify=>true)",
"time SegreClass(lift(X,P7),Certify=>true)",
"o4 == o6 and o5 == o7"},
PARA{"The method also accepts as input a ring map ",TT"phi"," representing a rational map ",TEX///$\Phi:X\dashrightarrow Y$///," between projective varieties. In this case, the method returns the push-forward to the Chow ring of the ambient projective space of ",TEX///$X$///," of the Segre class of the base locus of ",TEX///$\Phi$///, " in ",TEX///$X$///, ", i.e., it basically computes ",TT"SegreClass ideal matrix phi",". In the next example, we compute the Segre class of the base locus of a birational map ",TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^9 \dashrightarrow \mathbb{P}^6$///,"."},
EXAMPLE {
"use ZZ/100003[x_0..x_6]",
"time phi = inverseMap toMap(minors(2,matrix{{x_0,x_1,x_3,x_4,x_5},{x_1,x_2,x_4,x_5,x_6}}),Dominant=>2)",
"time SegreClass phi",
"B = ideal matrix phi",
"-- Segre class of B in G(1,4)
time SegreClass B",
"-- Segre class of B in P^9
time SegreClass lift(B,ambient ring B)"},
SeeAlso => {projectiveDegrees,ChernSchwartzMacPherson}}

document { 
Key => {ChernSchwartzMacPherson,(ChernSchwartzMacPherson,Ideal)}, 
Headline => "Chern-Schwartz-MacPherson class of a projective scheme", 
Usage => "ChernSchwartzMacPherson I", 
Inputs => { 
Ideal => "I" => {"a homogeneous ideal defining a closed subscheme ",TEX///$X\subset\mathbb{P}^n$///}}, 
Outputs => { 
RingElement => {"the push-forward to the Chow ring of ",TEX///$\mathbb{P}^n$///," of the Chern-Schwartz-MacPherson class ",TEX///$c_{SM}(X)$///," of ",TEX///$X$///,". In particular, the coefficient of ",TEX///$H^n$///," gives the Euler characteristic of the support of ",TEX///$X$///,", where ",TEX///$H$///," denotes the hyperplane class."}}, 
PARA{"This is an example of application of the method ", TO "projectiveDegrees",", due to results shown in ",HREF{"http://www.sciencedirect.com/science/article/pii/S0747717102000895","Computing characteristic classes of projective schemes"},", by P. Aluffi. See also the corresponding methods in the packages ", HREF{"http://www.math.fsu.edu/~aluffi/CSM/CSM.html", "CSM-A"},", by P. Aluffi, and ", TO "CharacteristicClasses::CharacteristicClasses",", by M. Helmer and C. Jost."},
PARA{"In the example below, we compute the push-forward to the Chow ring of ",TEX///$\mathbb{P}^4$///, " of the Chern-Schwartz-MacPherson class of the cone over the twisted cubic curve, using both a probabilistic and a non-probabilistic approach."},
EXAMPLE { 
"GF(5^7)[x_0..x_4]",
"C = minors(2,matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}})",
"time ChernSchwartzMacPherson C",
"time ChernSchwartzMacPherson(C,Certify=>true)",
"oo == ooo"},
PARA{"In the case when the input ideal ",TT"I"," defines a smooth projective variety ",TEX///$X$///,", the push-forward of ",TEX///$c_{SM}(X)$///," can be computed much more efficiently using ",TO SegreClass, ". Indeed, in this case, ", TEX///$c_{SM}(X)$///," coincides with the (total) Chern class of the tangent bundle of ",TEX///$X$///," and can be obtained as follows (in general the method below gives the push-forward of the so-called Chern-Fulton class)."},
EXAMPLE {
"ChernClass = method(Options=>{Certify=>false});",
"ChernClass (Ideal) := o -> (I) -> (
   s := SegreClass(I,Certify=>o.Certify);
   s*(1+first gens ring s)^(numgens ring I));",
"-- example: Chern class of G(1,4)
G = Grassmannian(1,4,CoefficientRing=>ZZ/190181)",
"time ChernClass G",
"time ChernClass(G,Certify=>true)"},
SeeAlso => {SegreClass,EulerCharacteristic,(euler,ProjectiveVariety)}}

document { 
Key => {EulerCharacteristic,(EulerCharacteristic,Ideal)}, 
Headline => "topological Euler characteristic of a (smooth) projective variety", 
Usage => "EulerCharacteristic I", 
Inputs => { 
Ideal => "I" => {"a homogeneous ideal defining a smooth projective variety ",TEX///$X\subset\mathbb{P}^n$///}}, 
Outputs => { 
ZZ => {"the topological Euler characteristics of ",TEX///$X$///,"."}}, 
PARA{"This is an application of the method ", TO "SegreClass",". See also the corresponding methods in the packages ", HREF{"http://www.math.fsu.edu/~aluffi/CSM/CSM.html", "CSM-A"},", by P. Aluffi, and ", TO "CharacteristicClasses::CharacteristicClasses", ", by M. Helmer and C. Jost."},
PARA{"In general, even if the input ideal defines a singular variety ",TEX///$X$///,", the returned value equals the degree of the component of dimension 0 of the Chern-Fulton class of ",TEX///$X$///,". The Euler characteristic of a singular variety can be computed via the method ",TO ChernSchwartzMacPherson,"."},
PARA{"In the example below, we compute the Euler characteristic of ",TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^{9}$///,", using both a probabilistic and a non-probabilistic approach."},
EXAMPLE { 
"I = Grassmannian(1,4,CoefficientRing=>ZZ/190181);",
"time EulerCharacteristic I",
"time EulerCharacteristic(I,Certify=>true)"},
Caveat => {"No test is made to see if the projective variety is smooth."},
SeeAlso => {(euler,ProjectiveVariety),ChernSchwartzMacPherson,SegreClass}}

document { 
Key => {LegacyRationalMap}, 
Headline => "the class of all (legacy) rational maps between absolutely irreducible projective varieties over a field", 
PARA{"The class is called ",EM "legacy"," because it is kept mainly for compatibility: for new code, use the class ",TO "MultiprojectiveVarieties::MultirationalMap"," of the package ",TO "MultiprojectiveVarieties::MultiprojectiveVarieties",". Up to version 5.2.1 of ",EM "Cremona",", this class was called ",TT "RationalMap","; the old name is still available as a deprecated synonym."},
PARA{"An object of the class ",EM "LegacyRationalMap", " can be basically replaced by a homogeneous ring map of quotients of polynomial rings by homogeneous ideals. One main advantage to using this class is that things computed using non-probabilistic algorithms are stored internally (or partially stored)."},
PARA{"The constructor for the class is ",TO "legacyRationalMap",", which works quite similar to ",TO "toMap",". 
See in particular the methods: ",TO (legacyRationalMap,RingMap),", ",TO (legacyRationalMap,Ideal,ZZ,ZZ),", ",TO (legacyRationalMap,Tally),", and ",TO (legacyRationalMap,PolynomialRing,List),"."},
PARA{"In the package ", TO "MultiprojectiveVarieties::MultiprojectiveVarieties",", this class has been extended to provide support to rational maps between multi-projective varieties, see ", TO "MultiprojectiveVarieties::MultirationalMap", "."}}

document { 
Key => {(symbol ^**,LegacyRationalMap,Ideal),(symbol ^*,LegacyRationalMap)}, 
Headline => "inverse image via a rational map", 
Usage => "phi^** I", 
Inputs => { 
LegacyRationalMap => "phi",
Ideal => "I" => {"a homogeneous ideal in the coordinate ring of the target of ",TT "phi"}}, 
Outputs => { 
Ideal => {"the ideal of the closure of the inverse image of ", TT"V(I)", " via ",TT"phi"}},
PARA{"In most cases this is equivalent to ",TT"phi^* I", ", which is faster but may not take into account other representations of the map."},
PARA{"In the example below, we apply the method to check the birationality of a map (deterministically)."},
EXAMPLE { 
"phi = quadroQuadricCremonaTransformation(5,1)",
"K := frac(QQ[vars(0..5)]); phi = phi ** K",
"p = trim minors(2,(vars K)||(vars source phi))",
"q = phi p",
"time phi^** q",
"oo == p"},
SeeAlso => {(symbol _*,LegacyRationalMap),(symbol **,LegacyRationalMap,Ring),(target,LegacyRationalMap)}}

document { 
Key => {(symbol _*,LegacyRationalMap),(symbol SPACE,LegacyRationalMap,Ideal)}, 
Headline => "direct image via a rational map", 
Usage => "phi_* I", 
Inputs => { 
LegacyRationalMap => "phi" => {TT"I", " a homogeneous ideal in the coordinate ring of the source of ",TT "phi"}}, 
Outputs => { 
Ideal => {"the ideal of the closure of the direct image of ", TT"V(I)", " via ",TT"phi"}},
PARA{"In most cases this is equivalent to ",TT"phi I", ", which is faster but may not take into account other representations of the map."},
SeeAlso => {(symbol ^**,LegacyRationalMap,Ideal),(source,LegacyRationalMap)}}

document { 
Key => {(symbol ^,LegacyRationalMap,ZZ)}, 
Headline => "power", 
Usage => "phi^n", 
Inputs => { 
LegacyRationalMap => "phi",
ZZ => "n"}, 
Outputs => { 
LegacyRationalMap => {TT"phi^n"}},
PARA{"If the map is birational, then negative values may be used and the inverse will be computed using ",TO2{(inverse,LegacyRationalMap),"inverse"},TT"(phi)","."}}

document { 
Key => {(inverse,LegacyRationalMap),(inverse,LegacyRationalMap,Option)}, 
Headline => "inverse of a birational map", 
Usage => "inverse phi
inverse(phi,Certify=>b)", 
Inputs => { 
LegacyRationalMap => "phi" => {"which has to be birational, and ",TT"b"," is a ",TO2{Boolean,"boolean value"},", that is, ",TT"true"," or ",TT"false"," (the default value is ",TT"false",")"}}, 
Outputs => { 
LegacyRationalMap => {"the inverse map of ",TT"phi"}},
PARA{TT"inverse(phi,Certify=>true)"," is the same as ",TO inverseMap,TT"(phi,Certify=>true,Verbose=>false)",", while ",
TT"inverse(phi,Certify=>false)"," applies a middle ground approach between ",TO inverseMap,TT"(phi,Certify=>true)"," and ",TO inverseMap,TT"(phi,Certify=>false)",
". The procedure for the latter is as follows: It first computes the inverse map of ",TT "phi"," using ",TT "psi = ",TO "inverseMap",TT" phi",". Then it is checked that ",TO2{(symbol SPACE,LegacyRationalMap,Ideal),"psi phi p"},TT" == p"," and ",TO2{(symbol SPACE,LegacyRationalMap,Ideal),"phi psi q"},TT" == q",", where ",TT"p,q"," are, respectively, a ",TO2{point,"random point"}," on the ",TO2{(source,LegacyRationalMap),"source"}," and the ",TO2{(target,LegacyRationalMap),"target"}," of ",TT"phi",". Finally, if the tests pass, the command ",TO forceInverseMap,TT"(phi,psi)"," is invoked and ",TT"psi"," is returned."},
EXAMPLE {
"R = QQ[x_0..x_4]; phi = legacyRationalMap minors(4,random(R^{4:1},R^5)) -- special Cremona transformation of P^4 of type (4,4)",
"time inverse phi"
},
SeeAlso => {inverseMap,(symbol ^,LegacyRationalMap,ZZ),(isInverseMap,LegacyRationalMap,LegacyRationalMap)}}

document { 
Key => {(ideal,LegacyRationalMap)}, 
Headline => "base locus of a rational map", 
Usage => "ideal phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
Ideal => {"the ideal of the base locus of ",TT"phi"}},
PARA {"This is generally difficult, but in some cases it is equivalent to ",TT"ideal matrix phi", ", which does not perform any computation."},
EXAMPLE {
"y = gens(QQ[x_0..x_5]/(x_2^2-x_2*x_3+x_1*x_4-x_0*x_5));",
"phi = legacyRationalMap {y_4^2-y_3*y_5,-y_2*y_4+y_3*y_4-y_1*y_5, -y_2*y_3+y_3^2-y_1*y_4, -y_1*y_2+y_1*y_3-y_0*y_4, y_1^2-y_0*y_3}",
"time ideal phi",
"assert(ideal phi == ideal matrix phi)",
"phi' = last graph phi",
"time ideal phi'",
"assert(ideal phi' != ideal matrix phi')"},
SeeAlso => {isMorphism,(matrix,LegacyRationalMap)}}

document { 
Key => {exceptionalLocus,(exceptionalLocus,LegacyRationalMap)}, 
Headline => "exceptional locus of a birational map", 
Usage => "exceptionalLocus phi", 
Inputs => { 
LegacyRationalMap => "phi" => {"a birational map ",TEX///$X\dashrightarrow Y$///}}, 
Outputs => { 
Ideal => {"the ideal defining the closure in ",TT"X"," of the locus where ",TT "phi"," is not a local isomorphism"}},
PARA{"This method simply calculates the ",TO2{(symbol ^**,LegacyRationalMap,Ideal),"inverse image"}," of the ",TO2{(ideal,LegacyRationalMap),"base locus"}," of the inverse map, which in turn is determined through the method ",TO2{(inverse,LegacyRationalMap),"inverse"},"."},
PARA{"Below, we compute the exceptional locus of the map defined by the linear system of quadrics through the quintic rational normal curve in ",TEX///$\mathbb{P}^5$///,"."},
EXAMPLE { 
"P5 := ZZ/100003[x_0..x_5];",
"phi = legacyRationalMap(minors(2,matrix{{x_0,x_1,x_2,x_3,x_4},{x_1,x_2,x_3,x_4,x_5}}),Dominant=>2);",
"E = exceptionalLocus phi;",
"assert(E == phi^* ideal phi^-1)",
"assert(E == minors(3,matrix{{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4},{x_2,x_3,x_4,x_5}}))"
},
SeeAlso => {(ideal,LegacyRationalMap),(inverseMap,LegacyRationalMap),(symbol ^**,LegacyRationalMap,Ideal),(isIsomorphism,LegacyRationalMap),forceInverseMap}}

document { 
Key => {(target,LegacyRationalMap)}, 
Headline => "coordinate ring of the target for a rational map", 
Usage => "target phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
Ring => {"the coordinate ring of the target of ",TT"phi"}},
PARA{"This is equivalent to ",TT "source map phi","."},
SeeAlso => {(source,LegacyRationalMap)}}

document { 
Key => {(source,LegacyRationalMap)}, 
Headline => "coordinate ring of the source for a rational map", 
Usage => "source phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
Ring => {"the coordinate ring of the source of ",TT"phi"}},
PARA{"This is equivalent to ",TT "target map phi","."},
SeeAlso => {(target,LegacyRationalMap)}}

document { 
Key => {(coefficientRing,LegacyRationalMap)}, 
Headline => "coefficient ring of a rational map", 
Usage => "coefficientRing phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
Ring => {"the coefficient ring of ",TT"phi"}},
SeeAlso => {(symbol **,LegacyRationalMap,Ring)}}

document { 
Key => {(degree,LegacyRationalMap)}, 
Headline => "degree of a rational map", 
Usage => "degree phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
ZZ => {"the degree of ",TT"phi",". So this value is 1 if and only if the map is birational onto its image."}},
PARA{"This is a shortcut for ",TT "degreeMap(phi,Certify=>true,Verbose=>false)",", see ",TO (degreeMap,LegacyRationalMap),"."}}

document { 
Key => {(symbol !,LegacyRationalMap)}, 
Headline => "calculates every possible thing", 
Usage => "phi!", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
LegacyRationalMap => {"the same rational map ",TT"phi"}},
PARA{"This method (mainly used for tests) applies almost all the deterministic methods that are available."},
EXAMPLE {
"QQ[x_0..x_5]; phi = legacyRationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3};", 
"describe phi",
"time phi! ;",
"describe phi",
"QQ[x_0..x_4]; phi = legacyRationalMap {-x_1^2+x_0*x_2,-x_1*x_2+x_0*x_3,-x_2^2+x_1*x_3,-x_1*x_3+x_0*x_4,-x_2*x_3+x_1*x_4,-x_3^2+x_2*x_4};",
"describe phi",
"time phi! ;",
"describe phi"},
SeeAlso => {(describe,LegacyRationalMap)}}

document { 
Key => {(degrees,LegacyRationalMap),(multidegree,LegacyRationalMap)}, 
Headline => "projective degrees of a rational map", 
Usage => "degrees phi 
multidegree phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
List => {"the list of projective degrees of ",TT"phi"}},
PARA{"This is a shortcut for ",TT "projectiveDegrees(phi,Certify=>true,Verbose=>false)",", see ",TO (projectiveDegrees,LegacyRationalMap),"."}}

document { 
Key => {(coefficients,LegacyRationalMap)}, 
Headline => "coefficient matrix of a rational map", 
Usage => "coefficients phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
Matrix => {"the coefficient matrix of the polynomials defining ",TT"phi"}},
EXAMPLE {
"K = QQ; ringP9 = K[x_0..x_9];",
"M = random(K^10,K^10)",
"phi = legacyRationalMap ((vars ringP9) * (transpose M));",
"M' = coefficients phi",
"M == M'"}}

document { 
Key => {(flatten,LegacyRationalMap)}, 
Headline => "write source and target as nondegenerate varieties", 
Usage => "flatten phi", 
Inputs => { 
LegacyRationalMap => "phi"}, 
Outputs => { 
LegacyRationalMap => {"a rational map isomorphic to the original map, flattened in the sense that the ideals of source and target contain no linear forms"}},
EXAMPLE {
"P5 = QQ[t_0..t_5]; phi = legacyRationalMap(P5/(35*t_1+45*t_2+21*t_3+525*t_4+1365*t_5,1575*t_0*t_2-3250*t_2^2+735*t_0*t_3-1890*t_2*t_3-1666*t_3^2+17150*t_0*t_4-47250*t_2*t_4-22050*t_3*t_4-276850*t_4^2+46550*t_0*t_5-122850*t_2*t_5-57330*t_3*t_5-1433250*t_4*t_5-1864450*t_5^2),P5/(315*t_0+280*t_1+45*t_2+21*t_3+210*t_4+1050*t_5),{-45*t_2-21*t_3-490*t_4-1330*t_5, 45*t_2+21*t_3+525*t_4+1365*t_5, 35*t_2, 35*t_3, 35*t_4, 35*t_5});",
"describe phi",
"psi = flatten phi;",
"describe psi"}}

document { 
Key => {(sub,LegacyRationalMap,PolynomialRing,PolynomialRing)},
Headline => "substitute the ambient projective spaces of source and target", 
Usage => "sub(phi,R,S)", 
Inputs => { 
LegacyRationalMap => "phi" => {TEX///$\phi:X\subseteq\mathbb{P}^n\dashrightarrow Y\subseteq\mathbb{P}^m$///},
PolynomialRing => "R" => {"the coordinate ring of ",TEX///$\mathbb{P}^n$///},
PolynomialRing => "S" => {"the coordinate ring of ",TEX///$\mathbb{P}^m$///}}, 
Outputs => { 
LegacyRationalMap => {"a rational map isomorphic to the original map such that the ",
TO ambient," of the ",TO2{(source,LegacyRationalMap),"source"}," is ",TEX///$R$///," and the ",
TO ambient," of the ",TO2{(target,LegacyRationalMap),"target"}," is ",TEX///$S$///}}, 
EXAMPLE {
"ZZ/3331[vars(0..5)];",
"phi = legacyRationalMap {e^2-d*f, c*e-b*f, c*d-b*e, c^2-a*f, b*c-a*e, b^2-a*d}",
"R = ZZ/3331[x_0..x_5], S = ZZ/3331[y_0..y_5];",
"sub(phi,R,S)"},
SeeAlso => {(symbol **,LegacyRationalMap,Ring)}}

document { 
Key => {(super,LegacyRationalMap),(legacyRationalMap,LegacyRationalMap)}, 
Headline => "get the rational map whose target is a projective space", 
Usage => "super phi
legacyRationalMap(phi,Dominant=>null)
legacyRationalMap phi", 
Inputs => { 
LegacyRationalMap => "phi" => {"whose target is a subvariety ",TEX///$Y\subset\mathbb{P}^n$///}}, 
Outputs => { 
LegacyRationalMap => {"the composition of ",TT"phi"," with the inclusion of ",TEX///$Y$///," into ",TEX///$\mathbb{P}^n$///}},
PARA {"So that, for instance, if ",TT"phi"," is a dominant map, then the code ",TT"legacyRationalMap(super phi,Dominant=>true)"," yields a map isomorphic to ",TT"phi","."},
EXAMPLE {
"phi = specialQuadraticTransformation 7;",
"phi' = super phi;",
"describe phi",
"describe phi'",
"describe legacyRationalMap(phi',Dominant=>true)"},
SeeAlso => {(target,LegacyRationalMap),ambient,super}}

document { 
Key => {(projectiveDegrees,LegacyRationalMap)}, 
Headline => "projective degrees of a rational map", 
Usage => "projectiveDegrees Phi", 
Inputs => { 
LegacyRationalMap => "Phi"}, 
Outputs => { 
List => {"the list of projective degrees of ",TT"Phi"}},
PARA{"This computation is done through the corresponding method for ring maps. See ",TO (projectiveDegrees,RingMap)," for more details and examples."},
SeeAlso => {(projectiveDegrees,RingMap),(degrees,LegacyRationalMap),(degree,LegacyRationalMap)}}

document { 
Key => {(degreeMap,LegacyRationalMap)}, 
Headline => "degree of a rational map", 
Usage => "degreeMap Phi", 
Inputs => { 
LegacyRationalMap => "Phi"}, 
Outputs => { 
ZZ => {"the degree of ",TT"Phi",". So this value is 1 if and only if the map is birational onto its image."}},
PARA{"This computation is done through the corresponding method for ring maps. See ",TO (degreeMap,RingMap)," for more details and examples."},
SeeAlso => {(degreeMap,RingMap),projectiveDegrees,(degree,LegacyRationalMap)}}

document { 
Key => {(symbol *,LegacyRationalMap,LegacyRationalMap),(compose,LegacyRationalMap,LegacyRationalMap),(compose,RingMap,RingMap)}, 
Headline => "composition of rational maps", 
Usage => "phi * psi 
compose(phi,psi)", 
Inputs => { 
LegacyRationalMap => "phi" => { TEX///$X \dashrightarrow Y$///},
LegacyRationalMap => "psi" => { TEX///$Y \dashrightarrow Z$///}}, 
Outputs => { 
LegacyRationalMap => { TEX///$X \dashrightarrow Z$///, ", the composition of ",TT"phi"," and ",TT"psi"}}, 
EXAMPLE { 
"R = QQ[x_0..x_3]; S = QQ[y_0..y_4]; T = QQ[z_0..z_4];", 
"phi = legacyRationalMap(R,S,{x_0*x_2,x_0*x_3,x_1*x_2,x_1*x_3,x_2*x_3})",
"psi = legacyRationalMap(S,T,{y_0*y_3,-y_2*y_3,y_1*y_2,y_2*y_4,-y_3*y_4})",
"phi * psi",
"(map phi) * (map psi)"},
SeeAlso => {(symbol *,RingMap,RingMap)}}

document { 
Key => {(symbol ==,LegacyRationalMap,LegacyRationalMap),(symbol ==,ZZ,LegacyRationalMap),(symbol ==,LegacyRationalMap,ZZ)}, 
Headline => "equality of rational maps", 
Usage => "phi == psi", 
Inputs => { 
LegacyRationalMap => "phi",
LegacyRationalMap => "psi"}, 
Outputs => { 
Boolean => {" whether ",TT"phi"," and ",TT"psi", " are the same rational map"}}, 
EXAMPLE {
"QQ[x_0..x_5]",
"phi = legacyRationalMap {x_0*x_4^2-x_0*x_3*x_5,x_0*x_2*x_4-x_0*x_1*x_5,x_0*x_2*x_3-x_0*x_1*x_4,x_0*x_2^2-x_0^2*x_5,x_0*x_1*x_2-x_0^2*x_4,x_0*x_1^2-x_0^2*x_3}",
"psi = legacyRationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3}", 
"phi == psi"}}

document { 
Key => {(isInverseMap,LegacyRationalMap,LegacyRationalMap)},
Headline => "checks whether two rational maps are one the inverse of the other", 
Usage => "isInverseMap(phi,psi)", 
Inputs => { 
"phi" => LegacyRationalMap, 
"psi" => LegacyRationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi * psi == 1 and psi * phi == 1"}},
Consequences => {"If the answer is affirmative, then the system will be informed and so commands like 'inverse phi' will execute fast."},
SeeAlso => {(symbol ==,LegacyRationalMap,LegacyRationalMap),(symbol *,LegacyRationalMap,LegacyRationalMap),(isInverseMap,RingMap,RingMap),(inverse,LegacyRationalMap),forceInverseMap}} 

undocumented {(image,ZZ,LegacyRationalMap)}

document { 
Key => {(image,LegacyRationalMap,ZZ),(image,LegacyRationalMap)}, 
Headline => "closure of the image of a rational map", 
Usage => "image Phi 
image(Phi,d)", 
Inputs => { 
"Phi" => LegacyRationalMap,
"d" => ZZ}, 
Outputs => { 
Ideal => {"the ideal defining the closure of the image of ",TT"Phi",", or its degree ",TT"d"," homogeneous component if ",TT"d", " is passed"}},
PARA{"This computation is done through the kernel of a ring map representing the rational map. See ",TO (kernel,RingMap), " and ",TO (kernel,RingMap,ZZ)," for more details."},
SeeAlso => {(kernel,RingMap,ZZ),(kernel,RingMap),(image,LegacyRationalMap,String)}}

document { 
Key => {(image,LegacyRationalMap,String)}, 
Headline => "closure of the image of a rational map using the F4 algorithm (experimental)", 
Usage => "image(Phi,\"F4\")
image(Phi,\"MGB\")",
Inputs => { "Phi" => LegacyRationalMap,{"\"F4\" or \"MGB\""}}, 
Outputs => {{"the ",TO2{"Ideal","ideal"}," defining the closure of the image of ",TT"Phi","; the calculation passes through ",TO groebnerBasis,"(...,Strategy=>\"F4\") or ",TO groebnerBasis,"(...,Strategy=>\"MGB\")."}}, 
SeeAlso => {(image,LegacyRationalMap),(image,LegacyRationalMap,ZZ),groebnerBasis}}

document {Key => {parametrize}, 
Headline => "parametrization of a rational projective variety", 
PARA{"See ", TO "MultiprojectiveVarieties::parametrize(MultiprojectiveVariety)",
" and ",TO2{(parametrize,QuotientRing),"parametrize(QuotientRing)"},"."}}

document { 
Key => {(parametrize,Ideal),(parametrize,QuotientRing),(parametrize,PolynomialRing)}, 
Headline => "parametrization of linear varieties and hyperquadrics", 
Usage => "parametrize I", 
Inputs => { 
"I" => Ideal => {"the ideal of a linear variety or of a hyperquadric"}}, 
Outputs => { 
LegacyRationalMap => {"a birational map ",TT"phi"," such that ",TT"I == image phi"}},
PARA{"This function has been improved and extended in the package ", TO "MultiprojectiveVarieties", ", see ", TO "MultiprojectiveVarieties::parametrize(MultiprojectiveVariety)", "."},
EXAMPLE {
"P9 := ZZ/10000019[x_0..x_9]",
 "L = trim ideal(random(1,P9),random(1,P9),random(1,P9),random(1,P9))",
"time parametrize L",
"Q = trim ideal(random(2,P9),random(1,P9),random(1,P9))",
"time parametrize Q"
}}

document { 
Key => {(map,LegacyRationalMap),(map,ZZ,LegacyRationalMap)}, 
Headline => "get the ring map defining a rational map", 
Usage => "map Phi", 
Inputs => { 
LegacyRationalMap => "Phi"}, 
Outputs => { 
RingMap => {"the ring map from which the rational map ",TT"Phi"," was defined"}},
EXAMPLE {
"QQ[t_0..t_3]",
"Phi = legacyRationalMap {t_1^2+t_2^2+t_3^2,t_0*t_1,t_0*t_2,t_0*t_3}",
"map Phi"},
PARA {"The command ",TT "map Phi"," is equivalent to ",TT "map(0,Phi)",". More generally, the command ",TT "map(i,Phi)"," returns the i-th representative of the map ",TT"Phi","."},
SeeAlso => {(matrix,LegacyRationalMap)}}

document { 
Key => {(matrix,LegacyRationalMap),(matrix,ZZ,LegacyRationalMap)}, 
Headline => "the matrix associated to a rational map", 
Usage => "matrix Phi", 
Inputs => { 
LegacyRationalMap => "Phi"}, 
Outputs => { 
Matrix => {"the matrix associated to the ring map defining the rational map ",TT"Phi"}},
PARA{"This is equivalent to ",TT "matrix map Phi",". Moreover, the command ",TT "matrix Phi"," is equivalent to ",TT "matrix(0,Phi)",", and more generally the command ",TT "matrix(i,Phi)"," returns the matrix of the i-th representative of ",TT"Phi","."},
SeeAlso => {(map,LegacyRationalMap),(matrix,RingMap)}}

document { 
Key => {(entries,LegacyRationalMap)}, 
Headline => "the entries of the matrix associated to a rational map", 
Usage => "entries Phi", 
Inputs => { 
LegacyRationalMap => "Phi"}, 
Outputs => { 
List => {"the entries of the matrix associated to the ring map defining the rational map ",TT"Phi"}},
PARA{"This is equivalent to ",TT "flatten entries matrix Phi","."},
SeeAlso => {(matrix,LegacyRationalMap)}}

document { 
Key => {(symbol |,LegacyRationalMap,Ideal),(symbol |,LegacyRationalMap,RingElement),(symbol |,LegacyRationalMap,Ring)}, 
Headline => "restriction of a rational map", 
Usage => "Phi | I", 
Inputs => { 
LegacyRationalMap => "Phi" => { TEX///$\phi:X \dashrightarrow Y$///},
Ideal => "I" => {"a homogeneous ideal of a subvariety ",TEX///$Z\subset X$///}}, 
Outputs => { 
LegacyRationalMap => {"the restriction of ",TEX///$\phi$///," to ",TEX///$Z$///,", ",TEX///$\phi|_{Z}: Z \dashrightarrow Y$///}}, 
EXAMPLE {
"P5 = ZZ/190181[x_0..x_5]",
"Phi = legacyRationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3}",
"I = ideal(random(2,P5),random(3,P5));",
"Phi' = Phi|I",
"describe Phi",
"describe Phi'"},
SeeAlso => {(symbol ||,LegacyRationalMap,Ideal)}}

document { 
Key => {(symbol ||,LegacyRationalMap,Ideal),(symbol ||,LegacyRationalMap,RingElement),(symbol ||,LegacyRationalMap,Ring)}, 
Headline => "restriction of a rational map", 
Usage => "Phi || J", 
Inputs => { 
LegacyRationalMap => "Phi" => { TEX///$\phi:X \dashrightarrow Y$///},
Ideal => "J" => {"a homogeneous ideal of a subvariety ",TEX///$Z\subset Y$///}}, 
Outputs => { 
LegacyRationalMap => {"the restriction of ",TEX///$\phi$///," to ",TEX///${\phi}^{(-1)} Z$///,", ",TEX///${{\phi}|}_{{\phi}^{(-1)} Z}: {\phi}^{(-1)} Z \dashrightarrow Z$///}}, 
EXAMPLE {
"P5 = ZZ/190181[x_0..x_5]",
"Phi = legacyRationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3}",
"J = ideal random(1,P5);",
"Phi' = Phi||J",
"describe Phi",
"describe Phi'"},
SeeAlso => {(symbol |,LegacyRationalMap,Ideal)}}

document { 
Key => {legacyRationalMap,(legacyRationalMap,RingMap),(legacyRationalMap,Matrix),(legacyRationalMap,List),(legacyRationalMap,Ring),(legacyRationalMap,Ring,Ring),(legacyRationalMap,Ring,Ring,Matrix),(legacyRationalMap,Ring,Ring,List)}, 
Headline => "makes a rational map", 
Usage => "legacyRationalMap phi 
legacyRationalMap F", 
Inputs => { 
RingMap => { "or ", ofClass {Matrix,List},", etc."}}, 
Outputs => { LegacyRationalMap => {"the rational map represented by ",TT"phi"," or by the ring map defined by ",TT"F"}}, 
PARA{"This is the basic construction for ",ofClass LegacyRationalMap,". The method is quite similar to ",TO toMap,", except that it returns a ", TO LegacyRationalMap," instead of a ",TO RingMap,"."},
PARA{"Up to version 5.2.1 of ",EM "Cremona",", this method was called ",TT "rationalMap"," (see ",TO rationalMap,"). For new code, it is recommended to use the package ",TO "MultiprojectiveVarieties::MultiprojectiveVarieties","."},
EXAMPLE { 
"R := QQ[t_0..t_8]", 
"F = matrix{{t_0*t_3*t_5, t_1*t_3*t_6, t_2*t_4*t_7, t_2*t_4*t_8}}",
"phi = toMap F",
"legacyRationalMap phi",
"legacyRationalMap F"}, 
PARA{"Multigraded rings are also permitted but in this case the method returns an object of the class ",TT"LegacyMultihomogeneousRationalMap",", which can be considered as an extension of the class ",TT"LegacyRationalMap","."},
EXAMPLE { 
"R' := newRing(R,Degrees=>{3:{1,0,0},2:{0,1,0},4:{0,0,1}})",
"F' = sub(F,R')",
"phi' = toMap F'",
"legacyRationalMap phi'",
"legacyRationalMap F'"}, 
SeeAlso => {toMap,(legacyRationalMap,Ideal),(legacyRationalMap,Tally)}}

document { 
Key => {(legacyRationalMap,Ideal,ZZ,ZZ),(legacyRationalMap,Ideal,ZZ),(legacyRationalMap,Ideal,List),(legacyRationalMap,Ideal)}, 
Headline => "makes a rational map from an ideal", 
Usage => "legacyRationalMap(I,d,e)
legacyRationalMap(I,d)
legacyRationalMap I", 
Inputs => { 
Ideal => "I" => {"a homogeneous ideal in the coordinate ring of a projective variety ",TEX///$X\subseteq\mathbb{P}^n$///},
ZZ => "d" => {"a positive integer (or a ",TO2{List,"list"}," of integers in the case ",TEX///$X$///," is embedded in a product of projective spaces)"},
ZZ => "e" => {"a positive integer (if omitted, it is taken to be 1)"}}, 
Outputs => { LegacyRationalMap => {"the rational map ",TEX///$X\dashrightarrow \mathbb{P}^m$///," defined by the linear system of hypersurfaces of degree ",TEX///$d$///, " having points of multiplicity ",TEX///$e$///," along the projective subscheme of ",TEX///$X$///," defined by ",TEX///$I$///,"."}}, 
PARA {"In most cases, the command ",TT"legacyRationalMap(I,d,e)"," yields the same output as ",TT"legacyRationalMap(saturate(I^e),d)",", but the former is implemented using pure linear algebra."},
PARA{"The command ",TT"legacyRationalMap I"," is basically equivalent to ", TT"legacyRationalMap(I,max degrees I)","."},
PARA{"In the following example, we calculate the rational map defined by the linear system of cubic hypersurfaces in ",TEX///$\mathbb{P}^6$///," having double points along a Veronese surface ",TEX///$V\subset\mathbb{P}^5\subset\mathbb{P}^6$///,"."},
EXAMPLE {
"ZZ/33331[x_0..x_6]; V = ideal(x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3,x_6);", 
"time phi = legacyRationalMap(V,3,2)",
"describe phi!"},
SeeAlso => {toMap,legacyRationalMap,(legacyRationalMap,Tally)}}

document { 
Key => {toMap,(toMap,Matrix),(toMap,Ideal),(toMap,Ideal,ZZ),(toMap,Ideal,List),(toMap,Ideal,ZZ,ZZ),(toMap,List),(toMap,RingMap)}, 
Headline => "rational map defined by a linear system", 
Usage => "toMap(\"linear system\")", 
Inputs => { 
Matrix => { "or ",ofClass List, ", etc."}}, 
Outputs => { RingMap}, 
PARA{"When the input represents a list of homogeneous elements ",TEX///$F_0,\ldots,F_m\in R=K[t_0,\ldots,t_n]/I$///," of the same degree, then the method returns the ring map ",TEX///$\phi:K[x_0,\ldots,x_m] \to R$///," that sends ",TEX///$x_i$///," into ",TEX///$F_i$///,"."}, 
EXAMPLE { 
"QQ[t_0,t_1];", 
"linSys=gens (ideal(t_0,t_1))^5",
"phi=toMap linSys"}, 
PARA{"If a positive integer ",TEX///$d$///," is passed to the option ", TO Dominant, ", then the method returns the induced map on ",TEX///$K[x_0,\ldots,x_m]/J_d$///,", where ",TEX///$J_d$///," is the ideal generated by all homogeneous elements of degree ",TEX///$d$///," of the kernel of ",TEX///$\phi$///," (in this case ", TO (kernel,RingMap,ZZ), " is called)."},
EXAMPLE { 
"phi'=toMap(linSys,Dominant=>2)"}, 
PARA{"If the input is a pair consisting of a homogeneous ideal ",TEX///$I$///, " and an integer ",TEX///$v$///,", then the output will be the map defined by the linear system of hypersurfaces of degree ",TEX///$v$///, " which contain the projective subscheme defined by ",TEX///$I$///,"."},
EXAMPLE { 
"I=kernel phi",
"toMap(I,2)"}, 
PARA{"This is identical to ", TT "toMap(I,v,1)", ", while the output of ", TT "toMap(I,v,e)", " will be the map defined by the linear system of hypersurfaces of degree ",TEX///$v$///, " having points of multiplicity ",TEX///$e$///," along the projective subscheme defined by ",TEX///$I$///,"."},
EXAMPLE { 
"toMap(I,2,1)", 
"toMap(I,2,2)", 
"toMap(I,3,2)"},
SeeAlso => {(legacyRationalMap,Matrix),(legacyRationalMap,Ideal,ZZ,ZZ)}} 

document { 
Key => {(symbol **,LegacyRationalMap,Ring)},
Headline => "change the coefficient ring of a rational map", 
Usage => "phi ** K", 
Inputs => { 
LegacyRationalMap => "phi" => {"defined over a coefficient ring ",TT"F"},
Ring => "K" => {"the new coefficient ring (which must be a field)"}}, 
Outputs => { 
LegacyRationalMap => {"a rational map defined over ",TT"K",", obtained by coercing the coefficients of the forms defining ",TT"phi", " into ",TT"K"}}, 
PARA {"It is necessary that all forms in the old coefficient ring ",TT"F"," can be automatically coerced into the new coefficient ring ",TT"K","."},
EXAMPLE {
"QQ[vars(0..5)]",
"phi = legacyRationalMap {e^2-d*f, c*e-b*f, c*d-b*e, c^2-a*f, b*c-a*e, b^2-a*d}",
"K = ZZ/65521;",
"phi ** K",
"phi ** frac(K[t])"
},
SeeAlso => {(coefficientRing,LegacyRationalMap)}}

document { 
Key => {(legacyRationalMap,PolynomialRing,List)},
Headline => "rational map defined by the linear system of hypersurfaces passing through random points with multiplicity", 
Usage => "legacyRationalMap(R,{a,i,j,k,...})",
Inputs => { 
PolynomialRing => "R",
List => {"a list ",TEX///$\{a,i,j,k,\ldots\}$///," of nonnegative integers"}}, 
Outputs => {LegacyRationalMap => {"the rational map defined by the linear system of hypersurfaces of degree ",TEX///$a$///," in ",TEX///$Proj(R)$///," having ",TEX///$i$///," random base points of multiplicity 1, ",TEX///$j$///," random base points of multiplicity 2, ",TEX///$k$///," random base points of multiplicity 3, and so on until the last integer in the given list."}},
PARA{"In the example below, we take the rational map defined by the linear system of septic plane curves with 3 random simple base points and 9 random double points."}, 
EXAMPLE { 
"ringP2 = ZZ/65521[vars(0..2)];", 
"phi = legacyRationalMap(ringP2,{7,3,9})",
"describe phi!"},
SeeAlso => {(legacyRationalMap,Ideal,ZZ,ZZ),point}} 

undocumented{(texMath,LegacyRationalMap),(net,LegacyRationalMap),(expression,LegacyRationalMap),(toString,LegacyRationalMap),(toMap,LegacyRationalMap),(lift,LegacyRationalMap)}

document { 
Key => {specialCremonaTransformation,(specialCremonaTransformation,Ring,ZZ),(specialCremonaTransformation,ZZ,Ring),(specialCremonaTransformation,ZZ)}, 
Headline => "special Cremona transformations whose base locus has dimension at most three", 
Usage => "specialCremonaTransformation i 
specialCremonaTransformation(i,K)", 
Inputs => { 
"i" => ZZ => {"an integer between 1 and 12"},
"K" => Ring => {"the ground field (optional, the default value is ",TO QQ,")"}}, 
Outputs => { 
LegacyRationalMap => {"an example of special Cremona transformation over ",TT"K",", according to the classification given in Table 1 of ",HREF{"https://www.degruyter.com/view/journals/advg/19/2/article-p191.xml","Special cubic Cremona transformations of P6 and P7"},"."}},
PARA{"A Cremona transformation is said to be special if the base locus scheme is smooth and irreducible. To ensure this condition, the field ",TT"K"," must be large enough but no check is made."},
EXAMPLE {
"time apply(1..12,i -> describe specialCremonaTransformation(i,ZZ/3331))"},
SeeAlso => {"quadroQuadricCremonaTransformation", "specialQuadraticTransformation","specialCubicTransformation"}}

document { 
Key => {quadroQuadricCremonaTransformation,(quadroQuadricCremonaTransformation,Ring,ZZ,ZZ),(quadroQuadricCremonaTransformation,ZZ,ZZ,Ring),(quadroQuadricCremonaTransformation,ZZ,ZZ)}, 
Headline => "quadro-quadric Cremona transformations", 
Usage => "quadroQuadricCremonaTransformation(n,i) 
quadroQuadricCremonaTransformation(n,i,K)", 
Inputs => { 
"n" => ZZ => {"the dimension of the projective space"},
"i" => ZZ => {"the ",TT"i","-th case in the classification for ",TT"P^n", " (for instance, if ",TT"n=5"," then ",TT"1<=i<=39",")"},
"K" => Ring => {"the ground field (optional, the default value is ",TO QQ,")"}}, 
Outputs => { 
LegacyRationalMap => {"an example of quadro-quadric Cremona transformation over ",TT"K",", according to the classifications given in the paper ",HREF{"https://aif.centre-mersenne.org/item/AIF_2014__64_1_71_0/","Quadro-quadric Cremona transformations in low dimensions via the JC-correspondence"}, ", by Pirio and Russo."}},
EXAMPLE {
"quadroQuadricCremonaTransformation(5,23)",
"describe oo"},
PARA{"In addition, the four pairs ",TT"(n,i)=(5,1),(8,1),(14,1),(26,1)"," correspond to the four examples of special quadro-quadric Cremona transformations:"},
EXAMPLE {
"describe quadroQuadricCremonaTransformation(5,1)",
"describe quadroQuadricCremonaTransformation(8,1)",
"describe quadroQuadricCremonaTransformation(14,1)",
"describe quadroQuadricCremonaTransformation(26,1)"},
SeeAlso => {"specialCremonaTransformation", "specialQuadraticTransformation","specialCubicTransformation"}}

document { 
Key => {specialQuadraticTransformation,(specialQuadraticTransformation,Ring,ZZ),(specialQuadraticTransformation,ZZ,Ring),(specialQuadraticTransformation,ZZ)}, 
Headline => "special quadratic transformations whose base locus has dimension three", 
Usage => "specialQuadraticTransformation i 
specialQuadraticTransformation(i,K)", 
Inputs => { 
"i" => ZZ => {"an integer between 1 and 11"},
"K" => Ring => {"the ground field (optional, the default value is ",TO QQ,")"}}, 
Outputs => { 
LegacyRationalMap => {"an example of special quadratic birational transformation over ",TT"K",", according to the classification given in Table 1 of ",HREF{"https://www.sciencedirect.com/science/article/pii/S0747717115001029?via%3Dihub","Examples of special quadratic birational transformations into complete intersections of quadrics"},"."}},
PARA{"The field ",TT"K"," is required to be large enough."},
EXAMPLE {
"time specialQuadraticTransformation 4",
"time describe oo"},
SeeAlso => {"specialCremonaTransformation","specialCubicTransformation","quadroQuadricCremonaTransformation"}}

document { 
Key => {specialCubicTransformation,(specialCubicTransformation,Ring,ZZ),(specialCubicTransformation,ZZ,Ring),(specialCubicTransformation,ZZ)}, 
Headline => "special cubic transformations whose base locus has dimension at most three", 
Usage => "specialCubicTransformation i 
specialCubicTransformation(i,K)", 
Inputs => { 
"i" => ZZ => {"an integer between 1 and 9"},
"K" => Ring => {"the ground field (optional, the default value is ",TO QQ,")"}}, 
Outputs => { 
LegacyRationalMap => {"an example of special cubic birational transformation over ",TT"K",", according to the classification given in Table 2 of ",HREF{"https://link.springer.com/article/10.1007/s13348-019-00251-8","Special cubic birational transformations of projective spaces"},"."}},
PARA{"The field ",TT"K"," is required to be large enough."},
EXAMPLE {
"time specialCubicTransformation 9",
"time describe oo"},
SeeAlso => {"specialCremonaTransformation","specialQuadraticTransformation","quadroQuadricCremonaTransformation"}}

document { 
Key => {[inverseMap,Verbose], [projectiveDegrees,Verbose],[degreeMap,Verbose],[approximateInverseMap,Verbose],[isDominant,Verbose],[isBirational,Verbose],[SegreClass,Verbose],[ChernSchwartzMacPherson,Verbose],[EulerCharacteristic,Verbose]}, 
PARA{"This option accepts a ", TO Boolean, " value. Set this to ",TT "false"," if you don't want to get the certification message from ",TO Certify,"."},
EXAMPLE {
"f = toMap vars(QQ[x_0..x_2]);",
"isBirational(f,Certify=>true)",
"isBirational(f,Certify=>true,Verbose=>false)"}} 

document { 
Key => {BlowUpStrategy,[graph,BlowUpStrategy],[inverseMap,BlowUpStrategy], [projectiveDegrees,BlowUpStrategy],[degreeMap,BlowUpStrategy],[isBirational,BlowUpStrategy],[SegreClass,BlowUpStrategy],[ChernSchwartzMacPherson,BlowUpStrategy],[EulerCharacteristic,BlowUpStrategy]}, 
PARA{"This is an optional argument for ", TO graph,", and for the methods that eventually call it. Currently, the possible values are \"Eliminate\" and \"Saturate\", which indicate two different ways of computing the (closure of the) graph of a rational map. The default choice is \"Eliminate\" and this is generally preferable."}} 


document { 
Key => {(describe,LegacyRationalMap),(symbol ?,Ideal)}, 
Headline => "describe a rational map", 
Usage => "describe phi", 
Inputs => { 
"phi" => LegacyRationalMap}, 
Outputs => { 
{"a description of ",TT"phi",", giving some indication of what has already been calculated."}},
EXAMPLE { 
"ZZ/33331[t_0..t_4];",
"phi = legacyRationalMap minors(2,matrix{{t_0..t_3},{t_1..t_4}});",
"describe phi",
"I = image phi;",
"describe phi",
"? I",
"phi!;",
"describe phi"},
SeeAlso => {(symbol !,LegacyRationalMap)}}

document { 
Key => {forceInverseMap,(forceInverseMap,LegacyRationalMap,LegacyRationalMap)}, 
Headline => "declare that two rational maps are one the inverse of the other", 
Usage => "forceInverseMap(Phi,Psi)", 
Inputs => { 
"Phi" => LegacyRationalMap,
"Psi" => LegacyRationalMap},
Outputs => {Nothing => {TO null}}, 
PARA{"This method allows to inform the system that two maps are one the inverse of the other without performing any computation. This is useful in particular if you calculate the inverse map using your own method."},
Caveat => {"If the declaration is false, nonsensical answers may result."},
SeeAlso => {(isInverseMap,LegacyRationalMap,LegacyRationalMap),(inverse,LegacyRationalMap)}}

document { 
Key => {forceImage,(forceImage,LegacyRationalMap,Ideal)}, 
Headline => "declare which is the image of a rational map", 
Usage => "forceImage(Phi,I)", 
Inputs => { 
"Phi" => LegacyRationalMap,
"I" => Ideal},
Outputs => {Nothing => {TO null}}, 
PARA{"This method allows to inform the system about the image of a given rational map without performing any computation. In particular, this can be used to declare that a rational map is dominant."},
EXAMPLE { 
"P6 = QQ[t_0..t_6]; X = minors(3,matrix{{t_0..t_4},{t_1..t_5},{t_2..t_6}});",
"Phi = legacyRationalMap(X,Dominant=>2);", 
"time forceImage(Phi,ideal 0_(target Phi))", 
"Phi;"},
Caveat => {"If the declaration is false, nonsensical answers may result."},
SeeAlso => {(image,LegacyRationalMap),forceInverseMap}}

document {Key => {point}, 
Headline => "pick a random rational point on a projective variety", 
PARA{"See ", TO "MultiprojectiveVarieties::point(MultiprojectiveVarieties)",
" and ",TO2{(point,QuotientRing),"point(QuotientRing)"},"."}}

undocumented {(point,Ideal),(point,Ideal,Boolean)}
document { 
Key => {(point,QuotientRing),(point,PolynomialRing)}, 
Headline => "pick a random rational point on a projective variety", 
Usage => "point R", 
Inputs => { 
QuotientRing => "R" => {"the homogeneous coordinate ring of a closed subscheme ",TEX///$X\subseteq\mathbb{P}^n$///," over a finite ground field"}}, 
Outputs => { 
Ideal => {"an ideal in ",TT "R"," defining a point on ",TEX///$X$///}}, 
PARA{"This function is a variant of the ",TO randomKRationalPoint," function, which has been further improved and extended in the package ", TO "MultiprojectiveVarieties", ", see ", TO "MultiprojectiveVarieties::point(MultiprojectiveVariety)", "."},
PARA{"Below we verify the birationality of a rational map."},
EXAMPLE { 
"f = inverseMap specialQuadraticTransformation(9,ZZ/33331);",
"time p = point source f",
"time p == f^* f p"},
SeeAlso => {randomKRationalPoint}}

undocumented {(segre,Ideal,Ideal)}
document { 
Key => {segre,(segre,LegacyRationalMap),(segre,QuotientRing),(segre,PolynomialRing)}, 
Headline => "Segre embedding", 
Usage => "segre phi 
segre R", 
Inputs => { 
"phi" => LegacyRationalMap => {"with source a closed subvariety ",TEX///$X\subseteq\mathbb{P}^{n_1}\times\mathbb{P}^{n_2}\times\cdots\times\mathbb{P}^{n_k}$///," of a product of projective spaces"}, 
"R" => QuotientRing => {"or a ",TO2{PolynomialRing,"polynomial ring"},", the coordinate ring of the subvariety ",TEX///$X\subseteq\mathbb{P}^{n_1}\times\mathbb{P}^{n_2}\times\cdots\times\mathbb{P}^{n_k}$///}}, 
Outputs => { 
LegacyRationalMap => {"the restriction to ",TEX///$X$///," of the Segre embedding of ",TEX///$\mathbb{P}^{n_1}\times\mathbb{P}^{n_2}\times\cdots\times\mathbb{P}^{n_k}$///,", where the linear span of the image is identified with a projective space"}},
PARA{"More properly, this method accepts and returns objects of the class ",TT"LegacyMultihomogeneousRationalMap","."},
EXAMPLE {
"phi = first graph quadroQuadricCremonaTransformation(3,1)",
"segre phi"}}

document { 
Key => {abstractRationalMap,(abstractRationalMap,PolynomialRing,PolynomialRing,FunctionClosure,ZZ),(abstractRationalMap,PolynomialRing,PolynomialRing,FunctionClosure),(abstractRationalMap,LegacyRationalMap)}, 
Headline => "make an abstract rational map", 
Usage => "abstractRationalMap(R,S,f,d) 
          abstractRationalMap(R,S,f)",
Inputs => { 
PolynomialRing => "R" => {"the coordinate ring of the source of the map"},
PolynomialRing => "S" => {"the coordinate ring of the target of the map"},
FunctionClosure => "f" => {"the abstract definition of the map"},
ZZ => "d" => {"(optional) an integer close to the degree of the forms defining the map"}}, 
Outputs => { 
LegacyRationalMap => {"the abstract rational map from ",TT"Proj(R)"," to ",TT"Proj(S)"," defined by f"}}, 
PARA{"The main ingredient behind this method is the interpolation of multivariate polynomials. We illustrate this feature with some examples."},
EXAMPLE {
"f = a -> {-a_1^3*max(sin(a_2),1)+a_0*a_1*a_2*ceiling((log(1 + abs a_0))^0),-a_1^2*a_2+a_0*a_1*a_3,-a_1*a_2^2+a_1^2*a_3,-a_1^2*a_3+a_0*a_1*a_4,-a_1*a_2*a_3+a_1^2*a_4,-a_1*a_3^2+a_1*a_2*a_4}",
"P4 := QQ[t_0..t_4]",
"P5 := QQ[u_0..u_5]",
"time psi = abstractRationalMap(P4,P5,f)"},
PARA{"Now we compute first the degree of the forms defining the abstract map ",TT"psi"," and then the corresponding concrete rational map."},
EXAMPLE {
"time projectiveDegrees(psi,3)",
"time legacyRationalMap psi"},
PARA{"As a second example, we apply the method to compute the inverse of a Cremona transformation."},
EXAMPLE {
"phi = legacyRationalMap map specialCremonaTransformation(3,ZZ/10000019);",
"phi' = abstractRationalMap phi",
"psi' = inverseMap phi'",
"psi = legacyRationalMap psi';",
"assert(isInverseMap(phi,psi))"},
PARA{"We now consider a more interesting application. Recall that a closed subvariety ",TEX///$X\subset\mathbb{P}^n$///," is called a subvariety with one apparent double point if a general point in ",TEX///$\mathbb{P}^n$///," lies on a unique secant of ",TEX///$X$///,". A subvariety ",TEX///$X\subset\mathbb{P}^n$///," with an apparent double point defines a Cremona involution of ",TEX///$\mathbb{P}^n$///,": for a general point ",TEX///$x\in\mathbb{P}^n$///," we find a unique secant of ",TEX///$X$///," intersecting ",TEX///$X$///," at two points ",TEX///$(a,b)$///,", and then define the unique ",TEX///$T(x)$///," such that the pair ",TEX///$\{x,T(x)\}$///," is harmonically conjugate to ",TEX///$\{a,b\}$///,". For more details, see Lecture 4 in ",HREF{"http://www.math.lsa.umich.edu/~idolga/cremonalect.pdf","Lectures on Cremona transformations, by I. Dolgachev"},". This abstract construction is implemented in the package: if ",TT"I"," is the ideal of one apparent double point variety ",TEX///$X\subset\mathbb{P}^n$///,", then the command ",TT"abstractRationalMap(I,\"OADP\")"," returns the abstract rational map above defined. For instance, we can take ",TEX///$X$///," to be the twisted cubic curve in ",TEX///$\mathbb{P}^3$///,"."},
EXAMPLE {
"ZZ/65521[x_0..x_3]; I = minors(2,matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}})", 
"time T = abstractRationalMap(I,\"OADP\")"},
PARA{"The degree of the forms defining the abstract map ",TT"T"," can be obtained by the following command:"},
EXAMPLE {
"time projectiveDegrees(T,2)"},
PARA{"We verify that the composition of ",TT"T"," with itself is defined by linear forms:"},
EXAMPLE {
"time T2 = T * T",
"time projectiveDegrees(T2,2)"},
PARA{"We verify that the composition of ",TT"T"," with itself leaves a random point fixed:"},
EXAMPLE {
"p = apply(3,i->random(ZZ/65521))|{1}",
"q = T p",
"T q"},
PARA{"We now compute the concrete rational map corresponding to ",TT"T",":"},
EXAMPLE {
"time f = legacyRationalMap T",
"describe f!"},
Caveat => {"This is under development yet."}
}
undocumented{(abstractRationalMap,Ideal,String)}

document { 
Key => {(toExternalString,LegacyRationalMap)}, 
Headline => "convert to a readable string", 
Usage => "toExternalString phi", 
Inputs => {LegacyRationalMap => "phi"}, 
Outputs => {String => {"a string representation of ",TT "phi",", which can be used, in conjunction with ",TO "value",", to read the object back into the program later"}},
PARA{"All internal data of the input are included in the returned string."},
EXAMPLE {
"phi = (specialCubicTransformation(2,ZZ/33331))!;",
"str = toExternalString phi;",
"#str",
"time phi' = value str;",
"time describe phi'",
"time describe inverse phi'"}
}

document { 
Key => {isMorphism,(isMorphism,LegacyRationalMap)}, 
Headline => "whether a rational map is a morphism", 
Usage => "isMorphism phi", 
Inputs => { 
"phi" => LegacyRationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi"," is a morphism (i.e., everywhere defined)"}},
EXAMPLE { 
"phi = quadroQuadricCremonaTransformation(5,1)",
"isMorphism phi",
"phi' = last graph phi;",
"isMorphism phi'"},
SeeAlso => {(ideal,LegacyRationalMap),(isIsomorphism,LegacyRationalMap)}}

document { 
Key => {(isIsomorphism,LegacyRationalMap)}, 
Headline => "whether a birational map is an isomorphism", 
Usage => "isIsomorphism phi", 
Inputs => {"phi" => LegacyRationalMap}, 
Outputs => {Boolean => {"whether ",TT"phi"," is an isomorphism"}},
PARA{"This method computes the inverse rational map using ",TO2{(inverse,LegacyRationalMap),"inverse"},"."},
EXAMPLE { 
"P1 := QQ[a,b]; P4 := QQ[x,y,z,w];",
"phi = legacyRationalMap({a^4,a^3*b,a^2*b^2,a*b^3,b^4},Dominant=>true)",
"isIsomorphism phi"},
SeeAlso => {(ideal,LegacyRationalMap),isBirational,isMorphism}}

document { 
Key => {(legacyRationalMap,Ring,Tally),(legacyRationalMap,Tally)}, 
Headline => "rational map defined by an effective divisor", 
Usage => "legacyRationalMap D
legacyRationalMap(R,D)", 
Inputs => {"R" => Ring => {"the coordinate ring of a locally factorial projective variety ",TEX///$X\subseteq\mathbb{P}^n$///},
"D" => Tally => {"a multiset of homogeneous ideals in ",TEX///$R$///," (or in ",TO ambient," ",TEX///$R$///,") defining pure codimension 1 subschemes of ",TEX///$X$///," with no embedded components; so that ",TT"D"," is interpreted as an effective divisor on ",TEX///$X$///}}, 
Outputs => {LegacyRationalMap => {"the rational map defined by the complete linear system ",TEX///$|D|$///}},
PARA{"In the example below, we take a smooth complete intersection ",TEX///$X\subset\mathbb{P}^5$///," of three quadrics containing a conic ",TEX///$C\subset\mathbb{P}^5$///,". Then we calculate the map defined by the linear system ",TEX///$|2H+C|$///,", where ",TEX///$H$///," is the hyperplane section class of ",TEX///$X$///,"."},
EXAMPLE {
"P5 = ZZ/65521[x_0..x_5];",
"C = ideal(x_1^2-x_0*x_2,x_3,x_4,x_5)",
"X = quotient ideal(-x_1^2+x_0*x_2-x_1*x_3+x_3^2-x_0*x_5+x_1*x_5+x_3*x_5,-x_0*x_3-x_1*x_3+x_2*x_4-x_3*x_4-x_4^2-x_1*x_5-x_2*x_5+x_5^2,-x_1^2+x_0*x_2+x_2*x_3+x_1*x_4-x_3*x_4-x_4*x_5);",
"H = ideal random(1,X)",
"D = new Tally from {H => 2,C => 1};",
"time phi = legacyRationalMap D",
"time ? image(phi,\"F4\")"},
PARA{"See also the package ", TO "WeilDivisors::WeilDivisors", ", which provides general tools for working with divisors."},
SeeAlso => {legacyRationalMap,(legacyRationalMap,Ideal,ZZ),(image,LegacyRationalMap,String)}}

document { 
Key => {rationalMap,(rationalMap,RingMap),(rationalMap,Matrix),(rationalMap,List),(rationalMap,Ideal),(rationalMap,Ideal,ZZ),(rationalMap,Ideal,List),(rationalMap,Ideal,ZZ,ZZ),(rationalMap,PolynomialRing,List),(rationalMap,LegacyRationalMap),(rationalMap,Ring,Ring,Matrix),(rationalMap,Ring,Ring,List),(rationalMap,Ring,Ring),(rationalMap,Ring),(rationalMap,Ring,Tally),(rationalMap,Tally),symbol RationalMap}, 
Headline => "deprecated synonyms of legacyRationalMap and LegacyRationalMap", 
Usage => "rationalMap(...)", 
PARA{"Up to version 5.2.1 of ",EM "Cremona",", the class ",TO LegacyRationalMap," and its constructor ",TO legacyRationalMap," were called ",TT "RationalMap"," and ",TT "rationalMap",". The old names are kept only for compatibility, and they will be removed in a future version: each method of ",TT "rationalMap"," simply calls the corresponding method of ",TO legacyRationalMap,", and ",TT "RationalMap"," is the same class as ",TO LegacyRationalMap,"."},
PARA{"Other packages may install further methods for ",TT "rationalMap",". In particular, when the package ",TO "MultiprojectiveVarieties::MultiprojectiveVarieties"," is loaded, ",TT "rationalMap"," also accepts the multi-projective varieties of that package, and in that case it returns a ",TO "MultiprojectiveVarieties::MultirationalMap",". To always obtain a ",TO LegacyRationalMap,", use ",TO legacyRationalMap,"."},
EXAMPLE {
"R = QQ[t_0..t_2];",
"phi = rationalMap {t_1*t_2,t_0*t_2,t_0*t_1}",
"phi == legacyRationalMap {t_1*t_2,t_0*t_2,t_0*t_1}",
"RationalMap === LegacyRationalMap"},
SeeAlso => {legacyRationalMap,LegacyRationalMap}}
