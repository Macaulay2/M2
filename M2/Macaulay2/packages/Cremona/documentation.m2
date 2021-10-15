beginDocumentation() 

document { 
Key => Cremona, 
Headline => "package for some computations on rational maps between projective varieties", 
EM "Cremona", " is a package to perform some basic computations on rational and birational maps between absolutely irreducible projective varieties over a field ",TEX///$K$///,". For instance, it provides general methods to compute degrees and projective degrees of rational maps (see ", TO"degreeMap", " and ", TO"projectiveDegrees",") and a general method to compute the push-forward to projective space of Segre classes (see ",TO"SegreClass","). Moreover, all the main methods are available both in version probabilistic and in version deterministic, and one can switch from one to the other with the boolean option ",TO "MathMode",".",
PARA{"Let ",TEX///$\Phi:X \dashrightarrow Y$///," be a rational map from a subvariety ",TEX///$X=V(I)\subseteq\mathbb{P}^n=Proj(K[x_0,\ldots,x_n])$///," to a subvariety ",TEX///$Y=V(J)\subseteq\mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". Then the map ",TEX///$\Phi $///," can be represented, although not uniquely, by a homogeneous ring map ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," of quotients of polynomial rings by homogeneous ideals. These kinds of ring maps, together with the objects of the ",TO RationalMap," class, are the typical inputs for the methods in this package. The method ", TO toMap," (resp. ",TO rationalMap,") constructs such a ring map (resp. rational map) from a list of ",TEX///$m+1$///," homogeneous elements of the same degree in ",TEX///$K[x_0,...,x_n]/I$///,"."}, 
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
PARA{"We repeat the example using the type ",TO RationalMap, " and using deterministic methods."},
EXAMPLE { 
"time phi = rationalMap minors(3,matrix{{t_0..t_4},{t_1..t_5},{t_2..t_6}})", 
"time phi = rationalMap(phi,Dominant=>2)", 
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
SeeAlso => {(degree,RationalMap),projectiveDegrees}} 

undocumented{(projectiveDegrees,MutableHashTable,ZZ),(projectiveDegrees,RingMap,ZZ)}

document { 
Key => {projectiveDegrees,(projectiveDegrees,RingMap)}, 
Headline => "projective degrees of a rational map between projective varieties", 
Usage => "projectiveDegrees phi", 
Inputs => { "phi" => RingMap => {"which represents a rational map ",TEX///$\Phi$///," between projective varieties"}}, 
Outputs => {List => {"the list of the projective degrees of ",TEX///$\Phi$///}}, 
PARA{"Let ",TEX///$\phi:K[y_0,\ldots,y_m]/J \to K[x_0,\ldots,x_n]/I$///," be a ring map representing a rational map ",TEX///$\Phi: V(I) \subseteq \mathbb{P}^n=Proj(K[x_0,\ldots,x_n]) \dashrightarrow V(J) \subseteq \mathbb{P}^m=Proj(K[y_0,\ldots,y_m])$///,". The ",TEX///$i$///,"-th projective degree of ",TEX///$\Phi$///," is defined in terms of dimension and degree of the closure of ",TEX///$\Phi^{-1}(L)$///,", where ",TEX///$L$///," is a general linear subspace of ",TEX///$\mathbb{P}^m$///," of a certain dimension; for the precise definition, see Harris's book (Algebraic geometry: A first course - Vol. 133 of Grad. Texts in Math., p. 240). If ",TEX///$\Phi$///," is defined by elements ",TEX///$F_0(x_0,\ldots,x_n),\ldots,F_m(x_0,\ldots,x_n)$///," and ",TEX///$I_L$///," denotes the ideal of the subspace ",TEX///$L\subseteq \mathbb{P}^m$///,", then the ideal of the closure of ",TEX///$\Phi^{-1}(L) $///," is nothing but the saturation of the ideal ",TEX///$(\phi(I_L))$///," by ",TEX///$(F_0,....,F_m)$///," in the ring ",TEX///$K[x_0,\ldots,x_n]/I$///,". So, replacing in the definition, ", EM "general linear subspace", " by ", EM "random linear subspace", ", we get a probabilistic algorithm to compute all projective degrees. Furthermore, we can considerably speed up this algorithm by taking into account two simple remarks: 1) the saturation ",TEX///$(\phi(I_L)):{(F_0,\ldots,F_m)}^{\infty}$///," is the same as ",TEX///$(\phi(I_L)):{(\lambda_0 F_0+\cdots+\lambda_m F_m)}^{\infty}$///,", where ",TEX///$\lambda_0,\ldots,\lambda_m\in\mathbb{K}$///," are general scalars; 2) the ",TEX///$i$///,"-th projective degree of ",TEX///$\Phi$///," coincides with the ",TEX///$(i-1)$///,"-th projective degree of the restriction of ",TEX///$\Phi$///," to a general hyperplane section of ",TEX///$X$///," (see ",EM"loc. cit.","). This is what the method uses if ", TO MathMode, " is set to ", TT "false",". If instead ", TO MathMode, " is set to ", TT "true", ", then the method simply computes the ",TO "multidegree"," of the ",TO "graph","."},
EXAMPLE { 
"-- map from P^4 to G(1,3) given by the quadrics through a rational normal curve of degree 4
GF(331^2)[t_0..t_4]; phi=toMap minors(2,matrix{{t_0..t_3},{t_1..t_4}})", 
"time projectiveDegrees(phi,MathMode=>true)",
"psi=inverseMap(toMap(phi,Dominant=>infinity))", 
"time projectiveDegrees(psi,MathMode=>true)"}, 
EXAMPLE { 
"-- Cremona transformation of P^6 defined by the quadrics through a rational octic surface
phi = map specialCremonaTransformation(7,ZZ/300007)",
"time projectiveDegrees phi", 
"time projectiveDegrees(phi,NumDegrees=>1)"}, 
PARA{"Another way to use this method is by passing an integer ",TT"i"," as second argument. However, this is equivalent to ",TT"first projectiveDegrees(phi,NumDegrees=>i)", " and generally it is not faster."},
SeeAlso => {(degrees,RationalMap),degreeMap, SegreClass}} 

document { 
Key => {MathMode, [inverseMap,MathMode], [projectiveDegrees,MathMode],[degreeMap,MathMode],[approximateInverseMap,MathMode],[isDominant,MathMode],[isBirational,MathMode],[SegreClass,MathMode],[ChernSchwartzMacPherson,MathMode],[EulerCharacteristic,MathMode],[exceptionalLocus,MathMode]}, 
Headline => "whether to ensure correctness of output", 
"This option accepts a ", TO Boolean, " value, default value ",TT "false",".",
PARA{"If turned on in the methods ", TO inverseMap," and ", TO approximateInverseMap, ", then it will be checked whether the maps in input and output are one the inverse of the other, throwing an error if they are not. Actually, ", TO approximateInverseMap, " will first try to fix the error of the approximation. When turned on in the methods ", TO projectiveDegrees,", ", TO degreeMap, ", ", TO isBirational,", ", TO isDominant, ", ", TO SegreClass, ", ", TO EulerCharacteristic, " and ", TO ChernSchwartzMacPherson, ", it means whether to use a non-probabilistic algorithm."}} 

document { 
Key => {Dominant, [toMap,Dominant], [rationalMap,Dominant]}, 
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
Key => {isBirational,(isBirational,RationalMap),(isBirational,RingMap)}, 
Headline => "whether a rational map is birational", 
Usage => "isBirational phi", 
Inputs => { 
"phi" => RationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi"," is birational"}},
PARA{"The testing passes through the methods ", TO projectiveDegrees, ", ", TO degreeMap," and ", TO isDominant,"."},
EXAMPLE { 
"GF(331^2)[t_0..t_4]",
"phi = rationalMap(minors(2,matrix{{t_0..t_3},{t_1..t_4}}),Dominant=>infinity)",
"time isBirational phi",
"time isBirational(phi,MathMode=>true)"},
SeeAlso => {isDominant}}

document { 
Key => {isDominant,(isDominant,RationalMap),(isDominant,RingMap)}, 
Headline => "whether a rational map is dominant", 
Usage => "isDominant phi", 
Inputs => { 
"phi" => RationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi"," is dominant"}},
PARA{"This method is based on the fibre dimension theorem. A more standard way would be to perform the command ", TT "kernel map phi == 0","."},
EXAMPLE { 
"P8 = ZZ/101[x_0..x_8];",
"phi = rationalMap ideal jacobian ideal det matrix{{x_0..x_4},{x_1..x_5},{x_2..x_6},{x_3..x_7},{x_4..x_8}};",
"time isDominant(phi,MathMode=>true)",
"P7 = ZZ/101[x_0..x_7];", 
"-- hyperelliptic curve of genus 3
C = ideal(x_4*x_5+23*x_5^2-23*x_0*x_6-18*x_1*x_6+6*x_2*x_6+37*x_3*x_6+23*x_4*x_6-26*x_5*x_6+2*x_6^2-25*x_0*x_7+45*x_1*x_7+30*x_2*x_7-49*x_3*x_7-49*x_4*x_7+50*x_5*x_7,x_3*x_5-24*x_5^2+21*x_0*x_6+x_1*x_6+46*x_3*x_6+27*x_4*x_6+5*x_5*x_6+35*x_6^2+20*x_0*x_7-23*x_1*x_7+8*x_2*x_7-22*x_3*x_7+20*x_4*x_7-15*x_5*x_7,x_2*x_5+47*x_5^2-40*x_0*x_6+37*x_1*x_6-25*x_2*x_6-22*x_3*x_6-8*x_4*x_6+27*x_5*x_6+15*x_6^2-23*x_0*x_7-42*x_1*x_7+27*x_2*x_7+35*x_3*x_7+39*x_4*x_7+24*x_5*x_7,x_1*x_5+15*x_5^2+49*x_0*x_6+8*x_1*x_6-31*x_2*x_6+9*x_3*x_6+38*x_4*x_6-36*x_5*x_6-30*x_6^2-33*x_0*x_7+26*x_1*x_7+32*x_2*x_7+27*x_3*x_7+6*x_4*x_7+36*x_5*x_7,x_0*x_5+30*x_5^2-11*x_0*x_6-38*x_1*x_6+13*x_2*x_6-32*x_3*x_6-30*x_4*x_6+4*x_5*x_6-28*x_6^2-30*x_0*x_7-6*x_1*x_7-45*x_2*x_7+34*x_3*x_7+20*x_4*x_7+48*x_5*x_7,x_3*x_4+46*x_5^2-37*x_0*x_6+27*x_1*x_6+33*x_2*x_6+8*x_3*x_6-32*x_4*x_6+42*x_5*x_6-34*x_6^2-37*x_0*x_7-28*x_1*x_7+10*x_2*x_7-27*x_3*x_7-42*x_4*x_7-8*x_5*x_7,x_2*x_4-25*x_5^2-4*x_0*x_6+2*x_1*x_6-31*x_2*x_6-5*x_3*x_6+16*x_4*x_6-24*x_5*x_6+31*x_6^2-30*x_0*x_7+32*x_1*x_7+12*x_2*x_7-40*x_3*x_7+3*x_4*x_7-28*x_5*x_7,x_0*x_4+15*x_5^2+48*x_0*x_6-50*x_1*x_6+46*x_2*x_6-48*x_3*x_6-23*x_4*x_6-28*x_5*x_6+39*x_6^2+38*x_1*x_7-5*x_3*x_7+5*x_4*x_7-34*x_5*x_7,x_3^2-31*x_5^2+41*x_0*x_6-30*x_1*x_6-4*x_2*x_6+43*x_3*x_6+23*x_4*x_6+7*x_5*x_6+31*x_6^2-19*x_0*x_7+25*x_1*x_7-49*x_2*x_7-16*x_3*x_7-45*x_4*x_7+25*x_5*x_7,x_2*x_3+13*x_5^2-45*x_0*x_6-22*x_1*x_6+33*x_2*x_6-26*x_3*x_6-21*x_4*x_6+34*x_5*x_6-21*x_6^2-47*x_0*x_7-10*x_1*x_7+29*x_2*x_7-46*x_3*x_7-x_4*x_7+20*x_5*x_7,x_1*x_3+22*x_5^2+4*x_0*x_6+3*x_1*x_6+45*x_2*x_6+37*x_3*x_6+17*x_4*x_6+36*x_5*x_6-2*x_6^2-31*x_0*x_7+3*x_1*x_7-12*x_2*x_7+19*x_3*x_7+28*x_4*x_7+30*x_5*x_7,x_0*x_3-47*x_5^2-43*x_0*x_6+6*x_1*x_6-40*x_2*x_6+21*x_3*x_6+26*x_4*x_6-5*x_5*x_6-5*x_6^2+4*x_0*x_7-15*x_1*x_7+18*x_2*x_7-31*x_3*x_7+50*x_4*x_7-46*x_5*x_7,x_2^2+4*x_5^2+31*x_0*x_6+41*x_1*x_6+31*x_2*x_6+28*x_3*x_6+42*x_4*x_6-28*x_5*x_6-4*x_6^2-7*x_0*x_7+15*x_1*x_7-9*x_2*x_7+31*x_3*x_7+3*x_4*x_7+7*x_5*x_7,x_1*x_2-46*x_5^2-6*x_0*x_6-50*x_1*x_6+32*x_2*x_6-10*x_3*x_6+42*x_4*x_6+33*x_5*x_6+18*x_6^2-9*x_0*x_7-20*x_1*x_7+45*x_2*x_7-9*x_3*x_7+10*x_4*x_7-8*x_5*x_7,x_0*x_2-9*x_5^2+34*x_0*x_6-45*x_1*x_6+19*x_2*x_6+24*x_3*x_6+23*x_4*x_6-37*x_5*x_6-44*x_6^2+24*x_0*x_7-33*x_2*x_7+41*x_3*x_7-40*x_4*x_7+4*x_5*x_7,x_1^2+x_1*x_4+x_4^2-28*x_5^2-33*x_0*x_6-17*x_1*x_6+11*x_3*x_6+20*x_4*x_6+25*x_5*x_6-21*x_6^2-22*x_0*x_7+24*x_1*x_7-14*x_2*x_7+5*x_3*x_7-39*x_4*x_7-18*x_5*x_7,x_0*x_1-47*x_5^2-5*x_0*x_6-9*x_1*x_6-45*x_2*x_6+48*x_3*x_6+45*x_4*x_6-29*x_5*x_6+3*x_6^2+29*x_0*x_7+40*x_1*x_7+46*x_2*x_7+27*x_3*x_7-36*x_4*x_7-39*x_5*x_7,x_0^2-31*x_5^2+36*x_0*x_6-30*x_1*x_6-10*x_2*x_6+42*x_3*x_6+9*x_4*x_6+34*x_5*x_6-6*x_6^2+48*x_0*x_7-47*x_1*x_7-19*x_2*x_7+25*x_3*x_7+28*x_4*x_7+34*x_5*x_7);",
"phi = rationalMap(C,3,2);",
"time isDominant(phi,MathMode=>true)"},
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

undocumented{(inverseMap,RationalMap,Nothing)} 

document { 
Key => {inverseMap,(inverseMap,RationalMap),(inverseMap,RingMap)}, 
Headline => "inverse of a birational map", 
Usage => "inverseMap phi", 
Inputs => { 
"phi" => RationalMap => {"a birational map"}}, 
Outputs => { 
RationalMap => {"the inverse map of ",TT"phi"}},
PARA{"If the source variety is a projective space and if a further technical condition is satisfied, then the algorithm used is that described in the paper by Russo and Simis - On birational maps and Jacobian matrices - Compos. Math. 126 (3), 335-358, 2001. For the general case, the algorithm used is the same as for ", HREF{"http://www.math.uiuc.edu/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/Parametrization/html/_invert__Birational__Map.html","invertBirationalMap"}, " in the package ", HREF{"https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/Parametrization/html","Parametrization"}, ". Note that in this case, the analogous method ",HREF{"http://www2.macaulay2.com/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/RationalMaps/html/_inverse__Of__Map.html","inverseOfMap"}," in the package ",HREF{"http://www2.macaulay2.com/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/RationalMaps/html/index.html","RationalMaps"}," generally turns out to be faster."},
EXAMPLE { 
"-- A Cremona transformation of P^20 
phi = rationalMap map quadroQuadricCremonaTransformation(20,1)", 
"time psi = inverseMap phi",
"assert(phi * psi == 1)"},
PARA{"The method also accepts as input a ",TO2{RingMap,"ring map"}," representing a rational map ",TEX///$\Phi$///, " between projective varieties. In this case, the ",TO2{RingMap,"ring map"}," defining ",TEX///$\Phi^{-1}$///," is returned."},
EXAMPLE { 
"-- A Cremona transformation of P^26 
phi = map quadroQuadricCremonaTransformation(26,1)",
"time psi = inverseMap phi", 
"assert isInverseMap(phi,psi)"},
Caveat => {"If the map passed is not birational and the option ", TO MathMode, " is set to ", TT "false", ", you might not get any error message."},
SeeAlso => {approximateInverseMap,(inverse,RationalMap)}}

document { 
Key => {approximateInverseMap,(approximateInverseMap,RationalMap,ZZ),(approximateInverseMap,RationalMap),(approximateInverseMap,RingMap),(approximateInverseMap,RingMap,ZZ)}, 
Headline => "random map related to the inverse of a birational map", 
Usage => "approximateInverseMap phi 
approximateInverseMap(phi,d)", 
Inputs => { 
RationalMap => "phi" => {"a birational map"},
ZZ => "d" => {"optional, but it should be the degree of the forms defining the inverse of ",TT"phi"}}, 
Outputs => { 
RationalMap => {"a random rational map which in some sense is related to the inverse of ",TT"phi"," (e.g., they should have the same base locus)"}}, 
PARA{"The algorithm is to try to construct the ideal of the base locus of the inverse by looking for the images via ", TT "phi"," of random linear sections of the source variety. Generally, one can speed up the process by passing through the option ", TO CodimBsInv," a good lower bound for the codimension of this base locus."},
EXAMPLE { 
"P8 = ZZ/97[t_0..t_8];", 
"phi = inverseMap rationalMap(trim(minors(2,genericMatrix(P8,3,3))+random(2,P8)),Dominant=>true)",
"time psi = approximateInverseMap phi",
"assert(phi * psi == 1 and psi * phi == 1)",
"time psi' = approximateInverseMap(phi,CodimBsInv=>5);",
"assert(psi == psi')"}, 
PARA{"A more complicated example is the following (here ", TO inverseMap," takes a lot of time!)."},
EXAMPLE { 
"phi = rationalMap map(P8,ZZ/97[x_0..x_11]/ideal(x_1*x_3-8*x_2*x_3+25*x_3^2-25*x_2*x_4-22*x_3*x_4+x_0*x_5+13*x_2*x_5+41*x_3*x_5-x_0*x_6+12*x_2*x_6+25*x_1*x_7+25*x_3*x_7+23*x_5*x_7-3*x_6*x_7+2*x_0*x_8+11*x_1*x_8-37*x_3*x_8-23*x_4*x_8-33*x_6*x_8+8*x_0*x_9+10*x_1*x_9-25*x_2*x_9-9*x_3*x_9+3*x_4*x_9+24*x_5*x_9-27*x_6*x_9-5*x_0*x_10+28*x_1*x_10+37*x_2*x_10+9*x_4*x_10+27*x_6*x_10-25*x_0*x_11+9*x_2*x_11+27*x_4*x_11-27*x_5*x_11,x_2^2+17*x_2*x_3-14*x_3^2-13*x_2*x_4+34*x_3*x_4+44*x_0*x_5-30*x_2*x_5+27*x_3*x_5+31*x_2*x_6-36*x_3*x_6-x_0*x_7+13*x_1*x_7+8*x_3*x_7+9*x_5*x_7+46*x_6*x_7+41*x_0*x_8-7*x_1*x_8-34*x_3*x_8-9*x_4*x_8-46*x_6*x_8-17*x_0*x_9+32*x_1*x_9-8*x_2*x_9-35*x_3*x_9-46*x_4*x_9+26*x_5*x_9+17*x_6*x_9+15*x_0*x_10+35*x_1*x_10+34*x_2*x_10+20*x_4*x_10+14*x_0*x_11+36*x_1*x_11+35*x_2*x_11-17*x_4*x_11,x_1*x_2-40*x_2*x_3+28*x_3^2-x_0*x_4+5*x_2*x_4-16*x_3*x_4+5*x_0*x_5-36*x_2*x_5+37*x_3*x_5+48*x_2*x_6-5*x_1*x_7-5*x_3*x_7+x_5*x_7+20*x_6*x_7+10*x_0*x_8+34*x_1*x_8+41*x_3*x_8-x_4*x_8+x_6*x_8+40*x_0*x_9-32*x_1*x_9+5*x_2*x_9-11*x_3*x_9-20*x_4*x_9+45*x_5*x_9-14*x_6*x_9-25*x_0*x_10+45*x_1*x_10-41*x_2*x_10-46*x_4*x_10+8*x_6*x_10-28*x_0*x_11+11*x_2*x_11+14*x_4*x_11-8*x_5*x_11),{t_4^2+t_0*t_5+t_1*t_5+35*t_2*t_5+10*t_3*t_5+25*t_4*t_5-5*t_5^2-14*t_0*t_6-14*t_1*t_6-5*t_2*t_6-13*t_4*t_6+37*t_5*t_6+22*t_6^2-31*t_3*t_7+26*t_4*t_7+12*t_5*t_7-45*t_6*t_7-46*t_3*t_8+37*t_4*t_8+28*t_5*t_8+33*t_6*t_8,t_3*t_4+4*t_0*t_5+39*t_1*t_5-40*t_2*t_5+40*t_3*t_5+26*t_4*t_5-20*t_5^2+41*t_0*t_6+36*t_1*t_6-22*t_2*t_6+36*t_4*t_6-30*t_5*t_6-13*t_6^2-25*t_3*t_7+5*t_4*t_7-35*t_5*t_7+10*t_6*t_7+11*t_3*t_8+46*t_4*t_8+29*t_5*t_8+28*t_6*t_8,t_2*t_4-5*t_0*t_5-40*t_1*t_5+12*t_2*t_5+47*t_3*t_5+37*t_4*t_5+25*t_5^2-27*t_0*t_6-22*t_1*t_6+27*t_2*t_6-23*t_4*t_6+5*t_5*t_6-13*t_6^2-39*t_3*t_7-29*t_4*t_7+9*t_5*t_7+39*t_6*t_7+36*t_3*t_8+13*t_4*t_8+26*t_5*t_8+37*t_6*t_8,t_0*t_4-t_0*t_5-8*t_1*t_5-35*t_2*t_5-10*t_3*t_5-33*t_4*t_5+5*t_5^2+15*t_0*t_6+15*t_1*t_6+5*t_2*t_6+15*t_4*t_6-38*t_5*t_6-22*t_6^2+31*t_3*t_7-25*t_4*t_7-19*t_5*t_7+47*t_6*t_7+46*t_3*t_8-36*t_4*t_8-35*t_5*t_8-31*t_6*t_8,t_2*t_3-t_0*t_5-t_1*t_5-35*t_2*t_5-10*t_3*t_5-33*t_4*t_5+5*t_5^2+14*t_0*t_6+14*t_1*t_6+5*t_2*t_6+14*t_4*t_6-31*t_5*t_6-24*t_6^2+32*t_3*t_7-25*t_4*t_7-19*t_5*t_7+47*t_6*t_7+46*t_3*t_8-36*t_4*t_8-35*t_5*t_8-31*t_6*t_8,t_1*t_3-7*t_1*t_5+t_1*t_6+t_4*t_6-7*t_5*t_6+2*t_6^2-t_3*t_7,t_0*t_3-46*t_0*t_5-39*t_1*t_5-43*t_2*t_5-41*t_3*t_5-26*t_4*t_5-28*t_5^2-35*t_0*t_6-36*t_1*t_6+20*t_2*t_6-36*t_4*t_6+9*t_5*t_6+15*t_6^2+26*t_3*t_7-5*t_4*t_7+35*t_5*t_7-10*t_6*t_7-10*t_3*t_8-46*t_4*t_8+47*t_5*t_8-25*t_6*t_8,t_2^2-46*t_1*t_4-33*t_0*t_5-45*t_1*t_5-39*t_2*t_5-39*t_3*t_5-46*t_4*t_5-29*t_5^2-48*t_0*t_6-38*t_1*t_6-30*t_2*t_6+19*t_4*t_6-44*t_5*t_6-47*t_6^2-36*t_0*t_7-46*t_1*t_7+t_2*t_7-44*t_3*t_7+48*t_4*t_7-14*t_5*t_7+4*t_6*t_7-36*t_0*t_8-46*t_1*t_8+47*t_2*t_8-34*t_3*t_8-24*t_4*t_8-12*t_5*t_8-47*t_6*t_8+47*t_7*t_8,t_1*t_2+6*t_1*t_5+5*t_0*t_6-2*t_1*t_6-t_4*t_6-t_5*t_6+5*t_0*t_7+t_1*t_7-2*t_2*t_7-7*t_5*t_7+2*t_6*t_7-2*t_1*t_8+3*t_7*t_8,t_0*t_2+t_1*t_4+5*t_0*t_5+32*t_1*t_5-20*t_2*t_5-47*t_3*t_5-37*t_4*t_5-25*t_5^2+19*t_0*t_6+22*t_1*t_6-25*t_2*t_6+25*t_4*t_6-5*t_5*t_6+13*t_6^2+5*t_0*t_7+t_1*t_7+39*t_3*t_7+28*t_4*t_7-9*t_5*t_7-39*t_6*t_7+4*t_0*t_8+t_1*t_8-36*t_3*t_8-14*t_4*t_8-26*t_5*t_8-37*t_6*t_8,t_0*t_1-39*t_1*t_4+40*t_1*t_5-37*t_0*t_6-39*t_1*t_6+19*t_4*t_6-39*t_5*t_6-38*t_0*t_7+39*t_1*t_7+19*t_2*t_7+18*t_5*t_7-19*t_6*t_7+19*t_1*t_8+20*t_7*t_8,t_0^2+12*t_1*t_4+20*t_0*t_5+27*t_1*t_5-8*t_2*t_5+37*t_3*t_5+28*t_4*t_5+30*t_5^2-46*t_0*t_6+24*t_1*t_6-40*t_2*t_6+25*t_4*t_6+16*t_5*t_6-35*t_6^2+29*t_0*t_7+12*t_1*t_7-35*t_2*t_7-8*t_3*t_7-18*t_4*t_7+42*t_5*t_7-12*t_6*t_7-6*t_0*t_8+12*t_1*t_8-15*t_3*t_8+9*t_4*t_8+20*t_5*t_8-30*t_6*t_8+4*t_7*t_8})", 
"-- without the option 'CodimBsInv=>4', it takes about triple time 
time psi=approximateInverseMap(phi,CodimBsInv=>4)",
"-- but...
phi * psi == 1",
"-- in this case we can remedy enabling the option MathMode
time psi = approximateInverseMap(phi,CodimBsInv=>4,MathMode=>true)",
"assert(phi * psi == 1)"},
PARA{"The method also accepts as input a ",TO2{RingMap,"ring map"}," representing a rational map between projective varieties. In this case, a ",TO2{RingMap,"ring map"}," is returned as well."},
Caveat => {"For the purpose of this method, the option ", TO MathMode, TT"=>true"," is too rigid, especially when the source of the passed map is not a projective space."},
SeeAlso => {inverseMap,(inverse,RationalMap)}}

document { 
Key => {graph,(graph,RationalMap)}, 
Headline => "closure of the graph of a rational map", 
Usage => "graph phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
RationalMap => {"the first projection"},
RationalMap => {"the second projection"}}, 
EXAMPLE { 
"(ZZ/190181)[x_0..x_4]; phi = rationalMap(minors(2,matrix{{x_0..x_3},{x_1..x_4}}),Dominant=>true)",
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
SeeAlso => {(graph,RationalMap),graphIdeal}}

document { 
Key => {SegreClass,(SegreClass,Ideal),(SegreClass,RingMap),(SegreClass,RationalMap)}, 
Headline => "Segre class of a closed subscheme of a projective variety", 
Usage => "SegreClass I", 
Inputs => { 
Ideal => "I" => {"a homogeneous ideal of a graded quotient ring ",TEX///$K[x_0,\ldots,x_n]/J$///," defining a subscheme ",TEX///$X=V(I)$///," of ",TEX///$Y=Proj(K[x_0,\ldots,x_n]/J)\subseteq \mathbb{P}^n=Proj(K[x_0,\ldots,x_n])$///}}, 
Outputs => { 
RingElement => {"the push-forward to the Chow ring of ",TEX///$\mathbb{P}^n$///," of the Segre class ",TEX///$s(X,Y)$///," of ",TEX///$X$///," in ",TEX///$Y$///}}, 
PARA{"This is an example of application of the method ", TO "projectiveDegrees","; see Proposition 4.4 in ",HREF{"http://link.springer.com/book/10.1007%2F978-3-662-02421-8","Intersection theory"},", by W. Fulton, and Subsection 2.3 in ",HREF{"http://www.math.lsa.umich.edu/~idolga/cremonalect.pdf","Lectures on Cremona transformations"},", by I. Dolgachev. See also the corresponding methods in the packages ", HREF{"http://www.math.fsu.edu/~aluffi/CSM/CSM.html", "CSM-A"},", by P. Aluffi, and ", HREF{"http://www.math.uiuc.edu/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/CharacteristicClasses/html/", "CharacteristicClasses"},", by M. Helmer and C. Jost."},
PARA{"In the example below, we take ", TEX///$Y\subset\mathbb{P}^7$///," to be the dual hypersurface of ",TEX///$\mathbb{P}^1\times\mathbb{P}^1\times\mathbb{P}^1\subset{\mathbb{P}^7}^*$///, " and ",TEX///$X\subset Y$///, " its singular locus. We compute the push-forward to the Chow ring of ",TEX///$\mathbb{P}^7$///, " of the Segre class both of ",TEX///$X$///, " in ",TEX///$Y$///, " and of ",TEX///$X$///, " in ",TEX///$\mathbb{P}^7$///, ", using both a probabilistic and a non-probabilistic approach."},
EXAMPLE { 
"P7 = ZZ/100003[x_0..x_7]",
"Y = ideal(x_3^2*x_4^2-2*x_2*x_3*x_4*x_5+x_2^2*x_5^2-2*x_1*x_3*x_4*x_6-2*x_1*x_2*x_5*x_6+4*x_0*x_3*x_5*x_6+x_1^2*x_6^2+4*x_1*x_2*x_4*x_7-2*x_0*x_3*x_4*x_7-2*x_0*x_2*x_5*x_7-2*x_0*x_1*x_6*x_7+x_0^2*x_7^2)",
"X = sub(ideal jacobian Y,P7/Y)",
"time SegreClass X",
"time SegreClass lift(X,P7)",
"time SegreClass(X,MathMode=>true)",
"time SegreClass(lift(X,P7),MathMode=>true)",
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
PARA{"This is an example of application of the method ", TO "projectiveDegrees",", due to results shown in ",HREF{"http://www.sciencedirect.com/science/article/pii/S0747717102000895","Computing characteristic classes of projective schemes"},", by P. Aluffi. See also the corresponding methods in the packages ", HREF{"http://www.math.fsu.edu/~aluffi/CSM/CSM.html", "CSM-A"},", by P. Aluffi, and ", HREF{"http://www.math.uiuc.edu/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/CharacteristicClasses/html/", "CharacteristicClasses"},", by M. Helmer and C. Jost."},
PARA{"In the example below, we compute the push-forward to the Chow ring of ",TEX///$\mathbb{P}^4$///, " of the Chern-Schwartz-MacPherson class of the cone over the twisted cubic curve, using both a probabilistic and a non-probabilistic approach."},
EXAMPLE { 
"GF(5^7)[x_0..x_4]",
"C = minors(2,matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}})",
"time ChernSchwartzMacPherson C",
"time ChernSchwartzMacPherson(C,MathMode=>true)",
"oo == ooo"},
PARA{"In the case when the input ideal ",TT"I"," defines a smooth projective variety ",TEX///$X$///,", the push-forward of ",TEX///$c_{SM}(X)$///," can be computed much more efficiently using ",TO SegreClass, ". Indeed, in this case, ", TEX///$c_{SM}(X)$///," coincides with the (total) Chern class of the tangent bundle of ",TEX///$X$///," and can be obtained as follows (in general the method below gives the push-forward of the so-called Chern-Fulton class)."},
EXAMPLE {
"ChernClass = method(Options=>{MathMode=>false});",
"ChernClass (Ideal) := o -> (I) -> (
   s := SegreClass(I,MathMode=>o.MathMode);
   s*(1+first gens ring s)^(numgens ring I));",
"-- example: Chern class of G(1,4)
G = Grassmannian(1,4,CoefficientRing=>ZZ/190181)",
"time ChernClass G",
"time ChernClass(G,MathMode=>true)"},
SeeAlso => {SegreClass,EulerCharacteristic,(euler,ProjectiveVariety)}}

document { 
Key => {EulerCharacteristic,(EulerCharacteristic,Ideal)}, 
Headline => "topological Euler characteristic of a (smooth) projective variety", 
Usage => "EulerCharacteristic I", 
Inputs => { 
Ideal => "I" => {"a homogeneous ideal defining a smooth projective variety ",TEX///$X\subset\mathbb{P}^n$///}}, 
Outputs => { 
ZZ => {"the topological Euler characteristics of ",TEX///$X$///,"."}}, 
PARA{"This is an application of the method ", TO "SegreClass",". See also the corresponding methods in the packages ", HREF{"http://www.math.fsu.edu/~aluffi/CSM/CSM.html", "CSM-A"},", by P. Aluffi, and ", HREF{"http://www.math.uiuc.edu/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/CharacteristicClasses/html/", "CharacteristicClasses"},", by M. Helmer and C. Jost."},
PARA{"In general, even if the input ideal defines a singular variety ",TEX///$X$///,", the returned value equals the degree of the component of dimension 0 of the Chern-Fulton class of ",TEX///$X$///,". The Euler characteristic of a singular variety can be computed via the method ",TO ChernSchwartzMacPherson,"."},
PARA{"In the example below, we compute the Euler characteristic of ",TEX///$\mathbb{G}(1,4)\subset\mathbb{P}^{9}$///,", using both a probabilistic and a non-probabilistic approach."},
EXAMPLE { 
"I = Grassmannian(1,4,CoefficientRing=>ZZ/190181);",
"time EulerCharacteristic I",
"time EulerCharacteristic(I,MathMode=>true)"},
Caveat => {"No test is made to see if the projective variety is smooth."},
SeeAlso => {(euler,ProjectiveVariety),ChernSchwartzMacPherson,SegreClass}}

document { 
Key => {RationalMap}, 
Headline => "the class of all rational maps between absolutely irreducible projective varieties over a field", 
PARA{"An object of the class ",EM "RationalMap", " can be basically replaced by a homogeneous ring map of quotients of polynomial rings by homogeneous ideals. One main advantage to using this class is that things computed using non-probabilistic algorithms are stored internally (or partially stored)."},
PARA{"The constructor for the class is ",TO "rationalMap",", which works quite similar to ",TO "toMap",". 
See in particular the methods: ",TO (rationalMap,RingMap),", ",TO (rationalMap,Ideal,ZZ,ZZ),", ",TO (rationalMap,Tally),", and ",TO (rationalMap,PolynomialRing,List),"."},
PARA{"In the package ",HREF{"https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2/share/doc/Macaulay2/MultiprojectiveVarieties/html/index.html","MultiprojectiveVarieties"},", this class has been extended to provide support to rational maps between multi-projective varieties, see ",HREF{"https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2/share/doc/Macaulay2/MultiprojectiveVarieties/html/___Multirational__Map.html", TT "MultirationalMap"},"."}}

document { 
Key => {(symbol ^**,RationalMap,Ideal),(symbol ^*,RationalMap)}, 
Headline => "inverse image via a rational map", 
Usage => "phi^** I", 
Inputs => { 
RationalMap => "phi",
Ideal => "I" => {"a homogeneus ideal in the coordinate ring of the target of ",TT "phi"}}, 
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
SeeAlso => {(symbol _*,RationalMap),(symbol **,RationalMap,Ring),(target,RationalMap)}}

document { 
Key => {(symbol _*,RationalMap),(symbol SPACE,RationalMap,Ideal)}, 
Headline => "direct image via a rational map", 
Usage => "phi_* I", 
Inputs => { 
RationalMap => "phi" => {TT"I", " a homogeneus ideal in the coordinate ring of the source of ",TT "phi"}}, 
Outputs => { 
Ideal => {"the ideal of the closure of the direct image of ", TT"V(I)", " via ",TT"phi"}},
PARA{"In most cases this is equivalent to ",TT"phi I", ", which is faster but may not take into account other representations of the map."},
SeeAlso => {(symbol ^**,RationalMap,Ideal),(source,RationalMap)}}

document { 
Key => {(symbol ^,RationalMap,ZZ)}, 
Headline => "power", 
Usage => "phi^n", 
Inputs => { 
RationalMap => "phi",
ZZ => "n"}, 
Outputs => { 
RationalMap => {TT"phi^n"}},
PARA{"If the map is birational, then negative values may be used and the inverse will be computed using ",TO2{(inverse,RationalMap),"inverse"},TT"(phi)","."}}

document { 
Key => {(inverse,RationalMap),(inverse,RationalMap,Option)}, 
Headline => "inverse of a birational map", 
Usage => "inverse phi
inverse(phi,MathMode=>b)", 
Inputs => { 
RationalMap => "phi" => {"which has to be birational, and ",TT"b"," is a ",TO2{Boolean,"boolean value"},", that is, ",TT"true"," or ",TT"false"," (the default value is ",TT"false",")"}}, 
Outputs => { 
RationalMap => {"the inverse map of ",TT"phi"}},
PARA{TT"inverse(phi,MathMode=>true)"," is the same as ",TO inverseMap,TT"(phi,MathMode=>true,Verbose=>false)",", while ",
TT"inverse(phi,MathMode=>false)"," applies a middle ground approach between ",TO inverseMap,TT"(phi,MathMode=>true)"," and ",TO inverseMap,TT"(phi,MathMode=>false)",
". The procedure for the latter is as follows: It first computes the inverse map of ",TT "phi"," using ",TT "psi = ",TO "inverseMap",TT" phi",". Then it is checked that ",TO2{(symbol SPACE,RationalMap,Ideal),"psi phi p"},TT" == p"," and ",TO2{(symbol SPACE,RationalMap,Ideal),"phi psi q"},TT" == q",", where ",TT"p,q"," are, respectively, a ",TO2{point,"random point"}," on the ",TO2{(source,RationalMap),"source"}," and the ",TO2{(target,RationalMap),"target"}," of ",TT"phi",". Finally, if the tests pass, the command ",TO forceInverseMap,TT"(phi,psi)"," is invoked and ",TT"psi"," is returned."},
EXAMPLE {
"R = QQ[x_0..x_4]; phi = rationalMap minors(4,random(R^{4:1},R^5)) -- special Cremona transformation of P^4 of type (4,4)",
"time inverse phi"
},
SeeAlso => {inverseMap,(symbol ^,RationalMap,ZZ),(isInverseMap,RationalMap,RationalMap)}}

document { 
Key => {(ideal,RationalMap)}, 
Headline => "base locus of a rational map", 
Usage => "ideal phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
Ideal => {"the ideal of the base locus of ",TT"phi"}},
PARA {"This is generally difficult, but in some cases it is equivalent to ",TT"ideal matrix phi", ", which does not perform any computation."},
EXAMPLE {
"y = gens(QQ[x_0..x_5]/(x_2^2-x_2*x_3+x_1*x_4-x_0*x_5));",
"phi = rationalMap {y_4^2-y_3*y_5,-y_2*y_4+y_3*y_4-y_1*y_5, -y_2*y_3+y_3^2-y_1*y_4, -y_1*y_2+y_1*y_3-y_0*y_4, y_1^2-y_0*y_3}",
"time ideal phi",
"assert(ideal phi == ideal matrix phi)",
"phi' = last graph phi",
"time ideal phi'",
"assert(ideal phi' != ideal matrix phi')"},
SeeAlso => {isMorphism,(matrix,RationalMap)}}

document { 
Key => {exceptionalLocus,(exceptionalLocus,RationalMap)}, 
Headline => "exceptional locus of a birational map", 
Usage => "exceptionalLocus phi", 
Inputs => { 
RationalMap => "phi" => {"a birational map ",TEX///$X\dashrightarrow Y$///}}, 
Outputs => { 
Ideal => {"the ideal defining the closure in ",TT"X"," of the locus where ",TT "phi"," is not a local isomorphism"}},
PARA{"This method simply calculates the ",TO2{(symbol ^**,RationalMap,Ideal),"inverse image"}," of the ",TO2{(ideal,RationalMap),"base locus"}," of the inverse map, which in turn is determined through the method ",TO2{(inverse,RationalMap),"inverse"},"."},
PARA{"Below, we compute the exceptional locus of the map defined by the linear system of quadrics through the quintic rational normal curve in ",TEX///$\mathbb{P}^5$///,"."},
EXAMPLE { 
"P5 := ZZ/100003[x_0..x_5];",
"phi = rationalMap(minors(2,matrix{{x_0,x_1,x_2,x_3,x_4},{x_1,x_2,x_3,x_4,x_5}}),Dominant=>2);",
"E = exceptionalLocus phi;",
"assert(E == phi^* ideal phi^-1)",
"assert(E == minors(3,matrix{{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4},{x_2,x_3,x_4,x_5}}))"
},
SeeAlso => {(ideal,RationalMap),(inverseMap,RationalMap),(symbol ^**,RationalMap,Ideal),(isIsomorphism,RationalMap),forceInverseMap}}

document { 
Key => {(target,RationalMap)}, 
Headline => "coordinate ring of the target for a rational map", 
Usage => "target phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
Ring => {"the coordinate ring of the target of ",TT"phi"}},
PARA{"This is equivalent to ",TT "source map phi","."},
SeeAlso => {(source,RationalMap)}}

document { 
Key => {(source,RationalMap)}, 
Headline => "coordinate ring of the source for a rational map", 
Usage => "source phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
Ring => {"the coordinate ring of the source of ",TT"phi"}},
PARA{"This is equivalent to ",TT "target map phi","."},
SeeAlso => {(target,RationalMap)}}

document { 
Key => {(coefficientRing,RationalMap)}, 
Headline => "coefficient ring of a rational map", 
Usage => "coefficientRing phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
Ring => {"the coefficient ring of ",TT"phi"}},
SeeAlso => {(symbol **,RationalMap,Ring)}}

document { 
Key => {(degree,RationalMap)}, 
Headline => "degree of a rational map", 
Usage => "degree phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
ZZ => {"the degree of ",TT"phi",". So this value is 1 if and only if the map is birational onto its image."}},
PARA{"This is a shortcut for ",TT "degreeMap(phi,MathMode=>true,Verbose=>false)",", see ",TO (degreeMap,RationalMap),"."}}

document { 
Key => {(symbol !,RationalMap)}, 
Headline => "calculates every possible thing", 
Usage => "phi!", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
RationalMap => {"the same rational map ",TT"phi"}},
PARA{"This method (mainly used for tests) applies almost all the deterministic methods that are available."},
EXAMPLE {
"QQ[x_0..x_5]; phi = rationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3};", 
"describe phi",
"time phi! ;",
"describe phi",
"QQ[x_0..x_4]; phi = rationalMap {-x_1^2+x_0*x_2,-x_1*x_2+x_0*x_3,-x_2^2+x_1*x_3,-x_1*x_3+x_0*x_4,-x_2*x_3+x_1*x_4,-x_3^2+x_2*x_4};",
"describe phi",
"time phi! ;",
"describe phi"},
PARA{"The command ",TT"phi(*)"," does more or less the same thing but it uses probabilistic methods and treats them as deterministic (the user should never use this)."},
EXAMPLE {
"phi = rationalMap rationalMap map specialQuadraticTransformation(8,ZZ/33331);",
"describe phi",
"time phi(*) ;",
"describe phi"},
SeeAlso => {(describe,RationalMap)}}

document { 
Key => {(degrees,RationalMap),(multidegree,RationalMap)}, 
Headline => "projective degrees of a rational map", 
Usage => "degrees phi 
multidegree phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
List => {"the list of projective degrees of ",TT"phi"}},
PARA{"This is a shortcut for ",TT "projectiveDegrees(phi,MathMode=>true,Verbose=>false)",", see ",TO (projectiveDegrees,RationalMap),"."}}

document { 
Key => {(coefficients,RationalMap)}, 
Headline => "coefficient matrix of a rational map", 
Usage => "coefficients phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
Matrix => {"the coefficient matrix of the polynomials defining ",TT"phi"}},
EXAMPLE {
"K = QQ; ringP9 = K[x_0..x_9];",
"M = random(K^10,K^10)",
"phi = rationalMap ((vars ringP9) * (transpose M));",
"M' = coefficients phi",
"M == M'"}}

document { 
Key => {(flatten,RationalMap)}, 
Headline => "write source and target as nondegenerate varieties", 
Usage => "flatten phi", 
Inputs => { 
RationalMap => "phi"}, 
Outputs => { 
RationalMap => {"a rational map isomorphic to the original map, flattened in the sense that the ideals of source and target contain no linear forms"}},
EXAMPLE {
"P5 = QQ[t_0..t_5]; phi = rationalMap(P5/(35*t_1+45*t_2+21*t_3+525*t_4+1365*t_5,1575*t_0*t_2-3250*t_2^2+735*t_0*t_3-1890*t_2*t_3-1666*t_3^2+17150*t_0*t_4-47250*t_2*t_4-22050*t_3*t_4-276850*t_4^2+46550*t_0*t_5-122850*t_2*t_5-57330*t_3*t_5-1433250*t_4*t_5-1864450*t_5^2),P5/(315*t_0+280*t_1+45*t_2+21*t_3+210*t_4+1050*t_5),{-45*t_2-21*t_3-490*t_4-1330*t_5, 45*t_2+21*t_3+525*t_4+1365*t_5, 35*t_2, 35*t_3, 35*t_4, 35*t_5});",
"describe phi",
"psi = flatten phi;",
"describe psi"}}

document { 
Key => {(sub,RationalMap,PolynomialRing,PolynomialRing)},
Headline => "substitute the ambient projective spaces of source and target", 
Usage => "sub(phi,R,S)", 
Inputs => { 
RationalMap => "phi" => {TEX///$\phi:X\subseteq\mathbb{P}^n\dashrightarrow Y\subseteq\mathbb{P}^m$///},
PolynomialRing => "R" => {"the coordinate ring of ",TEX///$\mathbb{P}^n$///},
PolynomialRing => "S" => {"the coordinate ring of ",TEX///$\mathbb{P}^m$///}}, 
Outputs => { 
RationalMap => {"a rational map isomorphic to the original map such that the ",
TO ambient," of the ",TO2{(source,RationalMap),"source"}," is ",TEX///$R$///," and the ",
TO ambient," of the ",TO2{(target,RationalMap),"target"}," is ",TEX///$S$///}}, 
EXAMPLE {
"ZZ/3331[vars(0..5)];",
"phi = rationalMap {e^2-d*f, c*e-b*f, c*d-b*e, c^2-a*f, b*c-a*e, b^2-a*d}",
"R = ZZ/3331[x_0..x_5], S = ZZ/3331[y_0..y_5];",
"sub(phi,R,S)"},
SeeAlso => {(symbol **,RationalMap,Ring)}}

document { 
Key => {(super,RationalMap),(rationalMap,RationalMap)}, 
Headline => "get the rational map whose target is a projective space", 
Usage => "super phi
rationalMap(phi,Dominant=>null)
rationalMap phi", 
Inputs => { 
RationalMap => "phi" => {"whose target is a subvariety ",TEX///$Y\subset\mathbb{P}^n$///}}, 
Outputs => { 
RationalMap => {"the composition of ",TT"phi"," with the inclusion of ",TEX///$Y$///," into ",TEX///$\mathbb{P}^n$///}},
PARA {"So that, for instance, if ",TT"phi"," is a dominant map, then the code ",TT"rationalMap(super phi,Dominant=>true)"," yields a map isomorphic to ",TT"phi","."},
EXAMPLE {
"phi = specialQuadraticTransformation 7;",
"phi' = super phi;",
"describe phi",
"describe phi'",
"describe rationalMap(phi',Dominant=>true)"},
SeeAlso => {(target,RationalMap),ambient,super}}

document { 
Key => {(projectiveDegrees,RationalMap)}, 
Headline => "projective degrees of a rational map", 
Usage => "projectiveDegrees Phi", 
Inputs => { 
RationalMap => "Phi"}, 
Outputs => { 
List => {"the list of projective degrees of ",TT"Phi"}},
PARA{"This computation is done through the corresponding method for ring maps. See ",TO (projectiveDegrees,RingMap)," for more details and examples."},
SeeAlso => {(projectiveDegrees,RingMap),(degrees,RationalMap),(degree,RationalMap)}}

document { 
Key => {(degreeMap,RationalMap)}, 
Headline => "degree of a rational map", 
Usage => "degreeMap Phi", 
Inputs => { 
RationalMap => "Phi"}, 
Outputs => { 
ZZ => {"the degree of ",TT"Phi",". So this value is 1 if and only if the map is birational onto its image."}},
PARA{"This computation is done through the corresponding method for ring maps. See ",TO (degreeMap,RingMap)," for more details and examples."},
SeeAlso => {(degreeMap,RingMap),projectiveDegrees,(degree,RationalMap)}}

document { 
Key => {(symbol *,RationalMap,RationalMap),(compose,RationalMap,RationalMap),(compose,RingMap,RingMap)}, 
Headline => "composition of rational maps", 
Usage => "phi * psi 
compose(phi,psi)", 
Inputs => { 
RationalMap => "phi" => { TEX///$X \dashrightarrow Y$///},
RationalMap => "psi" => { TEX///$Y \dashrightarrow Z$///}}, 
Outputs => { 
RationalMap => { TEX///$X \dashrightarrow Z$///, ", the composition of ",TT"phi"," and ",TT"psi"}}, 
EXAMPLE { 
"R = QQ[x_0..x_3]; S = QQ[y_0..y_4]; T = QQ[z_0..z_4];", 
"phi = rationalMap(R,S,{x_0*x_2,x_0*x_3,x_1*x_2,x_1*x_3,x_2*x_3})",
"psi = rationalMap(S,T,{y_0*y_3,-y_2*y_3,y_1*y_2,y_2*y_4,-y_3*y_4})",
"phi * psi",
"(map phi) * (map psi)"},
SeeAlso => {(symbol *,RingMap,RingMap)}}

document { 
Key => {(symbol ==,RationalMap,RationalMap),(symbol ==,ZZ,RationalMap),(symbol ==,RationalMap,ZZ)}, 
Headline => "equality of rational maps", 
Usage => "phi == psi", 
Inputs => { 
RationalMap => "phi",
RationalMap => "psi"}, 
Outputs => { 
Boolean => {" whether ",TT"phi"," and ",TT"psi", " are the same rational map"}}, 
EXAMPLE {
"QQ[x_0..x_5]",
"phi = rationalMap {x_0*x_4^2-x_0*x_3*x_5,x_0*x_2*x_4-x_0*x_1*x_5,x_0*x_2*x_3-x_0*x_1*x_4,x_0*x_2^2-x_0^2*x_5,x_0*x_1*x_2-x_0^2*x_4,x_0*x_1^2-x_0^2*x_3}",
"psi = rationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3}", 
"phi == psi"}}

document { 
Key => {(isInverseMap,RationalMap,RationalMap)},
Headline => "checks whether two rational maps are one the inverse of the other", 
Usage => "isInverseMap(phi,psi)", 
Inputs => { 
"phi" => RationalMap, 
"psi" => RationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi * psi == 1 and psi * phi == 1"}},
Consequences => {"If the answer is affirmative, then the system will be informed and so commands like 'inverse phi' will execute fast."},
SeeAlso => {(symbol ==,RationalMap,RationalMap),(symbol *,RationalMap,RationalMap),(isInverseMap,RingMap,RingMap),(inverse,RationalMap),forceInverseMap}} 

undocumented {(image,ZZ,RationalMap)}

document { 
Key => {(image,RationalMap,ZZ),(image,RationalMap)}, 
Headline => "closure of the image of a rational map", 
Usage => "image Phi 
image(Phi,d)", 
Inputs => { 
"Phi" => RationalMap,
"d" => ZZ}, 
Outputs => { 
Ideal => {"the ideal defining the closure of the image of ",TT"Phi",", or its degree ",TT"d"," homogeneous component if ",TT"d", " is passed"}},
PARA{"This computation is done through the kernel of a ring map representing the rational map. See ",TO (kernel,RingMap), " and ",TO (kernel,RingMap,ZZ)," for more details."},
SeeAlso => {(kernel,RingMap,ZZ),(kernel,RingMap),(image,RationalMap,String)}}

document { 
Key => {(image,RationalMap,String)}, 
Headline => "closure of the image of a rational map using the F4 algorithm (experimental)", 
Usage => "image(Phi,\"F4\")
image(Phi,\"MGB\")",
Inputs => { "Phi" => RationalMap,{"\"F4\" or \"MGB\""}}, 
Outputs => {{"the ",TO2{"Ideal","ideal"}," defining the closure of the image of ",TT"Phi","; the calculation passes through ",TO groebnerBasis,"(...,Strategy=>\"F4\") or ",TO groebnerBasis,"(...,Strategy=>\"MGB\")."}}, 
SeeAlso => {(image,RationalMap),(image,RationalMap,ZZ),groebnerBasis}}

document { 
Key => {parametrize,(parametrize,Ideal),(parametrize,QuotientRing),(parametrize,PolynomialRing)}, 
Headline => "parametrization of linear varieties and hyperquadrics", 
Usage => "parametrize I", 
Inputs => { 
"I" => Ideal => {"the ideal of a linear variety or of a hyperquadric"}}, 
Outputs => { 
RationalMap => {"a birational map ",TT"phi"," such that ",TT"I == image phi"}},
PARA{"This function has been improved and extended in the package ",HREF{"https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2/share/doc/Macaulay2/MultiprojectiveVarieties/html/index.html","MultiprojectiveVarieties"},", see ",HREF{"https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2/share/doc/Macaulay2/MultiprojectiveVarieties/html/_parametrize_lp__Multiprojective__Variety_rp.html", TT "parametrize(MultiprojectiveVariety)"},"."},
EXAMPLE {
"P9 := ZZ/10000019[x_0..x_9]",
 "L = trim ideal(random(1,P9),random(1,P9),random(1,P9),random(1,P9))",
"time parametrize L",
"Q = trim ideal(random(2,P9),random(1,P9),random(1,P9))",
"time parametrize Q"
}}

document { 
Key => {(map,RationalMap),(map,ZZ,RationalMap)}, 
Headline => "get the ring map defining a rational map", 
Usage => "map Phi", 
Inputs => { 
RationalMap => "Phi"}, 
Outputs => { 
RingMap => {"the ring map from which the rational map ",TT"Phi"," was defined"}},
EXAMPLE {
"QQ[t_0..t_3]",
"Phi = rationalMap {t_1^2+t_2^2+t_3^2,t_0*t_1,t_0*t_2,t_0*t_3}",
"map Phi"},
PARA {"The command ",TT "map Phi"," is equivalent to ",TT "map(0,Phi)",". More generally, the command ",TT "map(i,Phi)"," returns the i-th representative of the map ",TT"Phi","."},
SeeAlso => {(matrix,RationalMap)}}

document { 
Key => {(matrix,RationalMap),(matrix,ZZ,RationalMap)}, 
Headline => "the matrix associated to a rational map", 
Usage => "matrix Phi", 
Inputs => { 
RationalMap => "Phi"}, 
Outputs => { 
Matrix => {"the matrix associated to the ring map defining the rational map ",TT"Phi"}},
PARA{"This is equivalent to ",TT "matrix map Phi",". Moreover, the command ",TT "matrix Phi"," is equivalent to ",TT "matrix(0,Phi)",", and more generally the command ",TT "matrix(i,Phi)"," returns the matrix of the i-th representative of ",TT"Phi","."},
SeeAlso => {(map,RationalMap),(matrix,RingMap)}}

document { 
Key => {(entries,RationalMap)}, 
Headline => "the entries of the matrix associated to a rational map", 
Usage => "entries Phi", 
Inputs => { 
RationalMap => "Phi"}, 
Outputs => { 
List => {"the entries of the matrix associated to the ring map defining the rational map ",TT"Phi"}},
PARA{"This is equivalent to ",TT "flatten entries matrix Phi","."},
SeeAlso => {(matrix,RationalMap)}}

document { 
Key => {(symbol |,RationalMap,Ideal),(symbol |,RationalMap,RingElement),(symbol |,RationalMap,Ring)}, 
Headline => "restriction of a rational map", 
Usage => "Phi | I", 
Inputs => { 
RationalMap => "Phi" => { TEX///$\phi:X \dashrightarrow Y$///},
Ideal => "I" => {"a homogeneous ideal of a subvariety ",TEX///$Z\subset X$///}}, 
Outputs => { 
RationalMap => {"the restriction of ",TEX///$\phi$///," to ",TEX///$Z$///,", ",TEX///$\phi|_{Z}: Z \dashrightarrow Y$///}}, 
EXAMPLE {
"P5 = ZZ/190181[x_0..x_5]",
"Phi = rationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3}",
"I = ideal(random(2,P5),random(3,P5));",
"Phi' = Phi|I",
"describe Phi",
"describe Phi'"},
SeeAlso => {(symbol ||,RationalMap,Ideal)}}

document { 
Key => {(symbol ||,RationalMap,Ideal),(symbol ||,RationalMap,RingElement),(symbol ||,RationalMap,Ring)}, 
Headline => "restriction of a rational map", 
Usage => "Phi || J", 
Inputs => { 
RationalMap => "Phi" => { TEX///$\phi:X \dashrightarrow Y$///},
Ideal => "J" => {"a homogeneous ideal of a subvariety ",TEX///$Z\subset Y$///}}, 
Outputs => { 
RationalMap => {"the restriction of ",TEX///$\phi$///," to ",TEX///${\phi}^{(-1)} Z$///,", ",TEX///${{\phi}|}_{{\phi}^{(-1)} Z}: {\phi}^{(-1)} Z \dashrightarrow Z$///}}, 
EXAMPLE {
"P5 = ZZ/190181[x_0..x_5]",
"Phi = rationalMap {x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3}",
"J = ideal random(1,P5);",
"Phi' = Phi||J",
"describe Phi",
"describe Phi'"},
SeeAlso => {(symbol |,RationalMap,Ideal)}}

document { 
Key => {rationalMap,(rationalMap,RingMap),(rationalMap,Matrix),(rationalMap,List),(rationalMap,Ring),(rationalMap,Ring,Ring),(rationalMap,Ring,Ring,Matrix),(rationalMap,Ring,Ring,List)}, 
Headline => "makes a rational map", 
Usage => "rationalMap phi 
rationalMap F", 
Inputs => { 
RingMap => { "or ", ofClass {Matrix,List},", etc."}}, 
Outputs => { RationalMap => {"the rational map represented by ",TT"phi"," or by the ring map defined by ",TT"F"}}, 
PARA{"This is the basic construction for ",ofClass RationalMap,". The method is quite similar to ",TO toMap,", except that it returns a ", TO RationalMap," instead of a ",TO RingMap,"."},
EXAMPLE { 
"R := QQ[t_0..t_8]", 
"F = matrix{{t_0*t_3*t_5, t_1*t_3*t_6, t_2*t_4*t_7, t_2*t_4*t_8}}",
"phi = toMap F",
"rationalMap phi",
"rationalMap F"}, 
PARA{"Multigraded rings are also permitted but in this case the method returns an object of the class ",TT"MultihomogeneousRationalMap",", which can be considered as an extension of the class ",TT"RationalMap","."},
EXAMPLE { 
"R' := newRing(R,Degrees=>{3:{1,0,0},2:{0,1,0},4:{0,0,1}})",
"F' = sub(F,R')",
"phi' = toMap F'",
"rationalMap phi'",
"rationalMap F'"}, 
SeeAlso => {toMap,(rationalMap,Ideal),(rationalMap,Tally)}}

document { 
Key => {(rationalMap,Ideal,ZZ,ZZ),(rationalMap,Ideal,ZZ),(rationalMap,Ideal,List),(rationalMap,Ideal)}, 
Headline => "makes a rational map from an ideal", 
Usage => "rationalMap(I,d,e)
rationalMap(I,d)
rationalMap I", 
Inputs => { 
Ideal => "I" => {"a homogeneous ideal in the coordinate ring of a projective variety ",TEX///$X\subseteq\mathbb{P}^n$///},
ZZ => "d" => {"a positive integer (or a ",TO2{List,"list"}," of integers in the case ",TEX///$X$///," is embedded in a product of projective spaces)"},
ZZ => "e" => {"a positive integer (if omitted, it is taken to be 1)"}}, 
Outputs => { RationalMap => {"the rational map ",TEX///$X\dashrightarrow \mathbb{P}^m$///," defined by the linear system of hypersurfaces of degree ",TEX///$d$///, " having points of multiplicity ",TEX///$e$///," along the projective subscheme of ",TEX///$X$///," defined by ",TEX///$I$///,"."}}, 
PARA {"In most cases, the command ",TT"rationalMap(I,d,e)"," yields the same output as ",TT"rationalMap(saturate(I^e),d)",", but the former is implemented using pure linear algebra."},
PARA{"The command ",TT"rationalMap I"," is basically equivalent to ", TT"rationalMap(I,max degrees I)","."},
PARA{"In the following example, we calculate the rational map defined by the linear system of cubic hypersurfaces in ",TEX///$\mathbb{P}^6$///," having double points along a Veronese surface ",TEX///$V\subset\mathbb{P}^5\subset\mathbb{P}^6$///,"."},
EXAMPLE {
"ZZ/33331[x_0..x_6]; V = ideal(x_4^2-x_3*x_5,x_2*x_4-x_1*x_5,x_2*x_3-x_1*x_4,x_2^2-x_0*x_5,x_1*x_2-x_0*x_4,x_1^2-x_0*x_3,x_6);", 
"time phi = rationalMap(V,3,2)",
"describe phi!"},
SeeAlso => {toMap,rationalMap,(rationalMap,Tally)}}

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
SeeAlso => {(rationalMap,Matrix),(rationalMap,Ideal,ZZ,ZZ)}} 

document { 
Key => {(symbol **,RationalMap,Ring)},
Headline => "change the coefficient ring of a rational map", 
Usage => "phi ** K", 
Inputs => { 
RationalMap => "phi" => {"defined over a coefficient ring ",TT"F"},
Ring => "K" => {"the new coefficient ring (which must be a field)"}}, 
Outputs => { 
RationalMap => {"a rational map defined over ",TT"K",", obtained by coercing the coefficients of the forms defining ",TT"phi", " into ",TT"K"}}, 
PARA {"It is necessary that all forms in the old coefficient ring ",TT"F"," can be automatically coerced into the new coefficient ring ",TT"K","."},
EXAMPLE {
"QQ[vars(0..5)]",
"phi = rationalMap {e^2-d*f, c*e-b*f, c*d-b*e, c^2-a*f, b*c-a*e, b^2-a*d}",
"K = ZZ/65521;",
"phi ** K",
"phi ** frac(K[t])"
},
SeeAlso => {(coefficientRing,RationalMap)}}

document { 
Key => {(rationalMap,PolynomialRing,List)},
Headline => "rational map defined by the linear system of hypersurfaces passing through random points with multiplicity", 
Usage => "rationalMap(R,{a,i,j,k,...})",
Inputs => { 
PolynomialRing => "R",
List => {"a list ",TEX///$\{a,i,j,k,\ldots\}$///," of nonnegative integers"}}, 
Outputs => {RationalMap => {"the rational map defined by the linear system of hypersurfaces of degree ",TEX///$a$///," in ",TEX///$Proj(R)$///," having ",TEX///$i$///," random base points of multiplicity 1, ",TEX///$j$///," random base points of multiplicity 2, ",TEX///$k$///," random base points of multiplicity 3, and so on until the last integer in the given list."}},
PARA{"In the example below, we take the rational map defined by the linear system of septic plane curves with 3 random simple base points and 9 random double points."}, 
EXAMPLE { 
"ringP2 = ZZ/65521[vars(0..2)];", 
"phi = rationalMap(ringP2,{7,3,9})",
"describe phi!"},
SeeAlso => {(rationalMap,Ideal,ZZ,ZZ),point}} 

undocumented{(net,RationalMap),(expression,RationalMap),(toString,RationalMap),(toMap,RationalMap),(lift,RationalMap),(lift,RingMap),(symbol ~,RationalMap),(symbol (*),RationalMap)}

document { 
Key => {specialCremonaTransformation,(specialCremonaTransformation,Ring,ZZ),(specialCremonaTransformation,ZZ,Ring),(specialCremonaTransformation,ZZ)}, 
Headline => "special Cremona transformations whose base locus has dimension at most three", 
Usage => "specialCremonaTransformation i 
specialCremonaTransformation(i,K)", 
Inputs => { 
"i" => ZZ => {"an integer between 1 and 12"},
"K" => Ring => {"the ground field (optional, the default value is ",TO QQ,")"}}, 
Outputs => { 
RationalMap => {"an example of special Cremona transformation over ",TT"K",", according to the classification given in Table 1 of ",HREF{"https://www.degruyter.com/view/journals/advg/19/2/article-p191.xml","Special cubic Cremona transformations of P6 and P7"},"."}},
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
RationalMap => {"an example of quadro-quadric Cremona transformation over ",TT"K",", according to the classifications given in the paper ",HREF{"https://aif.centre-mersenne.org/item/AIF_2014__64_1_71_0/","Quadro-quadric Cremona transformations in low dimensions via the JC-correspondence"}, ", by Pirio and Russo."}},
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
RationalMap => {"an example of special quadratic birational transformation over ",TT"K",", according to the classification given in Table 1 of ",HREF{"https://www.sciencedirect.com/science/article/pii/S0747717115001029?via%3Dihub","Examples of special quadratic birational transformations into complete intersections of quadrics"},"."}},
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
RationalMap => {"an example of special cubic birational transformation over ",TT"K",", according to the classification given in Table 2 of ",HREF{"https://link.springer.com/article/10.1007/s13348-019-00251-8","Special cubic birational transformations of projective spaces"},"."}},
PARA{"The field ",TT"K"," is required to be large enough."},
EXAMPLE {
"time specialCubicTransformation 9",
"time describe oo"},
SeeAlso => {"specialCremonaTransformation","specialQuadraticTransformation","quadroQuadricCremonaTransformation"}}

document { 
Key => {[inverseMap,Verbose], [projectiveDegrees,Verbose],[degreeMap,Verbose],[approximateInverseMap,Verbose],[isDominant,Verbose],[isBirational,Verbose],[SegreClass,Verbose],[ChernSchwartzMacPherson,Verbose],[EulerCharacteristic,Verbose]}, 
PARA{"This option accepts a ", TO Boolean, " value. Set this to ",TT "false"," if you don't want to get the certification message from ",TO MathMode,"."},
EXAMPLE {
"f = toMap vars(QQ[x_0..x_2]);",
"isBirational(f,MathMode=>true)",
"isBirational(f,MathMode=>true,Verbose=>false)"}} 

document { 
Key => {BlowUpStrategy,[graph,BlowUpStrategy],[inverseMap,BlowUpStrategy], [projectiveDegrees,BlowUpStrategy],[degreeMap,BlowUpStrategy],[isBirational,BlowUpStrategy],[SegreClass,BlowUpStrategy],[ChernSchwartzMacPherson,BlowUpStrategy],[EulerCharacteristic,BlowUpStrategy]}, 
PARA{"This is an optional argument for ", TO graph,", and for the methods that eventually call it. Currently, the possible values are \"Eliminate\" and \"Saturate\", which indicate two different ways of computing the (closure of the) graph of a rational map. The default choice is \"Eliminate\" and this is generally preferable."}} 


document { 
Key => {(describe,RationalMap),(symbol ?,Ideal)}, 
Headline => "describe a rational map", 
Usage => "describe phi", 
Inputs => { 
"phi" => RationalMap}, 
Outputs => { 
{"a description of ",TT"phi",", giving some indication of what has already been calculated."}},
EXAMPLE { 
"ZZ/33331[t_0..t_4];",
"phi = rationalMap minors(2,matrix{{t_0..t_3},{t_1..t_4}});",
"describe phi",
"I = image phi;",
"describe phi",
"? I",
"phi!;",
"describe phi"},
SeeAlso => {(symbol !,RationalMap)}}

document { 
Key => {forceInverseMap,(forceInverseMap,RationalMap,RationalMap)}, 
Headline => "declare that two rational maps are one the inverse of the other", 
Usage => "forceInverseMap(Phi,Psi)", 
Inputs => { 
"Phi" => RationalMap,
"Psi" => RationalMap},
Outputs => {Nothing => {TO null}}, 
PARA{"This method allows to inform the system that two maps are one the inverse of the other without performing any computation. This is useful in particular if you calculate the inverse map using your own method."},
Caveat => {"If the declaration is false, nonsensical answers may result."},
SeeAlso => {(isInverseMap,RationalMap,RationalMap),(inverse,RationalMap)}}

document { 
Key => {forceImage,(forceImage,RationalMap,Ideal)}, 
Headline => "declare which is the image of a rational map", 
Usage => "forceImage(Phi,I)", 
Inputs => { 
"Phi" => RationalMap,
"I" => Ideal},
Outputs => {Nothing => {TO null}}, 
PARA{"This method allows to inform the system about the image of a given rational map without performing any computation. In particular, this can be used to declare that a rational map is dominant."},
EXAMPLE { 
"P6 = QQ[t_0..t_6]; X = minors(3,matrix{{t_0..t_4},{t_1..t_5},{t_2..t_6}});",
"Phi = rationalMap(X,Dominant=>2);", 
"time forceImage(Phi,ideal 0_(target Phi))", 
"Phi;"},
Caveat => {"If the declaration is false, nonsensical answers may result."},
SeeAlso => {(image,RationalMap),forceInverseMap}}

undocumented {(point,Ideal),(point,Ideal,Boolean)}
document { 
Key => {point,(point,QuotientRing),(point,PolynomialRing)}, 
Headline => "pick a random rational point on a projective variety", 
Usage => "point R", 
Inputs => { 
Ring => "R" => {"the homogeneous coordinate ring of a closed subscheme ",TEX///$X\subseteq\mathbb{P}^n$///," over a finite ground field"}}, 
Outputs => { 
Ideal => {"an ideal in ",TT "R"," defining a point on ",TEX///$X$///}}, 
PARA{"This function is a variant of the ",TO randomKRationalPoint," function, which has been further improved and extended in the package ",HREF{"https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2/share/doc/Macaulay2/MultiprojectiveVarieties/html/index.html","MultiprojectiveVarieties"},", see ",HREF{"https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2/share/doc/Macaulay2/MultiprojectiveVarieties/html/_point_lp__Multiprojective__Variety_rp.html", TT "point(MultiprojectiveVariety)"},"."},
PARA{"Below we verify the birationality of a rational map."},
EXAMPLE { 
"f = inverseMap specialQuadraticTransformation(9,ZZ/33331);",
"time p = point source f",
"time p == f^* f p"},
SeeAlso => {randomKRationalPoint}}

undocumented {(segre,Ideal,Ideal)}
document { 
Key => {segre,(segre,RationalMap),(segre,QuotientRing),(segre,PolynomialRing)}, 
Headline => "Segre embedding", 
Usage => "segre phi 
segre R", 
Inputs => { 
"phi" => RationalMap => {"with source a closed subvariety ",TEX///$X\subseteq\mathbb{P}^{n_1}\times\mathbb{P}^{n_2}\times\cdots\times\mathbb{P}^{n_k}$///," of a product of projective spaces"}, 
"R" => QuotientRing => {"or a ",TO2{PolynomialRing,"polynomial ring"},", the coordinate ring of the subvariety ",TEX///$X\subseteq\mathbb{P}^{n_1}\times\mathbb{P}^{n_2}\times\cdots\times\mathbb{P}^{n_k}$///}}, 
Outputs => { 
RationalMap => {"the restriction to ",TEX///$X$///," of the Segre embedding of ",TEX///$\mathbb{P}^{n_1}\times\mathbb{P}^{n_2}\times\cdots\times\mathbb{P}^{n_k}$///,", where the linear span of the image is identified with a projective space"}},
PARA{"More properly, this method accepts and returns objects of the class ",TT"MultihomogeneousRationalMap","."},
EXAMPLE {
"phi = first graph quadroQuadricCremonaTransformation(3,1)",
"segre phi"}}

document { 
Key => {abstractRationalMap,(abstractRationalMap,PolynomialRing,PolynomialRing,FunctionClosure,ZZ),(abstractRationalMap,PolynomialRing,PolynomialRing,FunctionClosure),(abstractRationalMap,RationalMap)}, 
Headline => "make an abstract rational map", 
Usage => "abstractRationalMap(R,S,f,d) 
          abstractRationalMap(R,S,f)",
Inputs => { 
PolynomialRing => "R" => {"the coordinate ring of the source of the map"},
PolynomialRing => "S" => {"the coordinate ring of the target of the map"},
FunctionClosure => "f" => {"the abstract definition of the map"},
ZZ => "d" => {"(optional) an integer close to the degree of the forms defining the map"}}, 
Outputs => { 
RationalMap => {"the abstract rational map from ",TT"Proj(R)"," to ",TT"Proj(S)"," defined by f"}}, 
PARA{"The main ingredient behind this method is the interpolation of multivariate polynomials. We illustrate this feature with some examples."},
EXAMPLE {
"f = a -> {-a_1^3*max(sin(a_2),1)+a_0*a_1*a_2*ceiling((log(1 + abs a_0))^0),-a_1^2*a_2+a_0*a_1*a_3,-a_1*a_2^2+a_1^2*a_3,-a_1^2*a_3+a_0*a_1*a_4,-a_1*a_2*a_3+a_1^2*a_4,-a_1*a_3^2+a_1*a_2*a_4}",
"P4 := QQ[t_0..t_4]",
"P5 := QQ[u_0..u_5]",
"time psi = abstractRationalMap(P4,P5,f)"},
PARA{"Now we compute first the degree of the forms defining the abstract map ",TT"psi"," and then the corresponding concrete rational map."},
EXAMPLE {
"time projectiveDegrees(psi,3)",
"time rationalMap psi"},
PARA{"As a second example, we apply the method to compute the inverse of a Cremona transformation."},
EXAMPLE {
"phi = rationalMap map specialCremonaTransformation(3,ZZ/10000019);",
"phi' = abstractRationalMap phi",
"psi' = inverseMap phi'",
"psi = rationalMap psi';",
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
"time f = rationalMap T",
"describe f!"},
Caveat => {"This is under development yet."}
}
undocumented{(abstractRationalMap,Ideal,String)}

document { 
Key => {(toExternalString,RationalMap)}, 
Headline => "convert to a readable string", 
Usage => "toExternalString phi", 
Inputs => {RationalMap => "phi"}, 
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
Key => {isMorphism,(isMorphism,RationalMap)}, 
Headline => "whether a rational map is a morphism", 
Usage => "isMorphism phi", 
Inputs => { 
"phi" => RationalMap}, 
Outputs => { 
Boolean => {"whether ",TT"phi"," is a morphism (i.e., everywhere defined)"}},
EXAMPLE { 
"phi = quadroQuadricCremonaTransformation(5,1)",
"isMorphism phi",
"phi' = last graph phi;",
"isMorphism phi'"},
SeeAlso => {(ideal,RationalMap),(isIsomorphism,RationalMap)}}

document { 
Key => {(isIsomorphism,RationalMap)}, 
Headline => "whether a birational map is an isomorphism", 
Usage => "isIsomorphism phi", 
Inputs => {"phi" => RationalMap}, 
Outputs => {Boolean => {"whether ",TT"phi"," is an isomorphism"}},
PARA{"This method computes the inverse rational map using ",TO2{(inverse,RationalMap),"inverse"},"."},
EXAMPLE { 
"P1 := QQ[a,b]; P4 := QQ[x,y,z,w];",
"phi = rationalMap({a^4,a^3*b,a^2*b^2,a*b^3,b^4},Dominant=>true)",
"isIsomorphism phi"},
SeeAlso => {(ideal,RationalMap),isBirational,isMorphism}}

document { 
Key => {(rationalMap,Ring,Tally),(rationalMap,Tally)}, 
Headline => "rational map defined by an effective divisor", 
Usage => "rationalMap D
rationalMap(R,D)", 
Inputs => {"R" => Ring => {"the coordinate ring of a locally factorial projective variety ",TEX///$X\subseteq\mathbb{P}^n$///},
"D" => Tally => {"a multiset of homogeneous ideals in ",TEX///$R$///," (or in ",TO ambient," ",TEX///$R$///,") defining pure codimension 1 subschemes of ",TEX///$X$///," with no embedded components; so that ",TT"D"," is interpreted as an effective divisor on ",TEX///$X$///}}, 
Outputs => {RationalMap => {"the rational map defined by the complete linear system ",TEX///$|D|$///}},
PARA{"In the example below, we take a smooth complete intersection ",TEX///$X\subset\mathbb{P}^5$///," of three quadrics containing a conic ",TEX///$C\subset\mathbb{P}^5$///,". Then we calculate the map defined by the linear system ",TEX///$|2H+C|$///,", where ",TEX///$H$///," is the hyperplane section class of ",TEX///$X$///,"."},
EXAMPLE {
"P5 = ZZ/65521[x_0..x_5];",
"C = ideal(x_1^2-x_0*x_2,x_3,x_4,x_5)",
"X = quotient ideal(-x_1^2+x_0*x_2-x_1*x_3+x_3^2-x_0*x_5+x_1*x_5+x_3*x_5,-x_0*x_3-x_1*x_3+x_2*x_4-x_3*x_4-x_4^2-x_1*x_5-x_2*x_5+x_5^2,-x_1^2+x_0*x_2+x_2*x_3+x_1*x_4-x_3*x_4-x_4*x_5);",
"H = ideal random(1,X)",
"D = new Tally from {H => 2,C => 1};",
"time phi = rationalMap D",
"time ? image(phi,\"F4\")"},
PARA{"See also the package ",HREF{"http://www2.macaulay2.com/Macaulay2/doc/Macaulay2-1.16/share/doc/Macaulay2/Divisor/html/index.html","Divisor"},", which provides general tools for working with divisors."},
SeeAlso => {rationalMap,(rationalMap,Ideal,ZZ),(image,RationalMap,String)}}

