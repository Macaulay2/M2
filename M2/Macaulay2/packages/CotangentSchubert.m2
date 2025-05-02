newPackage(
    "CotangentSchubert",
    AuxiliaryFiles => true,
    Version => "0.71",
    Date => "25 Jul 2023", -- "22 Mar 2021",
    Authors => {{Name => "Paul Zinn-Justin",
            Email => "pzinn@unimelb.edu.au",
            HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
    Headline => "Cotangent Schubert calculus",
    Keywords => {"Intersection Theory"},
    PackageImports => {"VectorGraphics"},
    AuxiliaryFiles => true,
    DebuggingMode => false,
    Configuration => { "Factor" => false, "PuzzleSize" => 7 },
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "The CotangentSchubert Macaulay2 package",
	"acceptance date" => "2024-02-06",
	"published article URI" => "https://msp.org/jsag/2024/14-1/p09.xhtml",
	"published article DOI" => "10.2140/jsag.2024.14.73",
	"published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x09-CotangentSchubert.m2",
	"release at publication" => "6a750d5611ff9685a31e1eb4e176bb71b6842a58",
	"version at publication" => "0.63",
	"volume number" => "14",
	"volume URI" => "https://msp.org/jsag/2024/14-1/"
	}
    )

if (options CotangentSchubert).Configuration#"Factor" then needsPackage "Factor" else factor PolynomialRing := opts -> identity;

opts = new OptionTable from {Ktheory => false, Equivariant => true} -- common options
export {"Ktheory", "Equivariant", "Separation", "LabelList" };

load "CotangentSchubert/cotangent.m2";
load "CotangentSchubert/puzzles.m2";

beginDocumentation()
multidoc ///
 Node
  Key
   CotangentSchubert
  Headline
   A package for cotangent Schubert calculus
  Description
   Text
    @BOLD "CotangentSchubert"@ is a package for calculations in cotangent Schubert calculus.
    Specifically, it allows to compute motivic Chern and Segre classes (as well as their limits in ordinary
    Schubert calculus, namely Schubert classes), and to independently compute the expansion of their products
    using puzzles. Puzzles and their "fugacities" are defined and computed using the results of [1,2,3].

    References: @BR{}@
    [1] A. Knutson and P. Zinn-Justin, Schubert puzzles and integrability I: invariant trilinear forms,
    @HREF{"http://arxiv.org/abs/1706.10019","arXiv:1706.10019"}@. @BR{}@
    [2] A. Knutson and P. Zinn-Justin, Schubert puzzles and integrability II: multiplying motivic Segre classes,
    @HREF{"http://arxiv.org/abs/2102.00563","arXiv:2102.00563"}@. @BR{}@
    [3] A. Knutson and P. Zinn-Justin, Schubert puzzles and integrability III: separated descents,
    @HREF{"http://arxiv.org/abs/2306.13855","arXiv:2306.13855"}@.
 Node
  Key
   setupCotangent
   [setupCotangent,Presentation]
   [setupCotangent,Ktheory]
   [setupCotangent,Equivariant]
  Headline
   Set up cotangent Schubert calculus rings
  Usage
   (A,B,FF,I) = setupCotangent(dimensions,Presentation=>Borel)
   (D,FF,I) = setupCotangent(dimensions,Presentation=>EquivLoc)
  Inputs
   dimensions : Sequence
    of integers
   Presentation => Symbol
   Ktheory => Boolean
   Equivariant => Boolean
  Description
   Text
    This function sets up the various rings used by @BOLD "CotangentSchubert"@. Its behaviour depends
    on the value of the option @TT "Presentation"@.

    With @TT "Presentation => Borel"@, @TT "setupCotangent"@ returns a sequence made of two rings, a field and a list:
   Example
    (A,B,FF,I) = setupCotangent(1,3,Presentation=>Borel)
   Text
    The first (resp. second) ring is the cohomology or K-theory ring of the cotangent bundle of the flag variety specifided by the sequence of dimensions
    of flags (resp. of the corresponding full flag variety). The field is the base field (fraction field
    of the cohomology or K-theory ring of a point). The list is a list of labels for the various classes that
    @BOLD "CotangentSchubert"@ computes.
    The boolean option @TT "Ktheory"@ specifies the choice of K-theory or cohomology, whereas @TT "Equivariant"@ specifies
    whether the equivariance is w.r.t. scaling of the fiber of the cotangent bundle only, or w.r.t. scaling and Cartan
    torus action.

    With @TT "Presentation => EquivLoc"@:
   Example
    (D,FF,I) = setupCotangent(1,3,Presentation=>EquivLoc)
   Text
    the first output is a "diagonal algebra", i.e., a vector space over @TT "FF"@ with componentwise product. The last two outputs
    are the same as above.
 Node
  Key
   chernClass
   stableClass
   chernClass'
   stableClass'
  Headline
   Compute a motivic Chern class
  Usage
   chernClass i
   chernClass I
   chernClass(i,A)
   chernClass(I,A)
   chernClass' i
   stableClass i
   stableClass' i
  Inputs
   i : String
   I : List
   A : Ring
  Description
   Text
    This function computes a motivic Chern class with label @TT "i"@ (a string made of characters from "0" to "d"
    where d is the number of steps of the flag variety) or list of labels @TT "I"@ in a K-theory ring @TT "A"@
    previously built using @TO{setupCotangent}@. If @TT "A"@ is not specified, then the ring that was defined last is used.
   Example
    (A,B,FF,I)=setupCotangent(1,3,Presentation=>Borel,Ktheory=>true,Equivariant=>false);
    chernClass "101"
   Text
    If a list of labels I is used, then a matrix of classes is returned.
   Example
    chernClass I
   Text
    @TT "stableClass"@ differs from @TT "chernClass"@ by division by the canonical class of the flag variety.
    The primed classes are dual classes.
 Node
  Key
   segreClass
   sClass
   segreClass'
   sClass'
  Headline
   Compute a motivic Segre class
  Usage
   segreClass i
   segreClass I
   segreClass(i,A)
   segreClass(I,A)
   segreClass' i
   sClass i
   sClass' i
  Inputs
   i : String
   I : List
   A : Ring
  Description
   Text
    This function computes a motivic Segre class with label @TT "i"@ (a string made of characters from "0" to "d"
    where d is the number of steps of the flag variety) or list of labels @TT "I"@ in a K-theory ring @TT "A"@
    previously built using @TO{setupCotangent}@. If @TT "A"@ is not specified, then the ring that was defined last is used.
   Example
    (A,B,FF,I)=setupCotangent(1,3,Presentation=>Borel,Ktheory=>true,Equivariant=>false);
    segreClass "101"
   Text
    If a list of labels I is used, then a matrix of classes is returned.
   Example
    segreClass I
   Text
    @TT "sClass"@ differs from @TT "segreClass"@ by multiplication by a power of the equivariant parameter $q$.
    The primed classes are dual classes.
 Node
  Key
   schubertClass
   schubertClass'
  Headline
   Compute a Schubert class
  Usage
   schubertClass i
   schubertClass I
   schubertClass(i,A)
   schubertClass(I,A)
   schubertClass' i
  Inputs
   i : String
   I : List
   A : Ring
  Description
   Text
    This function computes a Schubert class with label @TT "i"@ (a string made of characters from "0" to "d"
    where d is the number of steps of the flag variety) or list of labels @TT "I"@ in a K-theory ring @TT "A"@
    previously built using @TO{setupCotangent}@. If @TT "A"@ is not specified, then the ring that was defined last is used.
    The primed classes are dual classes.
 Node
  Key
   tautoClass
  Headline
   Compute the class of a tautological bundle
  Usage
   tautoClass (i,j)
   tautoClass (i,j,A)
  Inputs
   i : ZZ
   j : ZZ
   A : Ring
  Description
   Text
    This function computes the i-th Chern class of the j-th tautological bundle of the flag variety whose K-theory (or cohomology)
    ring is given by @TT "A"@. If @TT "A"@ is not specified, then the ring that was defined last is used.
 Node
  Key
   pushforwardToPoint
   pushforwardToPointFromCotangent
  Headline
   Push forward classes to a point
  Usage
   pushforwardToPoint a
   pushforwardToPointFromCotangent a
  Inputs
   a : RingElement
  Description
   Text
    This function returns the push forward of a (K-theory or cohomology) class from either the flag variety or its cotangent bundle
    to a point.
    The ring of input @TT "a"@ must have been previously created with @TO {setupCotangent}@.
 Node
  Key
   inversion
  Headline
   Inversion number of a string
  Description
   Text
    Given a string of digits "a_1 a_2 ... a_n", computes the inversion number of that string
    (the number of i<j such that a_i>a_j).
 Node
  Key
   restrict
  Headline
   Restriction to fixed points
  Description
   Text
    Given a (K-theory or cohomology) class in a ring defined by @TO {setupCotangent}@
    with the options @TT "Presentation => Borel"@ and @TT "Equivariance => true"@,
    this function computes its restriction to fixed points (effectively, giving the class
    as would be produced by the option @TT "Presentation => EquivLoc"@).
 Node
  Key
   zeroSection
  Headline
   Class of the zero section of the cotangent bundle of a flag variety
  Description
   Text
    This function returns the class of the base of the cotangent bundle of the flag variety
    in the cohomology ring given as argument (or the last ring defined with @TO {setupCotangent}@
    if no argument is given).
 Node
  Key
   dualZeroSection
  Headline
   Class of the zero section of the tangent bundle of a flag variety
  Description
   Text
    This function returns the class of the base of the tangent bundle of the flag variety
    in the cohomology ring given as argument (or the last ring defined with @TO {setupCotangent}@
    if no argument is given).
 Node
  Key
   canonicalClass
  Headline
   Class of the canonical bundle of a flag variety
  Description
   Text
    This function returns the class of the canonical bundle of the flag variety
    in the cohomology ring given as argument (or the last ring defined with @TO {setupCotangent}@
    if no argument is given).
 Node
  Key
   puzzle
   [puzzle, Generic]
   [puzzle, Ktheory]
   [puzzle, Ktheory']
   [puzzle, Equivariant]
   [puzzle, Labels]
   [puzzle, Paths]
   [puzzle, Steps]
   Puzzle
  Headline
   Compute puzzles with given boundaries
  Usage
   puzzle (a,b)
   puzzle (a,b,c)
  Inputs
   a : String
   b : String
   c : String
   Ktheory => Boolean
   Equivariant => Boolean
   Generic => Boolean
   Ktheory' => Boolean
    has an effect only if @TT "Generic => false"@
   Labels => Boolean
   Paths => Boolean
   Steps => ZZ
  Description
   Text
    This function produces a list of puzzles with boundaries given by strings @TT "a"@ (northwest side),
    @TT "b"@ (northeast side), @TT "c"@ (bottom side).
    The default @TT "Generic=>true"@ produces "generic" puzzles corresponding to the multiplication of
    motivic Segre classes @TO {sClass}@, whereas @TT "Generic=>false"@ produces "classic" puzzles
    corresponding to the multiplication of Schubert classes @TO {schubertClass}@ (in the latter case,
    @TT "Ktheory'=>true"@ produces puzzles for dual Schubert classes).

    The boundaries are encoded as strings of digits from "0" to "d", where d is the number of steps
    of the flag variety; alternatively, one can use lists of integers or strings (the latter allows for
    the possibility of having arbitrary puzzle labels on the boundary, such as "2(10)", even though
    this will not occur in normal puzzle computations). The special symbol "#" stands for any single digit,
    whereas "*" stands for any puzzle label.

    @TT "Labels"@ and @TT "Paths"@ are drawing options which only affect HTML and TeX output of puzzles.
   Example
    puzzle ("0101","1001",Equivariant=>false)
 Node
  Key
   fugacity
   fugacityVector
   fugacityTally
  Headline
   compute the fugacity of puzzles
  Usage
   fugacity P
   fugacityVector L
   fugacityTally L
  Inputs
   P : Puzzle
   L : List
    a list of puzzles
  Description
   Text
    To each puzzle is associated an element of the base field (cohomology/K-theory of a point), its so-called fugacity,
    in such a way that the sum of fugacities of puzzles with prescribed boundaries @TT"a"@, @TT"b"@, @TT"c"@, is equal
    to the coefficient of the expansion of the product of classes indexed by the strings @TT"a"@ and @TT"b"@ into the class
    class indexed by the string @TT"c"@. Here the classes are motivic Segre classes @TO{sClass}@ if @TT"Generic=>true"@,
    Schubert classes @TO{schubertClass}@ if @TT"Generic=>false"@.
    The options @TT "Ktheory"@, @TT "Equivariant"@, @TT "Generic"@ are inherited from the @TO{puzzle}@ by default,
    but they can be overridden.

    fugacityTally processes a list of puzzles, and returns a hash table where to each string appearing at the bottom
    of a puzzle is associated the sum of fugacities of the corresponding puzzles.

    fugacityVextor similarly processes a list of puzzles, and returns the result as a vector
    where each entry is the sum of fugacities of puzzles with a given bottom string.
    The ordering of strings is the same as the list returned by @TO{setupCotangent}@.
 Node
  Key
   bottom
  Headline
   The bottom string of a puzzle
  Usage
   bottom P
  Inputs
   P : Puzzle
  Description
   Text
    Reads off the bottom boundary string of a puzzle.
 Node
  Key
   basisCoeffs
  Headline
   Expand a finite-dimensional algebra element into its basis
  Description
   Text
    This is a simple helper function.
   Example
    (A,B,FF,I)=setupCotangent(2,4,Presentation=>Borel,Ktheory=>true,Equivariant=>false)
    basis A
    basisCoeffs(x_(1,{1,2})^2)
 Node
  Key
   doublePuzzle
   DoublePuzzle
  Headline
   Produces a rhombus puzzle
  Usage
   doublePuzzle (a,b,c,d)
  Inputs
   a : String
   b : String
   c : String
   d : String
  Description
   Text
    Given 4 strings, computes the number of pairs of puzzles glued together to form a rhombus with boundaries
    a,b,c,d in clockwise order.

    In WebApp mode, the output is interactive and allows to test associativity of puzzles.
   Example
    doublePuzzle("0101","0101","0101","0101",Equivariant=>false)
  Caveat
   At the moment, the interactive part only works on nonequivariant puzzles.
///
undocumented {
    Presentation, Ktheory, Equivariant, Partial, Borel, EquivLoc,
    Paths, Labels, Length, Steps, Ktheory', Separation,
    (restrict,Matrix),(restrict,Matrix,RingElement),
    (inversion,String),
    (html,Puzzle),(net,Puzzle),(tex,Puzzle),(texMath,Puzzle),
    (html,DoublePuzzle),(net,DoublePuzzle)
    }

TEST ///
(A,B,FF,I)=setupCotangent(2,4,Presentation=>Borel,Ktheory=>false,Equivariant=>false);
assert((schubertClass "0101")^2 == schubertClass "1001" + schubertClass "0110") -- classic Schubert calculus
assert(pushforwardToPoint((segreClass "0101")^2*chernClass "0101")==-4) -- ex of p43 of II: chi(S_0101^3)=-4
assert(sum apply(I,segreClass) == 1) -- sum rule for Segre classes
///

TEST ///
(D,FF,I)=setupCotangent(2,4,Ktheory=>true);
assert(sum apply(I,segreClass) == 1) -- sum rule for Segre classes
P=puzzle("0101","0110"); assert(#P == 12) -- generic puzzles
assert(sClass I * fugacityVector P == sClass "0101"*sClass "0110")
///

TEST ///
(D,FF,I)=setupCotangent(1,3,5,Ktheory=>false)
segreCls=sClass I;
i1=I#11;i2=I#27;
a=sClass i1*sClass i2;
P=puzzle(i1,i2); -- generic 2-step puzzles
b=segreCls*(fugacityVector P);
assert(a==b)
///

end

(A,FF,I)=setupCotangent(1,2,Ktheory=>true)
segreCls=sClass I
segreInv=segreCls^(-1);
Table table(I,I,(i,j)->segreInv*(sClass i * sClass j))
L=puzzle("01","01",Generic=>true,Equivariant=>true)
fugacityVector(L,Ktheory=>true)

(AA,BB,FF,I)=setupCotangent(2,4,Presentation=>Borel,Equivariant=>false,Ktheory=>true)
segreCls=sClass I;
L=puzzle("0101","0101",Generic=>true,Equivariant=>false)
fugacityVector(L,Ktheory=>true)
(segreCls*oo)_0
segreCls_(0,1)^2

-- d=2
(A,FF,I)=setupCotangent(1,2,4)
segreCls=sClass I;
i1=I#1;i2=I#2;
a=sClass i1*sClass i2;
P=puzzle(i1,i2)
b=segreCls*(fugacityVector P);
a==b
Table table(I,I,(i1,i2)->sClass i1*sClass i2==segreCls*(fugacityVector puzzle(i1,i2)))

-- note that for large examples, no need to compute the inverse
(FF,I)=setupCotangent(1,2,3,4)
segreCls=sClasses();
i1="3021"; i2="2130"; -- interesting because separated "3 2 " * " 1 0"
a=(sClass i1)*(sClass i2);
P=puzzle(i1,i2,Generic=>true,Equivariant=>true)
fugacityTally P
b=segreCls*(fugacityVector P);
a==b

Q=puzzle("3 2 "," 1 0",Generic=>true,Equivariant=>true,Separated=>true)
fugacityTally Q -- not the same as P!
b=segreCls*(fugacityVector Q);
a=(sClass "3021" - sClass "3120")*(sClass "2130" - sClass "3120");
a==b

i1="2103"; i2="0321"; i3="2301"; -- interesting because shows compensations
P=puzzle(i1,i2,i3,Generic=>true,Equivariant=>true)
fug=fugacity\P;
R=QQ[ϵ,x_1..x_4]; f=map(frac R,ring first fug,{1/ϵ,x_1..x_4})
fug2=f\fug;
load "series.m2"
apply(fug2,p->p+O(ϵ^2)) -- takes too long

puzzle("5 4 3 ","210   ",Separated=>true)
