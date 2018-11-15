newPackage(
        "ReflexivePolytopesDB",
        Version => "0.9", 
        Date => "6 May 2018",
        Authors => {{
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"http://www.math.cornell.edu/~mike"
                }},
        Headline => "simple access to Kreuzer-Skarke database of reflexive polytopes of dimensions 3 and 4",
        AuxiliaryFiles => true,
        DebuggingMode => true
        )

export {
    "matrixFromString", -- doc
    "generateOffline", -- doc
    "getKreuzerSkarke", -- doc
    "getKreuzerSkarkeDim3", -- doc
    "parseKS", -- doc
    -- Option names
    "Access",
    "Expected",
    "Vertices",
    "Facets",
    "LatticePoints",
    "DualLatticePoints",
    "H12"
    }

-*
exampleFromKS = method()
exampleFromKS(ZZ, String) := (which, str) -> (
    L := parseKS str;
    eg := L#which;
    (eg#0, matrixFromString eg#1)
    )
*-

matrixFromString = method()
matrixFromString String := (str) -> (
    -- expect input in one of the three forms:
    -*
      (a)
              [ 1 -1 -1  1 -1 -1 -1  1  1  0]
              [ 0  1  0 -1  1  0  0 -1 -1  0]
              [-1  0  0  0  1  1  0  0  0  0]
              [ 2  0  0  1 -1 -1 -1 -1  0  0]
      (b)
           [[1, 0, -1, 2], [-1, 1, 0, 0], [-1, 0, 0, 0], [1, -1, 0, 1], [-1, 1, 1, -1], [-1, 0, 1, -1], [-1, 0, 0, -1], [1, -1, 0, -1]]    
      (c) 
        "         1   0   0   0   1   1   0  -1  -1  -2  -4
                  0   1   1   0  -2   2   3  -1  -4   1  -1
                  0   0   2   0  -2   4   4  -1  -4  -2  -4
                  0   0   0   1   0  -2  -2   2   2   0   2"
    *-
    s0 := replace("\t", " ", str);
    s1 := replace(",", " ", s0);
    s2 := replace("] ", "]\n", s1);
    s3 := replace("\\[", "", s2);
    s4 := replace("]", "", s3);
    rows := for s in lines s4 list (
            t := separateRegexp(" +", s); 
            if #t > 0 and first t == "" then t = drop(t,1);
            if #t > 0 and last t == "" then t = drop(t,-1);
            if #t == 0 then continue;
            t/value
            );
    if # unique(rows/length) != 1 then error "expected rows all with the same length";
    matrix rows
    )

getURL = method()
getURL(String, String) := String => (str, access) -> (
    print str;
    if access === "m2" then
      last splitWWW getWWW str
    else if access === "curl" then
      get ("!curl \""|str|"\"")
    else if access === "wget" then 
      get ("!wget \""|str|"\" -O -")
    else error "expected Access argument to be \"m2\" (default), \"curl\", or \"wget\""
    )

getKreuzerSkarke = method(Options=>{
        Limit=>1000, 
        Access=>"m2",
        LatticePoints => null,
        DualLatticePoints => null,
        Vertices => null,
        Facets => null,
        H12 => null
        })

getKreuzerSkarke(ZZ,ZZ) := opts -> (h11,h12) -> (
    getKreuzerSkarke(h11, opts, H12=>h12)
    )

getKreuzerSkarke ZZ := opts -> (h11) -> (
    getopt := (ksStr, m2Key, opts) -> (
        if opts#m2Key === null then ""
        else (ksStr | "=" | toString opts#m2Key)
        );
    valV := getopt("V", Vertices, opts);
    valM := getopt("M", LatticePoints, opts);
    valN := getopt("N", DualLatticePoints, opts);
    valF := getopt("F", Facets, opts);
    valH21 := getopt("h12", H12, opts);
    valL := getopt("L", Limit, opts);
    str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?h11="|toString h11;
    ksopts := {valV, valM, valN, valF, valH21, valL};
    ksopts = select(ksopts, x -> x =!= "");
    str = concatenate between("&", prepend(str,ksopts));
    --str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?h11="|toString h11|"&L="|toString opts.Limit;
    getURL(str, opts.Access)
    )

getKreuzerSkarkeDim3 = () -> (
    --str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?M=&V=&N=&F=&h11="|toString h11|"&h12="|toString h12|"&chi=&L="|toString opts.Limit;
    str := "http://hep.itp.tuwien.ac.at/~kreuzer/pub/K3/RefPoly.d3";
    getURL(str, "m2")
    )

parseKS = method()
parseKS String := (str) -> (
    -- result is a List of pairs of strings.
    locA := regex("<b>Result:</b>\n", str);
    locB := regex("#NF", str);
    if locB === null then locB = regex("Exceeded", str);
    --if locA === null or locB === null then error "data not in correct Kreuzer-Skarke format";
    firstloc := if locA === null then 0 else locA#0#0 + locA#0#1;
    lastloc := if locB === null then #str else locB#0#0;
    --firstloc := locA#0#0 + locA#0#1;
    --lastloc := locB#0#0;
    cys := substring(firstloc, lastloc-firstloc, str);
    cys = lines cys;
    cys = select(cys, s -> #s > 0);
    starts := positions(cys, s -> s#0 != " ");
    starts = append(starts, #cys);
    for i from 0 to #starts-2 list (
        cys_(starts#i), demark("\n", cys_{starts#i+1 .. starts#(i+1)-1})
        )
    )

generateOffline = method(Options=>{Expected=>null, Limit=>1000, Prefix=>"./"})
generateOffline(ZZ,ZZ) := opts -> (h11, h12) -> (
    contents := getKreuzerSkarke(h11, h12, Limit=>opts.Limit);
    L := parseKS contents;
    nexpected := opts.Expected;
    if instance(nexpected,ZZ) and  #L =!= nexpected then 
      error ("expected "|nexpected|" examples, obtained "|#L);
    filename := opts.Prefix | "ks"|h11|"+"|h12|"-n"|#L|".txt";
    << "writing file " << filename << endl;
    filename << contents << close;
    filename
    )
generateOffline ZZ := opts -> (h11) -> (
    contents := getKreuzerSkarke(h11, Limit=>opts.Limit);
    L := parseKS contents;
    nexpected := opts.Expected;
    if instance(nexpected,ZZ) and  #L =!= nexpected then 
      error ("expected "|nexpected|" examples, obtained "|#L);
    filename := opts.Prefix|"ks"|h11|"-n"|#L|".txt";
    << "writing file " << filename << endl;
    filename << contents << close;
    filename
    )

-- the following line was generated using
-- getKreuzerSkarke(3, Limit=>10)
-- and is used for testing
h11'3'limit'10 = ///<head><title>SEARCH RESULTS</title></head>
<body><pre><b>Search command:</b>
class.x -di x -He EH3:MVNFL10

<b>Result:</b>
4 5  M:53 5 N:9 5 H:3,43 [-80]
   1   0   2   4 -10
   0   1   3   5  -9
   0   0   4   0  -4
   0   0   0   8  -8
4 5  M:48 5 N:8 5 H:3,45 [-84]
   1   0   2   4  -8
   0   1   5   3  -9
   0   0   6   0  -6
   0   0   0   6  -6
4 6  M:48 6 N:8 6 H:3,45 [-84]
   1   1   0   2   4  -8
   0   3   0   2   0  -6
   0   0   1  -1   3  -3
   0   0   0   0   6  -6
4 13  M:64 13 N:8 7 H:3,51 [-96]
   1   0   0   2  -2   0  -1  -1   0   2   2  -3  -3
   0   1   1   3  -5   2   0   0   2   4   4  -6  -6
   0   0   4   0  -4   5   4  -1   0   1   0  -4  -5
   0   0   0   4  -4   1  -1  -1   1   5   5  -5  -5
4 9  M:66 9 N:8 6 H:3,57 [-108]
   1   0   0   1  -3   3   3  -3   5
   0   1   0   0   2  -4  -6   4  -8
   0   0   1   0   2  -2  -2   0  -4
   0   0   0   2   0  -4  -6   6  -6
4 7  M:65 7 N:8 6 H:3,57 [-108]
   1   0   2   0   0  -2  -2
   0   1   3   1   1  -3  -3
   0   0   4   0   4  -6   0
   0   0   0   4  -4   6  -6
4 11  M:66 11 N:8 7 H:3,57 [-108]
   1   1   1   0   2   2  -3   3   3   1  -7
   0   2   0   0   3   0  -2   2   4   2  -6
   0   0   2   0   0   3  -2   4   2   2  -6
   0   0   0   1  -1  -1   2  -2  -2  -2   2
4 12  M:69 12 N:8 7 H:3,57 [-108]
   1   0   0   0   3  -3   3  -3  -1  -3   3  -3
   0   1   0   0   2   0   0  -2  -2  -3   0  -3
   0   0   1   0  -6   3  -3   6   4   7  -6   3
   0   0   0   1   2  -2   1  -3  -2  -3   4   1
4 9  M:66 9 N:8 6 H:3,59 [-112]
   1   1   0   0   0   0  -2  -2   3
   0   4   0   0   4   4  -2  -2  -2
   0   0   1   0   1   0   3  -2  -2
   0   0   0   1   0   1  -2   3  -2
4 7  M:77 7 N:9 6 H:3,59 [-112]
   1   0   2   0  -4  -6  -2
   0   1   3   1  -3  -5  -1
   0   0   4   0   0  -4  -4
   0   0   0   4  -4  -4   4
Exceeded limit of 10
</pre></body>
///

beginDocumentation()

doc ///
Key
  ReflexivePolytopesDB
Headline
  simple access to Kreuzer-Skarke database of reflexive polytopes of dimensions 3 and 4
Description
  Text
    In each given dimension $d$, it is known that the number of
    distinct (up to invertible integral change of basis) reflexive
    polytopes of dimension $d$ is finite in number.  For example, in
    dimension 1 there is 1, in dimension 2, there are 16 and in
    dimension 3, there are 4319 distinct refdlexive polytopes.

    In a major work, Max Kreuzer and Harold Skarke found algorithms for
    computing the set of such polytopes.  They used these algorithms to
    show that there are 473,800,776 distinct 4-dimensional reflexive
    polytopes.  The number is sufficiently large that they created
    a website @HREF "http://hep.itp.tuwien.ac.at/~kreuzer/CY/"@
    and an interface to access these examples.  See their website for
    references to the algorithms used.
        
    This package, {\tt ReflexivePolytopesDB}, provides access to this database of
    reflexive polytopes of dimension 3 and dimension 4.
    
    This package also contains a small part of this database for offline use,
    in case one cannot access the database.
  Text
    Here we describe a simple use of the package.  The actual
    investigation of the corresponding polytope or toric variety, or Calabi-Yau
    hypersurface, is done in Macaulay2 with the aid of other packages,
    such as @TO "Polyhedra::Polyhedra"@.
    
    Let's take one example polytope from the database, one whose corresponding Calabi-Yau
    3-fold has Hodge numbers $h^{1,1}(X) = 23$ and $h^{1,2}(X) = 17$.  We limit the
    number we obtain to 2.
  Example
    str = getKreuzerSkarke(23,17, Limit=>2)
  Text
    Now we parse this string, into a list of pairs of Strings.
  Example
    L = parseKS str;
    netList L
  Text
    The result consists of lists of two strings.  For each element in the list,
    the first is a header string, see @TO "Kreuzer-Skarke headers"@.
    The second is a string that corresponds to a matrix.  
    
    Let's consider the last example in this last.  We get that matrix
    via the utility function @TO "matrixFromString"@.
  Example
    eg = last L
    A = matrixFromString eg_1
  Text
    The corresponding reflexive polytope has 5 vertices, the columns of this matrix.
  Example
    needsPackage "Polyhedra"
    P = convexHull A
    isReflexive P
    P2 = polar P
    (numColumns vertices P, numColumns vertices P2)
    (# latticePoints P, # latticePoints P2)
///

doc ///
Key
  matrixFromString
  (matrixFromString, String)
Headline
  convert a string to a matrix of integers
Usage
  m = matrixFromString str
Inputs
  str:String
Outputs
  m:Matrix
Description
  Text
    This utility function is used to parse, as matrices of integers, strings
    returned by the Kreuzer-Skarke database.  As an example,
  Example
       str = " 1   0   0   0   0   1   2   1   0  -2   0  -2
               0   1   0   0   0   0  -2  -1   1   2  -1   0
               0   0   1   0   0  -1   0  -1  -1   1  -1   1
               0   0   0   1  -1   0   1   1  -1   0   1  -2
               "
      A = matrixFromString str
  Text
    The actual format allowed is the following: Spaces, tabs, and
    commas are all separators for elements of the array.  Newlines,
    and ] characters separate rows.  Finally, each empty line is
    ignored, and the remaining lines must all have the same number of
    elements.
    
    As another example, matrices coming from Sage often use square brackets, and can be
    read using this function.
  Example
    str = "[[1, -1, -1, 1, -1, -1, -1, 1, 1, 0], [0, 1, 0, -1, 1, 0, 0, -1, -1, 0], 
      [-1, 0, 0, 0, 1, 1, 0, 0, 0, 0], [2, 0, 0, 1, -1, -1, -1, -1, 0, 0]]"
    matrixFromString str
  Text
    Formatting of white space is generally not important, except that each row must be on one line,
    and if there are several on a line, each row is ended by a ].
  Example
    str = "
     [
      [1, -1, -1, 1, -1, -1, -1, 1, 1, 0], 
      [0, 1, 0, -1, 1, 0, 0, -1, -1, 0], 
      [-1, 0, 0, 0, 1, 1, 0, 0, 0, 0], 
      [2, 0, 0, 1, -1, -1, -1, -1, 0, 0]
      ]
     "
    matrixFromString str
///

doc ///
Key
  getKreuzerSkarke
  (getKreuzerSkarke, ZZ)
  (getKreuzerSkarke, ZZ, ZZ)
Headline
  access Kreuzer-Skarke dim 4 reflexive polytopes database
Usage
  getKreuzerSkarke(h11, Limit=>n)
  getKreuzerSkarke(h11, h12, Limit=>n)
Inputs
  h11:ZZ
    The desired Picard number of the Calabi-Yau hypersurface
  h12:ZZ
    The desired $h^{2,1}(X)$ (where $X$ is the corresponding smooth Calabi-Yau 3-fold.  If not given, then all are considered.
  Limit => ZZ
    The maximum number of examples to retrieve
  Vertices => ZZ
    Restrict to those examples with this number of vertices
  Facets => ZZ
    Restrict to those examples with this number of facets
  LatticePoints => ZZ
    Restrict to those examples with this number of lattice points
  DualLatticePoints => ZZ
    Restrict to those examples whose polar dual has this number of lattice points
  H12 => ZZ
    Restrict to those examples whose associated Calabi-Yau 3-fold has the given $h^{1,2}(X)$
Outputs
  :String
    The output from the web request (including header information).  
    Use @TO "parseKS"@ to make this into something usable from Macaulay2.
Description
  Text
    As a an example, let's take the 4th example with $h^{11}=5$, $h^{21}=53$.
  Example
    str = getKreuzerSkarke(5, Limit=>4, H12 => 53)
    polytopes = parseKS str;
    #polytopes
    tope = polytopes_3
    header = tope_0
    A = matrixFromString tope_1
  Text
    The first line gives some information about the example, see @TO "Kreuzer-Skarke headers"@ for more details.
    The polytope is the convex hull of the columns of the matrix $A$.
SeeAlso
  parseKS
  matrixFromString
  "Kreuzer-Skarke headers"
  getKreuzerSkarkeDim3
///

doc ///
   Key
     "Kreuzer-Skarke headers"
   Headline
     information contained in the header line
   Description
    Text
      Each 4D reflexive polytope in the Kreuzer-Skarke database
      contains summary information about the polytope.
      Here, we explain this information.
      
      We will do this on an example, and see how to
      obtain this information directly.
    Example
      str = getKreuzerSkarke(5,Limit=>1);
      eg = first parseKS str;
      A = matrixFromString eg_1
      header = eg_0      
    Text
      This header line is what we wish to explain now.
      
      The quick description: 
    Text
      @UL {
        { TEX "'4 10': the first 2 numbers are the number of rows and columns of the matrix $A$" },
        { TEX "'M:25 10': number of lattice points and the number of vertices of
          the 4-dimensional lattice polytope $P$
          which is the convex hull of
          the columns of the matrix $A$"},
        { TEX "'N: 10 9' is the number of lattice points and the number of vertices of
            the polar dual polytope $P^o$ of $P$" },
        { TEX "'H: 5,20 [-30]' are the Hodge numbers $h^{1,1}(X)$, $h^{1,2}(X)$, and the
            topological Euler characteristic of $X$, where $X$ is the 
            Calabi-Yau variety described next"}
        }@
    Text
      Here, $X$ is defined as follows.
      Consider the Fano toric variety
      corresponding to the polytope $P$ (or, equivalently) to the fan 
      determined by the polar dual polytope $P^o$. A fine regular star
      triangulation of $P^o$ defines a refined fan which corresponds
      which corresponds to a simplicial toric variety $V$, 
      such that a generic anti-canonical divisor $X$ is a smooth
      Calabi-Yau 3-fold hypersurface of $V$.  The final numbers 
      are about $X$: "H:5,20 [-30]" says that $h^{1,1}(X) = 5$ and
      $h^{1,2}(X) = 20$.  The topological Euler characteristic of $X$ is the
      number in square brackets:
      $2 h^{1,1}(X) - 2 h^{1,2}(X) = 10 - 40 = -30$.
    Text
      The first 2 integers are the dimensions of the matrix (4 by 10).
    Example
      needsPackage "Polyhedra";
      P = convexHull A
    Text
      $P$ is the convex hull of the columns in the $M = {\bf ZZ}^4$ lattice.
      $P$ has 10 vertices and 25 lattice points, explaining the part of the line
      "M:25 10".
    Example
      LP = latticePoints P
      #LP
      vertices P
      numColumns vertices P
    Example
      P2 = polar P
      LP2 = latticePoints P2
      #LP2
      vertices P2
      numColumns vertices P2
   SeeAlso
     getKreuzerSkarke
///

doc ///
Key
  getKreuzerSkarkeDim3
Headline
  download Kreuzer-Skarke dim 3 reflexive polytopes database of 4319 examples
Usage
  getKreuzerSkarkeDim3()
Outputs
  :String
    The output from the web request (including header information).  
    Use @TO "parseKS"@ to make this into something usable from Macaulay2.
Description
  Text
    As a an example, let's take the 101th example on this list.
  Example
    str = getKreuzerSkarkeDim3();
    polytopes = parseKS str;
    #polytopes
    tope = polytopes_100
    header = tope_0
    A = matrixFromString tope_1
  Text
    The first line gives some information about the example, see @TO "Kreuzer-Skarke headers"@ for more details.
    The polytope is the convex hull of the columns of the matrix $A$.
    
    One can use the packages @TO "Polyhedra::Polyhedra"@ and @TO "NormalToricVarieties::NormalToricVarieties"@
    to investigate these polyhedra, and the associated toric varieties.
  Example
    needsPackage "Polyhedra"
    P = convexHull A
    P2 = polar P
    # latticePoints P
    # latticePoints P2
    # vertices P
    # vertices P2
    isReflexive P
  Example
    needsPackage "NormalToricVarieties"
    V0 = normalToricVariety normalFan P
    dim V0
    max V0
    rays V0
    V = makeSimplicial V0
    isSimplicial V
    isProjective V
    isSmooth V
    dim V
SeeAlso
  getKreuzerSkarke
  matrixFromString
  parseKS
  "Kreuzer-Skarke headers"
///

doc ///
   Key
     parseKS
     (parseKS,String)
   Headline
     parse values from Kreuzer-Skarke database
   Usage
     L = parseKS str
   Inputs
     str:String
       result of a call to @TO "getKreuzerSkarke"@, or
       a string containing examples generated from the database
   Outputs
     L:List
       of pairs of strings
   Description
    Text
      The following request results in 5 examples.
    Example
      str = getKreuzerSkarke(5, 43, Limit=>5)
      L = parseKS str;
      #L
      netList L
    Text
      Let's get the one at index 3 (fourth one on the list, since
      lists are 0-based)
    Example
      eg = L_3;
      header = eg_0
      A = matrixFromString eg_1
    Text
      The input string can also be a string containing examples from the Kreuzer-Skarke list.
      In this case the 'header' line should start in the first character of the line, and
      the matrix part should be indented. Blank lines are ignored.
    Example
      str = "4 5  M:18 5 N:22 5 H:23,17 [12]

        1    0    1    1   -4
        0    1    0    0   -1
        0    0    3    0   -3
        0    0    0    3   -3

        "
      parseKS str
    Text 
      See @TO "Kreuzer-Skarke headers"@
   SeeAlso
     matrixFromString
     getKreuzerSkarke
     "Kreuzer-Skarke headers"
///

doc ///
Key
  generateOffline
  (generateOffline,ZZ)
  (generateOffline,ZZ,ZZ)
Headline
  generate tables of reflexive 4d poytopes from Kreuzer-Skarke list
Usage
  filename = generateOffline(h11,h12)
  filename = generateOffline h11
Inputs
  h11:ZZ
    As in getKreuzerSkarke, the desired $h^{1,1}(X)$ of the associated Calabi-Yau 3-fold
  h12:ZZ
    As in getKreuzerSkarke, the desired $h^{1,2}(X)$ of the associated Calabi-Yau 3-fold
  Limit => ZZ
    Only download at most this many examples
  Prefix => String
    The directory to place the resulting file, should end in a slash, or be the empty string
  Expected => ZZ
    The expected number of examples.  This is tested, and an error is given if the number
    is incorrect.
Outputs
  filename:String
    The name of the file to where the examples are written
Consequences
  Item
    A file is written in the directory given by {\tt prefix}. The name is
    made out of the arguments
Description
  Example
    filename = generateOffline(300, Limit=>1000, Expected=>20, Prefix=>"")
    polytopes = parseKS get filename
Caveat
  This function doesn't take all of the options that getKreuzerSkarke takes,
  and you cannot choose the file name
SeeAlso
  getKreuzerSkarke
///

TEST ///
-*
  restart
*-
  needsPackage "ReflexivePolytopesDB"
  answer = matrix {
      {13, -1, -1, 1, -1, -1, -1, 1, 1, 0}, 
      {0, 1, 0, -1, 1, 0, 0, -1, -1, 0}, 
      {-1, 0, 0, 0, 1, 1, 0, 0, 0, 0}, 
      {2, 0, 0, 1, -1, -1, -1, -1, 0, 0}
      };

  str = "13  -1 -1 1  -1 -1 -1 1  1  0
      0  1  0  -1 1  0  0  -1 -1 0 
     -1 0  0  0  1  1  0  0  0  0 
      2  0  0  1  -1 -1 -1 -1 0  0 
      "
  assert(matrixFromString str == answer) 

  str = "
      13  -1 -1 1  -1 -1 -1 1  1  0
      0  1  0  -1 1  0  0  -1 -1 0 
     -1 0  0  0  1  1  0  0  0  0 
      2  0  0  1  -1 -1 -1 -1 0  0 
      "
  assert(matrixFromString str == answer) 

  str = "
      13  -1 -1 1  -1 -1 -1 1  1  0
      0  1  0  -1 1  0  0  -1 -1 0 
     -1 0  0  0  1  1  0  0  0  0 
      2  0  0  1  -1 -1 -1 -1 0  0                  "
  assert(matrixFromString str == answer) 

  -- commas are equivalent to spaces, tabs
  str = "
      13,  -1 -1 1  -1 -1 -1 1  1,  0,
      0  1  0  -1 1  0  0  -1 -1 0 
     -1 0  0  0  1  1  0  0  0  0 
      2  0  0  1  -1 -1 -1 -1 0  0 
      "
  assert(matrixFromString str == answer) 
///


TEST ///
-*
  restart
*-
  needsPackage "ReflexivePolytopesDB"
  answer = matrix {
      {13, -1, -1, 1, -1, -1, -1, 1, 1, 0}, 
      {0, 1, 0, -1, 1, 0, 0, -1, -1, 0}, 
      {-1, 0, 0, 0, 1, 1, 0, 0, 0, 0}, 
      {2, 0, 0, 1, -1, -1, -1, -1, 0, 0}
      };
  
  str = "[[13, -1, -1, 1, -1, -1, -1, 1, 1, 0], [0, 1, 0, -1, 1, 0, 0, -1, -1, 0], [-1, 0, 0, 0, 1, 1, 0, 0, 0, 0], [2, 0, 0, 1, -1, -1, -1, -1, 0, 0]]"
  assert(matrixFromString str == answer) 

  str = "[
  [13, -1, -1, 1, -1, -1, -1, 1, 1, 0], [0, 1, 0, -1, 1, 0, 0, -1, -1, 0], [-1, 0, 0, 0, 1, 1, 0, 0, 0, 0], [2, 0, 0, 1, -1, -1, -1, -1, 0, 0]
  ]
"
  assert(matrixFromString str == answer) 

  str = "
  [[       13,-1, -1, 1, -1, -1, -1, 1, 1, 0], [0, 1, 0, -1, 1, 0, 0, -1, -1, 0], [-1, 0, 0, 0, 1, 1, 0, 0, 0, 0], [2, 0, 0, 1, -1, -1, -1, -1, 0, 0]]   "
  assert(matrixFromString str == answer) 
///

TEST ///
-*
  restart
*-
  needsPackage "ReflexivePolytopesDB"
  answer = matrix {
      {13, -1, -1, 1, -1, -1, -1, 1, 1, 0}, 
      {0, 1, 0, -1, 1, 0, 0, -1, -1, 0}, 
      {-1, 0, 0, 0, 1, 1, 0, 0, 0, 0}, 
      {2, 0, 0, 1, -1, -1, -1, -1, 0, 0}
      };

  str = "
      13  -1 -1 1  -1 -1 -1 1  1  0
      0  1  0  -1 1  0  0  -1 -1 0 
     -1 0  0  0  1  1  0  0  0  0 
      2  0  0  1  -1 -1 -1 -1 0  0 
      "
  assert(matrixFromString str == answer) 

  str = "
      13  -1 -1 1  -1 -1 -1 1  1  0
      0  1  0  -1 1  0  0  -1 -1 0 
     -1 0  0  0  1  1  0  0  0  0 
      2  0  0  1  -1 -1 -1 -1 0  0                  "
  assert(matrixFromString str == answer) 

  -- commas are equivalent to spaces, tabs
  str = "
      13,  -1 -1 1  -1 -1 -1 1  1,  0,
      0  1  0  -1 1  0  0  -1 -1 0 
     -1 0  0  0  1  1  0  0  0  0 
      2  0  0  1  -1 -1 -1 -1 0  0 
      "
  assert(matrixFromString str == answer) 
///

TEST ///
  -- yet another set of tests of matrixFromString
  str1 = "              [ 1 -1 -1  1 -1 -1 -1  1  1  0]
              [ 0  1  0 -1  1  0  0 -1 -1  0]
              [-1  0  0  0  1  1  0  0  0  0]
              [ 2  0  0  1 -1 -1 -1 -1  0  0]"
  str2 = "            [[1, 0, -1, 2], [-1, 1, 0, 0], [-1, 0, 0, 0], [1, -1, 0, 1], [-1, 1, 1, -1], [-1, 0, 1, -1], [-1, 0, 0, -1], [1, -1, 0, -1]]    "
  str3 =         "         1   0   0   0   1   1   0  -1  -1  -2  -4
                  0   1   1   0  -2   2   3  -1  -4   1  -1
                  0   0   2   0  -2   4   4  -1  -4  -2  -4
                  0   0   0   1   0  -2  -2   2   2   0   2"
  str4 =         "         1   0   0   0   1   1   0  -1  -1  -2  -4
                  0   1   1   0  -2   2   3  -1  -4   1  -1
                  0   0   2   0  -2   4   4  -1  -4  -2  -4
                  0   0   0   1   0  -2  -2   2   2   0   2  
                  "

  assert(
      matrixFromString str1 
      == 
      matrix {
          {1, -1, -1, 1, -1, -1, -1, 1, 1, 0}, 
          {0, 1, 0, -1, 1, 0, 0, -1, -1, 0}, 
          {-1, 0, 0, 0, 1, 1, 0, 0, 0, 0}, 
          {2, 0, 0, 1, -1, -1, -1, -1, 0, 0}}
      )

  assert(
      matrixFromString str2
      ==
      matrix {{1, 0, -1, 2}, {-1, 1, 0, 0}, {-1, 0, 0, 0}, {1, -1, 0, 1}, {-1, 1, 1, -1}, {-1, 0, 1, -1}, {-1, 0, 0, -1}, {1, -1, 0, -1}}
      )

  assert(
      matrixFromString str3
      ==
      matrix {
          {1, 0, 0, 0, 1, 1, 0, -1, -1, -2, -4}, 
          {0, 1, 1, 0, -2, 2, 3, -1, -4, 1, -1}, 
          {0, 0, 2, 0, -2, 4, 4, -1, -4, -2, -4}, 
          {0, 0, 0, 1, 0, -2, -2, 2, 2, 0, 2}}
      )
  
  assert(
      matrixFromString str4
      ==
      matrix {
          {1, 0, 0, 0, 1, 1, 0, -1, -1, -2, -4}, 
          {0, 1, 1, 0, -2, 2, 3, -1, -4, 1, -1}, 
          {0, 0, 2, 0, -2, 4, 4, -1, -4, -2, -4}, 
          {0, 0, 0, 1, 0, -2, -2, 2, 2, 0, 2}
          }
      )
///


TEST ///
  debug needsPackage "ReflexivePolytopesDB"
  -- testing retrieval and parsing for the 4D polytope case, using 
  str = getKreuzerSkarke(3, Limit=>10);
  str1 = getKreuzerSkarke(3, Limit=>10, Access=>"curl");
  str2 = getKreuzerSkarke(3, Limit=>10, Access=>"wget");
  assert(h11'3'limit'10  == str)
  assert(str == str1)
  assert(str == str2)
///

TEST ///
-- TODO: turn this into asserts.
  needsPackage "ReflexivePolytopesDB"
  getKreuzerSkarke(100, Limit=>10)
  getKreuzerSkarke(100, LatticePoints=>9, Limit=>3)
  getKreuzerSkarke(100, DualLatticePoints=>129, Limit=>3)
  getKreuzerSkarke(100, H12=>4, DualLatticePoints=>129, Limit=>3)
  getKreuzerSkarke(100, H12=>4, Facets=>9, Limit=>3)
///

TEST ///
  needsPackage "ReflexivePolytopesDB"
  str = getKreuzerSkarkeDim3();
  polytopes3 = parseKS str;
  assert(#polytopes3 == 4319)
  polytopes3_10
  assert(matrixFromString polytopes3_10_1 == id_(ZZ^3) | matrix{{-6},{-4},{-1}})
///

end--

restart
uninstallPackage "ReflexivePolytopesDB"
restart
needsPackage "ReflexivePolytopesDB"
installPackage "ReflexivePolytopesDB"
viewHelp

restart
check "ReflexivePolytopesDB"

-- Use the following lines to generate some offline files
///
-*
restart
*-
    needsPackage "ReflexivePolytopesDB"
    prefix = "./ReflexivePolytopesDB/"
    generateOffline(3,Limit=>250,Expected=>244,Prefix=>prefix);
    generateOffline(4,Limit=>2000,Expected=>1197,Prefix=>prefix);
    generateOffline(5,Limit=>10000,Expected=>4990,Prefix=>prefix);
    generateOffline(11,24,Expected=>200,Prefix=>prefix);
    generateOffline(491,Expected=>1,Prefix=>prefix);
    generateOffline(23,11,Limit=>1000,Prefix=>prefix);
    generateOffline(7,50,Limit=>2000,Expected=>590,Prefix=>prefix);
    -- the following are pretty big files
    generateOffline(6,Limit=>20000,Expected=>17101,Prefix=>prefix);    
///

-- see also https://arxiv.org/pdf/math/0406485.pdf