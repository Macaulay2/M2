newPackage(
        "KreuzerSkarke",
        Version => "0.1", 
        Date => "25 June 2017",
        Authors => {{
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"http://www.math.cornell.edu/~mike"
                }},
        Headline => "simple access to Kreuzer-Skarke database of reflexive polytopes",
        AuxiliaryFiles => true,
        DebuggingMode => true
        )

export {
    "exampleFromKS",
    "matrixFromString", -- doc
    "getKreuzerSkarke", -- doc
    "getKreuzerSkarkeDim3",
    "parseKS",
    "parseKSDim3",
    "Access",
    "Expected"
    }

exampleFromKS = method()
exampleFromKS(ZZ, String) := (which, str) -> (
    L := parseKS str;
    eg := L#which;
    (eg#0, matrixFromString eg#1)
    )

matrixFromString = method()
matrixFromString String := (str) -> (
    -- expect input in one of the three forms:
    {*
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
    *}
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

getKreuzerSkarke = method(Options=>{Limit=>1000, Access=>"m2"})
getKreuzerSkarke(ZZ,ZZ) := opts -> (h11,h12) -> (
    --str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?M=&V=&N=&F=&h11="|toString h11|"&h12="|toString h12|"&chi=&L="|toString opts.Limit;
    str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?h11="|toString h11|"&h12="|toString h12|"&L="|toString opts.Limit;
    getURL(str, opts.Access)
    )
getKreuzerSkarke(ZZ) := opts -> (h11) -> (
    --str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?M=&V=&N=&F=&h11="|toString h11|"&h12="|toString h12|"&chi=&L="|toString opts.Limit;
    str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?h11="|toString h11|"&L="|toString opts.Limit;
    getURL(str, opts.Access)
    )

getKreuzerSkarkeDim3 = () -> (
    --str := "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?M=&V=&N=&F=&h11="|toString h11|"&h12="|toString h12|"&chi=&L="|toString opts.Limit;
    str := "http://hep.itp.tuwien.ac.at/~kreuzer/pub/K3/RefPoly.d3";
    getURL(str, "m2")
    )

parseKS = method()
parseKS String := (str) -> (
    -- result is a List of strings.
    locA := regex("<b>Result:</b>\n", str);
    locB := regex("#NF", str);
    if locB === null then locB = regex("Exceeded", str);
    if locA === null or locB === null then error "data not in correct Kreuzer-Skarke format";
    firstloc := locA#0#0 + locA#0#1;
    lastloc := locB#0#0;
    cys := substring(firstloc, lastloc-firstloc, str);
    cys = lines cys;
    starts := positions(cys, s -> s#0 != " ");
    starts = append(starts, #cys);
    for i from 0 to #starts-2 list (
        cys_(starts#i), demark("\n", cys_{starts#i+1 .. starts#(i+1)-1})
        )
    )

parseKSDim3 = method()
parseKSDim3 String := (str) -> (
    -- result is a List of strings.
    cys := lines str;
    starts := positions(cys, s -> s#0 != " ");
    starts = append(starts, #cys);
    for i from 0 to #starts-2 list (
        cys_(starts#i), demark("\n", cys_{starts#i+1 .. starts#(i+1)-1})
        )
    )

generateOffline = method(Options=>{Expected=>null, Limit=>1000, Prefix=>"KreuzerSkarke/"})
generateOffline(ZZ,ZZ) := opts -> (h11, h12) -> (
    contents := getKreuzerSkarke(h11, h12, Limit=>opts.Limit);
    L := parseKS contents;
    nexpected := opts.Expected;
    if instance(nexpected,ZZ) and  #L =!= nexpected then 
      error ("expected "|nexpected|" examples, obtained "|#L);
    filename := opts.Prefix | "ks"|h11|"+"|h12|"-n"|#L|".txt";
    << "writing file " << filename << endl;
    filename << contents << close;
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
    )

generateOfflineFiles = () -> (
    -- generates these files, placing them inside the KreuzerSkarke directory in the current
    -- directory.  This directory must exist already.
    -- For each file, we do some sanity checking before writing the file.
    generateOffline(3,Limit=>250,Expected=>244);
    generateOffline(4,Limit=>2000,Expected=>1197);
    generateOffline(5,Limit=>10000,Expected=>4990);
    generateOffline(11,24,Expected=>200);
    generateOffline(491,Expected=>1);
    generateOffline(23,11,Limit=>1000);
    generateOffline(7,50,Limit=>2000,Expected=>590);
    -- the following are pretty big files
    generateOffline(6,Limit=>20000,Expected=>17101);    
    )

-- the following line was generated using
-- getKreuzerSkarke(3, Limit=>10)
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
  KreuzerSkarke
Headline
  simple access to Kreuzer-Skarke database of reflexive polytopes
Description
  Text
    This package provides access to the Kreuzer-Skarke database of
    reflexive polytops of dimension 3 and dimension 4.
    
    This package also contains a small part of this database for offline use,
    in case one cannot access the database.
  Text
    Here we describe a simple use of the package.  The actual
    investigation of the corresponding polytope or toric variety, or Calabi-Yau
    hypersurface, is done in other packages.
    
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
    The corresponding relfexive polytope has 5 vertices, the columns of this matrix.
  Example
    needsPackage "Polyhedra"
    P = convexHull A
    isReflexive P
    P2 = polar P
    (numColumns vertices P, numColumns vertices P2)
    (# latticePoints P, # latticePoints P2)
  Text
    or, once one has the data from the Kreuzer-Skare database ('str' above),
    one can do the following.
  Example
    (header, A) = exampleFromKS(1, str)
    
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
  access kreuzer-skarke dim 4 reflexive polytopes database
Usage
  getKreuzerSkarke(h11, Limit=>n)
  getKreuzerSkarke(h11, h12, Limit=>n)
Inputs
  h11:ZZ
  h12:ZZ
  n:ZZ
  Limit => ZZ
    The maximum number of examples to retrieve
Outputs
  :String
Description
  Text
  Example
    str = getKreuzerSkarke(13, 17, Limit=>10)
    L = parseKS str
    #L
    eg = L_0
    A = matrixFromString last eg
    first eg
    A
  Text
    The first line gives some information about the example.
    Let $\Delta$ be the convex hull of the 
    For example, this line says that the matrix is a 4 by 7 matrix
  Example
    first eg
  Example
    A = matrixFromString last eg
  Example
    needsPackage "Polyhedra"
    P = convexHull A
    # latticePoints P
    numColumns vertices P
    P2 = polar P
    # latticePoints P2
    numColumns vertices P2
  Text
    
  Example
    getKreuzerSkarke(13, 17)
Caveat
SeeAlso
  parseKS
  matrixFromString
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
   Caveat
   SeeAlso
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
       result of a call to @TO "getKreuzerSkarke"@
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
      See @TO "Kreuzer-Skarke headers"@
   SeeAlso
     matrixFromString
     getKreuzerSkarke
     "Kreuzer-Skarke headers"
///

TEST ///
{*
  restart
*}
  needsPackage "KreuzerSkarke"
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
{*
  restart
*}
  needsPackage "KreuzerSkarke"
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
{*
  restart
*}
  needsPackage "KreuzerSkarke"
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
  debug needsPackage "KreuzerSkarke"
  -- testing retrieval and parsing for the 4D polytope case, using 
  str = getKreuzerSkarke(3, Limit=>10);
  str1 = getKreuzerSkarke(3, Limit=>10, Access=>"curl");
  str2 = getKreuzerSkarke(3, Limit=>10, Access=>"wget");
  assert(h11'3'limit'10  == str)
  assert(str == str1)
  assert(str == str2)
///

end--

restart
uninstallPackage "KreuzerSkarke"
restart
needsPackage "KreuzerSkarke"
installPackage "KreuzerSkarke"
viewHelp

restart
check "KreuzerSkarke"

restart
debug needsPackage "KreuzerSkarke"
generateOfflineFiles()


doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///
