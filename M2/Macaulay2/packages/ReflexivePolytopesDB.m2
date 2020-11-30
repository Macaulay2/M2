-- TODO:
--   document:
--   KSEntry functions
--   availableOffline
--   find bad links
--   check in the ks files

newPackage(
        "ReflexivePolytopesDB",
        Version => "1.0", 
        Date => "22 May 2019",
        Authors => {{
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"http://www.math.cornell.edu/~mike"
                }},
        Headline => "simple access to Kreuzer-Skarke database of reflexive polytopes of dimensions 3 and 4",
	Keywords => {"Convex Geometry"},
        AuxiliaryFiles => true
        )

-- local, but could perhaps be exported:
--    "parseKSRawString"
--    "retrieve"
--     "kreuzerSkarkeURL"

export {
    -- new functionality
    "KSEntry",
    "description",
    "kreuzerSkarke",
    "kreuzerSkarkeDim3", -- so so name...
    "availableOffline",
    "onlineTests",
    -- internal functions that perhaps should not be exported:
    "matrixFromString",
    "generateOffline",
    -- Option names
    "Access",
    "Expected",
    "Vertices",
    "Facets",
    "LatticePoints",
    "DualLatticePoints",
    "H12"
    }

description = method()

KSEntry = new SelfInitializingType of BasicList
KSEntry.synonym = "entry from the Kreuzer-Skarke database"
new KSEntry from String := (KSEntry,str) -> new KSEntry from {str}
expression KSEntry := entry -> entry#0
net KSEntry := entry -> net entry#0
toString KSEntry := entry -> entry#0
toExternalString KSEntry := entry -> "KSEntry " | toExternalString entry#0
matrix KSEntry := opts -> entry -> matrixFromString concatenate between("\n", drop(lines entry#0, 1))
description KSEntry := String => entry -> first lines entry#0

kreuzerSkarkeURL = method(Options => {
        Limit => 1000, 
        LatticePoints => null,
        DualLatticePoints => null,
        Vertices => null,
        Facets => null,
        H12 => null
        })

kreuzerSkarkeURL ZZ := URL => opts -> (h11) -> (
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
    URL concatenate between("&", prepend(str,ksopts))
    )
kreuzerSkarkeURL(ZZ, ZZ) := URL => opts -> (h11, h12) -> kreuzerSkarkeURL(h11, opts, H12 => h12)

retrieve = method(Options => {Access => null})
retrieve URL := String => opts -> url -> (
    access := opts.Access;
    str := url#0;
    if debugLevel > 0 then << "URL: " << str << endl;
    if access === null then
      last splitWWW getWWW str
    else if access === "curl" then
      get ("!curl \""|str|"\"")
    else if access === "wget" then 
      get ("!wget \""|str|"\" -O -")
    else error "expected Access argument to be \"m2\" (default), \"curl\", or \"wget\""
    )

parseKSRawString = method()
parseKSRawString String := List => (str) -> (
    -- result is a List of KSEntry's: each has first line as a description, rest of the lines a matrix,
    locA := regex("<b>Result:</b>\n", str);
    locB := regex("#NF", str);
    if locB === null then locB = regex("Exceeded", str);
    firstloc := if locA === null then 0 else locA#0#0 + locA#0#1;
    lastloc := if locB === null then #str else locB#0#0;
    cys := substring(firstloc, lastloc-firstloc, str);
    cys = lines cys;
    cys = select(cys, s -> #s > 0);
    starts := positions(cys, s -> s#0 != " ");
    starts = append(starts, #cys);
    for i from 0 to #starts-2 list (
        KSEntry(cys_(starts#i) | " id:" | i | "\n" |  demark("\n", cys_{starts#i+1 .. starts#(i+1)-1}))
        )
    )

offlines = hashTable {
      1 => "ks1-n5.txt",
      2 => "ks2-n36.txt",
      3 => "ks3-n244.txt",
      5 => (50, "ks5-n50.txt"),
      (5, 53) => "ks5+53-n204.txt",
      (9, 21) => "ks9+21-n10.txt",
      (11, 24) => "ks11+24-n200.txt",
      21 => (100, "ks21-n100.txt"),
      300 => "ks300-n20.txt",
      491 => "ks491-n1.txt"
      }

offlineDirectory = currentFileDirectory | "ReflexivePolytopesDB/"

availableOffline = () -> (
    hashTable for k in keys offlines list (
        val := offlines#k;
        (maxlimit, filename) := if instance(val, Sequence) then val else (infinity, val);
        actualfile := offlineDirectory | filename;
        fcncall := if instance(k, ZZ) then (
          "kreuzerSkarke("|k|
            (if maxlimit === infinity then ")" else ", Limit => "|maxlimit|")")
          ) else (
          "kreuzerSkarke("|k#0|","|k#1 |
            (if maxlimit === infinity then ")" else ", Limit => "|maxlimit|")")
          );
       fcncall => actualfile
      )
  )

findOffline = method(Options => options kreuzerSkarkeURL)
findOffline ZZ := opts -> (h11) -> (
    if 0 =!= # select(keys opts, k -> opts#k =!= null and k =!= Limit and k != H12)
    then null
    else (
        k := if opts.H12 === null then h11 else (h11, opts.H12);
        if offlines#?k then (
            (maxlimit, filename) := if instance(offlines#k, Sequence) then
                                        offlines#k
                                    else
                                        (infinity, offlines#k);
            if opts.Limit > maxlimit then return null;
            << "using offline data file: " << filename << endl;
            rawstr := get (offlineDirectory | filename);
            ans := parseKSRawString rawstr;
            if #ans > opts.Limit then ans = take(ans, opts.Limit);
            ans
            )
    ))

kreuzerSkarke = method(Options => join(pairs options kreuzerSkarkeURL, {Access=>null}))
kreuzerSkarke ZZ := List => opts -> h11 -> (
    optsA := new OptionTable from select(pairs opts, k -> k#0 =!= Access);
    offlineResult := findOffline(h11, optsA);
    if offlineResult =!= null then return offlineResult;
    url := kreuzerSkarkeURL(h11, optsA);
    rawstr := retrieve(url, Access=>opts.Access);
    parseKSRawString rawstr
    )

kreuzerSkarke(ZZ, ZZ) := List => opts -> (h11, h12) -> kreuzerSkarke(h11, opts, H12 => h12)

kreuzerSkarke String := List => opts -> rawstr -> parseKSRawString rawstr

kreuzerSkarkeDim3 = () -> (
    parseKSRawString get (offlineDirectory | "ks-dim3.txt")
    --url := URL "http://hep.itp.tuwien.ac.at/~kreuzer/pub/K3/RefPoly.d3";
    --rawstr := retrieve url;
    --parseKSRawString rawstr
    )

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

TEST ///
-*
  needsPackage "ReflexivePolytopesDB"
*-
  str = "4 5  M:53 5 N:9 5 H:3,43 [-80] id:0
   1   0   2   4 -10
   0   1   3   5  -9
   0   0   4   0  -4
   0   0   0   8  -8
   "
  assert(matrix first kreuzerSkarke str == matrix {
          {1, 0, 2, 4, -10}, 
          {0, 1, 3, 5, -9}, 
          {0, 0, 4, 0, -4}, 
          {0, 0, 0, 8, -8}
          })
///

generateOffline = method(Options=>{Expected=>null, Limit=>1000, Prefix=>"./", FileName => null})
generateOffline(ZZ,ZZ) := opts -> (h11, h12) -> (
    url := kreuzerSkarkeURL(h11, h12, Limit => opts.Limit);
    contents := retrieve url;
    L := parseKSRawString contents;
    nexpected := opts.Expected;
    if instance(nexpected,ZZ) and  #L =!= nexpected then 
      error ("expected "|nexpected|" examples, obtained "|#L);
    filename := if opts.FileName =!= null then opts.FileName else
                   opts.Prefix | "ks"|h11|"+"|h12|"-n"|#L|".txt";
    << "writing file " << filename << endl;
    filename << contents << close;
    filename
    )
generateOffline ZZ := opts -> (h11) -> (
    url := kreuzerSkarkeURL(h11, Limit => opts.Limit);
    contents := retrieve url;
    L := parseKSRawString contents;
    nexpected := opts.Expected;
    if instance(nexpected,ZZ) and  #L =!= nexpected then 
      error ("expected "|nexpected|" examples, obtained "|#L);
    filename := if opts.FileName =!= null then opts.FileName else
                  opts.Prefix|"ks"|h11|"-n"|#L|".txt";
    << "writing file " << filename << endl;
    filename << contents << close;
    filename
    )

///
  -- Generating the offline files
  restart
  needsPackage "ReflexivePolytopesDB"
  generateOffline(1, Expected=>5)
  first kreuzerSkarkeURL(1, Limit=>5)
  kreuzerSkarke get "ks1-n5.txt"
  
  generateOffline 2
  generateOffline 3
  generateOffline(5, 53)
  generateOffline 491
  generateOffline(11, 24, Expected=>200)
  urlPrefix = "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?"

  generateOffline(9, 21)
///

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

onlineTests = () -> (
    << "trying tests which require being online" << endl;
    -- testing retrieval and parsing for the 4D polytope case, using 
    offlineL := kreuzerSkarke(3, Limit => 10);
    L := kreuzerSkarke retrieve kreuzerSkarkeURL(3, Limit=>10);
    L1 := kreuzerSkarke retrieve(kreuzerSkarkeURL(3, Limit=>10), Access=>"curl");
    L2 := kreuzerSkarke retrieve(kreuzerSkarkeURL(3, Limit=>10), Access=>"wget");
    assert(offlineL  === L);
    assert(L === L1);
    assert(L === L2);

    assert(10 === # kreuzerSkarke(100, Limit=>10));
    assert(3 === # kreuzerSkarke(100, LatticePoints=>9, Limit=>3)); -- there are 7 total, but that takes a while...
    -- kreuzerSkarke(100, LatticePoints=>9); -- this actually takes a while...
    assert(3 === # kreuzerSkarke(100, DualLatticePoints=>129, Limit=>3));
    assert(3 === # kreuzerSkarke(100, H12=>4, DualLatticePoints=>129, Limit=>3));
    assert(4 === # kreuzerSkarke(100, H12=>4, Facets=>9, Limit=>4));
    << "the online tests all passed!" << endl;
    )

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
    dimension 1 there is 1, in dimension 2, there are 16, in
    dimension 3, there are 4319 distinct reflexive polytopes.

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
    such as @TO "Polyhedra::Polyhedra"@ and @TO "NormalToricVarieties::NormalToricVarieties"@.
    
    Let's take one example polytope from the database, one whose corresponding Calabi-Yau
    3-fold has Hodge numbers $h^{1,1}(X) = 9$ and $h^{1,2}(X) = 21$.
  Example
    topes = kreuzerSkarke(9, 21);
  Text
    This returns a list of single entries from the Kreuzer-Skarke database.  Each one is
    essentially a string, containing a description line, together with the vertices of the
    corresponding polytope.
    
    In Macaulay2, each entry is an object of class @TO "KSEntry"@ (meaning: Kreuzer-Skarke database 
    entry).  Use @TO (matrix,KSEntry)@ to create the matrix whose columns are the vertices 
    of the reflexive polytope.  Use @TO (description, KSEntry)@ to see the associated description from the
    database (see @TO "Kreuzer-Skarke description headers"@ for the description of the format of this description).
  Example
    topes_1
    A = matrix topes_1
    description topes_1
  Text
    The corresponding reflexive polytope has 7 vertices, the columns of this matrix.
  Example
    needsPackage "Polyhedra"
    P = convexHull A
    assert isReflexive P
    P2 = polar P
    (numColumns vertices P, numColumns vertices P2)
    (# latticePoints P, # latticePoints P2)
  Text
    We can process many examples at one time, using the list facilities in Macaulay2.
    For instance,
    use @TO "Macaulay2Doc::List / Function"@ to apply {\tt matrix} to each element of the
    list, returning a list of the resulting matrices:
  Example
    L = topes/matrix;
    netList L
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
  (matrix, KSEntry)
Headline
  convert a Kreuzer-Skarke entry to a matrix of integers
Usage
  m = matrix e
Inputs
  e:KSEntry
Outputs
  m:Matrix
    The matrix over the integers whose columns are the vertices of the corresponding reflexive polytope
Description
  Text
    This utility function is used to parse, as matrices of integers, example polytopes
    returned by the Kreuzer-Skarke database.
  Example
    e = KSEntry "4 5  M:53 5 N:9 5 H:3,43 [-80] id:0
      1   0   2   4 -10
      0   1   3   5  -9
      0   0   4   0  -4
      0   0   0   8  -8
      ";
    A = matrix e
  Text
    The actual format allowed is the following: The first non-empty line is ignored (this 
    is the description line from the Kreuzer-Skarke database), the rest of the lines make up the matrix:
    spaces, tabs, and
    commas are all separators for elements of the array.  Newlines,
    and ] characters separate rows.  Finally, each empty line is
    ignored, and the remaining lines must all have the same number of
    elements.
    
    The actual format allowed for matrices is described in @TO "matrixFromString"@.  
    After calling @TO "kreuzerSkarke"@ to get a list of @TO "KSEntry"@'s, one uses @TO (matrix, KSEntry)@
    to obtain a matrix .  Then
    use this function to obtain the matrix.
  Example
    topes = kreuzerSkarke(300, Limit=>3)
    netList topes
    topes/matrix
SeeAlso
  matrixFromString
  kreuzerSkarke
  kreuzerSkarkeDim3
///

doc ///
Key
  kreuzerSkarke
  (kreuzerSkarke, ZZ)
  (kreuzerSkarke, ZZ, ZZ)
  (kreuzerSkarke, String)
  Vertices
  Facets
  LatticePoints
  DualLatticePoints
  H12
  Access
Headline
  access Kreuzer-Skarke dimension 4 reflexive polytopes database
Usage
  kreuzerSkarke(h11, Limit=>n)
  kreuzerSkarke(h11, h12)
  kreuzerSkarke str
Inputs
  h11:ZZ
    The desired Picard number of the Calabi-Yau hypersurface, a number between 1 and 491 inclusive.
  h12:ZZ
    (optional) the desired $h^{1,2}$ of the Calabi-Yau hypersurface
  Limit => ZZ
    The maximum number of examples to retrieve
  str:String
    parses the given string into a list of KSEntry's
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
  Access => String
    Either null (use Macaulay2's getWWW), "wget" or "curl".  Mainly an internal optional argument for developers
Outputs
  :List
    of @TO "KSEntry"@'s, each of which is one entry (one polytope) from the database.
    Use @TO (matrix, KSEntry)@ and @TO (description, KSEntry)@ to extract information from an entry.
Description
  Text
    As a an example, let's take the 4th example with $h^{11}=5$, $h^{21}=53$.
  Example
    topes = kreuzerSkarke(5, 53, Limit=>4)
    assert(#topes == 4)
    tope = topes_3
    header = description tope
    A = matrix tope
  Text
    The description line gives some information about the example, see @TO "Kreuzer-Skarke description headers"@ for more details.
    The polytope is the convex hull of the columns of the matrix $A$.
  Text
    Some of the Kreuzer-Skarke data is available from this package off-line.  To see what is
    available when not connected to the net, or when the website is down, call
    @TO "availableOffline"@.
  Example
    availableOffline()
  Text
    If data can be obtained locally, it will, and a message indicating this is output.
  Text
    If one has a string containing Kreuzer-Skarke entries, one can use this function to
    make a list of @TO "KSEntry"@'s
  Example
    str = "4 12  M:34 12 N:17 8 H:17,30 [-26] id:0
      1   1   1   1   0   0   0  -3  -3  -1  -1  -3
      0   3   0   1   0   1   0  -3  -5   0  -1  -3
      0   0   2   2   0   0   1  -2  -2  -1  -1  -2
      0   0   0   0   1   1   1  -1  -1   1   1   1"
    L = kreuzerSkarke str
    matrix first L
Caveat
  The database is organized by the {\tt h11} value.  Placing filters can significantly slow down the 
  database access.  Using any of the optional arguments other than {\tt Limit} also
  means that the locally stored datafiles will not be used.
SeeAlso
  (matrix, KSEntry)
  (description, KSEntry)
  "Kreuzer-Skarke description headers"
  kreuzerSkarkeDim3
///

doc ///
Key
  kreuzerSkarkeDim3
Headline
  the list of 4319 dimension 3 reflexive polytopes in the Kreuzer-Skarke database
Usage
  kreuzerSkarkeDim3()
Outputs
  :List
    A list of @TO "KSEntry"@'s, each one representing one of the 4319 polytopes
    (including description header information).  
    Use @TO (matrix, KSEntry)@ to make any element in this list into something usable from Macaulay2.
Description
  Text
    As a an example, let's take the 101th example on this list.
  Example
    topes = kreuzerSkarkeDim3();
    #topes
    tope = topes_100
    header = description tope
    A = matrix tope
  Text
    The first line gives some information about the example, see @TO "Kreuzer-Skarke description headers"@ for more details.
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
  kreuzerSkarke
  (matrix, KSEntry)
  (description, KSEntry)
  "Kreuzer-Skarke description headers"
///

doc ///
   Key
     "Kreuzer-Skarke description headers"
   Headline
     information contained in the description line
   Description
    Text
      Each 4D reflexive polytope in the Kreuzer-Skarke database
      contains summary information about the polytope.
      Here, we explain this information.  A 3D polytope description line
      is similar, but somehwat simpler.
      
      We will do this on an example, and see how to
      obtain this information directly.
    Example
      topes = kreuzerSkarke(5,Limit=>1);
      A = matrix topes_0
      header = description topes_0
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
            Calabi-Yau variety described next"},
        { TEX "'id: 0' is added by {\tt kreuzerSkarke}: each retrieved entry gets an id, starting with 0"}
        }@
    Text
      Here, $X$ is defined as follows.
      Consider the Fano toric variety
      corresponding to the polytope $P$ (or, equivalently) to the fan 
      determined by the polar dual polytope $P^o$. A fine regular star
      triangulation of $P^o$ defines a refined fan
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
      $P$ is the convex hull of the columns in the $M = \ZZ^4$ lattice.
      $P$ has 10 vertices and 25 lattice points, explaining the part of the line
      "M:25 10".
    Example
      LP = latticePoints P
      #LP
      vertices P
      numColumns vertices P
    Text
      $P_2$ is the polar dual of $P$ in the $N = \ZZ^4$ lattice.
      $P_2$ has 9 vertices and 10 lattice points, explaining the part of the line
      "N:10 9".
    Example
      P2 = polar P
      LP2 = latticePoints P2
      #LP2
      vertices P2
      numColumns vertices P2
   SeeAlso
     kreuzerSkarke
     KSEntry
     (description, KSEntry)
///

undocumented (expression, KSEntry)
undocumented (net, KSEntry)
undocumented (NewFromMethod, KSEntry, String)

doc ///
Key
  KSEntry
Headline
  an entry from the Kreuzer-Skarke database of dimension 3 and 4 reflexive polytopes
Description
  Text
    A {\tt KSEntry} is essentially a string containing a description header and the vertices
    of a reflexive polytope.  We have given it its own type for ease of use in Macaulay2.

  Text
    The most common way to obtain (lists of) KSEntry's is via a call 
    @TO "kreuzerSkarke"@ or @TO "kreuzerSkarkeDim3"@.
  Example
    L = kreuzerSkarke(3, Limit=>5)
    netList L
  Text
    Once one has a KSEntry, the most important function is to extract the matrix whose
    columns are the vertices of the corresponding polytope.
  Example
    A = matrix L_0
  Text
    Although the most common way to create (lists of) KSEntry's is via a call to
    @TO "kreuzerSkarke"@ or @TO "kreuzerSkarkeDim3"@ as above,
    it is also possible to start with a string that perhaps you found 
    while searching the Kreuzer-Skarke database directly.
  Example
    e = KSEntry "4 5  M:53 5 N:9 5 H:3,43 [-80] id:0
      1   0   2   4 -10
      0   1   3   5  -9
      0   0   4   0  -4
      0   0   0   8  -8
      ";
    A = matrix e
  Text
    For developers, a KSEntry is a @TO "SelfInitializingType"@.
SeeAlso
  (toString, KSEntry)
  (toExternalString, KSEntry)
  (matrix, KSEntry)
  (description, KSEntry)
  kreuzerSkarke
///

doc ///
Key
  description
  (description, KSEntry)
Headline
  the description header
Usage
  description e
Inputs
  e:KSEntry
Outputs
  :String
Description
  Text
    Returns the description header of the given entry from the Kreuzer-Skarke database.
    This is simply the first line of the given entry.
  Example
    e = KSEntry "4 5  M:53 5 N:9 5 H:3,43 [-80] id:0
      1   0   2   4 -10
      0   1   3   5  -9
      0   0   4   0  -4
      0   0   0   8  -8
      ";
    description e
    A = matrix e
SeeAlso
  KSEntry
  (matrix,KSEntry)
///

doc ///
Key
  (toString, KSEntry)
Headline
  the underlying string of a KSEntry
Usage
  toString e
Inputs
  e:KSEntry
Outputs
  :String
Description
  Text
    Returns the underlying string of the given entry from the Kreuzer-Skarke database.
  Example
    e = KSEntry "4 5  M:53 5 N:9 5 H:3,43 [-80] id:0
      1   0   2   4 -10
      0   1   3   5  -9
      0   0   4   0  -4
      0   0   0   8  -8
      ";
    toString e
SeeAlso
  KSEntry
  (matrix,KSEntry)
  (toExternalString, KSEntry)
///

doc ///
Key
  (toExternalString, KSEntry)
Headline
  a string suitable for input to Macaulay2
Usage
  toExternalString e
Inputs
  e:KSEntry
Outputs
  :String
Description
  Text
    Returns the underlying string of the given entry from the Kreuzer-Skarke database.
  Example
    e = KSEntry "4 5  M:53 5 N:9 5 H:3,43 [-80] id:0
      1   0   2   4 -10
      0   1   3   5  -9
      0   0   4   0  -4
      0   0   0   8  -8
      ";
    str = toExternalString e
  Text
    The main value of this function, is that one can use this string as input to Macaulay2.
  Example
    e1 = value str
    e1 === e
SeeAlso
  KSEntry
  (matrix,KSEntry)
  (toExternalString, KSEntry)
///

doc ///
Key
  availableOffline
Headline
  which Kreuzer-Skarke items are available offline
Usage
  availableOffline()
Outputs
  :HashTable
    the keys are the h11 and h12 entries which are available, and the value is either a string
    containing the file name, or a pair containing the number downloaded, and the file name.
Description
  Text
    In the examples below, the calls to @TO "kreuzerSkarke"@ will be handled without
    accessing the web interface.  If we had asked for {\tt kreuzerSkarke(11, 24, Limit => 1000)}
    then since only 100 are available locally, the local data could not be used.
  Example
    availableOffline()
    kreuzerSkarke 491
    kreuzerSkarke(11, 24, Limit => 10)
SeeAlso
  kreuzerSkarke
///


doc ///
Key
  generateOffline
  (generateOffline,ZZ)
  (generateOffline,ZZ,ZZ)
  Expected
Headline
  generate tables of reflexive 4d poytopes from the Kreuzer-Skarke list
Usage
  filename = generateOffline(h11,h12)
  filename = generateOffline h11
Inputs
  h11:ZZ
    As in getKreuzerSkarke, the desired $h^{1,1}(X)$ of the associated Calabi-Yau 3-fold
  h12:ZZ
    (Optional) As in getKreuzerSkarke, the desired $h^{1,2}(X)$ of the associated Calabi-Yau 3-fold
  Limit => ZZ
    Only download at most this many examples
  Prefix => String
    The directory to place the resulting file, should end in a slash, or be the empty string
  FileName => String
    If not null, then use this as the filename (ignoring also the {\tt Prefix} option)
  Expected => ZZ
    The expected number of examples.  This is tested, and an error is given if the number
    retrieved is different
Outputs
  filename:String
    The name of the file to where the examples are written
Consequences
  Item
    A file is written in the directory given by {\tt prefix}. The name is
    made out of the arguments
Description
  Pre
    filename = generateOffline(300, Limit=>1000, Expected=>20, Prefix=>"")
    polytopes = kreuzerSkarke get filename
Caveat
  This function doesn't take all of the options that kreuzerSkarke takes
SeeAlso
  kreuzerSkarke
///

doc ///
Key
  onlineTests
Headline
  run a few tests which test access to the Kreuzer-Skarke database
Usage
  onlineTests()
Consequences
  Item
    Several web access (to Kreuzer-Skarke database) tests are run.  Errors are
    generated if a problem is detected.
Description
  Text
    The following call, if it runs to completion, indicates that the connection
    to the Kreuzer-Skarke web server is functional.
  Pre
    onlineTests()
SeeAlso
  kreuzerSkarke
  generateOffline
  availableOffline
///

TEST ///
-*
  restart
  needsPackage "ReflexivePolytopesDB"
*-
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
  needsPackage "ReflexivePolytopesDB"
*-
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
  needsPackage "ReflexivePolytopesDB"
*-
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
-*
  needsPackage "ReflexivePolytopesDB"
*-
  topes3 = kreuzerSkarkeDim3();
  assert(#topes3 == 4319)
  assert(matrix topes3_10 == id_(ZZ^3) | matrix{{-6},{-4},{-1}})
///

TEST ///
-*
  restart
  needsPackage "ReflexivePolytopesDB"
*-
  set1 = kreuzerSkarke(3, Limit=>5)
  print toString set1_0
  toExternalString set1
  set1/matrix
  set1/description
  netList (set1/description)
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

///
-- Generate the file of 4319 reflexive polytopes
   url = URL "http://hep.itp.tuwien.ac.at/~kreuzer/pub/K3/RefPoly.d3";
    str = retrieve url;
    "ks-dim3.txt" << str << endl << close

-- Use the following lines to generate some offline files

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





