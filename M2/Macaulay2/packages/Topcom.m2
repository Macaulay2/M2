-- Functions used in Polyhedra:
-- topcomRegularFineTriangulation: in Polyhedra/core/polyhedron/methods.m2.

-- TODO:
--   1. how to check if a triangulation is correct?
--   2. generate the (oriented) circuits of a point set
--   3. Perhaps: add in a type "Chirotope" to facilitate the computation of circuits
--   4. find the lower hull of a polytope (maybe in Polyhedra? Where?)
--   5. check that going from a regular fine triangulation to a regular star fine triangulation 
--       (in the reflexive case) works.
--   6. generate (parts of) the flip graph, at least for regular triangulations.
--   7. topcom uses symmetry, place that into the interface here too
-- possible bugs:
--   why are the regular triangulation weights sometimes coming out negative?
--   need to be able to check that weights are correct.
newPackage(
        "Topcom",
        Version => "0.95",
        Date => "13 Nov 2022",
        Authors => {{
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"http://www.math.cornell.edu/~mike"
                }},
        Headline => "interface to the topcom software package which in particular computes triangulations",
        Keywords => {"Interfaces"},
        DebuggingMode => false
        )

export {
    -- triangulations
    "topcomAllTriangulations",
    "topcomNumTriangulations",
    "topcomFlips",
    "topcomNumFlips", 
    "topcomIsRegularTriangulation",
    "topcomRegularTriangulationWeights",
    "topcomRegularFineTriangulation",
    "topcomIsTriangulation",

    -- chirotopes
    "chirotopeString",
    "naiveChirotopeString",
    "orientedCircuits",
    "orientedCocircuits",
    
    -- option names
    "ConnectedToRegular",
    "Homogenize",
    "RegularOnly",
    "Fine"
    }

topcomProgram = null

augment = method()
augment Matrix := (A) -> (
    -- A is a matrix over ZZ
    -- add in a last row of 1's.
    n := numColumns A;
    ones := matrix {{n : 1}};
    A || ones
    )

topcomPoints = method(Options=>{Homogenize=>true})
topcomPoints Matrix := opts -> (A) -> (
    A1 := if opts.Homogenize then augment A else A;
    new Array from for a in entries transpose A1 list new Array from a
    )

-- Workhorse function for calling topcom.
-- command: one of the commands of topcom, with command line arguments included.
-- inputs: a list of objects that are in the format for topcom to understand.
-- output: 2 file names, one for the stdout, one for stderr.
-- If the executable fails, what happens?
-- debugLevel: set to 0 - 7 for varying verbose output
callTopcom = method()
callTopcom(String, List) := (command, inputs) -> (
    if topcomProgram === null then
	topcomProgram = findProgram("topcom", {"cube 3", "B_A 3"}, Prefix => {
	    (".*", "topcom-"), -- debian
	    ("^(cross|cube|cyclic|hypersimplex|lattice)$", "TOPCOM-"), --fedora
	    ("^cube$", "topcom_"), --gentoo
	    ("^(binomial|cross|cube|cyclic|lattice)$", "topcom-") --arch
	    });
    filename := temporaryFileName();
    infile := filename|".in";
    -- now create the output file
    F := openOut(infile);
    for f in inputs do (
        F << toString f << endl;
    );
    F << close;

    -- setting RaiseError to false because we sometimes get nonzero return
    -- values, e.g., the calls to topcomIsTriangulation in the
    -- "bad triangulations of the square" tests below
    retval := runProgram(topcomProgram, command, " < " | infile,
	KeepFiles => true, RaiseError => false);

    if debugLevel >= 1 then (
        << "-- calling topcom" << endl;
        << "-- " << command << ": using temporary file prefix " << filename << endl;
        );
    if debugLevel >= 7 then << "-- " << command << ": input = " << net get infile << endl;
    if debugLevel >= 2 then << "-- " << command << ": executing " << retval#"command" << endl;

    --if 0 =!= retval then error ("error running topcom:"| net get errfile);

    if debugLevel >= 5 then << "-- " << command << ": output = " << net retval#"output" << endl;
    if debugLevel >= 6 then << "-- " << command << ": stderr = " << net retval#"error" << endl;

    (retval#"output file", retval#"error file")
    )

-- before 1.1.0:
--   Checked 1 triangulations, 0 non-regular so far.
-- after 1.1.0:
--   checked 1 triangulations, 0 non-regular so far.

topcomIsRegularTriangulation = method(Options=>{Homogenize=>true})
topcomIsRegularTriangulation(Matrix, List) := Boolean => opts -> (A, tri) -> (
    -- now create the output file
    (outfile, errfile) := callTopcom("checkregularity --checktriang -v", {topcomPoints(A, opts), [], tri });
    match("[Cc]hecked 1 triangulations, 0 non-regular so far", get errfile)
    )

-- before 1.1.0:
--   (2,0,0,0)
-- after 1.1.0:
--   T[1] = {{1,2,3},{0,1,2}} is regular.
--   h[1] := [1,0,0,0];

topcomRegularTriangulationWeights = method(Options => options topcomIsRegularTriangulation)
topcomRegularTriangulationWeights(Matrix, List) := List => opts -> (A, tri) -> (
    -- returns null if the triangulation is not regular.
    -- otherwise returns a list of rational numbers which are the 
    -- heights that result in the triangulation.
    (outfile, errfile) := callTopcom("checkregularity --heights", {topcomPoints(A, opts), [], tri });
    output := get outfile;
    if match("non-regular", output) then return null;
    result := (
	if match(///^\(///, first lines output) -- TOPCOM < 1.1.0
	then value first lines output
	else value replace(///^h\[\d+\] := (.*);///, ///\1///,
	    (lines output)#1));
    return if instance(result, Number) then {result} else toList result
    )

topcomRegularFineTriangulation = method(Options => options topcomIsRegularTriangulation)
topcomRegularFineTriangulation Matrix := List => opts -> (A) -> (
    (outfile,errfile) := callTopcom("points2finetriang --regular", {topcomPoints(A, opts)});
    value get outfile
    )

-----------------------------------
-- Chirotope interface functions --
-----------------------------------
chirotopeString = method(Options => options topcomIsRegularTriangulation)
chirotopeString Matrix := String => opts -> A -> (
    (outfile,errfile) := callTopcom("points2chiro", {topcomPoints(A, opts)});
    get outfile
    )

-- The following is a slower version that allows us to check the result of chirotopeString.
naiveChirotopeString = method(Options => options chirotopeString)
naiveChirotopeString Matrix := String => opts -> A -> (
    A1 := if opts.Homogenize then augment A else A;
    n := numColumns A1;
    d := numRows A1;
    chiroHeader := (toString n) | "," | (toString d) | ":\n";
    subs := sort subsets(n,d);
    subs1 := pack(subs, 100);
    chiroHeader | concatenate for s in subs1 list (
        concatenate (for s1 in s list (
                d := det A1_s1;
                if d > 0 then "+" else if d == 0 then "0" else "-"
                )) | "\n"
        )
    )

-- before 1.1.0:
--   8,4:
--   {
--   [{0,3,6},{2,5}]
--   [{0,7},{2,5}]
--   ...
--   }
-- after 1.1.0:
--   C[0] := [{0,3},{1,2}];
--   C[1] := [{0,7},{1,2,4}];
--   ...

orientedCircuits = method(Options => {Homogenize=>true})
orientedCircuits String := List => opts -> (chiro) -> (
    (outfile,errfile) := callTopcom("chiro2circuits", {chiro});
    s := lines get outfile;
    if #s == 0 then return s;
    s = if match(///^C\[///, first s) -- TOPCOM >= 1.1.0
    then apply(s, line -> replace(///^C\[\d+\] := (.*);///, ///\1///, line))
    -- remove first 2 lines, and last line:
    else drop(drop(s, 2), -1);
    circs := s/(x -> toList value x);
    -- now sort it all
    circs/sort//sort
    )
orientedCircuits Matrix := List => opts -> A -> orientedCircuits chirotopeString(A, opts)

orientedCocircuits = method(Options => {Homogenize=>true})
orientedCocircuits String := List => opts -> (chiro) -> (
    (outfile,errfile) := callTopcom("chiro2cocircuits", {chiro});
    s := lines get outfile;
    s = drop(drop(s, 2), -1);
    s/(x -> toList value x)
    )
orientedCocircuits Matrix := List => opts -> A -> orientedCocircuits chirotopeString(A, opts)

-------------------------------------------
-- The key part: generate triangulations --
-------------------------------------------
allTriangsExecutable = hashTable {
    -- Fine?, COnnectedToRegular?
    (true, true) => "points2finetriangs",
    (true, false) => "points2allfinetriangs",
    (false, true) => "points2triangs",
    (false, false) => "points2alltriangs"
    }
numTriangsExecutable = hashTable {
    -- Fine?, COnnectedToRegular?
    (true, true) => "points2nfinetriangs",
    (true, false) => "points2nallfinetriangs",
    (false, true) => "points2ntriangs",
    (false, false) => "points2nalltriangs"
    }

-- before 1.1.0:
--   T[0]:=[0->4,3:{{0,1,2},{1,2,3}}];
-- after 1.1.0:
--   T[0] := {{0,1,2},{1,2,3}};

topcomAllTriangulations = method(Options => {
    Homogenize=>true, 
    RegularOnly => true, 
    Fine => false, 
    ConnectedToRegular => true
    })
topcomAllTriangulations Matrix := List => opts -> (A) -> (
    if not opts.ConnectedToRegular and opts.RegularOnly then error "cannot have both RegularOnly=>true and ConnectedToRegular=>false";
    executable := allTriangsExecutable#(opts.Fine, opts.ConnectedToRegular);
    args := if opts.RegularOnly then " --regular" else "";
    (outfile, errfile) := callTopcom(executable | args, {topcomPoints(A, Homogenize=>opts.Homogenize)});
    tris := lines get outfile;
    -- if ConnectToRegular is true, then the output is different, and needs to be parsed.
    -- in the other case, we can avoid the first 2 lines but they don't do anything either.
    for t in tris list (
        t1 := replace(///T\[[0-9,]+\] ?:= ?(\[.*:)?///, "", t);
        t2 := replace(///\];///, "", t1);
        t3 := sort value t2
        )
    )

topcomNumTriangulations = method(Options => {Homogenize=>true, RegularOnly => true, Fine => false, ConnectedToRegular => true})
topcomNumTriangulations Matrix := ZZ => opts -> (A) -> (
    if not opts.ConnectedToRegular and opts.RegularOnly then error "cannot have both RegularOnly=>true and ConnectedToRegular=>false";
    executable := numTriangsExecutable#(opts.Fine, opts.ConnectedToRegular);
    args := if opts.RegularOnly then " --regular" else "";
    (outfile, errfile) := callTopcom(executable | args, {topcomPoints(A, Homogenize=>opts.Homogenize)});
    value get outfile
    )

topcomNumFlips = method(Options => {Homogenize=>true, RegularOnly =>true})
topcomNumFlips(Matrix, List) := ZZ => opts -> (A, tri) -> (
    executable := "points2nflips";
    args := if opts.RegularOnly then " --regular" else "";
    (outfile, errfile) := callTopcom(executable | args, {topcomPoints(A, Homogenize=>opts.Homogenize), [], tri});
    value get outfile
    )

topcomFlips = method(Options => {Homogenize=>true, RegularOnly =>true})
topcomFlips(Matrix, List) := List => opts -> (A, tri) -> (
    executable := "points2flips";
    args := if opts.RegularOnly then " --regular" else "";
    (outfile, errfile) := callTopcom(executable | args, {topcomPoints(A, Homogenize=>opts.Homogenize), [], tri});
    s := get outfile;
    s = replace("->0","",s); -- I don't understand why this is in their notation...
    s = replace("[0-9,]*:", "", s);
    s = replace("\\[","{",s);
    s = replace("\\]","}",s);
    ans := first value s; -- seems to have an extra set of []'s around it.
    ans//sort//sort
    )

-*
  s = "[13,5:[[{9},{3,4}]->0,[{12},{5,6,7}]->0,[{1,4},{2,3}]->0,[{2,7},{0,4,6}]->0,[{11},{0,7}]->0,[{0,5},{2,3}]->0,[{10},{3,4}]->0,[{3,4,8},{5,7}]->0]]"
  s = replace("->0","",s)
  s = replace("[0-9,]*:", "", s)
  s = replace("\\[","{",s)
  s = replace("\\]","}",s)
  value s
*- 
-- remove the 13,5.  Remove the ->0


-- The code we add to this assumes that convexHull Vin is full dimensional.
-- TODO: also, can we give homogenized matrix too?
topcomIsTriangulation = method(Options => {Homogenize => true});
topcomIsTriangulation(Matrix, List) := Boolean => opts -> (Vin, T) -> (
   -- Topcom does not check whether the sets in T actually form simplices. In
   -- that case it throws an error instead of giving an answer.  -- So we do it
   -- manually:
   V := if opts.Homogenize then promote(augment Vin, QQ) else promote(Vin, QQ);
   d := numRows V;
   if not all(T, t-> #t == d) then (
      << "Index sets do not correspond to full-dimensional simplices" << endl;
      return false;
   );
   simplices := apply(T, t -> V_t);
   if not all(simplices, s->d==rank s) then (
      << "Index sets do not correspond to full-dimensional simplices" << endl;
      return false;
   );
   (outfile, errfile) := callTopcom("points2nflips --checktriang -v", {topcomPoints(V, Homogenize=>false), [], T });
   not match("not valid", get errfile)
)

beginDocumentation()

doc ///
Key
  Topcom
Headline
  interface to selected functions from topcom package
Description
  Text
    @HREF{"https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM/index.html", "Topcom"}@ 
    is mathematical software written by Jorg Rambau for 
    computing and manipulating triangulations of polytopes and chirotopes.

    This Macaulay2 package provides an interface for some of the functionality
    of this software.

    The package is meant as an internal package, to be called by
    packages such as @TO "Polyhedra::Polyhedra"@.  It is highly recommended
    to use those packages, rather than calling this package directly.
  Text
    @SUBSECTION "Example use of Topcom"@
  Text
    @UL {
      TO "An example use of TopCom"
    }@
  Text
    @SUBSECTION "Generating triangulations"@
  Text
    @UL {
      TO (topcomAllTriangulations, Matrix),
      TO (topcomNumTriangulations, Matrix)
    }@
  Text
    @SUBSECTION "Useful functions involving specific triangulations"@
  Text
    @UL {
      TO (topcomRegularFineTriangulation, Matrix),
      TO (topcomIsTriangulation, Matrix, List),
      TO (topcomIsRegularTriangulation, Matrix, List),
      TO (topcomRegularTriangulationWeights, Matrix, List)
    }@
  Text
    @SUBSECTION "Useful functions involving bistellar flips"@
  Text
    @UL {
      TO (topcomFlips, Matrix, List),
      TO (topcomNumFlips, Matrix, List)
    }@
  Text
    @SUBSECTION "Chirotope functions"@
  Text
    The {\it chirotope} of a point set (or vector set), consisting of the columns of the $d \times n$
    matrix $A$, is the function which assigns to each $d+1 \times d+1$ minor of $\overline{A}$ the sign
    of its determinant.  Topcom encodes this into a string.  See @TO "chirotopeString"@ for details about the
    output of this function.
  Text
    @UL {
      TO (chirotopeString, Matrix),
      TO (naiveChirotopeString, Matrix),
      TO (orientedCircuits, String),
      TO (orientedCocircuits, String)
    }@
SeeAlso
  "Polyhedra::Polyhedra"
  "Triangulations::Triangulations"
  "ReflexivePolytopesDB::ReflexivePolytopesDB"
///

doc ///
Key
  "An example use of TopCom"
Headline
  interface to selected functions from topcom package
Description
  Text
    Topcom is software written by Jorg Rambau which has very good support for computing
    the triangulations of a point set (or, regular triangulations, fine triangulations, and
        triangulations which are connected to regular triangulations via bistellar flips).
    
    Given a matrix $A$ (say of size $d \times n$), the columns represent points in $d$-space, and each
    point is labelled by its column index ($0, 1, \ldots, n-1$). Topcom appears to assume that
    the points are not contained in a hyperplane.  In this case, their convex hull is full dimensional
    ($d$-dimensional), and a set of $d$-simplices with vertices in the set of points is a triangulation
    if (1) their union is the convex hull of the columns of $A$, and (2) the simplices either do not
    intersect, or intersect only along lower dimensional sets.  We represent a triangulation by 
    the the list of lists of indices for each of these $d$-dimensional simplices.
  Text
    For example, consider a simple example: the square.
  Example
    sq = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0},{1,0},{-1,0},{0,1},{0,-1}}
    tri = topcomRegularFineTriangulation sq
    topcomIsTriangulation(sq, tri)    
    topcomIsRegularTriangulation(sq, tri)
    topcomRegularTriangulationWeights(sq, tri)
    topcomNumTriangulations sq
    topcomNumTriangulations(sq, Fine => true)
    Ts = topcomAllTriangulations(sq, Fine => true);    
    netList Ts
  Text
    Topcom assumes that the points do not lie on a hyperplane.  If they do, and the hyperplane is not
    through the origin (and they span that entire affine hyperplane), then give {\tt Homogenize => false} as an option.
    This is essentially equivalent to considering the point configuration as a vector configuration.
    
    For example, the following three points lie on the hyperplane $x_0 + x_1 + x_2 = 2$.
  Example
    A1 = transpose matrix{{1,1,0},{1,0,1},{0,1,1}}
    topcomRegularFineTriangulation(A1, Homogenize => false)
    topcomRegularFineTriangulation(A1, Homogenize => true)
  Text
    Topcom generates triangulations by starting with a triangulation, and generating more by bistellar flips.
    There is a beautiful story about the collection of all (regular, or connected to regular) triangulations of a point or vector 
    configuration.  See the book De Loera, Rambau, Santos, {\it Triangulations} for details about this.
  Example
    sqh = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1},{1,0,1},{-1,0,1},{0,1,1},{0,-1,1}}
    tri = topcomRegularFineTriangulation sqh -- none found!
    tri = topcomRegularFineTriangulation(sqh, Homogenize => false)
    topcomNumTriangulations(sqh, Homogenize => false)
    # topcomAllTriangulations(sqh, Homogenize => false)
    topcomIsTriangulation(sq, tri)    
    topcomIsRegularTriangulation(sq, tri)
    topcomRegularTriangulationWeights(sq, tri)
    topcomNumTriangulations sq
    topcomNumTriangulations(sq, Fine => true)
    Ts = topcomAllTriangulations(sq, Fine => true);    
    netList Ts
SeeAlso
  (topcomIsTriangulation, Matrix, List)
  (topcomIsRegularTriangulation, Matrix, List)
  (topcomRegularFineTriangulation, Matrix)
  (topcomAllTriangulations, Matrix)
  (topcomNumTriangulations, Matrix)
  (topcomRegularTriangulationWeights, Matrix, List)
///

doc ///
Key
  topcomIsRegularTriangulation
  (topcomIsRegularTriangulation, Matrix, List)
  [topcomIsRegularTriangulation, Homogenize]
Headline
  determine if a given triangulation is a regular triangulation
Usage
  topcomIsRegularTriangulation(A, tri)
Inputs
  A:Matrix
    A $d \times n$ matrix over ZZ.  Each column represents one of the points
    which can be used in a triangulation
  tri:List
    A triangulation of the point set C
  Homogenize => Boolean
    If true, $A$ determines a point configuration, and the matrix is then "homogenized":
    a row of 1's is appended to $A$, creating a vector configuration of $n$ vectors in $\mathbb{R}^{d+1}$.
Outputs
  :Boolean
    whether the given triangulation is regular
Description
  Text
    The following example is one of the simplest examples of a non-regular
    triangulation.  Notice that {\tt tri} is a triangulation of the 
    polytope which is the convex hull of the columns of $A$, which are 
    the only points allowed in the triangulation.
  Example
    A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
    tri = {{0,1,2}, {1,3,5}, {2,3,4}, {0,1,5}, 
        {0,2,4}, {3,4,5}, {1,2,3}}
    topcomIsRegularTriangulation(A,tri)
    assert not topcomIsRegularTriangulation(A,tri)
    assert topcomIsTriangulation(A, tri)
  Text
    Setting debugLevel to either 1,2, or 5 will give more detail about
    what files are written to Topcom, and what the executable is.
    Setting debugLevel to 0 means that the function will run silently.
Caveat
  Do we check that the triangulation is actually well defined?
SeeAlso
  topcomRegularFineTriangulation  
  topcomIsTriangulation
///

doc ///
  Key
    (topcomAllTriangulations, Matrix)
    topcomAllTriangulations
    [topcomAllTriangulations, ConnectedToRegular]
    [topcomAllTriangulations, Fine]
    [topcomAllTriangulations, Homogenize]
    [topcomAllTriangulations, RegularOnly]
  Headline
    generate all triangulations of a point or vector configuration
  Usage
    topcomAllTriangulations A
    topcomAllTriangulations(A, Homogenize => true, Fine => true, RegularOnly => true)
  Inputs
    A:Matrix
    Homogenize => Boolean
    ConnectedToRegular => Boolean
    Fine => Boolean
    RegularOnly => Boolean    
  Outputs
    :List
  Description
    Text
      This function constructs all triangulations of the point set corresponding to $A$
      (or triangulation of the cone over $A$, if {\tt Homogenize => false} is given).
      With no optional arguments, the default is to construct all regular triangulations.
      
      A triangulation is a list of lists of the indices of the maximal simplices in the triangulation.
      (the index of the point corresponding to the $i$-th column (starting at $0$) is simply $i$).
      
      For example, the following point set is the smallest which has a non-regular triangulation.
      Note that @TO (topcomAllTriangulations, Matrix)@ only generates all the regular triangulations.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      Ts = topcomAllTriangulations A;
      #Ts == 16
      netList Ts
      tri = Ts#0
      topcomIsTriangulation(A, tri)
      topcomIsRegularTriangulation(A, tri)
      topcomRegularTriangulationWeights(A, tri)
    Text
      The following code determines the support of each triangulation, and tallies them.
      Thus for example, we see that there are 6 regular fine triangulations (fine means that all of the
      points are begin used).
    Example
      tally for tri in Ts list sort unique flatten tri
    Text
      The method that topcom uses depends on the optional arguments {\tt Fine}, {\tt ConnectedToRegular}
      and {\tt RegularOnly}. 
    Example 
      options topcomAllTriangulations
    Text
      If the optional argument {\tt Fine} is set to true, then only 
      {\it fine} triangulations (i.e.
          those that involve every column of $A$) will be generated.
    Example 
      Ts = topcomAllTriangulations(A, Fine => true);
      #Ts == 6
    Text
      If the optional argument {\tt RegularOnly} is set to false, but
      {\tt ConnectedToRegular} is true, it will generally take less time,
      as the program doesn't need to check each triangulation to see if it is regular.
    Example
      T1s = topcomAllTriangulations(A, RegularOnly => true)
      T2s = topcomAllTriangulations(A, RegularOnly => false)
      #T1s
      #T2s
    Text
      The following search yields {\bf all} triangulations, even those not connected via
      bistellar flips to regular triangulations.
    Example
      T3s = topcomAllTriangulations(A, RegularOnly => false, ConnectedToRegular => false)
      #T3s
    Text
      Given the list of triangulations, we can query them using other topcom functions.
      See also @TO "Triangulations::Triangulations"@ for other functionality.
    Example
      netList Ts
      for tri in Ts list topcomIsTriangulation(A, tri)
      for tri in Ts list topcomIsRegularTriangulation(A, tri)
      for tri in Ts list topcomRegularTriangulationWeights(A, tri)
  Caveat
    With no optional arguments, this function returns all {\bf regular} triangulations, not {\bf all}
    triangulations!
  SeeAlso
    (topcomNumTriangulations, Matrix)
    topcomIsTriangulation
    topcomIsRegularTriangulation
    topcomRegularTriangulationWeights
    topcomNumTriangulations
///

doc ///
  Key
    (topcomNumTriangulations, Matrix)
    topcomNumTriangulations
    [topcomNumTriangulations, ConnectedToRegular]
    [topcomNumTriangulations, Fine]
    [topcomNumTriangulations, Homogenize]
    [topcomNumTriangulations, RegularOnly]
  Headline
    the number of triangulations of a point or vector configuration
  Usage
    topcomNumTriangulations A
    topcomNumTriangulations(A, Homogenize => true, Fine => true, RegularOnly => true)
  Inputs
    A:Matrix
    Homogenize => Boolean
    ConnectedToRegular => Boolean
    Fine => Boolean
    RegularOnly => Boolean    
  Outputs
    :ZZ
  Description
    Text
      This function is identical in function to @TO (topcomAllTriangulations, Matrix)@ (including optional
          arguments), but instead just counts the number, rather than enumerate them.
      
      For example, to use the same example as in @TO (topcomAllTriangulations, Matrix)@:
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      topcomNumTriangulations A == 16
      topcomNumTriangulations A == # topcomAllTriangulations A
    Text
      Similarly, one can count the number of triangulations with different properties using the
      optional arguments.
    Example
      topcomNumTriangulations(A, RegularOnly => false)
      assert(topcomNumTriangulations(A, RegularOnly => false) == 18)
      assert(topcomNumTriangulations(A, RegularOnly => false) == # topcomAllTriangulations(A, RegularOnly => false))
  SeeAlso
    (topcomAllTriangulations, Matrix)
///

doc ///
  Key
    (topcomRegularFineTriangulation, Matrix)
    topcomRegularFineTriangulation
    [topcomRegularFineTriangulation, Homogenize]
  Headline
    compute a regular triangulation using all of the given points
  Usage
    topcomRegularFineTriangulation A
  Inputs
    A:Matrix
    Homogenize => Boolean
  Outputs
    :List
  Description
    Text
      This function returns a regular fine triangulation of the point
      set $A$ (that is, a list of lists of indices in the range $0,
      \ldots, n-1$, where $A$ is $d \times n$, and has full rank.
      
      Recall: a fine triangulation is one which uses all of the points, and a regular
      triangulation is one which is induced as the lower faces of a lift of the points
      to one dimension higher.
      
      Here we find a regular fine triangulation of the cyclic polytope with 7 vertices in 3-space.
    Example
      A = matrix {{0, 1, 2, 3, 4, 5, 6}, {0, 1, 4, 9, 16, 25, 36}, {0, 1, 8, 27, 64, 125, 216}}
      tri = topcomRegularFineTriangulation A
      assert topcomIsTriangulation(A, tri)
      assert topcomIsRegularTriangulation(A, tri)
      topcomRegularTriangulationWeights(A, tri)
  SeeAlso
    topcomIsTriangulation
    topcomRegularTriangulationWeights
    "Polyhedra::convexHull"
///

doc ///
  Key
    (chirotopeString, Matrix)
    chirotopeString
    [chirotopeString, Homogenize]
  Headline
    compute the chirotope string of a point or vector configuration
  Usage
    chirotopeString A
  Inputs
    A:Matrix
    Homogenize => Boolean
  Outputs
    :String
  Description
    Text
      Let $A^h$ be the matrix obtained from $A$ by adding a row of
      one's (called the homogenization of $A$).  Sort all of the
      subsets of size $d+1$ of $0, \ldots, n-1$ lexicographically.
      The resulting string has two parts: the header, which has $n$
      and $d+1$, and then a single string consisting of "0", "-" and
      "+", one for each subset, where the character records the sign
      of the determinant of the submatrix consisting of the columns in
      the subset.
    Example
      A = matrix {
          {0, -1, 2, 3, 4, -5, 6}, 
          {0, 1, -4, 9, 16, 25, 36}, 
          {0, 1, 8, -27, 64, 125, -216}}
      om = chirotopeString A
      om == naiveChirotopeString A
    Text
      Topcom often works as follows: it creates the oriented matroid
      (string, or an internal version of it), and uses that to find
      triangulations.  It is important to note that one can find all
      triangulations this way.  However, one needs the matrix $A$ to
      be able to determine if a given triangulation is regular.
    Example
      orientedCircuits om
      orientedCocircuits om
  Caveat
    If the number of minors of size $(d+1) \times (d+1)$ of $A^h$ is too large, then this won't work so well....!
    For example, in dimension 4, with 100 vertices, the number of such determinants is about 75 million.  That
    is fine perhaps, but for 200 vertices, the size jumps to 2.5 billion.
  SeeAlso
    (naiveChirotopeString, Matrix)
    (orientedCircuits, String)
    (orientedCocircuits, String)
///

doc ///
  Key
    (naiveChirotopeString, Matrix)
    naiveChirotopeString
    [naiveChirotopeString, Homogenize]
  Headline
    compute the chirotope string of a point or vector configuration
  Usage
    naiveChirotopeString A
  Inputs
    A:Matrix
    Homogenize => Boolean
  Outputs
    :String
  Description
    Text
      This is a simple function written in Macaulay2 to generate the same output as
      @TO (chirotopeString, Matrix)@.  However, it is much slower.  But we wrote it to make sure 
      we know what order Topcom is generating the determinants.
    Example
      A = matrix {
          {0, -1, 2, 3, 4, -5, 6}, 
          {0, 1, -4, 9, 16, 25, 36}, 
          {0, 1, 8, -27, 64, 125, -216}}
      om = naiveChirotopeString A
      om == chirotopeString A
  SeeAlso
    (chirotopeString, Matrix)
    (orientedCircuits, String)
    (orientedCocircuits, String)
///

doc ///
  Key
    orientedCircuits
    (orientedCircuits, String)
    (orientedCircuits, Matrix)
    [orientedCircuits, Homogenize]
  Headline
    compute the oriented circuits of an oriented matroid or point or vector configuration
  Usage
    orientedCircuits om
    orientedCocircuits A
  Inputs
    om:String
    A:Matrix
    Homogenize => Boolean
      Only valid in the second form.
  Outputs
    :List
  Description
    Text
      An oriented circuit is determined by a linear relationship on the columns of 
      (the augmented matrix of) $A$, of minimal support.  The circuit is the pair of lists
      of indices of the columns where the coefficients is positive (respectively negative).
    Example
      A = matrix {
          {0, -1, 2, 3, 4, -5, 6}, 
          {0, 1, -4, 9, 16, 25, 36}, 
          {0, 1, 8, -27, 64, 125, -216}}
      om = naiveChirotopeString A
      netList orientedCircuits om
    Text
      Let's look at the linear relation giving rise to $\{\{0,3\}, \{ 2, 5, 6\}\}$.
    Example
      Ahomog = A || matrix{{7:1}}
      Ahomog_{0,3,2,5,6}
      syz oo
  SeeAlso
    (chirotopeString, Matrix)
    orientedCocircuits
///

-- TODO: not finished.
doc ///
  Key
    orientedCocircuits
    (orientedCocircuits, String)
    (orientedCocircuits, Matrix)
    [orientedCocircuits, Homogenize]
  Headline
    compute the oriented cocircuits of an oriented matroid
  Usage
    orientedCocircuits om
    orientedCocircuits A
  Inputs
    om:String
      representing a chirotope
    A:Matrix
    Homogenize => Boolean
      Only valid in the second form.
  Outputs
    :List
  Description
    Text
  SeeAlso
    chirotopeString
    orientedCircuits
///

-- TODO: not finished.
doc ///
  Key
    (topcomIsTriangulation, Matrix, List)
    topcomIsTriangulation
    [topcomIsTriangulation, Homogenize]
  Headline
    determine if a set of subsets is a triangulation of a point set
  Usage
    topcomIsTriangulation(A, tri)
  Inputs
    A:Matrix
    tri:List
    Homogenize => Boolean
  Outputs
    :Boolean
  Description
    Text
  Caveat
    topcom doesn't check that the subsets given are simplices, so we do that
    directly.
  SeeAlso
    (topcomIsRegularTriangulation, Matrix, List)
///

doc ///
  Key
    (topcomRegularTriangulationWeights, Matrix, List)
    topcomRegularTriangulationWeights
    [topcomRegularTriangulationWeights, Homogenize]
  Headline
    find a list of heights of a regular triangulation
  Usage
    topcomRegularTriangulationWeights(A, tri)
  Inputs
    A:Matrix
    tri:List
    Homogenize => Boolean
  Outputs
    :List
  Description
    Text
      If the given triangulation is regular, this returns the corresponding list of weights.
      If we lift each point to this height (hieights can be negative too), then the
      lower convex hull of these lifted points is the given triangulation.  This can be checked
      via the routine @TO "Polyhedra::regularSubdivision"@ in the package @TO "Polyhedra"@.
    Example
      A = matrix {
          {0, -1, 2, 3, 4, -5, 6}, 
          {0, 1, -4, 9, 16, 25, 36}, 
          {0, 1, 8, -27, 64, 125, -216}}
      Ts = topcomAllTriangulations A;
      #Ts == 25
      netList Ts
      Ts/(tri -> topcomIsRegularTriangulation(A, tri))
      weights = topcomRegularTriangulationWeights(A, Ts#0)
      needsPackage "Polyhedra"
      Ts#0
      tri0 = regularSubdivision(A, matrix{weights})
      tri0 = tri0//sort/sort
      assert(tri0 == Ts#0)
  SeeAlso
    (topcomIsRegularTriangulation, Matrix, List)
    "Polyhedra::regularSubdivision"
///

-- TODO: not finished.
doc ///
  Key
    (topcomFlips, Matrix, List)
    topcomFlips
    [topcomFlips, Homogenize]
    [topcomFlips, RegularOnly]
  Headline
    find the neighboring triangulations (bistellar flips) of a triangulation
  Usage
    topcomFlips(A, tri)
  Inputs
    A:Matrix
    tri:List
    Homogenize => Boolean
    RegularOnly => Boolean
  Outputs
    :List
  Description
    Text
    Example
      A = transpose matrix {
          {-1, -1, -1, 0}, {-1, -1, 0, -1}, {-1, -1, 0, 0}, {-1, 0, -1, -1}, 
          {-1, 0, -1, 2}, {-1, 0, 0, -1}, {0, -1, 1, -1}, {1, 1, -1, 2}, 
          {1, 1, 1, -1}, {-1, 0, -1, 0}, {-1, 0, -1, 1}, {0, 0, -1, 1}, {0,0,0,0}}
      tri1 = topcomRegularFineTriangulation A
      topcomFlips(A, tri1)
    
  SeeAlso
    (topcomNumFlips, Matrix, List)
///

-- TODO: not finished.
doc ///
  Key
    (topcomNumFlips, Matrix, List)
    topcomNumFlips
    [topcomNumFlips, Homogenize]
    [topcomNumFlips, RegularOnly]
  Headline
    find the number of neighboring triangulations (bistellar flips) of a triangulation
  Usage
    topcomNumFlips(A, tri)
  Inputs
    A:Matrix
    tri:List
    Homogenize => Boolean
    RegularOnly => Boolean
  Outputs
    :ZZ
  Description
    Text
  SeeAlso
    (topcomNumFlips, Matrix, List)
///

doc ///
  Key
    Homogenize
  Headline
    an optional argument to most functions in Topcom to indicate whether to homogenize the matrix
  Description
    Text
      Most functions in this package take a $d \times n$ matrix $A$, representing a point or vector
      configuration: the columns are the points or vectors.  Topcom's input is always a matrix
      representing a vector configuration.  A matrix $A$ whose columns represent a point
      configuration can be "homogenized" to a vector configuration by adding a row of ones to $A$,
      forming a $(d+1) \times n$ matrix $A^h$.
///

doc ///
  Key
    ConnectedToRegular
  Headline
    an optional argument used in some functions in the Topcom interface package
///

doc ///
  Key
    Fine
  Headline
    an optional argument used in some functions in the Topcom interface package
///

doc ///
  Key
    RegularOnly
  Headline
    an optional argument used in some functions in the Topcom interface package
///






TEST ///
-*
  restart
  debug needsPackage "Topcom"
*-
  -- test of topcomIsRegularTriangulation
  A = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  topcomRegularFineTriangulation(A, Homogenize=>false)
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  assert topcomIsRegularTriangulation(A,tri)
  topcomRegularTriangulationWeights(A,tri,Homogenize=>false) == {1,1,0,0,0}
///

TEST ///
  needsPackage "Topcom"
  -- test of topcomIsRegularTriangulation
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert not topcomIsRegularTriangulation(A,tri)
  assert(null === topcomRegularTriangulationWeights(A,tri))
  topcomNumTriangulations A
  topcomAllTriangulations A  
  topcomAllTriangulations(A, Fine=>true)
  topcomAllTriangulations(A, Fine=>true, RegularOnly=>false)
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert topcomIsRegularTriangulation(A,tri)
  topcomRegularTriangulationWeights(A,tri) -- Question: how to test that this is correct
    -- TODO: need a function which takes a point set, weights, and creates the lift (easy)
    --       compute the lower hull of this polytope.

  assert(chirotopeString A == naiveChirotopeString A)
  orientedCircuits A
  orientedCocircuits A
  A = transpose matrix {{1,0},{0,1}}
  tri = {{0,1}}
  assert topcomIsRegularTriangulation(A,tri)
  topcomRegularTriangulationWeights(A,tri) == {0,1} -- TODO: check that this is the correct answer
  
  A = transpose matrix {{0}}
  tri = {{0}}
  assert topcomIsRegularTriangulation(A,tri)
  topcomRegularTriangulationWeights(A,tri) == {1}
///

TEST ///
-- TODO: This test needs to be made to assert correct statements
-- How to test that triangulations are correct?  What I thought worked does not.
  needsPackage "Topcom"
  A = transpose matrix{{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  topcomRegularFineTriangulation A  
  tri1 = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  tri2 = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2, 3}}

  assert topcomIsTriangulation(A, tri1)
  assert topcomIsRegularTriangulation(A, tri1)

  assert not topcomIsTriangulation(A, tri2)
  topcomIsRegularTriangulation(A, tri2) -- Wrong!!

  debug needsPackage "Triangulations"
  assert isTriangulation(A, tri1)
  assert not isTriangulation(A, tri2)

  badtri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2, 3}}
  debugLevel = 6
  topcomIsRegularTriangulation(A,badtri) -- this should fail! But it doesn't seem to do so. BUG in something!!!
  debugLevel = 0
  -- hmmm, we can make non-sensical triangulations, without it noticing.
  -- this should be a bug?  
  A = transpose matrix {{0,0},{0,1},{1,0},{1,1}}

  tri = {{0,1,2},{0,2,3}}
  assert not isTriangulation(A,tri) -- upshot: topcomIsRegularTriangulation NEEDS a triangulation.
  assert not topcomIsTriangulation(A,tri)
  topcomIsRegularTriangulation(A,tri) -- this is (incorrectly) true though.
    
  tri = {{0,1,2},{1,2,3}}
  assert topcomIsRegularTriangulation(A,tri) 
  assert isTriangulation(A,tri)
  assert topcomIsTriangulation(A,tri)
///

TEST ///  
  needsPackage "Topcom"
  needsPackage "Polyhedra"
  
  A = transpose matrix {{-1,-1,2},{-1,0,1},{-1,1,1},{0,-1,2},{0,1,1},{1,-1,3},{1,0,-1},{1,1,-2}}
  debugLevel = 0
  tri = topcomRegularFineTriangulation A
  assert topcomIsRegularTriangulation(A, tri)
  assert(topcomRegularTriangulationWeights(A, tri) =!= null)
  assert topcomIsTriangulation(A, tri)

  A = transpose matrix {{-1, 0, -1, -1}, {-1, 0, 0, -1}, {-1, 1, 2, -1}, {-1, 1, 2, 0}, {1, -1, -1, -1}, {1, -1, -1, 1}, {1, 0, -1, 2}, {1, 0, 1, 2}}
  P2 = polar convexHull A
  C = matrix {latticePoints P2}
  tri = topcomRegularFineTriangulation C
  assert topcomIsRegularTriangulation(C, tri)
  topcomRegularTriangulationWeights(C, tri) -- is this correct?  Some weights have negative values??
  
  elapsedTime assert topcomIsTriangulation(C, tri) -- .09 seconds
  --elapsedTime assert isTriangulation(C, tri) -- *much* slower (51 seconds)
///

TEST ///
-- simple example of chirotope
-*
  restart
  debug needsPackage "Topcom"
*-
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  ch1 = chirotopeString A
  ch2 = naiveChirotopeString A
  assert(ch1 == ch2)
///

TEST ///
-- Bad triangulations of the square
  V = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
  T1 = {{0,1,2}}
  T2 = {{0,1,2},{0,1,3}}
  T3 = {{0,1,2,3}}
  assert(not topcomIsTriangulation(V, T1))
  assert(not topcomIsTriangulation(V, T2))
  assert(not topcomIsTriangulation(V, T3))
///

-- This example is a good one, but takes too long to be run automatically
///
restart
  needsPackage "Topcom"  
  needsPackage "Polyhedra"
  pts =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1}}
  A = transpose matrix pts 
  -- debugLevel = 7

  elapsedTime n1 = topcomNumTriangulations(A, Fine=>true, ConnectedToRegular=>true) -- 6.9 sec, 408 of these CORRECT
  elapsedTime n2 = topcomNumTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false) -- 116 sec, 448 of these WRONG
  elapsedTime n3 = topcomNumTriangulations(A, Fine=>false, ConnectedToRegular=>true)  -- 8 sec, 520 of these CORRECT
  elapsedTime n4 = topcomNumTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false) -- 115 sec, 564 of these WRONG

  elapsedTime n5 = topcomNumTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false) -- .09 sec, 448 of these
  elapsedTime n6 = topcomNumTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false) -- 115.5 sec, 448 of these
  elapsedTime n7 = topcomNumTriangulations(A, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false)  -- .11 sec, 564 of these
  elapsedTime n8 = topcomNumTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false) -- 116 sec, 564 of these

  elapsedTime set1 = topcomAllTriangulations(A, Fine=>true, ConnectedToRegular=>true); -- 6.9 sec, 408  CORRECT
  elapsedTime set2 = topcomAllTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false); -- 118 sec, 448 WRONG
  elapsedTime set3 = topcomAllTriangulations(A, Fine=>false, ConnectedToRegular=>true); -- 8.1 sec, 520 CORRECT
  elapsedTime set4 = topcomAllTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false); -- 116 sec.  564 of these. WRONG

  elapsedTime set5 = topcomAllTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false); -- .15 sec, 448 of these
  elapsedTime set6 = topcomAllTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false); -- 116 sec, 448 of these
  elapsedTime set7 = topcomAllTriangulations(A, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false); -- .22 sec, 564 of these
  elapsedTime set8 = topcomAllTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false); -- 117 sec, 564 of these

  assert((n1,n2,n3,n4,n5,n6,n7,n8) == (#set1, #set2, #set3, #set4, #set5, #set6, #set7, #set8))
  fineTris = select(set8, x -> # unique flatten x == numColumns A);
  regularFineTris = select(fineTris, x -> topcomIsRegularTriangulation(A, x));
  regularTris = select(set8, x -> topcomIsRegularTriangulation(A, x));

  assert(#regularFineTris == 408)
  assert(#fineTris == 448)
  assert(#regularTris == 520)  

  assert(set set5 === set set6) -- in general, this doesn't need to hold, but it is rare for this to be the case
  assert(set set7 === set set8) -- same: rare for this to not hold
  assert(set set4 === set set8) -- this one should not be true?  
  assert(set select(set7, x -> topcomIsRegularTriangulation(A, x)) === set set3)
  assert(set select(set5, x -> topcomIsRegularTriangulation(A, x)) === set set1)
///

///
-- same example as the last one
restart
  needsPackage "Topcom"  
  needsPackage "Polyhedra"
  pts =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1}}
  A = transpose matrix pts 
  elapsedTime n5 = topcomNumTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false) -- .09 sec, 448 of these
  elapsedTime set5 = topcomAllTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false); -- .15 sec, 448 of these

  set5_0
  elapsedTime for tri in set5 do assert topcomIsTriangulation(A, tri)

  options topcomFlips
  topcomNumFlips(A, set5_0)  
  # topcomFlips(A, set5_0) == 6 -- these do not match?!
  # topcomFlips(A, set5_0, RegularOnly => false) -- these do not match?!
///

///
  restart
  needsPackage "ReflexivePolytopesDB"
  needsPackage "Topcom"  
  needsPackage "Polyhedra"
 str = "4 18  M:53 18 N:11 10 H:6,45 [-78]
        1   0   0  -2   0   2   1   3  -2   2  -2   2   3  -1   0  -2   0  -1
        0   1   0   2   0   0   1  -2   1  -2   0   0  -1   0  -2   0  -2  -1
        0   0   1   1   0  -1  -1  -2   2  -2   0  -2  -2   2  -1   2   1   2
        0   0   0   0   1  -1  -1   0  -1   1   1  -1  -1  -1   2  -1   0  -1"
 str = "4 12  M:50 12 N:11 9 H:6,44 [-76]
        1   1   1  -1  -1   0   2  -3  -2   3  -5   1
        0   2   0  -2   0   0   1  -3  -2   4  -5   4
        0   0   2   0  -2   0   2  -3   0   2  -3   0
        0   0   0   0   0   1  -1   1   1  -2   1  -2"
 A = matrix first kreuzerSkarke str
 P = convexHull A
 P2 = polar P
 A1 = vertices P2
 LP = matrix{select(latticePoints P2, x -> x != 0)}
 topcomNumTriangulations(LP, Fine => true)
 topcomAllTriangulations(LP, Fine=>true);
 topcomNumTriangulations(LP)
 topcomAllTriangulations(LP);

///

end----------------------------------------------------

restart
uninstallPackage "Topcom"
restart
needsPackage "Topcom"
restart
check "Topcom"
restart
installPackage "Topcom"
viewHelp Topcom
-----------------------------------------------------------------
-- The stuff below this point is kept here to possibly      -----
-- aid in adding in interfaces to other functions in topcom -----
-----------------------------------------------------------------

/// 
-- I am keeping this here as an aid to testing the other topcom
-- functions in case we want to interface to them too.
-*
  restart
  needsPackage "Topcom"
*-
  debug needsPackage "Topcom"
  A = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}

  -- points2chiro
  chiro = chirotopeString(A, Homogenize => false)
  assert(chiro == naiveChirotopeString(A, Homogenize => false))

  toppath = "/opt/homebrew/bin"
  run (toppath|"/points2chiro"|" -h")
  "topcomfoo.in" << topcomPoints(A, Homogenize=>false) << endl << close;
  chiro = get ("!"|toppath|"/points2chiro"|" <topcomfoo.in")
  #chiro
  chiro2 = "5,3:\n" | (concatenate for s in sort subsets(5,3) list (
      d := det A_s;
      if d > 0 then "+" else if d == 0 then "0" else "-"
      )) | "\n"
  chiro == chiro2

  -- chiro2circuits
  "topcomfoo.in" << chiro << endl << [] << endl << close;
  circs = get ("!"|toppath|"/chiro2circuits"|"  <topcomfoo.in")
  cocircs = get ("!"|toppath|"/chiro2cocircuits"|"  <topcomfoo.in")
  drop(drop(circs, 2), -1)
  oo/value

r12'chiro = "12, 4:
-+--+++---++---++-+---++-+++-----++--++-++++--++---+++-+++--+---++---++--++-++++
--+---++-+++--+++--++--+----+-+++--+++--++--+----+---++--++-++++-++--+-----+----
-++---++---+++-+++--+---++---++--++-++++--+---++-+++--+++--++--+----+-+++--+++--
++--+----+---++--++-++++---++-+++++-+++++--++++---++-+++--+++--++--+----+-+++--+
++--++--+----+---++--++-++++---++-+++++-+++++--+++---++---++--++-++++-+++--++--+
----+++--+-----+-----++--+++--++--+----+++--+-----+-----++----++-+++++-+++++--++
--------++--++-
"
  chiro = r12'chiro
  -- chiro2alltriangs, chiro2nalltriangs
  "topcomfoo.in" << "5, 3:" << endl << chiro << endl << [] << endl << close;
  "topcomfoo.in" << chiro << [] << endl << close;
  get ("!"|toppath|"/chiro2placingtriang"|" -v <topcomfoo.in")
  get ("!"|toppath|"/chiro2circuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2cocircuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2alltriangs"|" <topcomfoo.in");
  get ("!"|toppath|"/chiro2ntriangs"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2finetriang"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2finetriangs"|" <topcomfoo.in") -- what is the format of the output here??
  get ("!"|toppath|"/chiro2nfinetriangs"|" -v <topcomfoo.in")
  
///

///
-- code for testing topcom functions, part 2.
  restart
  debug needsPackage "Topcom"
  needsPackage "ReflexivePolytopesDB"
  needsPackage "StringTorics" 
  toppath = "/opt/homebrew/bin/"
 
  polytopes = kreuzerSkarke(50, Limit=>10);
  tope = polytopes_5
  A = matrix tope
  P = convexHull A
  P2 = polar P
  A = matrix{latticePoints P2}

  LP = drop(latticePointList P2, -1);
  A = transpose matrix LP;
  --debugLevel = 6
  elapsedTime tri = topcomRegularFineTriangulation A;
  assert topcomIsTriangulation(A, tri)
  assert topcomIsRegularTriangulation(A, tri)
  
  -- XXX
  augment A
  "topcomfoo.in" << topcomPoints(augment A, Homogenize=>false) << endl << close;
  chiro = get ("!"|toppath|"/points2chiro"|" <topcomfoo.in");
  "topcomfoo.in" << chiro << "[]" << endl << close;
  get ("!"|toppath|"/chiro2circuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2ntriangs"|" <topcomfoo.in")
  --get ("!"|toppath|"/chiro2alltriangs"|"  <topcomfoo.in")
  get ("!"|toppath|"/chiro2cocircuits"|" <topcomfoo.in")    
///

///
-- code for testing topcom functions, part 3.
-- how to check a triangulation?  I don't think that Topcom has this implemented for general use.
-*
  restart
  debug needsPackage "Topcom"
*-
  -- test of topcomIsRegularTriangulation
  toppath = "/opt/homebrew/bin/"
  A = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  badtri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2}}
  debugLevel = 6

  -- a regular triangulation
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  "topcomfoo.in" << topcomPoints(A, Homogenize=>true) << endl << "[]" << endl << tri << endl << close;
  run (toppath|"checkregularity"|" --heights <topcomfoo.in >topcomfoo.out")  

  -- points2chiro
  "topcomfoo.in" << topcomPoints(A, Homogenize=>false) << endl << "[]" << endl << badtri << endl << close;
  print (toppath|"/points2alltriangs"|" --checktriang -v <topcomfoo.in") 


  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  "topcomfoo.in" << topcomPoints(A, Homogenize=>true) << endl << "[]" << endl << tri << endl << close;
  run (toppath|"checkregularity"|" --heights <topcomfoo.in >topcomfoo.out")
  assert not topcomIsRegularTriangulation(A,tri)
///

///
-- generate examples to use for this package
-- from reflexive polytopes of dim 4
  restart
  needsPackage "StringTorics"

  topes = kreuzerSkarke(10, Limit=>5)
--  topes = kreuzerSkarke(20, Limit=>5)
--  topes = kreuzerSkarke(30, Limit=>5)
  tope = topes_4
  A = matrix tope
  P = convexHull A
  P2 = polar P
  LP = drop(latticePointList P2, -1)
  A1 = transpose matrix LP
  A2 = transpose matrix latticePointList P2
  tri = topcomRegularFineTriangulation A1
  tri2 = topcomRegularFineTriangulation A2
  #tri
  #tri2
  elapsedTime chiro1 = chirotopeString A1;
  elapsedTime chiro2 = chirotopeString A2;
  elapsedTime # orientedCircuits chiro1
  elapsedTime # orientedCircuits chiro2
  elapsedTime # orientedCocircuits chiro1
  elapsedTime # orientedCocircuits chiro2
  (select(orientedCocircuits A2, f -> #f#0 == 0 or #f#1 == 0))/first
  netList annotatedFaces P2  
  tri2
  -- I'm not quite sure what the following code is trying to do.  Maybe
  -- checking triangulations using cocircuits?
  -- fine:
  assert(sort unique flatten tri2 == toList (0..14))
  walls = tri2/(x -> subsets(x, #x-1))//flatten
  nfacets = tally walls
  facs = (select((annotatedFaces P2), x -> x_0 == 3))/(x -> x#2)
  walls = partition(k -> nfacets#k, keys nfacets)
  for w in walls#1 list (
      # select(facs, f -> isSubset(w, f))
      )
  for w in walls#2 list (
      # select(facs, f -> isSubset(w, f))
      )
  -- check overlaps of elements of tri2:
  C = orientedCircuits A2;
  elapsedTime for c in C list (
      val1 := select(tri2, x -> isSubset(c_0, x));
      val2 := select(tri2, x -> isSubset(c_1, x));
      if #val1 > 0 and #val2 > 0 then print (c, val1, val2);
      (c, #val1, #val2));
  
  tri_0
///
  
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
  Caveat
  SeeAlso
///

