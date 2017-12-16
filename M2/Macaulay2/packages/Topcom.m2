newPackage(
        "Topcom",
        Version => "0.1", 
        Date => "",
        Authors => {{
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"http://www.math.cornell.edu/~mike"
                }},
        Headline => "interface to a small part of topcom",
        Configuration => {
            "path" => ""
            },
        DebuggingMode => true
        )

export {
    "isRegularTriangulation",
    "regularFineTriangulation"
    }

exportMutable {
    "topcompath"
    }

topcompath = Topcom#Options#Configuration#"path"
if topcompath == "" then topcompath = prefixDirectory | currentLayout#"programs"

augment = (A) -> (
    -- A is a matrix over ZZ
    -- add in a last row of 1's.
    n := numColumns A;
    ones := matrix {{n : 1}};
    A || ones
    )

isRegularTriangulation = method()
isRegularTriangulation(Matrix, List) := (A, tri) -> (
    A1 := augment A;
    -- now create the output file
    filename := temporaryFileName();
    infile := filename|".in";
    outfile := filename|".out";
    F := infile << (new Array from 
        apply(entries transpose A1, 
            a -> new Array from a)) << endl;
    F << "[]" << endl;
    F << tri << endl << close;
    executable := topcompath|"checkregularity --checktriang -v <"|infile|" 2>"|outfile|" 1>/dev/null";
    if debugLevel >= 1 then << "-- isRegularTriangulation: using temporary file prefix " << filename << endl;
    if debugLevel >= 2 then << "-- isRegularTriangulation: executing " << executable << endl;
    run(executable);
    output := get(outfile);
    if debugLevel >= 5 then << "-- isRegularTriangulation: output = " << net output << endl;
    match("Checked 1 triangulations, 0 non-regular so far", output)
    )

regularFineTriangulation = method()
regularFineTriangulation Matrix := (A) -> (
    -- if you want the origin, you should make it a column of A.
    -- columns should be the points.
    A1 := augment A;
    filename := temporaryFileName();
    infile := filename|".in";
    outfile := filename|".out";
    errfile := filename|".err";
    -- now create the output file
    F := infile << (new Array from 
        apply(entries transpose A1, 
            a -> new Array from a)) << endl;
    F << endl << close;
    executable := topcompath|"points2finetriang --regular <"|infile|" 1>"|outfile|" 2>"|errfile;
    if debugLevel >= 1 then << "-- regularFineTriangulation: using temporary file prefix " << filename << endl;
    if debugLevel >= 2 then << "-- regularFineTriangulation: executing " << executable << endl;
    run executable;
    output := get outfile;
    if debugLevel >= 5 then << "-- regularFineTriangulation: output = " << net output << endl;    
    if debugLevel >= 6 then << "-- regularFineTriangulation: stderr = " << net get errfile << endl;    
    tri := value output;
    tri
    )

beginDocumentation()

doc ///
Key
  Topcom
Headline
  interface to selected functions from topcom package
Description
  Text
    This package implements two key functions from the topcom package (XXX).
    @TO "isRegularTriangulation"@  checks whether a triangulation of a point set is a regular triangulation,
    and @TO "regularFineTriangulation"@ computes a triangulation which involves all lattice points of a polytope.
Caveat
SeeAlso
///

TEST ///
  needsPackage "Topcom"
  -- test of isRegularTriangulation
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert not isRegularTriangulation(A,tri)

  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert isRegularTriangulation(A,tri)
  
  A = transpose matrix {{1,0},{0,1}}
  tri = {{0,1}}
  assert isRegularTriangulation(A,tri)

  A = transpose matrix {{0}}
  tri = {{0}}
  assert isRegularTriangulation(A,tri)

  -- hmmm, we can make non-sensical triangulations, without it noticing.
  -- this should be a bug?  
  A = transpose matrix {{0,0},{0,1},{1,0},{1,1}}
  tri = {{0,1,2},{0,2,3}}
  assert isRegularTriangulation(A,tri)  
  tri = {{0,1,2},{1,2,3}}
  assert isRegularTriangulation(A,tri) 

///

TEST ///  
  needsPackage "Topcom"
  needsPackage "Polyhedra"
  
  A = transpose matrix {{-1,-1,2},{-1,0,1},{-1,1,1},{0,-1,2},{0,1,1},{1,-1,3},{1,0,-1},{1,1,-2}}
  debugLevel = 0
  tri = regularFineTriangulation A
  assert isRegularTriangulation(A, tri)

  A = transpose matrix {{-1, 0, -1, -1}, {-1, 0, 0, -1}, {-1, 1, 2, -1}, {-1, 1, 2, 0}, {1, -1, -1, -1}, {1, -1, -1, 1}, {1, 0, -1, 2}, {1, 0, 1, 2}}
  P2 = polar convexHull A
  C = matrix {latticePoints P2}
  tri = regularFineTriangulation C
  assert isRegularTriangulation(C, tri)
///

end--


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

restart
needsPackage "Topcom"
check "Topcom"
