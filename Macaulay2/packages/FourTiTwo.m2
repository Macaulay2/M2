newPackage(
	"FourTiTwo",
    	Version => "0.1", 
    	Date => "March 11, 2007",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => ""}
	     },
    	Headline => "Interface to the 4ti2 package",
	Configuration => { "path" => "",
	     "keep files" => true
	      },
    	DebuggingMode => true
    	)

export {
     toBinomial,
     getMatrix,
     putMatrix,
     markovBasis,
     toricGB
     }

path'4ti2 = FourTiTwo#Options#Configuration#"path"

getFilename = () -> (
     filename := temporaryFileName();
     while fileExists filename do filename = temporaryFileName();
     filename)

putMatrix = method()
putMatrix(File,Matrix) := (F,B) -> (
     B = entries B;
     numrows := #B;
     numcols := #B#0;
     F << numrows << " " << numcols << endl;
     for i from 0 to numrows-1 do (
	  for j from 0 to numcols-1 do (
	       F << B#i#j << " ";
	       );
	  F << endl;
	  );
     )

getMatrix = method()
getMatrix String := (filename) -> (
     L := lines get filename;
     L = drop(L,1);
     matrix apply(L, v -> (w := value("{" | replace(" +",",",v)|"}"); select(w, wi -> wi =!= null)))
     )     

toBinomial = method()
toBinomial(Matrix,Ring) := (M,S) -> (
     toBinom := (b) -> (
       pos := 1_S;
       neg := 1_S;
       scan(#b, i -> if b_i > 0 then pos = pos*S_i^(b_i)
                   else if b_i < 0 then neg = neg*S_i^(-b_i));
       pos - neg);
     ideal apply(entries M, toBinom)
     )

markovBasis = method(Options=>{Weights=>null})
markovBasis Matrix := o -> (A) -> (
     filename := getFilename();
     F := openOut(filename);
     putMatrix(F,A);
     close F;
     if o.Weights =!= null then (
	  cost = concatenate apply(o.Weights, x -> (x|" "));
	  (filename|".cost") << "1 " << #o.Weights << endl << cost << endl  << close;
	  );
     execstr = path'4ti2|"markov -q "|filename;
     ret := run(execstr);
     if ret =!= 0 then error "error occurred while executing external program 4ti2: markov";
     getMatrix(filename|".mar")
     )
markovBasis(Matrix,Ring) := o -> (A,S) -> toBinomial(markovBasis(A,o), S)

toricGB = method(Options=>{Weights=>null})
toricGB Matrix := o -> (A) -> (
     filename := getFilename();
     F := openOut(filename);
     putMatrix(F,A);
     close F;
     if o.Weights =!= null then (
	  cost = concatenate apply(o.Weights, x -> (x|" "));
	  (filename|".cost") << "1 " << #o.Weights << endl << cost << endl  << close;
	  );
     execstr = path'4ti2|"groebner -q "|filename;
     ret := run(execstr);
     if ret =!= 0 then error "error occurred while executing external program 4ti2: groebner";
     getMatrix(filename|".gro")
     )
toricGB(Matrix,Ring) := o -> (A,S) -> toBinomial(toricGB(A,o), S)


beginDocumentation()


end
restart
load "4ti2.m2"

debug FourTiTwo

A = matrix{{1,1,1,1},{0,1,3,4}}
B = syz A
time markovBasis A
R = QQ[a..d]
time markovBasis(A,R,Weights=>{1,2,3,4})
time markovBasis(A,R)
time toricGB(A,R)

7 9
A = matrix"
1,1,1,-1,-1,-1, 0, 0, 0;
1,1,1, 0, 0, 0,-1,-1,-1;
0,1,1,-1, 0, 0,-1, 0, 0;
1,0,1, 0,-1, 0, 0,-1, 0;
1,1,0, 0, 0,-1, 0, 0,-1;
0,1,1, 0,-1, 0, 0, 0,-1;
1,1,0, 0,-1, 0,-1, 0, 0"
transpose A
markovBasis transpose A


27 27
A = matrix"
1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0;
0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0;
0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0;
0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0;
0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0;
0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0;
0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0;
0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0;
0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1;
1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0;
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0;
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1;
1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0;
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0;
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1"
markovBasis A
R = QQ[x_1..x_27]
markovBasis(A,R)
toricGB(A,R)
gens gb oo
