-- -*- coding: utf-8 -*-
----------------------------------------------------
----------------------------------------------------
-- previous version: 0.2 30Jun08, submitted by Josephine Yu.
-- author: Mike Stillman -- 
--     	    	      	   core
-- author: Josephine Yu -- 
--     	    	      	   all remaining functions; documentation
-- Sonja Petrovic -- 
--     	    	      	   interface for windows; edited documentation; tests
--     	    	      	   
-- latest major update: 6Jul08;  
-- small revision in Documentation: 6oct08; SP.
----------------------------------------------------
----------------------------------------------------

newPackage(
	"FourTiTwo",
    	Version => "1.0", 
    	Date => "February 8, 2009",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu"},
	     {Name => "Josephine Yu", Email => "jyu@math.mit.edu"},
	     {Name => "Sonja Petrovic", Email => "petrovic@psu.edu"}
	     },
    	Headline => "Interface to 4ti2",
	Keywords => {"Interfaces"},
	Configuration => { "path" => "",
	     "keep files" => true
	      },
	PackageExports => {"Polyhedra"}, -- for hilbertBasis
    	DebuggingMode => false
    	)

export {
     "toBinomial",
     "getMatrix",
     "putMatrix",
     "toricMarkov",
     "toricGroebner",
     "toricCircuits",
     "toricGraver",
     "toricGraverDegrees",
     -- "hilbertBasis", -- defined in Polyhedra
     "InputType",
     }

-- for backward compatibility
if not programPaths#?"4ti2" and FourTiTwo#Options#Configuration#"path" != ""
    then programPaths#"4ti2" = FourTiTwo#Options#Configuration#"path"

fourTiTwo = null
debugLimit = 5

run4ti2 = (exe, args) -> (
    if fourTiTwo === null then
	fourTiTwo = findProgram("4ti2", "markov -h",
	    Prefix => {(".*", "4ti2-"), -- debian
		       (".*", "4ti2_")}, -- suse
	    AdditionalPaths =>
		{"/usr/lib/4ti2/bin", "/usr/lib64/4ti2/bin"}); -- fedora
     runProgram(fourTiTwo, exe, args)
)

getFilename = () -> (
     filename := temporaryFileName();
     while fileExists(filename) or fileExists(filename|".mat") or fileExists(filename|".lat") do filename = temporaryFileName();
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
     l := toString first L;
     v := value("{" | replace(" +",",",l)|"}"); 
     dimensions := select(v, vi -> vi =!= null);
     if dimensions#0 == 0 then ( -- matrix has no rows
	  matrix{{}}
     ) else(
     	  L = drop(L,1);
     	  --L = select(L, l-> regex("^[:space:]*$",l) === null);--remove blank lines
     	  matrix select(
	       apply(L, v -> (w := value("{" | replace(" +",",",v)|"}"); w = select(w, wi -> wi =!= null))),
	       row -> row =!= {}
     	       ) 
     ))    

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

toricMarkov = method(Options=> {InputType => null, Precision => 64})
toricMarkov Matrix := Matrix => o -> (A) -> (
     filename := getFilename();
     if debugLevel >= debugLimit then << "using temporary file name " << filename << endl;
     if o.InputType === "lattice" then
     	  F := openOut(filename|".lat")
     else 
       	  F = openOut(filename|".mat");
     putMatrix(F,A);
     close F;
     run4ti2("markov",
	 "-p " | toString o.Precision | " " | rootPath | filename);
     getMatrix(filename|".mar")
     )
toricMarkov(Matrix,Ring) := o -> (A,S) -> toBinomial(toricMarkov(A,o), S)

toricGroebner = method(Options=>{Weights=>null, Precision => 64})
toricGroebner Matrix := o -> (A) -> (
     filename := getFilename();
     if debugLevel >= debugLimit then << "using temporary file name " << filename << endl;
     F := openOut(filename|".mat");
     putMatrix(F,A);
     close F;
     if o.Weights =!= null then (
	  cost := concatenate apply(o.Weights, x -> (x|" "));
	  (filename|".cost") << "1 " << #o.Weights << endl << cost << endl  << close;
	  );
     run4ti2("groebner",
	 "-p " | toString o.Precision | " " | rootPath | filename);
     getMatrix(filename|".gro")
     )
toricGroebner(Matrix,Ring) := o -> (A,S) -> toBinomial(toricGroebner(A,o), S)

toricCircuits = method(Options => {Precision => 64})
toricCircuits Matrix := Matrix => (o -> A ->(
     filename := getFilename();
     if debugLevel >= debugLimit then << "using temporary file name " << filename << endl;
     F := openOut(filename|".mat");
     putMatrix(F,A);
     close F;
     run4ti2("circuits",
	 "-p " | toString o.Precision | " " | rootPath | filename);
     getMatrix(filename|".cir")
     ))

toricGraver = method(Options => {Precision => 32})
toricGraver Matrix := Matrix => (o -> A ->(
     filename := getFilename();
     if debugLevel >= debugLimit then << "using temporary file name " << filename << endl;
     F := openOut(filename|".mat");
     putMatrix(F,A);
     close F;
     run4ti2("graver",
	 "-q -p " | toString o.Precision | " " | rootPath | filename);
     getMatrix(filename|".gra")
     ))
toricGraver (Matrix,Ring) := Ideal => (o -> (A,S)->toBinomial(toricGraver(A),S))

-- hilbertBasis is defined in Polyhedra
hilbertBasis Matrix := Matrix => { InputType => null, Precision => 32 } >> o -> A -> (
     filename := getFilename();
     if debugLevel >= debugLimit then << "using temporary file name " << filename << endl;
     if o.InputType === "lattice" then
     	  F := openOut(filename|".lat")
     else 
       	  F = openOut(filename|".mat");
     putMatrix(F,A);
     close F;
     run4ti2("hilbert",
	 "-p " | toString o.Precision | " " | rootPath | filename);
     getMatrix(filename|".hil")
     )

rays Matrix := Matrix => { Precision => 64 } >> o -> (A ->(
     filename := getFilename();
     if debugLevel >= debugLimit then << "using temporary file name " << filename << endl;
     F := openOut(filename|".mat");
     putMatrix(F,A);
     close F;
     run4ti2("rays",
	 "-p " | toString o.Precision | " " | rootPath | filename);
     getMatrix(filename|".ray")
     ))

-- SP: the output command interface
-- I would like to have a command that gives the list of degrees of Graver/Groebner/Circuit/Markov file;
-- the way 4ti2 does this is you tell it the whatever.mar or whatever.cir file and it writes the degrees
-- to the screen.
-- On the other hand, it doesn't matter because you can ask M2 for those degrees directly! 
toricGraverDegrees = method(Options => {Precision => 32})
toricGraverDegrees Matrix := Matrix => (o -> A ->(
     filename := getFilename();
     if debugLevel >= debugLimit then << "using temporary file name " << filename << endl;
     F := openOut(filename|".mat");
     putMatrix(F,A);
     close F;
     run4ti2("graver",
	 "-p " | toString o.Precision | " " | rootPath | filename);
     ret := run4ti2("output", "--degrees " | rootPath | filename|".gra");
     print ret#"output"
     ))


beginDocumentation()

doc ///
     Key 
          FourTiTwo
     Headline
     	  Interface for 4ti2
     Description
          Text
	       Interfaces most of the functionality of the software {\tt 4ti2} available at
	       @HREF"http://www.4ti2.de/"@.
	       (The user needs to have {\tt 4ti2} installed on his/her machine.)
	        
	       A $d\times n$ integral matrix $A$ (with nonnegative entries) specifies a map from a polynomial 
	       ring in d variables to a polynomial ring with n variables by specifying exponents of the variables indexing
	       its columns. For example, if $A$ is a matrix 
	       $$\begin{pmatrix}
	       3&2&1&0\\
	       0&1&2&3
	       \end{pmatrix}$$
	       the map from $k[s,t]$ to $k[a,b,c,d]$ is given by
	       $(s,t) \mapsto \ (s^3,s^2t,st^2,t^3)$.
	       
	       The toric ideal $I_A$ is the kernel of this map. 
	       It is minimally generated by the 2-minors of the matrix
	       $$\begin{pmatrix}
	       x&y&z\\
	       y&z&w
	       \end{pmatrix}$$
	       Given the matrix $A$, one can compute its lattice basis ideal specified by the integral basis
	       of the lattice $A$, the toric ideal $I_A$, its Groebner bases, etc. In practice, however, 
	       these are nontrivial computational tasks. 
	       The software {\tt 4ti2} is very efficient in computing these objects. 	      
	       
	       For more theoretical details (and more generality), see the standard reference: 
	       B. Sturmfels, {\bf Gr\"obner bases and convex polytopes.} 
	       American Mathematical Society, University Lectures Series, No 8, Providence, Rhode Island, 1996. 
	       
               {\bf Note for cygwin users:} 
	       If a problem occurs during package installation and/or loading, it should be fixed 
	       by setting the path inside the file {\tt .Macaulay2/init-FourTiTwo.m2}  to whatever folder {\tt 4ti2} is installed.
	       For example, if  {\tt 4ti2} has been installed in {\tt C:/cygwin/4ti2/win32}, then the line 
	       inside the {\tt init-FourTiTwo.m2} file will look like this:  {\tt "path" => "C:/cygwin/4ti2/win32/"}  .
	       Alternately, the path for {\tt 4ti2} may be set when loading the package using the following command:
	         loadPackage("FourTiTwo", Configuration=>{"path"=>"C:/cygwin/4ti2/win32/"})
	       assuming that 4ti2 has been installed in C:/cygwin/4ti2/win32.
      	       
     Caveat
      	       If the package SimpleDoc is not found when installing {\tt FourTiTwo.m2}, 
	       see questions and answers 6, 7, and 8 on the Macaulay 2 web site.	       
///;

doc ///
    Key
	getMatrix
	(getMatrix, String)
    Headline
	reads a matrix from a 4ti2-formatted input file
    Usage
	getMatrix s
    Inputs
    	s:String
	     file name
    Outputs
	A:Matrix
    Description
	Text
	     The file should contain a matrix in the format such as

	     2 4\break
	     1 1 1 1\break
	     1 2 3 4\break

	     The first two numbers are the numbers of rows and columns.
    SeeAlso
	putMatrix
///;

doc ///
     Key 
	  putMatrix
     	  (putMatrix,File,Matrix)
     Headline
     	  writes a matrix into a file formatted for 4ti2
     Usage
     	  putMatrix(F,A)
     Inputs
     	  F:File
       	  A:Matrix
     Description
     	  Text
		Write the matrix {\tt A} in file {\tt F} in {\tt 4ti2} format.
	  Example
		A = matrix "1,1,1,1; 1,2,3,4"
		s = temporaryFileName()
		F = openOut(s)
		putMatrix(F,A)
		close(F)
		getMatrix(s)
     SeeAlso
     	  getMatrix
///;

doc ///
     Key
          toBinomial
     	  (toBinomial, Matrix, Ring)	  
     Headline
     	  creates a toric ideal from a given set of exponents of its generators
     Usage
     	  toBinomial(M,R)
     Inputs
     	  M: Matrix
	  R: Ring
	       ring with as least as many generators as the columns of {\tt M}
     Outputs
     	  I: Ideal
     Description
     	  Text
	       Equivalent to "output --binomials" in 4ti2.
	       Returns the ideal in the ring {\tt R} generated by the binomials corresponding to rows of {\tt M}.
	  Example
	       A = matrix "1,1,1,1; 1,2,3,4"
	       B = syz A 
	       R = QQ[a..d]	     
	       toBinomial(transpose B,R)
///;

doc ///
     Key
     	  toricGroebner
          (toricGroebner, Matrix)
     	  (toricGroebner, Matrix, Ring)
     	  [toricGroebner, Weights]
	  [toricGroebner, Precision]
     Headline
     	  calculates a Groebner basis of the toric ideal I_A, given A; invokes "groebner" from 4ti2
     Usage
     	  toricGroebner(A) or toricGroebner(A,R)
     Inputs
      	  A:Matrix    
	       whose columns parametrize the toric variety. The toric ideal $I_A$ is the kernel of the map defined by {\tt A}.
     	  R:Ring
	       ring with as least as many generators as the columns of {\tt A}
	  Precision => {ZZ, String}
	       32, 64, or "arbitrary", the precision of the integers used during
	       the computation
     Outputs
     	  B:Matrix 
	       whose rows give binomials that form a Groebner basis of the toric ideal of {\tt A}
     	  I:Ideal
	       whose generators form a Groebner basis for the toric ideal
     Description
	  Example
	       A = matrix "1,1,1,1; 1,2,3,4"
	       toricGroebner(A)
	  Text
	       Note that the output of the command is a matrix whose rows are the exponents of the binomials that for a Groebner basis of the 
	       toric ideal $I_A$.
	       As a shortcut, one can ask for the output to be an ideal instead:
	  Example
	       R = QQ[a..d]
	       toricGroebner(A,R)
	  Text
	       {\tt 4ti2} offers the use of weight vectors representing term orders, as follows:
	  Example
	       toricGroebner(A,Weights=>{1,2,3,4})
     Caveat
      	       It seems that some versions of 4ti2 do not pick up on the weight vector. It may be better to run gb computation in M2 directly with specified weights.	       

 ///;
 
 doc ///
     Key
     	  toricMarkov
          (toricMarkov, Matrix)
	  (toricMarkov, Matrix, Ring)
	  [toricMarkov, InputType]
	  [toricMarkov, Precision]
     Headline
     	  calculates a generating set of the toric ideal I_A, given A; invokes "markov" from 4ti2
     Usage
     	  toricMarkov(A) or toricMarkov(A, InputType => "lattice") or toricMarkov(A,R)
     Inputs
      	  A:Matrix
	       whose columns parametrize the toric variety; the toric ideal is the kernel of the map defined by {\tt A}.
	       Otherwise, if InputType is set to "lattice", the rows of {\tt A} are a lattice basis and the toric ideal is the 
	       saturation of the lattice basis ideal.	       
	  InputType=>String
	       which is the string "lattice" if rows of {\tt A} specify a lattice basis
	  R:Ring
	       polynomial ring in which the toric ideal $I_A$ should live
	  Precision => {ZZ, String}
	       32, 64, or "arbitrary", the precision of the integers used during
	       the computation
     Outputs
     	  B:Matrix 
	       whose rows form a Markov Basis of the lattice $\{z : z \text{ is integral and } A z = 0\}$
	       or the lattice spanned by the rows of {\tt A} if the option {\tt InputType => "lattice"} is used
     Description
     	  Text
	       Suppose we would like to comput the toric ideal defining the variety parametrized by the following matrix:
	  Example
	       A = matrix"1,1,1,1;0,1,2,3"
	  Text
	       Since there are 4 columns, the ideal will live in the polynomial ring with 4 variables.
	  Example
	       R = QQ[a..d]
	       M = toricMarkov(A)
	  Text
	       Note that rows of M are the exponents of minimal generators of $I_A$.  To get the ideal, we can do the following:
	  Example
      	       I = toBinomial(M,R)
	  Text
	       Alternately, we might wish to give a lattice basis ideal instead of the matrix A. The lattice basis will be specified 
	       by a matrix, as follows:
	  Example
	       B = syz A 
	       N = toricMarkov(transpose B, InputType => "lattice")	  
	       J = toBinomial(N,R) -- toricMarkov(transpose B, R, InputType => "lattice")	     
	  Text
	       We can see that the two ideals are equal:
	  Example
     	       I == J
	  Text
	       Also, notice that instead of the sequence of commands above, we could have used the following:
	  Example
	       toricMarkov(A,R)	       
///;

doc ///
     Key
     	  toricGraver
          (toricGraver, Matrix)
     	  (toricGraver, Matrix, Ring)
	  [toricGraver, Precision]
     Headline
     	  calculates the Graver basis of the toric ideal; invokes "graver" from 4ti2
     Usage
     	  toricGraver(A) or toricGraver(A,R)
     Inputs
      	  A:Matrix    
	       whose columns parametrize the toric variety. The toric ideal $I_A$ is the kernel of the map defined by {\tt A}
	  R:Ring
	       polynomial ring in which the toric ideal $I_A$ should live
	  Precision => {ZZ, String}
	       32, 64, or "gmp", the precision of the integers used during the
	       computation
     Outputs
     	  B:Matrix 
	       whose rows give binomials that form the Graver basis of the toric ideal of {\tt A}, or
     	  I:Ideal
	       whose generators form the Graver basis for the toric ideal
     Description
     	  Text
	       The Graver basis for any toric ideal $I_A$ contains (properly) the union of all reduced Groebner basis of $I_A$.  
	       Any element in the Graver basis of the ideal is called a primitive binomial.
	  Example
	       A = matrix "1,1,1,1; 1,2,3,4"
	       toricGraver(A)
	  Text
	       If we prefer to store the ideal instead, we may use:
	  Example
	       R = QQ[a..d]
	       toricGraver(A,R)
	  Text
	       Note that this last ideal equals the toric ideal $I_A$ since every Graver basis element is actually in $I_A$.
///;

doc ///
     Key
     	  toricGraverDegrees
          (toricGraverDegrees, Matrix)
	  [toricGraverDegrees, Precision]
     Headline
     	  displays the degrees of all Graver basis elements for the toric ideal I_A
     Usage
     	  toricGraverDegrees(A) 
     Inputs
      	  A:Matrix    
	       whose columns parametrize the toric variety. The toric ideal $I_A$ is the kernel of the map defined by {\tt A}
	  Precision => {ZZ, String}
	       32, 64, or "gmp", the precision of the integers used during the
	       computation
     Description
     	  Text
	       Equivalent to "output --degrees foo.gra" in 4ti2.
	       Very often the Graver basis consists of too many binomials, and one is only interested in their degrees. In this case,
	       instead of looking at the Graver basis of $I_A$, we may just want to look for the degrees of binomials which show up:
	  Example
	       A = matrix "1,1,1,1; 1,2,3,4"
	       toricGraver(A) -- the Graver basis
	       toricGraverDegrees(A) -- just the degrees
	  Text
	       Note that these are all 1-norms of the vectors. Since $I_A$ is homogeneous, there are 3 binomials of degree 2 (norm 4) 
	       and 2 binomials of degree 3 (norm 6).
	       
	       Here is a more complicated example, where one may not want to see the Graver basis elements explicitly. 
	       The toric ideal I_M is the ideal of the rational normal scroll S(3,2,3):
	  Example
	       M = matrix "1,1,1,1,1,1,1,1,1,1,1; 1,1,1,1,0,0,0,0,0,0,0; 0,0,0,0,1,1,1,0,0,0,0; 0,0,0,0,0,0,0,1,1,1,1; 1,2,3,4,1,2,3,1,2,3,4"
	       toricGraverDegrees(M)
	  Text
	       Here is another example where with many Graver basis elements. The following matrix is a design matrix for a particular statistical model for a 4-node p1 network; see Fienberg-Petrovic-Rinaldo.
	  Example
	        A = matrix "1,0,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0;0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,0,0,0;0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1;0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1;0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0;1,0,1,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0;0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,1,1;0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1;0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1";
                toricGraverDegrees(A)
///;


doc ///
     Key
          (hilbertBasis, Matrix)
	 [(hilbertBasis, Matrix), InputType]
	 [(hilbertBasis, Matrix), Precision]
     Headline
     	  calculates the Hilbert basis of the cone; invokes "hilbert" from 4ti2
     Usage
     	  hilbertBasis(A) or hilbertBasis(A, InputType => "lattice")
     Inputs
      	  A:Matrix    
	       defining the cone $\{z : Az = 0, z \ge 0 \}$
	  InputType => String
	       use the string "lattice" if rows of {\tt A} specify a lattice basis
	  Precision => {ZZ, String}
	       32, 64, or "gmp", the precision of the integers used during the
	       computation
     Outputs
     	  B:Matrix 
	       whose rows form the Hilbert basis of the cone $\{z : Az = 0, z \ge 0 \}$
	       or the cone $\{z A : z \text{ is an integral vector and } z A \ge 0 \}$ if {\tt InputType => "lattice"} is used
     Description
	  Example
	       A = matrix "1,1,1,1; 1,2,3,4"
	       B = syz A
	       hilbertBasis(transpose B)
     	       hilbertBasis(A, InputType => "lattice")     	       
///;


doc ///
     Key
          (rays, Matrix)
     Headline
     	  calculates the extreme rays of the cone; invokes "rays" from 4ti2
     Usage
     	  rays(A)
     Inputs
      	  A:Matrix   
	       defining the cone $\{z : Az = 0, z \ge 0 \}$
	  Precision => {ZZ, String}
	       32, 64, or "arbitrary", the precision of the integers used during
	       the computation
     Outputs
     	  B:Matrix 
	       whose rows are the extreme rays of the cone $\{z : Az = 0, z \ge 0 \}$
     Description
	  Example
	       A = matrix "1,1,1,1; 1,2,3,4"
	       B = syz A
	       rays(transpose B)
///;



doc ///
     Key
     	  toricCircuits
          (toricCircuits, Matrix)
	  [toricCircuits, Precision]
     Headline
     	  calculates the circuits of the toric ideal; invokes "circuits" from 4ti2
     Usage
     	  toricCircuits(A)
     Inputs
      	  A:Matrix    
               whose columns parametrize the toric variety. The toric ideal $I_A$ is the kernel of the map defined by {\tt A} 
	  Precision => {ZZ, String}
	       32, 64, or "arbitrary", the precision of the integers used during
	       the computation
     Outputs
     	  B:Matrix 
	       whose rows form the circuits of A
     Description
     	  Text
	       The circuits are contained in the Graver basis of $I_A$. In fact, they are precisely the primitive binomials in the ideal
	       with  minimal support.
	  Example
	       A = matrix "1,1,1,1; 1,2,3,4"
	       C = toricCircuits A
	  Text 
	       The ideal generated by the circuits of A in general differs from the toric ideal of A. For example:
	  Example
	       R = QQ[a..d]
	       Icircuit = toBinomial(toricCircuits(A), R) -- this is the circuit ideal of A
	       I = toBinomial(toricMarkov(A), R)
	       I==Icircuit  
	  Text
	       The two ideals are not the same. There is a minimal generator of I which is not a circuit:
	  Example
	       a*d-b*c % I -- this binomial is in I:
	       a*d-b*c % Icircuit -- but not in Icircuit:
///;

doc ///
     Key
     	  InputType
     Description
          Text
     	      Put {\tt InputType => "lattice"} as an argument in the functions toricMarkov and hilbertBasis
     SeeAlso
     	  toricMarkov
	  hilbertBasis
///;


TEST/// 
  A = matrix "1,1,1,1; 1,2,3,4"
  M = toricMarkov(A)
  R = QQ[x_0,x_1,x_2,x_3]
  I = toBinomial(M,R)
  Irnc3 = ideal(x_0*x_2-x_1^2,x_1*x_3-x_2^2,x_0*x_3-x_1*x_2)
  assert(I==Irnc3)
///
TEST ///   
  B = matrix "1,-2,1,0; 0,1,-2,1"
  M = toricMarkov(B, InputType => "lattice")
  R = QQ[x_0,x_1,x_2,x_3]
  I = toBinomial(M,R)
  Irnc3 = ideal(x_0*x_2-x_1^2,x_1*x_3-x_2^2,x_0*x_3-x_1*x_2)
  assert(I== Irnc3)  
///
TEST ///     
  R=CC[x_0,x_1,x_2,x_3]
  A = matrix "1,1,1,1; 1,2,3,4"
  C = toricCircuits(A)  --circuits returned by 4ti2
  Icir = toBinomial(C,R) -- circuit ideal returned by 4ti2
  Ctrue = matrix{{0,1,-2,1},{1,-2,1,0},{1,0,-3,2},{2,-3,0,1}} --known: all circuits
  IcirTrue = toBinomial(Ctrue,R) --known: circuit ideal
  Irnc3 = ideal(x_0*x_2-x_1^2,x_1*x_3-x_2^2,x_0*x_3-x_1*x_2)
  assert(Icir==IcirTrue)
  shouldBeFalse = (Icir==Irnc3)
  assert(shouldBeFalse==false)
  assert(target C == target Ctrue)
  assert(source C == source Ctrue)
///
TEST ///     
  B = matrix "1,-2,1,0; 0,1,-2,1"  
  R = QQ[a..d]
  I = toBinomial(B,R)
  assert(a*c-b^2 % I == 0)
  assert(a*c-d^2 %I =!= 0)
  assert(degree I == 4)
  M = hilbertBasis B
  assert(numrows M == 3)
  assert(numcols M == 4)     
  M1 = rays B
  assert(numrows M1 == 2)
  assert(numcols M1 == 4)
///
TEST///
  A = matrix "1,0,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0;0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,0,0,0;0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1;0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1;0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0;1,0,1,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0;0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,1,1;0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1;0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1"
  M = toricGroebner(A); --note this matrix is the design matrix for the p1 statistical model on 4 nodes using a constant rho. (see fienberg/rinaldo/petrovic; in prep-missing reference).
  assert(numrows M == 137)
  assert(numcols M == 18)
  R = QQ[x_1..x_18]
  I = toBinomial(M,R);
  assert(degree I == 192)
  A1 = matrix "3,2,1,0;0,1,2,3" --one more example
  R=QQ[x_0..x_3]
  G4ti2=gens toricGroebner(A1,R)
  GM2 =gens gb toricMarkov(A1,R)
  Gtrue=toList flatten entries GM2  
  G = toList flatten entries G4ti2
  apply(0..#Gtrue-1, j->  (isSubset({Gtrue_j},G) )) --checking 4ti2's gb against M2's gb
  assert(numrows GM2 == numrows G4ti2)
  assert(numcols GM2 == numcols G4ti2)
  Rwt=QQ[x_0..x_3,Weights=>{3,2,4,1}] --with wt vector
  G4ti2=gens toBinomial(toricGroebner(A1,Weights=>{3,2,4,1}),Rwt) 
  GM2=gens gb toricMarkov(A1,Rwt)
  Gtrue=toList flatten entries GM2  
  G = toList flatten entries G4ti2
  assert(       numrows GM2 == numrows G4ti2       )
  assert(       numcols GM2 == numcols G4ti2       )
  apply(0..#Gtrue-1, j-> assert(isSubset({Gtrue_j},G)) ) --checking 4ti2's gb against M2's gb
///
TEST///
  needsPackage "FourTiTwo"   --testing graver  
  A1 = matrix "3,2,1,0;0,1,2,3"
  R=QQ[x_0..x_3]
  G = toricGraver(A1)
  assert( numrows G==5)
  assert(numcols G==4)
  Gtrue = toBinomial(matrix{{1,-2,1,0},{0,1,-2,1},{1,-1,-1,1},{2,-3,0,1},{1,0,-3,2}},R) --known: Graver basis
  Gtrue=toList flatten entries gens Gtrue  
  G = toList flatten entries gens toBinomial(G,R)
  apply(0..#Gtrue-1, j->  assert(isSubset({Gtrue_j},G)) ) --testing 4ti2 output against by-hand calculation!
  A = matrix "1,0,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0;0,1,1,0,0,0,0,0,0,1,0,1,1,0,1,0,0,0;0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1;0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1;0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0;1,0,1,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0;0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,1,1;0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,1;0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1"
  M = toricGraver(A);   --note this matrix is the design matrix for the p1 statistical model on 4 nodes using a constant rho. (see fienberg/rinaldo/petrovic; in prep-missing reference).
  assert(numrows M == 7462)
  assert(numcols M == 18)
  AS=matrix"1,1,1,1,1,1,1,1;1,1,1,0,0,0,0,0;1,2,3,1,2,3,4,5"--scroll S(2,4)
  R=QQ[x_1..x_8]
  G4ti2 = toList flatten entries gens toBinomial(toricGraver(AS),R)
  assert(#G4ti2 == 82)
  Gtrue=toList flatten entries gens toBinomial(matrix" 1,-2,1,0,0,0,0,0;1,-1,0,-1,1,0,0,0;2,-2,0,-1,0,1,0,0;3,-3,0,-1,0,0,1,0;4,-4,0,-1,0,0,0,1;3,-3,0,0,-1,0,0,1;2,-2,0,0,0,-1,0,1;1,-1,0,0,0,0,-1,1;2,-2,0,0,-1,0,1,0;1,-1,0,0,0,-1,1,0;1,-1,0,0,-1,1,0,0;0,1,-1,-1,1,0,0,0;0,1,-1,0,0,-1,1,0;0,1,-1,0,0,0,-1,1;0,1,-1,0,-1,1,0,0;1,0,-1,-1,0,1,0,0;1,0,-1,0,-1,0,1,0;1,0,-1,0,0,-1,0,1;2,-1,-1,-1,0,0,1,0;2,-1,-1,0,-1,0,0,1;3,-2,-1,-1,0,0,0,1;0,2,-2,0,-1,0,1,0;1,1,-2,-1,0,0,1,0;0,2,-2,-1,0,1,0,0;2,0,-2,-1,0,0,0,1;0,2,-2,0,0,-1,0,1;1,1,-2,0,-1,0,0,1;0,0,0,1,-1,-1,1,0;0,0,0,1,-1,0,-1,1;0,0,0,1,-2,1,0,0;1,-1,0,1,-2,0,1,0;1,-1,0,1,-1,-1,0,1;2,-2,0,1,-2,0,0,1;0,0,0,0,0,1,-2,1;0,0,0,0,1,-2,1,0;1,-1,0,-1,0,2,-1,0;0,0,0,0,1,-1,-1,1;1,-1,0,-1,0,1,1,-1;1,-1,0,0,-1,0,2,-1;2,-2,0,-1,0,0,2,-1;1,-1,0,0,1,-2,0,1;0,0,0,1,0,-2,0,1;0,1,-1,0,1,-2,0,1;0,1,-1,-1,0,1,1,-1;0,1,-1,-1,0,2,-1,0;1,0,-1,-1,0,0,2,-1;0,1,-1,0,-1,0,2,-1;0,1,-1,1,-1,-1,0,1;0,1,-1,1,-2,0,1,0;1,0,-1,1,-2,0,0,1;0,3,-3,-1,0,0,1,0;1,2,-3,-1,0,0,0,1;0,3,-3,0,-1,0,0,1;0,2,-2,1,-2,0,0,1;0,2,-2,-1,0,0,2,-1;0,4,-4,-1,0,0,0,1;0,0,0,1,-2,0,2,-1;0,0,0,2,-2,-1,0,1;0,0,0,2,-3,0,1,0;1,-1,0,2,-3,0,0,1;0,0,0,1,0,-3,2,0;0,0,0,1,0,-1,-2,2;1,-1,0,-1,0,0,3,-2;0,0,0,0,1,0,-3,2;0,0,0,0,2,-3,0,1;0,1,-1,-1,0,0,3,-2;0,1,-1,2,-3,0,0,1;0,0,0,3,-4,0,0,1;0,0,0,1,0,0,-4,3;1,0,-1,0,-1,1,-1,1;1,0,-1,0,-2,2,0,0;1,0,-1,-1,1,-1,1,0;1,0,-1,-1,1,0,-1,1;1,0,-1,-2,2,0,0,0;1,0,-1,0,0,-2,2,0;1,0,-1,0,0,0,-2,2;2,0,-2,0,-2,1,0,1;2,0,-2,-2,1,0,1,0;2,0,-2,-1,0,-1,2,0;2,0,-2,0,-1,0,-1,2;3,0,-3,-2,0,0,2,0;3,0,-3,0,-2,0,0,2"  ,R)
   apply(0..#Gtrue-1, j->  assert(isSubset({Gtrue_j},G4ti2)) ) -- checking 4ti2 output against by hand input!!
///


end



restart
--load "4ti2.m2"
installPackage ("FourTiTwo", RemakeAllDocumentation => true, UserMode=>true)
installPackage("FourTiTwo",UserMode=>true,DebuggingMode => true)
viewHelp FourTiTwo


check FourTiTwo

debug FourTiTwo


A = matrix{{1,1,1,1},{0,1,2,3}}
A = matrix{{1,1,1,1},{0,1,3,4}}
B = syz A
time toricMarkov A
A
toricMarkov(A, InputType => "lattice")
R = QQ[a..d]
time toricGroebner(A)
toBinomial(transpose B, R)
toricCircuits(A)
H = hilbertBasis(A)
hilbertBasis(transpose B)
toBinomial(H,QQ[x,y])
toricGraver(A)
A
toricMarkov(A)

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
toricMarkov transpose A
hilbertBasis transpose A
toricGraver transpose A
toricCircuits transpose A

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
toricMarkov A
R = QQ[x_1..x_27]
toricMarkov(A,R)
toricGroebner(A,R)
gens gb oo

I = toBinomial(matrix{{}}, QQ[x])
gens I
gens gb I


-- Notes from Mike talking with Peter Malkin, 4/21/09

in 1.3.2 (and in 1.3.1):

These routines use the structure below:

groebner, markov, 
hilbert, graver, zsolve
rays, circuits, qsolve

also: minimise, walk, normalform

a.mat: m by n
a.rel: 1 by m: symbols: >, =, <  (means: >= 0, == 0, <= 0)
a.sign: 1 by n matrix: 0,1,-1,2

a.sign: 0:  x_i unrestricted in sign
        1:  x_i >= 0
	-1: x_i <= 0.
	2:  x_i is a Graver component

a.mat, a.rel, a.sign.

groebner, markov, zsolve: also can give a.rhs (1 x n matrix).

groebner does this:
Ax >= 0, x>=0.

Ax-Iy = 0, x >= 0, y >= 0.

doc on main page of 4ti2 web site: manual, and the slides.
.rhs doesn't work for qsolve though, possibly.

for hilbert, graver, zsolve, have the following filter:
.ub, .lb can be used to provide upper and lower bounds (lower only for Graver components, or for <= vars).

install glpk first, and make sure gmp is visible.
./configure --with-gmp=.... --with-glpk=....

call the different routines as: -p32, -p64, -parb

tests are in 
4ti2-1.3.2/test

email Peter if I have more questions
4ti2 google group: joined.

glpk:open source linear programming
