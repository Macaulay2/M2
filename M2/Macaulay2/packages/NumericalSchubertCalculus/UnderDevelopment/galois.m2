export {
    "findGaloisElement",
    "isFullSymmetric",
    "isGaloisFullSymmetric"
    }
-------------------------------
---  findGaloisElement
-------------------------------
--- A function that computes
--- another instances of a
--- given Schubert problem to
--- create a short loop
--- and track the solutions
--- using homotopy continuation,
--- then extracts the created
--- permutation of the solutions

findGaloisElement = method(TypicalValue => List)
findGaloisElement(Sequence, List, List) :=(prblm, flgs, solns) ->(
    ---------------------------------
     -- prblm 
     -- is a List that contains partitions 
     -- l and m, and integers k,n 
     -- that define the simple 
     -- Schubert problem in Gr(k,n)
     (l,m,k,n):=prblm;
     l = verifyLength(l, k);
     m = verifyLength(m, k);
     d := k*(n-k)-sum(l)-sum(m);
     -- create a random flag to start a loop
     -- We will work only from a short loop
     -- so we need only the first two rows
     -- of a random flag
     F := matrix apply(2, i->apply(n,j->random FFF));
     swaps := {0,1,0,1};
     tmpMtrx := mutableMatrix(flgs#(d-1) || F);
     tempSlns := solns;
     apply(swaps, j->(
	       M1 := submatrix'(matrix tmpMtrx, {n-k, n-k+1},);
	       rowSwap(tmpMtrx, j, n-k+j);
	       M2 := submatrix'(matrix tmpMtrx, {n-k,n-k+1},);
	       tempSlns = trackSimpleSchubert((k,n), (l,m), drop(flgs, -1) | {M1}, drop(flgs, -1) | {M2}, StartSolutions=>tempSlns);
	       ));
     apply(solns, s->positions(tempSlns, j->areEqual(j,s))) / first
)

------------------
-- isFullSymmetric
-- is a function that
-- takes a list of permutations
-- creates a file and run
-- GAP to test if the list
-- generates the full symmetric group
--
-- CAVIAT: it assumes that GAP runs
-- 	   when you type "gap" in a 
--	   terminal
------------------

getFileName = () -> (
	filename := temporaryFileName();
	while fileExists(filename) or fileExists(filename|".mat") or fileExists(filename|".lat") do filename = temporaryFileName();
	filename
	)

--GAPexe ="/Applications/gap4r4/bin/./gap.sh";	
--GAPexe := "gap";
-- With GAP's workspace (ultrafast)
GAPexe := "gap -L /Applications/gap4r4/bin/wsgap4";

isFullSymmetric = method(TypicalValue => Boolean)
isFullSymmetric(List) := (perms)->(
	--
	-- perms is a list of permutations
	-- of [n] = {1,2,...,n}
	--
	F := getFileName();
	file := openOut(F|".gapjob");
	file << "u := Group(" << endl;
	scan(#perms, i->
		(
			p := perms#i;
			file << "PermList([" ;
			scan(p, j->( 
				file << j+1; 
				if j=!= last p then file << ", " ;
			));
			file <<"])";
			if i=!=#perms-1 then file << ", "<<endl; 
		)
	);
	file <<endl << ");"<<endl;

	n := max perms#0;
	file <<"if NrMovedPoints(u)="<< n+1 << " and IsNaturalSymmetricGroup(u) then RemoveFile(\""<< toString(file) <<"\"); fi;\n";
	file << "QUIT;\n";
	close file;
  	--------------
	--
	-- Running GAP
	--
	--------------
	run(GAPexe|" -q "|toString(file));
	if fileExists toString(file) then (
		removeFile toString(file); 
		return false;
	)else(
		return true;
	)
)

-------------------
--
--	isGaloisFullSymmetric
--
-------------------
-- function that find Galois
-- elements of a Schubert Problem
-- until it gets the full symmetric
-- group
--
-- CAVIAT: this assumes that we
-- know that Gal(Prblm) = symm_n
--
-------------------
isGaloisFullSymmetric = method(TypicalValue => Boolean)
isGaloisFullSymmetric(Sequence, List, List, ZZ) := (prblm, flgs, solns, mx) ->(
	-- mx is the maximal number of loops we want to run
 	(l,m,k,n) := prblm;
	permuts := new List;
	cntr:= 0;
	tempOut := false;
	for i from 1 to mx when (tempOut===false) do (
	     permuts = permuts | {findGaloisElement(prblm,flgs,solns)};
	     cntr = i;
	     tempOut = isFullSymmetric(permuts);
	);
	if tempOut then (
		(tempOut, cntr) 
	)else (
		(tempOut, permuts)
	)
)


