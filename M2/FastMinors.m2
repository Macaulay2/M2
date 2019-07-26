newPackage(
	"FastMinors",
    	Version => "0.1",
    	Date => "3/7/2019",
    	Authors => {
	     {Name => "Boyana Martinova", Email => "u1056124@utah.edu", HomePage => ""},
	     {Name => "Marcus Robinson", Email => "robinson@math.utah.edu", HomePage => "http://www.math.utah.edu/~robinson"},
	     {Name => "Karl Schwede", Email => "schwede@math.utah.edu", HomePage => "http://www.math.utah.edu/~schwede"}
	     },
    	Headline => "Fast computation of minors",
    	DebuggingMode => true,
        Reload => true
    	)

export {
     "fastMinors",
     "Threads",
     "MinorsCache"
     }

fastMinors =method(Options=>{MinorsCache => true, Threads=>(allowableThreads-3)});

fastMinors(ZZ, Matrix) := Ideal => opts -> (n, M1) -> (
    if not ((M1#cache)#?MinorsCache) then (M1.cache)#MinorsCache = new MutableHashTable from {};
    --if not ((M1#cache)#?MinorsCache) then print "hello?";
    --if (n == numColumns M1) then M1 = transpose M1;
    H1 := null;
    if ((M1#cache)#MinorsCache)#?n then (
        if (debugLevel > 0) then print "fastMinors: we found stored data, using it.";
        H1 = ((M1#cache)#MinorsCache)#n; )
    else (
        H1 = fastMinorsTableP(n, M1, opts);
        if (opts.MinorsCache == true) then (((M1.cache).MinorsCache)#n = H1;); --store data only if told to
    );
    ideal values H1
);


--this is the multithreaded internal function
fastMinorsTableP = method(Options=>{MinorsCache => true, Threads=>(allowableThreads-3)});

fastMinorsTableP(ZZ, Matrix) := opts -> (n, M1) -> (
    if not ((M1#cache)#?MinorsCache) then (M1.cache)#MinorsCache = new MutableHashTable from {};
    if (opts.Threads <= 1) then (
        if (debugLevel > 0) then print "fastMinorsP: Going to single threaded version.";
        return fastMinorsTable(n, M1);
    );
    rowCt := numRows M1;
    colCt := numColumns M1;
    gg := subsets(rowCt, n);
    ff := subsets(colCt, n);
    L := toList ((set gg)**(set ff));
    lenL := length L;
    R1 := {};
    if (debugLevel > 0) then    print "fastMinorsTableP: about to recurse";
    if (n > 2) then (
        if ((M1.cache).MinorsCache)#?(n-1) then (
            if (debugLevel > 0) then print "fastMinorsTable: we found stored data, using it to avoid recursion.";
            R1 = ((M1.cache).MinorsCache)#(n-1); )
        else (
            R1 = fastMinorsTableP(n-1, submatrix'(M1, {rowCt-1}, ),opts);
        );
    )
    else (
        if ((M1.cache).MinorsCache)#?n then (
            if (debugLevel > 0) then print "fastMinorsTableP: we found stored data, using it.";
            return ((M1.cache).MinorsCache)#n; )
        else (
            return new HashTable from  apply(L, i -> (i => det (M1^(i#0)_(i#1))) ); --this should be multithreaded too presumably...
        );
    );
    tempL := null;
    if (debugLevel > 0) then print "fastMinorsTableP: did recursion, going to do tasks";
    taskList := apply(opts.Threads, i -> (tempL =  take(L, {(i*(lenL-1))//(opts.Threads), ((i+1)*(lenL-1))//(opts.Threads)});
        return createTask(temp, (tempL, n, M1, R1) ); ) );
    apply(taskList, t -> schedule t);
    while true do (--sleep 1; --print "wait";--print "i am waiting";
        if (all(taskList, t->isReady(t))) then break;
        );
    myList := flatten flatten apply(taskList, t -> taskResult(t));
    H := new HashTable from myList;
    apply(taskList, tt -> cancelTask(tt));
    return H;
)

temp = (Lis,n, M1, R1) -> (
    --print "temp started";
	K := apply(Lis, i -> (i => sum(0..n-1, curCol -> (-1)^(curCol)*M1_(i#0#(n-1), i#1#curCol)*R1#(toList drop(i#0, {n-1,n-1}), toList drop(i#1, {curCol, curCol})))));
	return K
);

--this is the single threaded function
fastMinorsTable = (n, M1) -> (
    if not ((M1#cache)#?MinorsCache) then (M1.cache)#MinorsCache = new MutableHashTable from {};
    rowCt := numRows M1;
    colCt := numColumns M1;
    gg := subsets(rowCt, n);
    ff := subsets(colCt, n);
    L := toList ((set gg)**(set ff));
    R := {};
    if (n > 2) then (
        if ((M1.cache).MinorsCache)#?(n-1) then (
            if (debugLevel > 0) then print "fastMinorsTable: we found stored data, using it to avoid recursion.";
            R = ((M1.cache).MinorsCache)#(n-1); )
        else (
            R = fastMinorsTable(n-1, submatrix'(M1, {rowCt-1}, )))
        )
    else (
        if ((M1.cache).MinorsCache)#?n then (
            if (debugLevel > 0) then print "fastMinorsTable: we found stored data, using it.";
            return ((M1.cache).MinorsCache)#n; )
        else (
            return new HashTable from  apply(L, i -> (i => det (M1^(i#0)_(i#1))) )
        );
    );
    H := new HashTable from  apply(L, i -> (i => sum(0..n-1, curCol -> (-1)^(curCol)*M1_(i#0#(n-1), i#1#curCol)*R#(toList drop(i#0, {n-1,n-1}), toList drop(i#1, {curCol, curCol}))))); --det (M^(i#0)_(i#1))

    return H;
)


beginDocumentation()


document{
     Key => FastMinors,
     Headline => "A Faster Computation of Minors",
     PARA {
         "This function takes a Matrix of any size, M, and an integer, t, and returns the ideal of the determinants of all txt minors
         within M."
     },
     PARA {
         "The fastMinors method finds determinants using cofactor expansion using the determinant of the appropriate minors of size (t-1)x(t-1).
         In order to do so, it calculates the determinants of all 2x2 minors and uses them to find all 3x3 minors and so forth until the txt
         minors are found. Additionally, the method utilizes Macaulay2's parallel programming to compute determinants of subsets of all the
         possible minors simultaneously and combine them once concluded."
     },
     PARA {
         "The method is faster than the default minors method, using the cofactor expansion, when the multiplications involved in cofactor are
          expensive.  For sparse or matrices with linear entries, frequently the default minors(, Strategy=>Cofactor) is faster.  "
     },
     PARA {},
     "More details can be found in ",
     }

     doc ///
         Key
             fastMinors
             (fastMinors, ZZ, Matrix)
             [fastMinors, MinorsCache]
             [fastMinors, Threads]
             MinorsCache
         Headline
             uses a recursive cofactor algorithm to compute the ideal of minors of a matrix
         Usage
             I = fastMinors(n, M, Threads=>t, MinorsCache=>b)
         Inputs
             n: ZZ
                the size of minors to compute
             M: Matrix
                the matrix which to compute the ideal of minors
             t: ZZ
                an optional input, which describes the number of threads to uses
             b: Boolean
                an optional input, which says whether to cache in input
         Outputs
             I: Ideal
                the ideal of minors
         Description
             Text
                Given a matrix $M$, this computes the ideal of determinants of size $n \times n$ submatrices.
                This is a recursive function with some possibility to 
             Example
                R = QQ[x,y];
                M = random(R^{5,5,5,5,5,5}, R^7);
                time I2 = fastMinors(5, M, Threads=>0);
                time I1 = minors(5, M, Strategy=>Cofactor);
                I1 == I2
     ///

TEST///
    R = QQ[x,y,z];
	  M = random(R^{5,5,5,5,5,5}, R^7);
		n = numRows M; while n > 0  assert(minors(n,M) == fastMinors(n,M)) do n = n-1
///
end

--restart
--uninstallPackage "BGG"
--installPackage "BGG"
--check "BGG"
--viewHelp BGG
