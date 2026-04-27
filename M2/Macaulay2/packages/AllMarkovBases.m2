newPackage(
    "AllMarkovBases",
    Version => "1.0",
    Date => "May 08, 2025",
    Headline => "computing all minimal Markov bases of a configuration matrix",
    Authors => {
        {Name => "Alexander Milner",
            Email => "A.J.C.Milner@sms.ed.ac.uk",
            HomePage => "https://alexandermilner.github.io/"},
        {Name => "Oliver Clarke",
            Email => "oliver.clarke@durham.ac.uk",
            HomePage => "https://www.oliverclarkemath.com/"}
        },
    Keywords => {"Algebraic Statistics"},
    AuxiliaryFiles => false,
    DebuggingMode => false,
    PackageExports => {"FourTiTwo","Graphs","Normaliz"}
    )

-------------
-- Exports --
-------------

export {
    --exported methods
    "computeFiber",
    "fiberGraph",
    "pruferSequence",
    "markovBases",
    "randomMarkov",
    "countMarkov",
    "toricIndispensableSet",
    "toricUniversalMarkov",
     --exported options
    "ReturnConnectedComponents",
    "FiberAlgorithm",
    "NumberOfBases",
    "AlwaysReturnList",
    "ReturnFiberValues"
    }

----------
-- Code -----------------------------------------------------------------------------------------------
----------


-- computes one fiber
computeFiber = method(
    Options => {
        ReturnConnectedComponents => false,
        FiberAlgorithm => "decompose" -- choose from {decompose,markov,fast,lattice} ordered from best to worst
        }
    );

computeFiber (Matrix,Vector) := opts -> (A, b) -> (
    if not A.cache#?"MarkovBasis" then setupFibers A;
    val := entries b;
    computeFiberInternal(A,val,ReturnConnectedComponents=>opts.ReturnConnectedComponents,FiberAlgorithm=>opts.FiberAlgorithm);
    if opts.ReturnConnectedComponents then (
        if (A.cache#"fiberStarters")#?val then (A.cache#"fiberComponents")#val else (if #((A.cache#"fibers")#val) == 0 then {} else {toList (A.cache#"fibers")#val})
        )else for el in keys (A.cache#"fibers")#val list vector el
    );


computeFiberInternal = method(
    Options => {
        ReturnConnectedComponents => false,
        FiberAlgorithm => "decompose"
        }
    );
computeFiberInternal (Matrix,List) := opts -> (A, val) -> (
    if not (A.cache#"fibers")#?val or (opts.ReturnConnectedComponents and (A.cache#"fiberStarters")#?val and not (A.cache#"fiberComponents")#?val) then(
	if opts.FiberAlgorithm == "fast" then computeFiberInternalFast(A,val)
        else if opts.FiberAlgorithm == "lattice" then computeFiberInternalLattice(A,val,ReturnConnectedComponents=>opts.ReturnConnectedComponents)
        else if opts.FiberAlgorithm == "decompose" or opts.FiberAlgorithm == "markov" then computeFiberInternalDecompose(A,val,ReturnConnectedComponents=>opts.ReturnConnectedComponents,FiberAlgorithm=>opts.FiberAlgorithm)
        else error("unknown input for FiberAlgorithm option");
        );
    );



computeFiberInternalFast = method();
computeFiberInternalFast (Matrix,List) := (A,val) -> (
    buildFiber := if (A.cache#"fiberStarters")#?val then toList (A.cache#"fiberStarters")#val else(
        u:={};
        for row in entries toricMarkov ((matrix vector val) | A) do(
            r := drop(row,1);
            if row#0 == 1 and all(r,z->z<=0) then (u = {-r}; break;);
            );
        u
        );
    validMoves := if A.cache#"NN" then(
	for move in entries A.cache#"MarkovBasis" list if ((A.cache#"fiberValues")#move << val) and ((A.cache#"fiberValues")#move != val) then move else continue
	)else(
	for move in entries A.cache#"MarkovBasis" list if  (A.cache#"fiberValues")#move != val then move else continue
	);
    validMoves = new MutableHashTable from ((v -> {v,true}) \ validMoves);
    out := for i from 0 to #buildFiber - 1 list(
        cc := set {buildFiber#i};
        lenCC := 0;
        movesUnused := for kvs in pairs validMoves list if kvs#1 then kvs#0 else continue;
        while lenCC != #cc do(
            lenCC = #cc;
            for move in movesUnused do(
                cc = set flatten for el in keys cc list(
                    flatten for checkIndex in {1,0,-1} list(
                        if checkIndex == 0 then continue {el};
                        n := checkIndex;
                        while all(el+n*move, z -> z >= 0) list (el+n*move) do (validMoves#move = false; n = n + checkIndex)
                        )
                    );
                );
            );
        toList cc
        );
    if (A.cache#"fiberStarters")#?val then (A.cache#"fiberComponents")#val = out;
    (A.cache#"fibers")#val = set flatten out;
    );



computeFiberInternalLattice = method(
    Options => {
        ReturnConnectedComponents => false,
        }
    );
computeFiberInternalLattice (Matrix,List) := opts -> (A,val) -> (
    M := transpose A.cache#"MarkovBasis";
    if (numColumns M)==0 then M = transpose matrix{toList((numColumns A):0)};
    latticeOut := if (A.cache#"fiberStarters")#?val then (
	set latticePointsFromMoves(M,transpose matrix{first toList (A.cache#"fiberStarters")#val})
    )else(
        u:={};
        for row in entries toricMarkov ((matrix vector val) | A) do(
            r := drop(row,1);
            if row#0 == 1 and all(r,z->z<=0) then (u = {-r}; break;);
            );
        if #u==0 then set{} else set latticePointsFromMoves(M,transpose matrix u)
        );
    (A.cache#"fibers")#val = latticeOut;
    if opts.ReturnConnectedComponents then (A.cache#"fiberComponents")#val = computeCCs(toList latticeOut);
    );


-- uses Normaliz to compute lattice points (unexported)
latticePointsFromMoves = method();
latticePointsFromMoves(Matrix, Matrix) := (I, v) -> (
    M := I | v;
    normalizOutput := normaliz(M, "inhom_inequalities"); --normaliz(M, "inhom_inequalities");
    normalizOut := transpose normalizOutput#"gen"_{0..(numColumns normalizOutput#"gen")-2};
    for i from 0 to numColumns normalizOut - 1 list flatten entries (I*(normalizOut_{i})+v)
    );



--computes the connected components of a list of positive vectors (unexported)
computeCCs = method();
computeCCs List := L -> (
    G := new HashTable from for i from 0 to #L-1 list (L#i,for j from i+1 to #L-1 list if any(L#i,L#j,(x,y)-> x>0 and y>0) then L#j else continue);
    connectedComponents graph(L,pairs G)
    );




computeFiberInternalDecompose = method(
    Options => {
        ReturnConnectedComponents => false,
        FiberAlgorithm => "fast"
        }
    );
computeFiberInternalDecompose (Matrix,List) := opts -> (A,val) -> (
    if opts.FiberAlgorithm == "decompose" and not A.cache#?"Ring" then(
	if A.cache#"NN" then(
	    A.cache#"Ring" = ZZ(monoid[Variables => numRows A + numColumns A,MonomialOrder=>Eliminate numRows A]);
	    A.cache#"RingGenerators" = gens A.cache#"Ring";
	    A.cache#"toricIdeal"=toricGroebner(id_(ZZ^(numRows A)) | A, A.cache#"Ring");
	    )else(
	    A.cache#"Ring" = ZZ(monoid[Variables => 2*numRows A + numColumns A,MonomialOrder=>Eliminate (2*numRows A)]);
	    A.cache#"RingGenerators" = gens A.cache#"Ring";
	    A.cache#"toricIdeal"=toricGroebner(id_(ZZ^(numRows A)) | -id_(ZZ^(numRows A)) | A, A.cache#"Ring");
	    );
        );
    if (A.cache#"fiberStarters")#?val and opts.ReturnConnectedComponents then (
        out := for z in pairs A.cache#"fiberValues" list(
	    if z#1 == val then continue;
	    resid := val-z#1;
	    if A.cache#"NN" then(
	        if not (z#1 << val) then continue;
		)else(
	        if any(A.cache#"supportingHyperplanes",z->(matrix{z}*(vector resid))_0<0) then continue;
		);
            if not (A.cache#"fibers")#?resid then fibRecursion(A,resid,FiberAlgorithm=>opts.FiberAlgorithm);
            if #((A.cache#"fibers")#resid)==0 then continue;
            fibAdd((A.cache#"posNeg")#(z#0),(A.cache#"fibers")#resid)
            );
        G := new HashTable from for i from 0 to #out-1 list (out#i,for j from i+1 to #out-1 list if not  #intersect(out#i,out#j)==0 then out#j else continue);
        out = apply(connectedComponents graph(out,pairs G),z->toList union z);
        out = out | apply(toList ((A.cache#"fiberStarters")#val - (flatten out)),v->{v});
        (A.cache#"fiberComponents")#val = out;
        (A.cache#"fibers")#val = flatten out;
        )else fibRecursion(A,val,FiberAlgorithm=>opts.FiberAlgorithm);
    );


-- recursive method using decompose algorithm (unexported)
fibRecursion = method(
    Options => {
        FiberAlgorithm => "decompose"
        }

    );
fibRecursion (Matrix,List) := opts -> (A, val) -> (
    out := union for z in pairs A.cache#"fiberValues" list(
	if z#1==val and (A.cache#"fiberStarters")#?val then continue (A.cache#"posNeg")#(z#0);
	resid := val-z#1;
	if A.cache#"NN" then(
	    if not (z#1 << val) then continue;
	    )else(
	    if any(A.cache#"supportingHyperplanes",z->(matrix{z}*(vector resid))_0<0) then continue;
	    );
        if not (A.cache#"fibers")#?resid then fibRecursion(A,resid,FiberAlgorithm=>opts.FiberAlgorithm);
        if #((A.cache#"fibers")#resid) == 0 then continue;
        fibAdd((A.cache#"posNeg")#(z#0),(A.cache#"fibers")#resid)
        );
    if #out==0 then(
        if opts.FiberAlgorithm == "markov" then (
            for row in entries toricMarkov ((matrix vector val) | A) do(
                r := drop(row,1);
                if row#0 == 1 and all(r,z->z<=0) then (out = set{-r}; break;);
                );
            )else (
	    if A.cache#"NN" then(
		e1:=first exponents (product(for i from 0 to #val-1 list ((A.cache#"RingGenerators")#i)^(val#i)) % A.cache#"toricIdeal");
		if take(e1,numRows A) == toList((numRows A):0) then out = set{take(e1,-numColumns A)};
		)else(
		e2:=first exponents (product(for i from 0 to #val-1 list if val#i>0 then ((A.cache#"RingGenerators")#i)^(val#i) else ((A.cache#"RingGenerators")#(i+numRows A))^(-val#i)) % A.cache#"toricIdeal");
		if take(e2,2*numRows A) == toList((2*numRows A):0) then out = set{take(e2,-numColumns A)};
		);
            );
        );
    (A.cache#"fibers")#val = out;
    );

fibAdd = method();
fibAdd (Set,Set) := (L1,L2) -> (
    set flatten for l1 in keys L1 list for l2 in keys L2 list l1+l2
    );


---------------------------------------------------------------------------------------------------


fiberGraph = method(
    Options => {
        ReturnConnectedComponents => false,
        FiberAlgorithm => "fast", -- choose from {fast,decompose,lattice,markov} ordered from best to worst
        }
    );
fiberGraph Matrix := opts -> A -> (
    if not A.cache#?"MarkovBasis" then setupFibers A;
    if opts.ReturnConnectedComponents then(
        if not A.cache#"componentsComputed" then (
            for val in rsort keys A.cache#"fiberStarters" do computeFiberInternal(A,val,ReturnConnectedComponents=>true,FiberAlgorithm=>opts.FiberAlgorithm);
            A.cache#"componentsComputed" = true;
            );
        ) else (if #(values A.cache#"fiberGraphs")==0 then (
            graphicFiberGraph A;
            ) else A.cache#"fiberGraphs"
        );
    if opts.ReturnConnectedComponents then values A.cache#"fiberComponents" else values A.cache#"fiberGraphs"
    );



-- computes and stores information about A in A.cache which is needed for fiberGraph (unexported)
setupFibers = method();
setupFibers Matrix := A -> (
    A.cache#"MarkovBasis" = toricMarkov A;
    if A.cache#"MarkovBasis" == 0 then error("no Markov bases for the input matrix");
    fiberStarters := new MutableHashTable;
    fiberValues := new MutableHashTable;
    posNeg := new MutableHashTable;
    for basisElement in entries A.cache#"MarkovBasis" do(
        if (all(basisElement, z -> z >= 0) or all(basisElement, z -> z <= 0)) then error("semigroup associated to input matrix is not pointed");
        elPos := for coord in basisElement list(if coord >= 0 then coord else 0);
        elNeg := elPos - basisElement;
        fiberVal := flatten entries (A * transpose matrix{elPos});
        if fiberStarters#?fiberVal then (
            fiberStarters#fiberVal = union(fiberStarters#fiberVal, set{elPos,elNeg});
            )else fiberStarters#fiberVal = set{elPos,elNeg};
        fiberValues#basisElement = fiberVal;
        posNeg#basisElement = set{elPos,elNeg};
        );
    A.cache#"fiberStarters" = new HashTable from fiberStarters;
    A.cache#"fiberValues" = new HashTable from fiberValues;
    A.cache#"posNeg" = new HashTable from posNeg;
    A.cache#"fibers" = new MutableHashTable from {{toList((numRows A):0),set {toList((numColumns A):0)}}};
    A.cache#"fiberComponents" = new MutableHashTable;
    A.cache#"componentsComputed" = false;
    A.cache#"fiberGraphs" = new MutableHashTable;
    A.cache#"NN"= all(for row in entries A list all(for el in row list el>=0,z->z),z->z);
    N:=normaliz(A,"normalization",allComputations=>true);
    S:=entries N#"sup";
    G:=entries N#"gen";
    tem := transpose matrix for row in entries A list S#(position(G,z->z==row));
    A.cache#"supportingHyperplanes" = for el in entries tem list if el == toList(#S:0) then continue else el;
    );


-----------------------------------------------------------------------------------------------------


-- use Normaliz to compute the points in a fiber
-- A is the matrix of the toric ideal
-- v is the fiber:
-- return all point in \NN^n \cap \{x : Ax = Av\}
pointsInFiber = method();
pointsInFiber(Matrix, Matrix) := (A, v) -> (
    n := numColumns A;
    I := id_(ZZ^n);
    O := transpose matrix {toList(n : 0)};
    M := (I | O) || (A | -v);
    normaliz(M, "inhom_inequalities") --normaliz(M, "inhom_inequalities")
    );


-----------------------------------------------------------------------------------------------------


-- Uses Graphs package to compute the connected components of fiber graphs and returns list of graphs

graphicFiberGraph = method();
graphicFiberGraph Matrix := A -> (
    starterMarkovBasis := A.cache#"MarkovBasis";
    n := numColumns A;
    fiberStarters := new MutableHashTable;
    possibleMoves := new MutableList;
    for i from 0 to numRows starterMarkovBasis - 1 do(
        starterFiberElement := for j from 0 to n-1 list(if starterMarkovBasis_(i,j)>=0 then starterMarkovBasis_(i,j) else 0);
        fiberValue := A * transpose matrix{starterFiberElement};
        if not fiberStarters#?fiberValue then fiberStarters#(flatten entries fiberValue) = starterFiberElement;
        validMove := flatten entries starterMarkovBasis^{i};
        possibleMoves##possibleMoves = validMove;
        possibleMoves##possibleMoves = - validMove;
        );
    for starterFiberElement in pairs fiberStarters do(
        queueOfFiberElements := new MutableList from {starterFiberElement#1};
        adjacencyMatrixIndex := new MutableHashTable from {starterFiberElement#1 => 0};
        adjacencyMatrix := matrix("0");
        fiberSize := 1;
        while #queueOfFiberElements != 0 do(
            currentFiberElement := queueOfFiberElements#0;
            for move in possibleMoves do(
                testFiberElement := move + currentFiberElement;
                if (all(testFiberElement, z -> z>=0) and not adjacencyMatrixIndex#?testFiberElement) then (
                    queueOfFiberElements##queueOfFiberElements=testFiberElement;
                    intersectionIndex := mutableMatrix(ZZ,1,fiberSize);
                    for keyVals in pairs adjacencyMatrixIndex do(
                        if (not all(testFiberElement, keyVals_0,(y,z) -> y<=0 or z<=0)) then (
                            intersectionIndex_(0,keyVals_1) = 1
                            );
                        );
                    intersectionIndex = matrix intersectionIndex;
                    adjacencyMatrix = matrix{{adjacencyMatrix, transpose intersectionIndex}, {intersectionIndex, matrix{{0}}}};
                    adjacencyMatrixIndex#testFiberElement = fiberSize;
                    fiberSize = fiberSize + 1
                    );
                );
            remove(queueOfFiberElements,0);
            );
        orderedAdjacencyMatrixIndex := new MutableList from toList(numColumns adjacencyMatrix:0);
        for l in keys adjacencyMatrixIndex do orderedAdjacencyMatrixIndex#(adjacencyMatrixIndex#l)=l;
        outG := graph(toList orderedAdjacencyMatrixIndex, adjacencyMatrix);
        (A.cache#"fiberGraphs")#(starterFiberElement#0) = outG;
        (A.cache#"fiberComponents")#(starterFiberElement#0) = connectedComponents outG;
        );
    );




----------------------------------------------------------------------------------------------------



pruferSequence = method();
pruferSequence List := L -> (
    numberOfNodes := #L + 2;
    edgeList := new MutableList;
    nodeDegrees := new MutableList from toList(numberOfNodes : 1);
    for j in L do(
        nodeDegrees#j = nodeDegrees#j + 1;
        );
    for j in L do(
        for node from 0 to numberOfNodes - 1 do(
            if nodeDegrees#node == 1 then(
                edgeList##edgeList = set {node, j};
                nodeDegrees#j = nodeDegrees#j - 1;
                nodeDegrees#node = nodeDegrees#node - 1;
                break;
                );
            );
        );
    edgeList##edgeList = set positions(nodeDegrees, x -> x==1);
    toList edgeList
    );


-- direct product of lists (unexported)
listProd = method();
listProd List := L -> (
    fold(
        (combinedLists, listToBeAdded) -> (
            flatten for combinedElement in combinedLists list (
                for newElement in listToBeAdded list (
                    append(combinedElement, newElement)
                    )
                )
            ),
        {{}},
        L)
    );


-- sortMarkov takes a list L of elements of a minimal Markov basis of a matrix A
-- and returns a minimal Markov basis of elements whose first non-zero entry is positive
-- the elements are then sorted by fiber, which are sorted lexicographically (E.g. sort subsets(4, 2))
sortMarkov = method()
sortMarkov(List, Matrix) := (L, A) -> (
    L = (rsort(L | -L))_{0 .. #L -1};
    sort(L, x -> first entries transpose sum for i from 0 to #x -1 list if x_i > 0 then x_i * A_{i} else continue)
    )



------------------------------------------------------------------------------------------------------






markovBases = method();
markovBases Matrix := A -> (
    allFibersConnectedComponents := fiberGraph(A,
        ReturnConnectedComponents => true,
        FiberAlgorithm => "fast");
    allFibersSpanningTrees := for fiberConnectedComponents in allFibersConnectedComponents list(
        for pruferList in listProd splice {
            #fiberConnectedComponents - 2 : toList(0..#fiberConnectedComponents-1)
            } list pruferSequence pruferList
        );
    markovBasesAsLists := listProd for k from 0 to #allFibersSpanningTrees-1 list(
        flatten for spanningTree in allFibersSpanningTrees#k list(
            listProd for edge in spanningTree list(
                pairsOfFiberElements := listProd for l in keys edge list allFibersConnectedComponents#k#l;
                for pair in pairsOfFiberElements list pair#0-pair#1
                )
            )
        );
    markovBasesAsLists = sort(apply(markovBasesAsLists, x -> sortMarkov(flatten x, A)));
    for markovBasisAsList in markovBasesAsLists list matrix markovBasisAsList
    );

markovBases(Matrix, Ring) := (A, R) -> (
    listOfBases := markovBases A;
    apply(listOfBases, B -> toBinomial(B, R))
    )


randomMarkov = method(
    Options => {
        NumberOfBases => 1,
        AlwaysReturnList => false
        }
    );


randomMarkov Matrix := opts -> A -> (
    allFibersConnectedComponents := fiberGraph(A,
        ReturnConnectedComponents => true,
        FiberAlgorithm => "fast");
    randomMarkovBases := for i from 0 to opts.NumberOfBases - 1 list(
        allFibersRandomSpanningTree := for fiberConnectedComponents in allFibersConnectedComponents list(
            pruferSequence for j from 1 to #fiberConnectedComponents-2 list random(#fiberConnectedComponents)
            );
        matrix sortMarkov(
            flatten for k from 0 to #allFibersRandomSpanningTree-1 list(
                for edge in allFibersRandomSpanningTree#k list(
                    randomPairOfFiberElements := for l in keys edge list allFibersConnectedComponents#k#l#(random(#allFibersConnectedComponents#k#l));
                    randomPairOfFiberElements#0-randomPairOfFiberElements#1
                    )
                ),
            A)
        );
    if (not opts.AlwaysReturnList and opts.NumberOfBases == 1) then randomMarkovBases_0 else randomMarkovBases
    );


randomMarkov(Matrix, Ring) := opts -> (A, R) -> (
    listOfBases := randomMarkov(A, NumberOfBases => opts.NumberOfBases, AlwaysReturnList => true);
    listOfIdeals := apply(listOfBases, B -> toBinomial(B, R));
    if (not opts.AlwaysReturnList and opts.NumberOfBases == 1) then listOfIdeals_0 else listOfIdeals
    );


countMarkov = method(
    Options => {
        FiberAlgorithm => "fast"
        }
    )
countMarkov Matrix := opts -> A -> (
    allFibersConnectedComponents := fiberGraph(A,
        ReturnConnectedComponents => true,
        FiberAlgorithm => opts.FiberAlgorithm);
    product for fiberConnectedComponents in allFibersConnectedComponents list(
        k := #fiberConnectedComponents;
        if k==2 then continue #fiberConnectedComponents#0 * #fiberConnectedComponents#1;
        ccSizes := (v -> #v) \ fiberConnectedComponents;
        (product ccSizes) * (sum ccSizes)^(k-2)
        )
    )


toricIndispensableSet = method(
    Options => {
        ReturnFiberValues => false
        }
    );

toricIndispensableSet Matrix := opts -> A -> (
    starterMarkovBasis := toricMarkov A;
    toricIndispensableSet(A,starterMarkovBasis, opts)
    )

toricIndispensableSet (Matrix,Matrix) := opts -> (A,starterMarkovBasis) -> (
    starterMB := entries starterMarkovBasis;
    B := for el in starterMB list(
        elPos := for coord in el list(if coord >= 0 then coord else 0);
        {elPos,elPos - el}
        );
    F := for pair in B list(
        newB := flatten delete(pair,B);
        check0 := all(newB,z -> not all(pair#0-z,y -> y>=0));
        check1 := all(newB,z -> not all(pair#1-z,y -> y>=0));
        check2 := not isMember(pair#0,newB);
        check3 := not isMember(pair#1,newB);
        if check0 and check1 and check2 and check3 then pair#0 - pair#1 else continue
        );
    if not opts.ReturnFiberValues then matrix F
    else for el in F list flatten entries (A * transpose matrix{for coord in el list(if coord >= 0 then coord else 0)})
    );


toricIndispensableSet(Matrix, Ring) := opts -> (A, R) -> (
    indispensableMatrix := toricIndispensableSet A;
    toBinomial(indispensableMatrix, R)
    )


toricUniversalMarkov = method()
toricUniversalMarkov Matrix := A -> (
    fiberComponents := fiberGraph(A,
        ReturnConnectedComponents => true,
        FiberAlgorithm => "fast");
    matrix flatten for vertexList in fiberComponents list (
        flatten for pairsOfVertices in subsets(vertexList, 2) list (
            for movePairs in listProd pairsOfVertices list (
                movePairs_0 - movePairs_1
                )
            )
        )
    )

toricUniversalMarkov(Matrix, Ring) := (A, R) -> (
    indispensableMatrix := toricUniversalMarkov A;
    toBinomial(indispensableMatrix, R)
    )

-------------------
-- Documentation ------------------------------------------------------------------------------------
-------------------


beginDocumentation()

doc ///
  Key
    AllMarkovBases
  Headline
    compute all minimal Markov Bases of a toric ideal
  Description
    Text
      Fix a matrix $A = (a_{i,j}) \in \ZZ^{d \times n}$ satisfying
      $\ker(A) \cap (\ZZ_{\ge 0})^n = \{0\}$. The toric ideal $I_A$
      is the kernel of the associated monomial map
      $\phi_A : k[x_1, \dots, x_n] \rightarrow k[t_1, \dots, t_d]$
      given by $\phi(x_i) = t_1^{a_{1,i}} t_2^{a_{2,i}} \dots t_d^{a_{d,i}}$
      for each $i \in [n]$. A Markov basis is a minimal generating
      set for a toric ideal.
      Markov bases are used in Algebraic Statistics in hypothesis testing
      for contingency tables; for further details we refer to:
      M. Drton, B. Sturmfels, and S. Sullivant
      {\bf Lectures on Algebraic Statistics}

      This package computes the set of all minimal Markov bases of a
      given toric ideal $I_A$. We do this by using @TO FourTiTwo@ to
      efficiently compute one Markov basis $M$. We then compute all
      spanning forests of the {\it fiber graph} of $A$ in the
      {\it generating fibers} using the well-known bijection
      of Prüfer. Each spanning forest corresponds to
      a Markov basis, which gives us an efficient way to compute the
      number of Markov bases of $A$ as well as produce the Markov bases.
      For further theoretical details, see
      H. Charalambous, K. Anargyros, A. Thoma
      {\bf Minimal systems of binomial generators and the indispensable
          complex of a toric ideal}.

      Below is an example showing how to compute all Markov bases for
      the matrix $A = (7 \ 8 \ 9 \ 10) \in \ZZ^{1 \times 4}$.

    Example
      A = matrix "7,8,9,10";
      countMarkov A
      netList markovBases A

    Text
      The format of the output follows that of @TO toricMarkov@, so each
      minimal Markov basis is given by a matrix whose rows correspond to
      elements of the Markov basis.

      If the configuration matrix has too many minimal
      Markov bases, then it may be
      convenient to use a random Markov basis.
      This package provides a method to produce a random minimal Markov basis
      that is uniformly sampled from the set of all minimal Markov bases.

    Example
      A = matrix "2,3,5,7,30,31,32";
      countMarkov A
      randomMarkov A

    Text
      The package also provides methods to compute the indispensable set and
      universal Markov basis; see @TO toricIndispensableSet@ and
      @TO toricUniversalMarkov@. These methods exploit the fiber graph to
      compute these respective sets of binomials.

  References
    @UL{
        {"B. Sturmfels.    Groebner bases and Convex Polytopes.
            Volume 8 of ",
            EM "University Lecture Series",
            ". American Mathematical Society, Providence, RI, 1996."
            },
        {"M. Drton, B. Sturmfels, and S. Sullivant.    Lectures on
            Algebraic Statistics.",
            EM "Oberwolfach Seminar Series",
            "39 Basel, Switzerland, Birkhäuser Verlag, 2009."
            },
        {"H. Charalambous, K. Anargyros, and A. Thoma. Minimal
            systems of binomial generators and the indispensable
            complex of a toric ideal. Volume 135 of ",
            EM "Proceedings of the American Mathematical Society",
            "2007."},
        {"Prüfer, H. (1918). Neuer Beweis eines Satzes über Permutationen.",
            EM "Arch. Math. Phys. 27: 742–744."}
        }@

  Subnodes
    randomMarkov
    countMarkov
    fiberGraph
    markovBases
    pruferSequence
    toricIndispensableSet
    toricUniversalMarkov
///

doc ///
  Key
    computeFiber
    (computeFiber, Matrix,Vector)
    [computeFiber, ReturnConnectedComponents]
    [computeFiber, FiberAlgorithm]
    ReturnConnectedComponents
    FiberAlgorithm
  Headline
    compute a single fiber of configuration matrix
  Usage
    F = computeFiber(A,b)
  Inputs
    A : Matrix
      configuration matrix
    b : Vector
      value of fiber
    ReturnConnectedComponents => Boolean
      if true then return the list of connected components
      of the fiber, otherwise return fiber as list of vectors
    FiberAlgorithm => String
      Choices are "decompose", "markov", "fast" or "lattice": when using "decompose", lots of smaller
      fibers will autmoaticaly be computed and stored in @TT "A.cache"@. Thus, when computing lots
      of fibers for one configuration matrix, we recommend "decompose", especially for large fibers.
      "markov" works very similarly to "decompose" and will be faster for complicated input matrices
      with lots of rows and columns. Otherwise, "fast" is the best for a one-off computation of a
      smaller fiber and, if the values of the input matrix $A$ are very large, it is worth trying "lattice".
  Outputs
    F : List
      a complete list of vectors in the fiber b
  Description
    Text
      This function constructs one fiber $b$ of the configuration matrix $A$, which is returned as
      a list of vectors.

      The algorithm used to compute the fiber can be one of "decompose", "markov", "fast" or
      "lattice" with "decompose" as the default option. All algorithms will return either the fiber
      as a list of vectors or a list of the connected components of the fiber, depending on whether
      the @TO ReturnConnectedComponents@ option is set to @TT "false"@ or @TT "true"@.

      When computing lots of fibers for one configuration matrix, we recommend "decompose",
      especially for large fibers. "markov" works very similarly to "decompose" and will be faster
      for complicated input matrices with lots of rows and columns. Otherwise, "fast" is the best
      for a one-off computation of a smaller fiber and, if the values of the input matrix $A$ are
      very large, it is worth trying "lattice".

    Example
      computeFiber(matrix "3,5,11", vector {27})
      netList computeFiber(matrix "3,4,6,8,12", vector {12}, ReturnConnectedComponents => true)
      computeFiber(matrix "51,52,53,54,55,56", vector {614}, FiberAlgorithm => "fast")
      computeFiber(matrix "2,4,5,8;7,2,6,1;11,4,3,10", vector{26828,37890,62792}, FiberAlgorithm => "lattice")

  SeeAlso
    fiberGraph
    markovBases
    AllMarkovBases
///


doc ///
  Key
    fiberGraph
    (fiberGraph, Matrix)
    [fiberGraph, ReturnConnectedComponents]
    [fiberGraph, FiberAlgorithm]
  Headline
    generating fibers of a configuration matrix
  Usage
    G = fiberGraph A
  Inputs
    A : Matrix
      configuration matrix
    ReturnConnectedComponents => Boolean
      if true then return the list of connected components
      of each fiber, otherwise return the whole graphs
    FiberAlgorithm => String
      affects the computation only when @TO ReturnConnectedComponents@ is @TT "true"@.
      Choices are "fast", "decompose", "markov" or "lattice": "fast" is recommended as the
      most reliably quick option, however, if the generating fibers are particularly large,
      we recommend "decompose" for smaller input matrices $A$ and "markov" for larger input
      matrices $A$. If the values of the input matrix $A$ are very large, it is worth trying "lattice".
  Outputs
    G : List
      a list of graphs, one for each generating fiber of A
  Description
    Text
      This function constructs the generating fibers of
      the configuration matrix $A$. The fibers are returned
      as a list of graphs where two vectors in a fiber are
      adjacent if their supports have non-trivial intersection.

      If the option @TO ReturnConnectedComponents@ is @TT "false"@,
      the default setting, then the generating fibers are exhaustively searched
      using a breadth first search algorithm and vectors
      in the fibers are added to graph objects from the package
      @TO Graphs@, with edges added between two vectors if they have non-trivial support.

      If the option @TO ReturnConnectedComponents@ is @TT "true"@ then,
      instead of returning a list of graphs, the function returns
      a list of the connected components of each fiber. In this case, an algorithm for
      the computation can then be chosen using the @TO FiberAlgorithm@ option and these algorithms
      are much more efficient than using the breadth first search algorithm alonside the
      @TO Graphs@ package, which is used when the option @TO ReturnConnectedComponents@ is
      @TT "false"@.

    Example
      netList fiberGraph matrix "3,4,5"
      netList fiberGraph matrix "1,2,3"
      netList fiberGraph(matrix "1,2,3", ReturnConnectedComponents => true, FiberAlgorithm => "decompose")
      netList fiberGraph(matrix "3,4,6,8,12", ReturnConnectedComponents => true, FiberAlgorithm => "fast")

  SeeAlso
    computeFiber
    markovBases
    AllMarkovBases
///


doc ///
  Key
    pruferSequence
    (pruferSequence, List)
  Headline
    the edge set of a spanning tree corresponding to a Prüfer sequence
  Usage
    E = pruferSequence L
  Inputs
    L : List
      Prüfer sequence, an element of $\{0, \dots, n-1\}^{n-2}
  Outputs
    E : List
      the edge set of the spanning tree corresponding $L$
  Description
    Text
      Cayley's formula in graph theory is the result that the number of
      trees with vertices labelled from $0$ to $n-1$ is $n^{n-2}$. The Prüfer
      sequence of a labelled tree is an element of $\{0, \dots, n-1\}^{n-2}$
      and gives an explicit bijection between the two sets.

      This function produces the edge set of the spanning tree corresponding
      to a given Prüfer sequence.
    Example
      pruferSequence {2}
      pruferSequence {1,3}
      pruferSequence {3,3,3,4}
///


doc ///
  Key
    markovBases
    (markovBases, Matrix)
    (markovBases, Matrix, Ring)
  Headline
    all minimal Markov bases of a configuration matrix
  Usage
    K = markovBases A
    L = markovBases(A, R)
  Inputs
    A : Matrix
      configuration matrix
    R : Ring
      ring with one generator for each column of $A$
  Outputs
    K : List
      the minimal Markov bases A
    L : List
      ideals in R generated by the minimal Markov bases of $A$
  Description
    Text
      This method produces all minimal Markov bases of a
      given configuration matrix $A \in \ZZ^{d \times n}$.
      By default, the output is formatted in the same way
      as @TO toricMarkov@: each Markov basis is a $k \times n$
      matrix whose rows correspond to the elements of the Markov basis.
    Example
      netList markovBases matrix "3,4,5" -- unique Markov basis
      netList markovBases matrix "1,2,3"
      netList markovBases matrix "1,2,3,4"
      netList markovBases matrix "1,2,3,4;4,5,6,7"
    Text
      Similarly to @TO toricMarkov@, we may also specify a ring $R$.
      In this case, the method produces a list of ideals in $R$
      with each ideal generated by a different minimal Markov basis of $A$.
    Example
      markovBases(matrix "1,2,3",QQ[x_1,x_2,x_3])
      gens \ (markovBases(matrix "1,2,3",QQ[x_1,x_2,x_3]))
  SeeAlso
    randomMarkov
///


doc ///
  Key
    countMarkov
    (countMarkov, Matrix)
    [countMarkov, FiberAlgorithm]
  Headline
    the number of minimal Markov bases of a configuration matrix
  Usage
    N = countMarkov A
  Inputs
    A : Matrix
      configuration matrix
    FiberAlgorithm => String
      Choices are "fast", "decompose", "markov" or "lattice". See
      @TO [computeFiber, FiberAlgorithm]@.
  Outputs
    N : ZZ
      the number of minimal Markov bases of $A$
  Description
    Text
      This method counts all minimal Markov bases of $A$.
    Example
      countMarkov matrix "3,4,5" -- unique Markov basis
      countMarkov matrix "1,2,3"
      countMarkov matrix "2,3,5,7,30,31,32"
    Text
      This method does not produce all minimal Markov bases, so it is much
      faster to use {\tt countMarkov A} than {\tt #markovBases A}. This method
      computes the fiber graphs of $A$, see @TO fiberGraph@. The speed of this
      computation depends on the choice of algorithm, which may be specified
      using the option @TT "FiberAlgorithm"@. For more details, see
      @TO [computeFiber, FiberAlgorithm]@.
  SeeAlso
    markovBases
    fiberGraph
    computeFiber
///


doc ///
  Key
    randomMarkov
    (randomMarkov, Matrix)
    (randomMarkov, Matrix, Ring)
    [randomMarkov, NumberOfBases]
    [randomMarkov, AlwaysReturnList]
    NumberOfBases
    AlwaysReturnList
  Headline
    get a random minimal Markov basis
  Usage
    B = randomMarkov A
    I = randomMarkov(A, R)
  Inputs
    A : Matrix
      the configuration matrix
    R : Ring
      with one generator for each column of $A$
    NumberOfBases => ZZ
      the number of Markov bases to return
    AlwaysReturnList => Boolean
      if true (or NumberOfBases > 1) then returns the result as a list
  Outputs
    B : Matrix
      a random Markov basis of $A$
    I : Ideal
      an ideal in $R$ generated by a random Markov basis of $A$

  Description
    Text
      This method outputs one minimal Markov basis of $A$ chosen
      uniformly at random.
    Example
      randomMarkov matrix "1,2,3,4"
      A = matrix "2,3,5,7,30,31,32"
      randomMarkov A
    Text
      The option @TO NumberOfBases@ allows us specify how many Markov
      bases to return. If this number is greater than one, then the
      function returns a list of Markov bases. Since each Markov basis
      is produced independently, it is possible for the list to contain
      repeats. One may check how many minimal Markov bases exist with
      the function @TO countMarkov@, and the list of all Markov bases
      can be computed with @TO markovBases@. Just like @TO markovBases@,
      we may specify a ring, which produces an ideal whose generating set
      is given by a random minimal Markov basis.
    Example
      A = matrix "2,3,5,7,30,31,32"
      randomMarkov(A, NumberOfBases => 2)
      countMarkov A
      R = ZZ[x_1 .. x_7]
      netList randomMarkov(A, R, NumberOfBases => 2)
  SeeAlso
    markovBases
    countMarkov
///

doc ///
  Key
    toricIndispensableSet
    (toricIndispensableSet, Matrix)
    (toricIndispensableSet, Matrix, Ring)
    [toricIndispensableSet, ReturnFiberValues]
  Headline
    the indispensable set of toric binomials
  Usage
    S = toricIndispensableSet A
    I = toricIndispensableSet(A, R)
  Inputs
    A : Matrix
      the configuration matrix
    R : Ring
      with one generator for each column of $A$
    ReturnFiberValues => Boolean
      whether to return the fibers of indispensable binomials
  Outputs
    S : Matrix
      indispensable set of toric binomials
    I : Ideal
      an ideal in $R$ generated by the indispensable binomials
  Description
    Text
      A binomial $x^u - x^v$ of a toric ideal $I_A$ is called indispensable if
      it belongs to every minimal Markov basis of A.
      The set of all indispensable elements is called the indispensable set,
      and is often denoted with $S(A)$.

      This method computes the indispensable set of a matrix $A$ and returns
      the indispensibles as the rows of a matrix. Similarly to @TO markovBases@,
      if a ring $R$ is supplied, then the result is an ideal generated by
      the indispensable set.
    Example
      A = matrix "7,8,9,10";
      toricIndispensableSet A
      toricIndispensableSet(A, QQ[x_1 .. x_4])
    Text
      If the optional argument @TT "ReturnFiberValues"@ is set to true, then
      the function instead returns the list of fibers of the indispensable
      binomials.
    Example
      toricIndispensableSet(A, ReturnFiberValues => true)
    Text
      The function computes the indispensable elements by checking the connected
      components of the fiber graph of $A$; see @TO fiberGraph@. A fiber
      gives rise to an indispensable element if and only if it has exactly two
      connected components and each component contains a single a single point
      in the fiber.
  SeeAlso
    markovBases
    fiberGraph
///


doc ///
  Key
    toricUniversalMarkov
    (toricUniversalMarkov, Matrix)
    (toricUniversalMarkov, Matrix, Ring)
  Headline
    the universal Markov basis
  Usage
    U = toricUniversalMarkov A
    I = toricUniversalMarkov(A, R)
  Inputs
    A : Matrix
      the configuration matrix
    R : Ring
      with one generator for each column of $A$
  Outputs
    U : Matrix
      the universal Markov basis of $A$
    I : Ideal
      an ideal in $R$ generated by the universal Markov basis of $A$
  Description
    Text
      The universal Markov basis, often denoted $U(A)$, of a configuration
      matrix $A$ is the union of all minimal Markov bases of $A$. This
      method computes the universal Markov basis of $A$ and returns
      the elements as the rows of a matrix. Similarly to @TO markovBases@,
      if a ring $R$ is supplied, then the result is an ideal generated by
      the universal Markov basis.
    Example
      A = matrix "7,8,9,10";
      toricUniversalMarkov A
      toricUniversalMarkov(A, QQ[x_1 .. x_4])
    Text
      The function computes the universal Markov basis elements using the
      the fiber graph of $A$; see @TO fiberGraph@. An irreducible binomial
      $x^u - x^v$ is an element of the universal Markov basis if and only if
      $u$ and $v$ belong to different connected components of the fiber graph.

  SeeAlso
    markovBases
    fiberGraph
///


-----------
-- Tests --
-----------

TEST /// -- a fiber of a monomial curve in A^3
assert(
    set computeFiber(matrix "3,5,11",vector {8})
    ==
    set {vector {1,1,0}}
    )
///

TEST /// -- a fiber of a monomial curve in A^4
assert(
    set computeFiber(matrix "8,9,10,11",vector {37})
    ==
    set (vector \ {{2,0,1,1},{0,3,1,0},{1,2,0,1},{1,1,2,0}})
    )
///

TEST /// -- a fiber with connected components of a monomial curve in A^4
assert(
    set ((v-> set v)\computeFiber(matrix "3,4,6,8,12",vector {12},ReturnConnectedComponents=>true))
    ==
    set {set {{4, 0, 0, 0, 0}, {0, 0, 2, 0, 0}, {2, 0, 1, 0, 0}}, set {{0, 0, 0, 0, 1}}, set {{0, 1, 0, 1, 0}, {0, 3, 0, 0, 0}}}
    )
///

TEST /// -- unique minimal Markov basis for monomial curve in A^3
assert(
    set entries (markovBases matrix "3,4,5")#0
    ==
    set entries toricMarkov matrix "3,4,5"
    )
///

TEST ///
assert(countMarkov matrix "2,4,5,8;7,2,6,1" == 1)
///

TEST /// -- two minimal Markov bases for (CI) monomial curve in A^3
result := {
    {{2,-1,0},{3,0,-1}},
    {{2,-1,0},{1,1,-1}}};
assert(
    set((A -> set flatten ((v -> {v,-v}) \ entries A)) \ markovBases matrix "1,2,3")
    ==
    set((A -> set flatten ((v -> {v,-v}) \ A)) \ result)
    )
///

TEST /// -- hypersurface in A^3
result := {{1,-2,1}};
assert(
    set flatten ((v -> {v,-v}) \ entries (markovBases matrix "1,2,3;4,5,6")_0)
    ==
    set flatten ((v -> {v,-v}) \ result)
    )
///

TEST /// -- monomial curve in A^5 with five minimal Markov bases
result := {
    {{-5, 2, 0},{20, 0, -1}},
    {{-5, 2, 0},{15, 2, -1}},
    {{-5, 2, 0},{10, 4, -1}},
    {{-5, 2, 0},{5, 6, -1}},
    {{-5, 2, 0},{0, 8, -1}}};
assert(
    set((A -> set flatten ((v -> {v,-v}) \ entries A)) \ markovBases matrix "2,5,40")
    ==
    set((A -> set flatten ((v -> {v,-v}) \ A)) \ result)
    )
///

TEST ///
result := {{2,0,-1}};
assert(
    set flatten ((v -> {v,-v}) \ entries toricIndispensableSet matrix "2,3,4")
    ==
    set flatten ((v -> {v,-v}) \ result)
    )
///

TEST ///
result := {
    {1, -2, 1, 0},
    {1, -1, -1, 1},
    {0, 1, -2, 1},
    {-3, -1, 1, 2}};
assert(
    set flatten ((v -> {v,-v}) \ entries toricIndispensableSet matrix "7,8,9,10")
    ==
    set flatten ((v -> {v,-v}) \ result)
    )
///

TEST ///
result := {
    {1, -2, 1},
    {2, 0, -1},
    {3, -2, 0}};
assert(
    set flatten ((v -> {v,-v}) \ entries toricUniversalMarkov matrix "2,3,4")
    ==
    set flatten ((v -> {v,-v}) \ result)
    )
///

TEST ///
result := {
    {-4, 0, 2, 1},
    {-4, 1, 0, 2},
    {-3, -1, 1, 2},
    {0, 1, -2, 1},
    {1, -1, -1, 1},
    {1, -2, 1, 0},
    {2, 2, 0, -3},
    {3, 0, 1, -3}};
assert(
    set flatten ((v -> {v,-v}) \ entries toricUniversalMarkov matrix "7,8,9,10")
    ==
    set flatten ((v -> {v,-v}) \ result)
    )
///


end--
restart
needsPackage "AllMarkovBases"
installPackage "AllMarkovBases"

check AllMarkovBases

#markovBases matrix "5,6,7,8,9"
markovBases matrix "5,6,7,8,9"
randomMarkov matrix "5,6,7,8,9"

E2=transpose matrix {{1,0,-3,-5,-7,0,0,0,0,0,0,0,0},
    {0,0,1,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,1,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,1,0,0,0,0,0,0,0,0},
    {0,4,0,0,0,1,0,0,0,0,0,0,0},
    {0,0,0,0,0,1,0,0,0,0,0,0,0},
    {0,5,0,0,0,0,4,3,0,0,0,0,0},
    {0,0,0,0,0,0,7,0,0,0,0,0,0},
    {0,10,0,0,0,0,0,7,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,-1,2024,0,0,0},
    {0,6,0,0,0,0,0,0,2023,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,2023,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,-3,2,2},
    {0,7,0,0,0,0,0,0,0,0,1,0,0},
    {0,7,0,0,0,0,0,0,0,0,0,1,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,1}};

E3=transpose matrix {
    {0,4,0,0,0,1,0,0,0,0,0,0,0},
    {0,0,0,0,0,1,0,0,0,0,0,0,0},
    {0,5,0,0,0,0,4,3,0,0,0,0,0},
    {0,0,0,0,0,0,7,0,0,0,0,0,0},
    {0,10,0,0,0,0,0,7,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,-1,2024,0,0,0},
    {0,6,0,0,0,0,0,0,2023,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,2023,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,-3,2,2},
    {0,7,0,0,0,0,0,0,0,0,1,0,0},
    {0,7,0,0,0,0,0,0,0,0,0,1,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,1}};

A=2;
E4=transpose matrix {
    {0,4,0,0,0,1,0,0,0,0,0,0,0},
    {0,0,0,0,0,1,0,0,0,0,0,0,0},
    {0,5,0,0,0,0,4,3,0,0,0,0,0},
    {0,0,0,0,0,0,7,0,0,0,0,0,0},
    {0,10,0,0,0,0,0,7,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,-1,A,0,0,0},
    {0,6,0,0,0,0,0,0,A-1,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,-3,2,2},
    {0,7,0,0,0,0,0,0,0,0,1,0,0},
    {0,7,0,0,0,0,0,0,0,0,0,1,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,1}};

A=40;
E9=transpose matrix {{0,0,0,0,0,0,0,0,0,0,1,0,0},
    {1,0,-3,-5,-7,0,0,0,0,0,0,0,0},
    {0,0,1,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,1,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,1,0,0,0,0,0,0,0,0},
    {0,4,0,0,0,1,0,0,0,0,0,0,0},
    {0,0,0,0,0,1,0,0,0,0,0,0,0},
    {0,5,0,0,0,0,4,3,0,0,0,0,0},
    {0,0,0,0,0,0,7,0,0,0,0,0,0},
    {0,10,0,0,0,0,0,7,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,-1,A,0,0,0},
    {0,6,0,0,0,0,0,0,A-1,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,-3,2,2},
    {0,7,0,0,0,0,0,0,0,0,1,0,0},
    {0,7,0,0,0,0,0,0,0,0,0,1,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,1}};


A=3;
E8=transpose matrix {{1,0,-3,-5,-7,0,0,0,0,0,0,0,0},
    {0,0,1,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,1,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,1,0,0,0,0,0,0,0,0},
    {0,4,0,0,0,1,0,0,0,0,0,0,0},
    {0,0,0,0,0,1,0,0,0,0,0,0,0},
    {0,5,0,0,0,0,4,3,0,0,0,0,0},
    {0,0,0,0,0,0,7,0,0,0,0,0,0},
    {0,10,0,0,0,0,0,7,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,-1,A,0,0,0},
    {0,6,0,0,0,0,0,0,A-1,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,-3,2,2},
    {0,7,0,0,0,0,0,0,0,0,1,0,0},
    {0,7,0,0,0,0,0,0,0,0,0,1,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,1}};




E4

cF = (m) -> binomial(m+4, 4);
elapsedTime U = countMarkov E4;
elapsedTime V = countMarkov E5;
elapsedTime W = 5^3 * (cF(0))^8 * (cF(A))^8 * (cF(2*A))^4 * (cF(3*A))^4 * (cF(4*A))^2 * (cF(5*A))^2 * (cF(7*A));
U
V
W





b=4;
cF = (m) -> binomial(m+b, b);

5^3 * (cF(0))^8 * (cF(2024))^8 * (cF(4048))^4 * (cF(6072))^4 * (cF(8096))^2 * (cF(10120))^2 * (cF(14168))
10 + 8 * cF(0) + 8 * cF(2024) + 4 * cF(4048) + 4 * cF(6072) + 2 * cF(8096) + 2 * cF(10120) + cF(14168)



(b+1)^3 * (cF(0))^8 * (cF(A))^8 * (cF(2*A))^4 * (cF(3*A))^4 * (cF(4*A))^2 * (cF(5*A))^2 * (cF(7*A))
10 + 8 * cF(0) + 8 * cF(2024) + 4 * cF(4048) + 4 * cF(6072) + 2 * cF(8096) + 2 * cF(10120) + cF(14168)


fiberGraph' = profile fiberGraph


debugLevel = 10
elapsedTime countMarkov(E8, FiberAlgorithm => "decompose") -- 5.86078 seconds
elapsedTime countMarkov(E8, FiberAlgorithm => "fast") -- 34.2109 seconds
elapsedTime countMarkov(E8, FiberAlgorithm => "lattice") -- 156.734 seconds
remove(E8.cache, "FiberGraphComponents")


for key in keys fiberValues do (
    v := transpose matrix {(fiberValues#key)};
    print(numRows ((pointsInFiber(A, v))#"gen"));
    );

debugLevel = 2
showNmzOptions()
setNmzOption("normal_l", true)











--time testing (DNF = >3 minutes)



V1 = matrix "51,52,53,54,55,56,91,92,93,94,95,96,97,98,99,100,101"

elapsedTime countMarkov(V1,FiberAlgorithm=>"fast") -- 0.7 seconds
elapsedTime countMarkov(V1, FiberAlgorithm => "decompose") -- 3.0 seconds
elapsedTime countMarkov(V1,FiberAlgorithm=>"lattice") -- DNF
elapsedTime countMarkov(V1, FiberAlgorithm => "markov") -- 4.3 seconds

--WINNER: fast



B = matrix "1,2,6,12,24,48,96,128"

elapsedTime countMarkov(B,FiberAlgorithm=>"fast") -- 4.0 seconds
elapsedTime countMarkov(B, FiberAlgorithm => "decompose") -- 1.5 seconds
elapsedTime countMarkov(B, FiberAlgorithm => "lattice") -- DNF
elapsedTime countMarkov(B, FiberAlgorithm => "markov") -- 3.7 seconds

--WINNER: decompose


hi = matrix "1078,2947,3002,965,9041,7480"

elapsedTime countMarkov(hi,FiberAlgorithm=>"fast") -- 0.67 seconds
elapsedTime countMarkov(hi, FiberAlgorithm => "decompose") -- 34 seconds
elapsedTime countMarkov(hi, FiberAlgorithm => "lattice") -- 0.5 seconds
elapsedTime countMarkov(hi, FiberAlgorithm => "markov") -- 7.4 seconds

--WINNER: lattice


A=4;
E8=transpose matrix {{1,0,-3,-5,-7,0,0,0,0,0,0,0,0},
    {0,0,1,0,0,0,0,0,0,0,0,0,0},
    {0,0,0,1,0,0,0,0,0,0,0,0,0},
    {0,0,0,0,1,0,0,0,0,0,0,0,0},
    {0,4,0,0,0,1,0,0,0,0,0,0,0},
    {0,0,0,0,0,1,0,0,0,0,0,0,0},
    {0,5,0,0,0,0,4,3,0,0,0,0,0},
    {0,0,0,0,0,0,7,0,0,0,0,0,0},
    {0,10,0,0,0,0,0,7,0,0,0,0,0},
    {0,0,0,0,0,0,0,0,-1,A,0,0,0},
    {0,6,0,0,0,0,0,0,A-1,0,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,A-1,0,0,0},
    {0,0,0,0,0,0,0,0,0,0,-3,2,2},
    {0,7,0,0,0,0,0,0,0,0,1,0,0},
    {0,7,0,0,0,0,0,0,0,0,0,1,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,1}};

elapsedTime countMarkov(E8, FiberAlgorithm => "fast") -- 67.7 seconds
elapsedTime countMarkov(E8, FiberAlgorithm => "decompose") -- DNF
elapsedTime countMarkov(E8, FiberAlgorithm => "lattice") -- DNF
elapsedTime countMarkov(E8, FiberAlgorithm => "markov") -- 13.6 seconds

--WINNER: markov



restart
uninstallPackage "AllMarkovBases"
restart
installPackage "AllMarkovBases"

check AllMarkovBases

viewHelp AllMarkovBases
