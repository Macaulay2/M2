
-----------------------
-- Producing new forms
-----------------------

-- Input: A field kk, and a list of elements a_1, ... , a_n of kk
-- Output: The diagonal form <a_1,...,a_n> 

diagonalForm = method()
diagonalForm(Ring,RingElement) := GrothendieckWittClass => (kk,a) -> (
    return gwClass(matrix(kk,{{sub(a,kk)}}))
    )

diagonalForm(Ring,ZZ) := GrothendieckWittClass => (kk,a) -> (
    return gwClass(matrix(kk,{{sub(a,kk)}}))
    )

diagonalForm(Ring,QQ) := GrothendieckWittClass => (kk,a) -> (
    return gwClass(matrix(kk,{{sub(a,kk)}}))
    )

diagonalForm(Ring, Sequence) := GrothendieckWittClass => (kk,L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Build an n x n identity matrix
    A := mutableIdentity(kk, n);
    
    for i from 0 to (n-1) do(
	A_(i,i) = sub(L_i,kk);
	);
    
    -- A is mutable so we take matrix(A) to plug it into gwClass
    return gwClass(matrix(A))
    )

diagonalForm(InexactFieldFamily,RingElement) := GrothendieckWittClass => (kk,a) -> (
    return gwClass(matrix(kk,{{sub(a,kk)}}))
    )

diagonalForm(InexactFieldFamily,ZZ) := GrothendieckWittClass => (kk,a) -> (
    return gwClass(matrix(kk,{{sub(a,kk)}}))
    )

diagonalForm(InexactFieldFamily,QQ) := GrothendieckWittClass => (kk,a) -> (
    return gwClass(matrix(kk,{{sub(a,kk)}}))
    )

diagonalForm(InexactFieldFamily, Sequence) := GrothendieckWittClass => (kk,L) -> (
    -- Get the length of the input sequence
    n := #L;
    
    -- Build an n x n identity matrix
    A := mutableIdentity(kk, n);
    
    for i from 0 to (n - 1) do(
	A_(i,i) = sub(L_i,kk);
	);
    
    -- A is mutable so we take matrix(A) to plug it into gwClass
    return gwClass(matrix(A))
    )

-- Input: A field kk
-- Output: An even number, giving an optional rank n for a totally hyperbolic form

hyperbolicForm = method()

hyperbolicForm(Ring) := GrothendieckWittClass => (kk) -> (
    return gwClass(matrix(kk,{{1,0},{0,-1}}))
    )

-- Can take an optional input specifying an (even) rank of a totally hyperbolic form. Default is 2
hyperbolicForm(Ring,ZZ) := GrothendieckWittClass => (kk,n) -> (
	if odd n then error "inputted rank is odd";
	H := matrix(kk,{{1,0},{0,-1}});
    	k := sub(n/2,ZZ);
    	outputMatrix := diagonalMatrix(kk,{});
    	for i from 0 to k - 1 do(
	    outputMatrix = outputMatrix ++ H;
	    );
        return gwClass(outputMatrix)
    )

hyperbolicForm(InexactFieldFamily) := GrothendieckWittClass => (kk) -> (
    return gwClass(matrix(kk,{{1,0},{0,-1}}))
    )

-- Can take an optional input specifying an (even) rank of a totally hyperbolic form. Default is 2
hyperbolicForm(InexactFieldFamily,ZZ) := GrothendieckWittClass => (kk,n) -> (
	if odd n then error "inputted rank is odd";
	H := matrix(kk,{{1,0},{0,-1}});
    	k := sub(n/2,ZZ);
    	outputMatrix := diagonalMatrix(kk,{});
    	for i from 0 to k - 1 do(
	    outputMatrix = outputMatrix ++ H;
	    );
        return gwClass(outputMatrix)
    )

-- Input: A field kk, and a list of elements a_1, ... , a_n of kk
-- Output: The Pfister form <<a_1,...,a_n>>

PfisterForm = method()
PfisterForm(Ring,RingElement) := GrothendieckWittClass => (kk,a) -> (
    return diagonalForm(kk,(1,(-1)*a))
    );

PfisterForm(Ring,ZZ) := GrothendieckWittClass => (kk,a) -> (
    return diagonalForm(kk,(1,(-1)*a))
    );

PfisterForm(Ring,QQ) := GrothendieckWittClass => (kk,a) -> (
    return diagonalForm(kk,(1,(-1)*a))
    );

PfisterForm(Ring,Sequence) := GrothendieckWittClass => (kk,L) -> (
    -- Get the length of the input sequence
    n := #L;
    if n == 0 then error "list is empty";
    
    -- If there is just one entry L_0 return the form <1, -L_0>
    firstPfisterForm := diagonalForm(kk,(1,(-1)*L_0));
    if n == 1 then(
	return firstPfisterForm
	);
    
    -- If n>1 iteratively multiply <1,-L_0>*<1,-L_1>*...
    outputForm := firstPfisterForm;
    for i from 1 to (n-1) do (
	ithPfister := diagonalForm(kk,(1,(-1)*L_i));
	outputForm = gwMultiply(outputForm,ithPfister);
	);
    
    return outputForm
   )

PfisterForm(InexactFieldFamily,RingElement) := GrothendieckWittClass => (kk,a) -> (
    return diagonalForm(kk,(1,(-1)*a))
    );

PfisterForm(InexactFieldFamily,ZZ) := GrothendieckWittClass => (kk,a) -> (
    return diagonalForm(kk,(1,(-1)*a))
    );

PfisterForm(InexactFieldFamily,QQ) := GrothendieckWittClass => (kk,a) -> (
    return diagonalForm(kk,(1,(-1)*a))
    );

PfisterForm(InexactFieldFamily,Sequence) := GrothendieckWittClass => (kk,L) -> (
    -- Get the length of the input sequence
    n := #L;
    if n == 0 then error "list is empty";
    
    -- If there is just one entry L_0 return the form <1, -L_0>
    firstPfisterForm := diagonalForm(kk,(1,(-1)*L_0));
    if n == 1 then(
	return firstPfisterForm
	);
    
    -- If n>1 iteratively multiply <1,-L_0>*<1,-L_1>*...
    outputForm := firstPfisterForm;
    for i from 1 to (n-1) do(
	ithPfister := diagonalForm(kk,(1,(-1)*L_i));
	outputForm = gwMultiply(outputForm,ithPfister);
	);
    
    return outputForm
   )
