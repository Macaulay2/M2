

------------------------------
-- INPUT: an integer n between 1 and 7
-- OUTPUT: a list of matrices containing all ASMs of size n
------------------------------

ASMFullList = method()
ASMFullList ZZ := List => (n) -> (
    if (n < 1 or n > 7) then error("expected an integer between 1 and 7");
    filename := concatenate(
	MatrixSchubert#"source directory",
	"MatrixSchubert/ASMData/full/", toString n, ".txt");
    listOfMatrices := apply(lines get filename, i -> matrix value i);
    listOfMatrices
);


------------------------------
-- INPUT: an integer n between 1 and 7 for the size of ASMs, and an integer m for number of random ASMs.
-- OUTPUT: a list m random matrices from ASM(n).
------------------------------
ASMRandomList = method()
ASMRandomList (ZZ,ZZ) := List => (n,m) -> (
    if (n < 1 or n > 7) then error("expected an integer between 1 and 7");
    fullList := ASMFullList(n);
    randNums := for i to m-1 list random (#fullList-1);
    listOfMatrices := apply(randNums, i -> fullList#i);
    listOfMatrices
);

------------------------------
-- INPUT: an integer n between 1 and 6
-- OUTPUT: a list of matrices containing all CM ASMs of size n
------------------------------
cohenMacaulayASMsList = method()
cohenMacaulayASMsList ZZ := List => (n) -> (
    if (n < 1 or n > 6) then error("expected an integer between 1 and 6");
    filename := concatenate(
	MatrixSchubert#"source directory",
	"MatrixSchubert/ASMData/CM/good", toString n, ".txt");
    listOfMatrices := apply(lines get filename, i -> matrix value i);
    listOfMatrices
);

------------------------------
-- INPUT: an integer n between 1 and 6
-- OUTPUT: a list of matrices containing all non-CM ASMs of size n
------------------------------
nonCohenMacaulayASMsList = method()
nonCohenMacaulayASMsList ZZ := List => (n) -> (
    if (n < 1 or n > 6) then error("expected an integery between 1 and 6");
    if (n <= 3) then return {};
    filename := concatenate(
	MatrixSchubert#"source directory",
	"MatrixSchubert/ASMData/notCM/bad", toString n, ".txt");
    listOfMatrices := apply(lines get filename, i -> matrix value i);
    listOfMatrices
);

------------------------------
-- INPUT: an integer n between 3 and 6
-- OUTPUT: a list of matrices containing all antidiagonal initial ideals of size n
-- TODO: test and docs
------------------------------
initialIdealsList = method()
initialIdealsList ZZ := List => (n) -> (
    if (n < 3 or n > 6) then error("expected an integer between 3 and 6");
    filename := concatenate(
	MatrixSchubert#"source directory",
	"MatrixSchubert/ASMData/antiDiagIniIdeal/ideals", toString n, ".txt");
    z := getSymbol "z";
    S := QQ(monoid[z_(1,1)..z_(n,n)]);
    listOfIdeals := apply(lines get filename, i -> (use S; value i));
    listOfIdeals
);
