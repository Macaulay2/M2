---------------------------------------------------------------------
--THE TYPE OF A POLYNOMIAL PATH
---------------------------------------------------------------------

------------------------------------------------------------------
--Defining the Type "Path" as a subclass of MutableHashTable.
--It should be able to allow for concatenation of piecewise linear
--and polynomial paths and to correctly call the functions
--already implemented depending on the type of path.
--A Path will then be an (ordered) list of LinPaths and PolyPaths
------------------------------------------------------------------


Path = new Type of MutableHashTable

protect type
protect pieces
protect dimension
protect numberOfPieces
protect bR


--A piecewise polynomial path
--polyPath takes a list of polynomials in some variable and constructs the corresponding polynomial path from it
--the polynomials can be given as actual polynomials or directly in listForm

polyPath = method();
polyPath List := Path => (polyPathList) -> (
    if(polyPathList === {}) then return new Path from {pieces => {}, dimension => -1, numberOfPieces => 0};

    if isListForm (polyPathList#0) then (
        apply(polyPathList, i-> if not(isListForm(i)) then error("the input was neither a list of polynomials nor a list of polynomials in listForm"));
        return new Path from{
            pieces => {polyPathList},
            dimension => length polyPathList,
            numberOfPieces => 1,
            bR => class (product apply(polyPathList, i -> i#0#(-1)))
        };
        );

    if (instance(product(polyPathList), RingElement)) then (
        tR := class product(polyPathList);
        if(#gens(tR) != 1) then error("expected a vector of polynomials in one variable.");
        baseR := coefficientRing (tR);
        P := new Path from{
            bR => baseR,
            pieces => {apply(polyPathList,i-> listForm (i*1_baseR - sub(i, {tR_0 => 0})))},
            dimension => length polyPathList,
            numberOfPieces => 1
        };
        return(P);
    );
)

--Take parts of a path

Path _ List := Path => (X, l) -> (
    if(l === {}) then return polyPath({});
    P := new Path from{
        bR => X.bR,
        pieces => (X.pieces)_l,
        dimension => X.dimension,
        numberOfPieces => length(l)
    };

    P
)

Path _ Sequence := Path => (X,l) -> (
    return X_(toList l);
)

Path _ ZZ := Path => (X, z) -> (
    X_{z}
)

getDimension = method();
getDimension Path := (X) -> X.dimension;
dim Path := (X) -> X.dimension; -- can we have two aliases for the same function?

getPieces = method();
getPieces Path := (X) -> X.pieces;

getCoefficientRing = method();
getCoefficientRing Path := (X) -> X.bR;
coefficientRing Path := (X) -> X.bR;

getNumberOfPieces = method();
getNumberOfPieces Path := (X) -> X.numberOfPieces;

-- Concatenation of paths

sub(Path,Ring) := Path => (X,R) -> (
    npieces := X.pieces;
    npieces = apply(npieces, polvec -> apply(polvec, pol -> apply(pol, mon -> (mon#0, sub(mon#1,R)))));
    P := new Path from{
        bR => R,
        pieces => npieces,
        dimension => X.dimension,
        numberOfPieces => X.numberOfPieces
    };

    P
);

concatPath = method();
concatPath(Path,Path) := Path => (X,Y) -> (
    if(X.dimension != Y.dimension) then error("cannot concatenate paths of different ambient dimension.");
    R := if((X.bR === Y.bR)) then X.bR else (
        if (isMember(Y.bR, (X.bR).baseRings)) then (
            Y = sub(Y,X.bR); return(X ** Y);
        ) else if (isMember(X.bR, (Y.bR).baseRings)) then (
            X = sub(X, Y.bR); return(X ** Y);
        ) else if (coefficientRing X.bR === coefficientRing Y.bR) then  (
            nR := X.bR ** Y.bR;
            return(sub(X,nR)**sub(Y,nR));
         )
        else error("the base rings 'bR' of the two paths were different, namely they were X.bR = ", toString X.bR, " and Y.bR = ", toString Y.bR, ". Moreover no trivial relation between them was found.");
    );
    
    P := new Path from{
        bR => X.bR,
        pieces => X.pieces | Y.pieces,
        dimension => X.dimension,
        numberOfPieces => X.numberOfPieces + Y.numberOfPieces
    };

    P
)

Path ** Path := Path => (X,Y) -> concatPath(X,Y);



Path ^ ZZ := Path => (X,n) -> (
    if(n > 0) then return(fold(n:X, (X,Y) -> X**Y));
    if(n == -1) then (
        rpath := X.pieces;
        s := getSymbol("s");
        S := X.bR monoid([s]);
        rpath = apply(rpath, polyvec -> apply(polyvec, pol -> sum(pol, mon -> ((mon)#1)_S * (S_0)^((mon)#0#0)))); -- transform pieces back to polynomial vectors in variable s
        rpath = apply(rpath,polyvec -> apply(polyvec, pol -> sub(pol,S_0 => (1 - S_0)))); -- replace s by 1-s
        rpath = reverse(rpath); -- reverse order of pieces
        rpath = apply(rpath, P-> polyPath(P));
        return(fold(rpath,(i,j)->i**j));
    );
    if(n < 1) then (
        return((X^(-1))^(abs(n)))
    );
    t:= getSymbol("t");
    auxR := X.bR [t];

    polyPath(toList(X.dimension:(0_(auxR))))
)




--------------------------------------------------------------
--Printing of paths
--------------------------------------------------------------
net Path := (X) ->
(
    myNet := net ("Path in " | X.dimension | "-dimensional space with " | X.numberOfPieces | (if(X.numberOfPieces == 1) then " polynomial segment:" else " polynomial segments:") );
    t:= getSymbol("t");
    locR := X.bR [local t];
    pieces := apply(X.pieces,l->
        apply(l,
            p-> sum(p, 
                i-> i#1*t^(i#0#0)
                )
            )
        );
    myNet = myNet || "" || (net pieces);

    myNet
)

--Constructs the linear polynomial path t*v for a vector v

linPath = method();
linPath List := Path => (v) ->(
    baseR := class product(v); 
    if(baseR === ZZ) then (baseR = QQ);
    new Path from{
        bR => baseR,
        pieces => {apply(v,i->{({1},i)})},
        dimension => #v,
        numberOfPieces => 1
    }
)

--Constructs a pw linear path from a given matrix of increments

pwLinPath = method();
pwLinPath Matrix := Path => (pwlMatrix) -> (
    pathList := apply(transpose entries pwlMatrix, i-> linPath(i));

    fold(pathList,(i,j)->i**j)
)


--Basic checks to verify if a list is the listForm of a polynomial
isListForm = method(); 
isListForm Thing := (L) ->(
    if not(instance(L, List)) or not(instance(L#0, Sequence)) or not(instance(L#0#0, List))  then return false;
    l := length(L#0#0);
    c := L#0#(-1);
    if not(instance(c, Number)) and not(instance(c, RingElement)) then return false;
    apply(L, i-> 
        if length(i#0)!=l or (not(instance(i#(-1), Number)) and not(instance(#i(-1), RingElement))) then return false);
    true
);
