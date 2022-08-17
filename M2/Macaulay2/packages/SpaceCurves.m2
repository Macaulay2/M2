newPackage(
        "SpaceCurves",
        Version => "1.0", 
        Date => "May 26th 2018",
        Authors => {{Name => "Frank-Olaf Schreyer", 
                  Email => "schreyer@math.uni-sb.de", 
                  HomePage => "https://www.math.uni-sb.de/ag/schreyer/"},
	      {Name => "Mike Stillman", 
                  Email => "mike@math.cornell.edu", 
                  HomePage => "http://www.math.cornell.edu/~mike/"},
	      {Name => "Mengyuan Zhang", 
                  Email => "myzhang@berkeley.edu", 
                  HomePage => "https://math.berkeley.edu/~myzhang/"}
	      },
        Headline => "space curves",
	Keywords => {"Examples and Random Objects"},
        DebuggingMode => false,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "The SpaceCurves package in Macaulay2",
	     "acceptance date" => "18 May 2018",
	     "published article URI" => "https://msp.org/jsag/2018/8-1/p04.xhtml",
	     "published article DOI" => "10.2140/jsag.2018.8.31",
	     "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x04-SpaceCurves.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/SpaceCurves.m2",
	     "release at publication" => "7853d911a8a484766a7828dc8e17aed701ce9fd6",	    -- git commit number in hex
	     "version at publication" => "1.0",
	     "volume number" => "8",
	     "volume URI" => "https://msp.org/jsag/2018/8-1/"
	     }
        )

export {
--Surface	
	"QuadricSurface",
	"IntersectionPairing",	      
	"CanonicalClass",
	"HyperplaneClass",
	"quadricSurface",
	"CubicSurface",
	"BlowUpPoints",
	"MapToP3",
	"cubicSurface",
	"QuarticSurfaceRational",
	"quarticSurfaceRational",
--Divisor
	"Divisor",
	"Coordinate",
	"Surface",
	"divisor",
	"surface",
	"smoothDivisors",
--Curves
    	"Curve",
	"curve",
	"isSmooth",
--ACM curves
	"positiveChars",
	"isACMBetti",
	"isSmoothACMBetti",
	"generalACMBetti",
	"specializeACMBetti",
	"allACMBetti",
	"degreeMatrix",
	"randomDeterminantalIdeal",
--Minimal curves
    	"minimalCurve",
	"raoModule",
	"minimalCurveBetti",
--Plotting
	"dgTable"	
}


--I. Surfaces

QuadricSurface = new Type of HashTable
net QuadricSurface := X -> net X.Ideal
ideal QuadricSurface := X -> X.Ideal
quadricSurface = method()
quadricSurface Ring := R -> (
    X := gens R;
    assert(isField coefficientRing R and #X == 4);
    new QuadricSurface from {
	symbol Ideal => ideal(X_0*X_3-X_1*X_2),
	symbol IntersectionPairing => matrix{{0,1},{1,0}},
	symbol CanonicalClass => {-2,-2},
	symbol HyperplaneClass => {1,1}
    }
)

CubicSurface = new Type of HashTable
net CubicSurface := X -> net X.Ideal
ideal CubicSurface := X -> X.Ideal
cubicSurface = method()
cubicSurface Ring := R -> (
    kk := coefficientRing R;
    X := gens R;
    assert(isField kk and #X == 4);
    y := getSymbol "y";
    S := kk(monoid[y_0..y_2]);
    RS := R ** S;
    while (
    	M := diagonalMatrix({1,1,1}) | matrix{{1},{1},{1}} | random(S^3,S^2);
        points := apply(6,i-> ideal (vars S * syz transpose M_{i}));
        I := intersect points;
        fI := res I;
        if numgens R =!= numColumns fI.dd_1 then
          error "randomization produced an error: try again, and/or increase the size of your base field";
        phi := map(S,R,fI.dd_1);
        matS := sub(diff(transpose sub(vars S,RS),sub(vars R,RS) * sub(fI.dd_2,RS)),R);
        f := ideal det matS;
        dim (f + ideal jacobian f) != 0
    ) do ();	--passes a smoothness check
    new CubicSurface from {
	symbol IntersectionPairing => diagonalMatrix splice{1,6:-1},
	symbol CanonicalClass => -splice{3,6:1},
        symbol HyperplaneClass => splice{3,6:1},
	symbol Ideal => f,
	symbol BlowUpPoints => points,
	symbol MapToP3 => phi
    }
)
linesOnCubic := () -> (
    --Produces the coordinates of the 27 lines
    Ds := entries diagonalMatrix(splice{7:1});
    Es := drop(entries diagonalMatrix(splice{0,6:-1}),1);
    Fs := for p in subsets(splice {1..6}, 2) list (
            first Ds + Ds#(p#1) + Ds#(p#0)
            );
    Gs := for i from 0 to 5 list (
            2*(first Ds) - sum drop(Es, {i,i})
            );
    join(Es,Fs,Gs)
)

QuarticSurfaceRational = new Type of HashTable
net QuarticSurfaceRational := X -> net X.Ideal
ideal QuarticSurfaceRational := X -> X.Ideal
quarticSurfaceRational = method()
quarticSurfaceRational Ring := R -> (
    kk := coefficientRing R;
    X := gens R;
    assert(isField kk and #X == 4);
    y := getSymbol "y";
    S := kk(monoid[y_0..y_2]);
    while (
    	M := diagonalMatrix({1,1,1}) | matrix{{1},{1},{1}} | random(S^3,S^5);
    	points := apply(entries transpose M, p->minors(2,matrix{gens S,p}));
	G := gens trim intersect points;
    	C := ideal (G*random(source G,S^{-3}));
        (numcols basis(3,intersect points) != 1) or 
	dim (C+ideal jacobian C) != 0
	--passes a check that a unique elliptic passes through the 9 points
    ) do ();
    H := trim intersect ({(first points)^2} | drop(points,1));
    phi := map(S,R,gens H);
    Q := kernel phi;    
    new QuarticSurfaceRational from {
    	symbol IntersectionPairing => diagonalMatrix splice{1,9:-1},
	symbol CanonicalClass => -splice{3,9:1},
	symbol HyperplaneClass => splice{4,2,8:1},
	symbol Ideal => Q,
	symbol BlowUpPoints => points,
	symbol MapToP3 => phi	
    }	
)

--II.Divisors

Divisor = new Type of HashTable
net Divisor := X -> net X.Coordinate
divisor = method()
divisor(List, QuadricSurface) := (C, X) -> (
    new Divisor from {
        symbol Coordinate => C,
        symbol Surface => X
        }
    )
divisor(List, CubicSurface) := (C, X) -> (
    new Divisor from {
        symbol Coordinate => C,
        symbol Surface => X
        }
    )
divisor(List, QuarticSurfaceRational) := (C,X) -> (
    new Divisor from {
        symbol Coordinate => C,
        symbol Surface => X
        } 
)
surface = method() 
surface Divisor := D -> D.Surface

ZZ * Divisor := (n,D) -> divisor(n * D.Coordinate, D.Surface)
Divisor + Divisor := (C,D) -> (
    assert(C.Surface === D.Surface);
    divisor(C.Coordinate + D.Coordinate, D.Surface)
    )
Divisor - Divisor := (C,D) -> (
    assert(C.Surface === D.Surface);
    divisor(C.Coordinate - D.Coordinate, D.Surface)
    )
Divisor * Divisor := (C,D) -> (
    X := C.Surface;
    assert(X === D.Surface);
    assert(X.IntersectionPairing =!= null);
    (matrix{C.Coordinate} * X.IntersectionPairing * 
	transpose matrix{D.Coordinate})_(0,0)
    )
degree Divisor := C -> (
    X := C.Surface;
    (matrix{C.Coordinate} * X.IntersectionPairing * 
	transpose matrix{X.HyperplaneClass})_(0,0)
    )
genus Divisor := C -> (
    X := C.Surface;
    K := divisor(X.CanonicalClass,X);
    1/2*((K+C)*C)+1
    )

 
--III.Curves

Curve = new Type of HashTable
net Curve := C -> net C.Ideal
ideal Curve := C -> C.Ideal
divisor Curve := C -> C.Divisor
surface Curve := C -> C.Divisor.Surface

curve = method()
curve Divisor := D -> (
	X := D.Surface;
	R := ring X.Ideal;
	kk := coefficientRing R;
	I := ideal(0);
	
	if class X === QuadricSurface then (
	    	z := getSymbol "z";
	    	cox := kk(monoid[z_0..z_3,Degrees=>{{0,1},{0,1},{1,0},{1,0}}]);
    	    	segre := {cox_0*cox_2,cox_0*cox_3,cox_1*cox_2,cox_1*cox_3};
	    	I = ideal random(D.Coordinate,cox);
	    	if I == 0 then return null;
	    	segre = apply(segre, p -> sub(p,cox/I));
    	    	I = kernel map(cox/I,R,segre);	    
	);
    	n := 0;
	if class X === CubicSurface then n = 6;
	if class X === QuarticSurfaceRational then n = 9;
	if class X === CubicSurface or class X === QuarticSurfaceRational then (
	    attempt := 0;
	    while (
		--Find a plane curve with given multiplicities at points and its image	    	
		phi := X.MapToP3;
	    	pts := X.BlowUpPoints;
            	S := target phi;
            	R = source phi;
            	ab := D.Coordinate;
            	a := ab_0;
            	ipts := trim intersect (for i from 1 to n list (pts_(i-1))^(ab_i));
            	gipts := gens ipts;
                cplane := ideal (gipts*random(source gipts,S^{-a}));
               	SC := S/cplane;
            	I = ker map(SC,R,phi.matrix);
		(degree I != degree D) or (genus I != genus D) and attempt<3
	    ) do (attempt = attempt+1;);
	);
        return new Curve from {
			symbol Divisor => D,
			symbol Ideal => I
	 		};
)
degree Curve := C -> degree ideal C
genus Curve := C -> genus ideal C
isSmooth = method()
isSmooth Ideal := I -> (
    c := codim I;
    dim(I + minors(c, jacobian I)) == 0
    )
isSmooth Curve := C -> isSmooth ideal C
isPrime Curve := {} >> o -> C -> isPrime ideal C

--V.Minimal Curves

raoModule = method()
raoModule Ideal := I -> (
    assert( dim I == 2);
    coker (dual res Ext^3(comodule I,ring I)).dd_(-3)
)
raoModule Curve := C -> raoModule ideal C
minimalCurve = method()
alphaBeta = M -> (
    --takes a matrix M 
    --returns (numcols M, rank M, inf_D rank M ** R/D) where D is a ht 1 prime
    kk := coefficientRing ring M;
    t := getSymbol "t";
    S := kk[t];
    L := apply(4,i->random(1,S)+random(0,S));
    (A,B,C) := smithNormalForm sub(M,matrix{L});
    D := apply(min(numrows A,numcols A), i-> A_(i,i));
    (numcols M,#select(D,d->d!=0),#select(D,d->d==1))    
)
selectColumns = (d,M) -> (
    --selects the columns of a matrix of degree <= d
    L := select(flatten last degrees M,D->D<= d);
    M_(splice{0..#L-1})
)
minimalCurve Module := M -> (
    --Take a finite length module
    --returns a minimal curve
    R := ring M;
    if M == 0 then return ideal(R_0,R_1);
    assert(dim M == 0);
    Q := res M;
    r := rank Q_2 - rank Q_3 + rank Q_4 - 1;
    degs := flatten last degrees Q.dd_2;
    ok := 1;
    L := {};
    degloop := unique degs;
    deg := first degloop;
    while #L < r do (
	--This algorithm computes the correct columns to select
    	(s,a,b) := alphaBeta selectColumns(deg,Q.dd_2);
	if (s != a or s != b) then (
	    L = L | splice{(min(a-1,b)-#L):deg};
	    ok = 0;
	) else (
	    if ok == 1 then L = L | select(degs,d -> d == deg);
	);
    	degloop = drop(degloop,1);
	if degloop != {} then deg = first degloop;
    ); 
    cols := for i from 0 to #degs-1 list (	
    	j := position(L, l -> l == degs#i);
	if j === null then (
	    i   
	) else (    	    
	    L = drop(L,{j,j});
	    continue    
	)    		
    );
    ideal gens kernel transpose (random(Q_2,Q_2)*(Q.dd_3))^cols   
)
minimalCurve Ideal := I -> minimalCurve raoModule I
minimalCurve Curve := C -> minimalCurve raoModule ideal C
minimalCurveBetti = method()
minimalCurveBetti Module := M -> (
    --Take a finite length module
    --returns the Betti table of a minimal curve
    R := ring M;
    if M == 0 then return ideal(R_0,R_1);
    assert(dim M == 0);
    Q := res M;
    r := rank Q_2 - rank Q_3 + rank Q_4 - 1;
    degs := flatten last degrees Q.dd_2;
    ok := 1;
    L := {};
    degloop := unique degs;
    deg := first degloop;
    while #L < r do (
	--This algorithm computes the correct columns to select
    	(s,a,b) := alphaBeta selectColumns(deg,Q.dd_2);
	if (s != a or s != b) then (
	    L = L | splice{(min(a-1,b)-#L):deg};
	    ok = 0;
	) else (
	    if ok == 1 then L = L | select(degs,d -> d == deg);
	);
    	degloop = drop(degloop,1);
	if degloop != {} then deg = first degloop;
    ); 
    h := sum L - sum flatten degrees Q_2 + sum flatten degrees Q_3 - sum flatten degrees Q_4;
    cols := for i from 0 to #degs-1 list (	
    	j := position(L, l -> l == degs#i);
	if j === null then (
	    i    
	) else (    	    
	    L = drop(L,{j,j});
	    continue    
	)    		
    );
    betti chainComplex {random(R^1,(target (Q.dd_3)^cols)**R^{-h}),(Q.dd_3)^cols**R^{-h},
	 Q.dd_4**R^{-h}, Q.dd_5**R^{-h}}
)
minimalCurveBetti Ideal := I -> minimalCurveBetti raoModule I
minimalCurveBetti Curve := C -> minimalCurveBetti raoModule ideal C

--VI.ACM Curves
Delta := L-> (
    --numerical differentiation, auxiliary
    M := for i from 1 to #L-1 list L#i-L#(i-1);
    {L#0} | M    
)
reduce := L -> (
    while last L == 0 do L = drop(L,-1);
    L    
)
positiveChars = method()
positiveChars (ZZ,ZZ) :=  List => (d,s) -> (
    --Generates all positive characters of degree d and least degree surface s
    a := getSymbol "a";
    deg := apply(splice{s..(d-1)},i->{1,i});
    R := (ZZ/2)(monoid[a_s..a_(d-1),Degrees=> deg]);
    normalize := {s,d-sum apply(s,k-> -k)};
    L := flatten entries basis(normalize,R);
    apply(L,p-> toList(s:-1) | flatten exponents p) / reduce
)
positiveChars ZZ := List => d -> (
    --Generates all positive characters of degree d
    flatten apply(splice{1..d-1},s-> positiveChars(d,s))
)
bettiToList := B -> (
    --turns a BettiTally into a list of degrees in the free resolution
    --auxillary
    n := max apply(keys B, k -> first k);
    for i from 0 to n list (
    	b := select(keys B, k -> first k == i);
	flatten apply(b, j -> splice{B#j: last j})	
    )    
)
listToBetti := L -> (
    --turns a list of degrees in the free resolution into a BettiTally
    --auxillary
    L = L / tally;
    new BettiTally from 
    flatten apply(#L, i -> flatten apply(keys L#i, j -> (i, {j},j) => L#i#j))    
)

degreeMatrix = method()
degreeMatrix BettiTally := Matrix => B -> (
    --turns the BettiTally of an ACM curve into a degree matrix
    L := drop(bettiToList B,1);
    if #L != 2 or #(L#0) != #(L#1)+1 then return matrix {{0}};
    matrix apply(reverse sort L#0, l -> 
	apply(reverse sort L#1, m -> if m < l then 0 else m-l))   
)
isACMBetti = method()
isACMBetti BettiTally := Boolean => B -> (
    --checks if there is an ACM curve having Betti table B
    L := sort (bettiToList B)#1;
    mat := degreeMatrix B;
    diag := apply(#L-1, i -> mat_(i,i));
    if any(diag, j -> j <= 0) then return false;
    
    x := getSymbol "x"; 
    R := (ZZ/101)(monoid[x_0,x_1]);
    newL := sort flatten degrees randomDeterminantalIdeal(R,mat);
    
    if newL != L then return false else return true  
)
isSmoothACMBetti = method()
isSmoothACMBetti BettiTally := Boolean => B -> (
    --checks if there is a smooth ACM curve having Betti table B
    if not isACMBetti B then return false;
    mat := degreeMatrix B; 
    if any(apply(numcols mat -1, i -> mat_(i,i+1)), j-> j <= 0) then return false else return true
)
generalACMBetti = method()
generalACMBetti List := gamma -> (
    --takes a postulation character and returns the Betti table of 
    --the general ACM curve having this character
    alt := reduce Delta(-gamma | {0});
    alt = {0} | drop(alt,1); 
    T := {(0,{0},0) => 1} | apply(#alt, i -> (
	    if alt#i < 0 then (1,{i},i) => -alt#i
	    else if alt#i > 0 then (2,{i},i) => alt#i));
    new BettiTally from delete(null,T)
)
specializeACMBetti = method()
specializeACMBetti BettiTally := List => B -> (
    --takes Betti table of an ACM curves
    --returns all valid Betti tables of 1-specializations
    L := bettiToList B;
    if #(L#1) >= min(L#1)+1 then return {};	  
    deg := splice{min(L#1)+1..max(L#2)-1};
    Sp := unique apply(deg , d -> {L#0, L#1 | {d}, L#2 | {d}});
    select(Sp / listToBetti, b -> isACMBetti b) 
    
)
allACMBetti = method()
allACMBetti List := gamma ->(
    --takes a postulation character and returns all Betti tables
    --of ACM curves having that character
    B := generalACMBetti gamma;
    final := {B};
    current := {B};
    while (
    	L := bettiToList first current;
	#(L#1) < min(L#1)+1
    ) do (
    	current = flatten apply(current, b -> specializeACMBetti(b));
	final = unique (final | current);    
    );
    return final   
)
randomDeterminantalIdeal = method()
randomDeterminantalIdeal (Ring,Matrix) := (R,M) -> (
    --produces a random determinantal ideal in the ring R
    --with forms of degrees specified by the matrix M
    --nonpositive degrees entries are set to be 0
    if M != matrix{{}} then (
    	N := matrix apply(entries M, row -> apply(row, a-> if a <= 0 then 0 else random(a,R)))
    ) else return ideal(0);
    minors(numcols N,N)    
)


--VII.Generation and plotting

smoothDivisors = method()
smoothDivisors (ZZ,QuadricSurface) := (d,X) -> (
    --generates all smooth divisors on the QuadricSurface of degree d
    maxdeg := floor(1/2*d);
    for a from 1 to maxdeg list divisor({a,d - a},X)    
)
smoothDivisors (ZZ,CubicSurface) := (d,X) -> (
    --generates all smooth divisors of degree d on the CubicSurface
    --unique up to monodromy
    flatten for a from ceiling(d/3) to d list (
	    degreeList := apply(select(partitions(3*a-d),p->#p<=6),q ->
		{a} | toList q | splice{(6-#q):0});    --gives all divisors of degree d with given a and b_1 >= .. >= b_6 >= 0
	    degreeList = select(degreeList, L -> L#0 >= L#1+L#2+L#3); --normalized
	   apply(degreeList, L-> divisor(L,X))
    )    
)
smoothDivisors (ZZ,QuarticSurfaceRational) := (d,X) -> (
    --generates some smooth divisors on the QuarticSurfaceRational
    --not uniquely represented 
    flatten for a from max(d-2,ceiling(d/4)) to d+2 list (
	    degreeList := apply(select(partitions(2*a+d-4),p->#p<=8),q ->
		{a,a-d+2} | toList q | splice{(8-#q):0});
	    degreeList = select(degreeList, L -> (L#0 >= L#1+L#2+L#3)
		and binomial(L#0+2,2) > sum drop(L,1)); 
	    --We need unique representation criterion
	    Ld := apply(degreeList, L-> divisor(L,X));
    	    select(Ld, D -> genus D >= 0 and D.Coordinate != X.HyperplaneClass) 
	    --We need numerical criterion of smoothness
    )     
)
smoothDivisors (ZZ,ZZ,Ring) := (d,g,R) -> (
    --generates smooth divisors of degree d and genus g
    L := {};
    if g > floor(1/4*d^2-d+1) then return {}; 
    --By Castelnuovo's theorem

    --check on quadric surface
    deg := select(splice{1..d},a-> (a-1)*(d-a-1) == g);
    if deg != {} then (
    	L = L | {divisor({first deg,d-first deg},quadricSurface(R))};	 
    );

    if g > 1/6*d*(d-3)+1 then return L;
    --by Halphen's theorem
    
    L = L | smoothDivisors(d,cubicSurface(R)) | 
                 smoothDivisors(d,quarticSurfaceRational(R));
    select(L,D -> genus D == g)
)
smoothDivisors (ZZ,ZZ) := (d,g) -> (
    --generates smooth divisors of degree d and genus g   
    x := getSymbol "x";
    R := (ZZ/32003)(monoid[x_0..x_3]);
    smoothDivisors(d,g,R)
)
curve (ZZ,ZZ,Ring) := (d,g,R) -> (
    --generates a random curve of degree d and genus g in a given ring
    if 2*g == (d-1)*(d-2) then return ideal(random(1,R),random(d,R));
    L := smoothDivisors(d,g,R);
    if L != {}  then return curve first random L
    else print "No smooth curve with this degree and genus exists!";   
)
curve (ZZ,ZZ) := (d,g) -> (
    x := getSymbol "x";
    R := (ZZ/32003)(monoid[x_0..x_3]);
    if 2*g == (d-1)*(d-2) then return ideal(random(1,R),random(d,R));
    curve(d,g,R)   
)
dgTable = method()
dgTable List := L ->(
    --Takes a list of AbstractDivisors or RealizedDivisors
    --returns a (degree, genus) occurrence matrix    
    Ldg := apply(L, C -> (lift(degree C,ZZ), lift(genus C,ZZ)));
    dmax := max apply(Ldg,dg->first dg);
    dmin := min apply(Ldg,dg->first dg);
    gmax := max apply(Ldg,dg->last dg);
    gmin := min apply(Ldg,dg->last dg);
    M := mutableMatrix map(ZZ^(gmax-gmin+1),ZZ^(dmax-dmin+1),0);
    for dg in Ldg do (
	j := first dg - dmin;
	i := gmax - last dg;
    	M_(i,j) = M_(i,j)+1;		
    );
    yaxis := reverse splice{gmin..gmax};
    xaxis := toString splice{dmin..dmax};
    xaxis = replace(" ([0-9]),", "  \\1", replace("\\{", "g/d| ", replace("\\}", "", xaxis)));
    S := toString(transpose matrix{yaxis} | matrix M) | "\n";
    S = replace("matrix ", "", replace("\\{\\{", "{", replace("\\}\\}", "", S)));
    S = replace("\\}, ","\n", S);
    S = replace("\\{([0-9]+)", "\\1 |", S);
    S = replace(" ([0-9]),", "  \\1,", S);
    S = replace(",","",S);
    S = replace("\n([0-9]) ", "\n \\1 ", S);
    S = replace(" ([0-9])\n", "  \\1\n", S);
    S = replace("  0", "  .", S);
    S = net substring(S, 0, #S-1);
    xaxisbar := "---+";
    for i from 4 to width S do xaxisbar = xaxisbar | "-";
    S || xaxisbar || replace(",", "", xaxis)
    --transpose matrix{yaxis} | (matrix M || matrix{xaxis})
)

--VIII.Documentations

beginDocumentation()
--Headline
document { 
Key => SpaceCurves,
Headline => "generation of space curves",
PARA{
EM "SpaceCurves", " is a package dedicated to generation of curves in P3. 
The 1.0 version of the package generates smooth curves of a given degree and genus, 
ACM curves of a given Hilbert function as well as
minimal curves in biliaison class with a given Rao-module."
},
PARA { 
"The method ", TO "smoothDivisors", " produces a list of ",
TO "Divisor", " of a given degree on a given surface.
The method ", TO (curve,Divisor),
 " produces a random 
curve in a given divisor class.
For a given degree, as one varies the input surface from  a smooth quadric, a smooth cubic 
and a rational quartic surface with a double line, 
all obtainable genus of a smooth curve will occur (save that of a plane curve). 
The methods to create the said surfaces are: ", TO (quadricSurface,Ring), ", ",
TO (cubicSurface,Ring), " and ", TO (quarticSurfaceRational,Ring), 
"."
},
EXAMPLE {
    "R = ZZ/101[x,y,z,w];",
    "X = quadricSurface(R);",
    "Y = cubicSurface(R);",
    "Z = quarticSurfaceRational(R);",
    "LD = smoothDivisors(4,X) | smoothDivisors(4,Y) | smoothDivisors(4,Z)",
    "LC = apply(LD,D->curve D);"    
},
PARA {
" The method ", TO (curve,ZZ,ZZ), 
" generates a random curve with the specified degree and genus." 
},
EXAMPLE {
    "C = curve(5,2);",
    "degree C, genus C, isPrime C, isSmooth C"    
},
PARA { 
"The postulation character of a curve is defined to be the negative of the 
third numerical difference of its Hilbert function, and gives equivalent 
information as the Hilbert function. The method function ", TO (positiveChars,ZZ), 
" generates all possible postulation characters of an ACM curve of a given degree. The method ",
TO (allACMBetti,List), " generates all Betti tables of ACM curves with a given 
postulation character. The method function ", TO (degreeMatrix, BettiTally), " converts the
Betti table of an ACM curve to its Hilbert-Burch degree matrix. Finally ", 
TO (randomDeterminantalIdeal,Ring,Matrix), " generates a random determinantal ideal
in a given ring with specified degree format. Combining all three methods we can generate 
ACM curves of any degree d, exhausting all possibilities of Betti tables." 
},
EXAMPLE {
    "G = positiveChars(8);",
    "L = G / allACMBetti;",
    "netList L",
    "apply(L, g 
    -> apply(g, B -> randomDeterminantalIdeal(ZZ/101[x,y,z,w], degreeMatrix B)));"    
},
PARA {
"The method ", TO (minimalCurve,Module), " produces
a minimal curve in the biliaison class specified by the finite length module.
The method ", TO (minimalCurve,Ideal), " produces a random minimal curve in the biliaison
class of a given curve." 
},
EXAMPLE {
    "I = monomialCurveIdeal(R,{1,3,4})",
    "M = raoModule(I)",
    "J = minimalCurve M;",
    "betti res J"    
},
PARA{},
SUBSECTION "Surfaces",  
UL{  TO    "QuadricSurface",
     TO	   "IntersectionPairing",
     TO	   "CanonicalClass",
     TO	   "HyperplaneClass",
     TO	   "quadricSurface",
     TO	   "CubicSurface",
     TO	   "BlowUpPoints",
     TO	   "MapToP3",
     TO	   "cubicSurface",
     TO	   "QuarticSurfaceRational",
     TO	   "quarticSurfaceRational"
     
},
PARA{},
SUBSECTION "Divisors",  
UL{ TO	  "Divisor",
    TO	  "Coordinate",
    TO	  "Surface",
    TO	  "divisor",
    TO	  "surface"
},
PARA{},
SUBSECTION "Curves",  
UL{ 
    TO	  "Curve",
    TO	  "curve",
    TO	  "isSmooth"
},
PARA{},
SUBSECTION "Minimal curves",  
UL{   TO "minimalCurve",
      TO "raoModule"
},
PARA{},
SUBSECTION "ACM Curves",  
UL{ 
    TO	  "positiveChars",
    TO	  "generalACMBetti",
    TO	  "specializeACMBetti",
    TO	  "allACMBetti",
    TO	  "isACMBetti",
    TO	  "isSmoothACMBetti",
    TO	  "degreeMatrix",
    TO 	  "randomDeterminantalIdeal"
}
}
--Surfaces
document{
     Key => QuadricSurface,
     Headline => "type of HashTable",
     "QuadricSurface is a type of HashTable storing information about a smooth quadric 
     surface in P^3. The keys of a QuadricSurface are:
     IntersectionPairing, HyperplaneClass, CanonicalClass, Ideal."
}
document{
    Key => {quadricSurface,(quadricSurface,Ring)},
    Headline => "creates a QuadricSurface",
    {"This function takes a polynomial ring in 4 variables over a field as the 
	coordinate ring of P3 and creates a ", TO "QuadricSurface", " in this ring."},
    SYNOPSIS (   
    	Usage => "Q = quadricSurface(R)",
    	Inputs => { "R" => Ring},
	Outputs => {"Q" => QuadricSurface},
    	EXAMPLE {
       	    "quadricSurface(ZZ/32003[x,y,z,w])"
    	    }
    ),
    SeeAlso => {"cubicSurface","quarticSurfaceRational"}   
}
document{
     Key => CubicSurface,
     Headline => "type of HashTable",
     "CubicSurface is a type of HashTable storing information about a smooth cubic 
     surface in P^3. The keys of a QuadricSurface are:
     IntersectionPairing, HyperplaneClass, CanonicalClass, Ideal, BlowUpPoints, MapToP3."
}
document{
    Key => {cubicSurface,(cubicSurface,Ring)},
    Headline => "creates a cubicSurface",
    {"This function takes a polynomial ring in 4 variables over a field as the coordinate
    ring of P3 and creates a ", TO "CubicSurface", " in this ring. 
    The equation of the cubic surface is computed from the blowup of 6 points in P2, 
     listed in the key BlowUpPoints, along with a rational map P2 -> P3 whose base loci
     is the 6 given points."}, 
    SYNOPSIS (
     	   Usage => "X = quadricSurface(R)",
    	   Inputs => {"R" => Ring},
    	   Outputs => {"X" => CubicSurface},
    	   EXAMPLE {
       	       "cubicSurface(ZZ/32003[x,y,z,w])"
    	       }
    ),
    SeeAlso => {"quadricSurface","quarticSurfaceRational"}   
}
document{
     Key => QuarticSurfaceRational,
     Headline => "type of HashTable",
     "QuarticSurfaceRational is a type of HashTable storing information about a rational quartic surface
     with a double line in P^3. The keys of a QuadricSurface are:
     IntersectionPairing, HyperplaneClass, CanonicalClass, Ideal, BlowUpPoints, MapToP3."
}
document{
    Key => {quarticSurfaceRational,(quarticSurfaceRational,Ring)},
    Headline => "creates a QuarticSurfaceRational", 
    {"This function takes a polynomial ring in 4 variables over a field as the coordinate
    ring of P3 and creates a rational quartic surface with a double line as a ",
     TO "QuarticSurfaceRational", " in this ring. 
    To do this We blow up P2 at 10 points, listed in BlowUpPoints, 
    and map the blownup surface to P3. 
    The map is specified as a rational map P2->P3 given in MapToP3."
     },
    SYNOPSIS (  
    	Usage => "X = quadricSurface(R)",
    	Inputs => {"R" => Ring},
    	Outputs => {"X" => QuarticSurfaceRational},
    	EXAMPLE {
       	    "X = quarticSurfaceRational(ZZ/32003[x,y,z,w])"
    	    }
    ),
    SeeAlso => {"quadricSurface","cubicSurface"}   
}
document {
    Key => HyperplaneClass,
    Headline => "key for QuadricSurface, CubicSurface and QuarticSurfaceRational",
    {"The symbol ", TT "HyperplaneClass", " is a key for ", TO "QuadricSurface", 
	", ", TO "CubicSurface",
	" and ", TO "QuarticSurfaceRational", ". It stores a ",
    TT "List", " encoding the coordinates of a globally generated divisor used to
    map the surface to P^3."
    },
    SeeAlso => {"CanonicalClass","IntersectionPairing","BlowUpPoints","MapToP3"}    
}
document {
    Key => CanonicalClass,
    Headline => "key for QuadricSurface, CubicSurface and QuarticSurfaceRational",
    {"The symbol ", TT "CanonicalClass", " is a key for ", TO "QuadricSurface", 
	", ", TO "CubicSurface",
	" and ", TO "QuarticSurfaceRational", ". It stores a ",
    TT "List", " encoding the coordinates of the canonical divisor."	
    },
    SeeAlso => {"HyperplaneClass","IntersectionPairing","BlowUpPoints","MapToP3"}    
}
document {
    Key => IntersectionPairing,
    Headline => "key for QuadricSurface, CubicSurface and QuarticSurfaceRational",
    {"The symbol ", TT "IntersectionPairing", " is a key for ", TO "QuadricSurface", 
	", ", TO "CubicSurface",
	" and ", TO "QuarticSurfaceRational", ". It stores a ",
    TT "Matrix", " encoding the intersection pairing of the surface."
    },
    SeeAlso => {"CanonicalClass","HyperplaneClass","BlowUpPoints","MapToP3"}    
}
document {
    Key => BlowUpPoints,
    Headline => "key for CubicSurface and QuarticSurfaceRational",
    {"The symbol ", TT "BlowUpPoints", " is a key for ", TO "CubicSurface",
	" and ", TO "QuarticSurfaceRational", ". It stores a 
	list of ideals encoding the points on P2 whose blowup
    produces the surface."
    },
    SeeAlso => {"MapToP3"}    
}
document {
    Key => MapToP3,
    Headline => "key for CubicSurface and QuarticSurfaceRational",
    {"The symbol ", TT "MapToP3", " is a key for ", TO "CubicSurface",
	" and ", TO "QuarticSurfaceRational", ". It stores a ",
    TT "map", " specifying a rational map from P^2 to P^3 obtained
    by the restriction of the embedding of the blown-up surface to P^3."
    },
    SeeAlso => {"BlowUpPoints"}    
}
--Divisors
document {
    Key => Divisor,
    Headline => "type of HashTable",
    {TO "Divisor", " is a type of ", TT "HashTable", 
	" that specifies a divisor class
	on a surface. The keys are ", TO "Coordinate", " and ",
	TO "Surface", "."
    },
    SeeAlso => {"Curve"}
}
document {
    Key => Coordinate,
    Headline => "key of Divisor",
    {
    TO "Coordinate", " is a key of ", TO "Divisor", " storing a ",
    TT "List", " encoding the coordinates of the divisor class."	
    }
}
document {
    Key => Surface,
    Headline => "key of Divisor",
    {
    TO "Surface", " is a key of ", TO "Divisor", " storing a ",
    TT "HashTable", " which can be ", TO "QuadricSurface", ", ",
    TO "CubicSurface", " or ", TO "QuarticSurfaceRational", "."	
    }
}
document {
    Key => {divisor,(divisor,List,QuadricSurface),
	(divisor,List,CubicSurface),(divisor,List,QuarticSurfaceRational)},
    Headline => "creates a Divisor",
    {"Creates a ", TO "Divisor", " from a given ", TT "List",
	" of coordinates and a surface."
    },
    SYNOPSIS (
    	Usage => "D = divisor(L,X)",
	Inputs => {"L" => List => " of coordinate",
	    "X" => QuadricSurface
	    },
	Outputs => {"D" => Divisor},
	EXAMPLE {
	    "X = quadricSurface(ZZ/101[x,y,z,w]);",
	    "D = divisor({3,2},X)"
	}
    ),
    SYNOPSIS (
       	Usage => "D = divisor(L,X)",
	Inputs => {"L" => List => " of coordinate",
	    "X" => CubicSurface
	    },
	Outputs => {"D" => Divisor},
	EXAMPLE {
	    "X = cubicSurface(ZZ/101[x,y,z,w]);",
	    "D = divisor({3,1,1,1,1,1,1},X)"
	}
    ),
    SYNOPSIS (
	Usage => "D = divisor(L,X)",
	Inputs => {"L" => List => " of coordinate",
	    "X" => QuarticSurfaceRational
	    },
	Outputs => {"D" => Divisor},
	EXAMPLE {
	    "X = quarticSurfaceRational(ZZ/101[x,y,z,w]);",
	    "D = divisor(splice{3,9:1},X)"
	}
    )	       
}
document {
    Key => (divisor,Curve),
    Headline => "extracts the Divisor of a Curve",
    {
    "Outputs the key ", TT "Divisor", " of a ", TT "Curve"	
    },
    EXAMPLE {
    	"C = curve(5,2);",
	"D = divisor C"
    }    
}
document {
    Key => {surface,(surface,Divisor),(surface,Curve)},
    Headline => "the surface key of a Divisor or a Curve",
    {
    "Returns the key ", TO "Surface", " of a ", TO "Divisor", "."
    },
    EXAMPLE {
	"C = curve(5,2);",
	"D = divisor C",
	"Q = surface D"
    }   
}
document {
    Key => Curve,
    Headline => "type of HashTable",
    {
    TT "Curve", " is a type of ", TT "HashTable", " that
    stores information about a curve. It has keys ",
    TT "Divisor", " and ", TT "Ideal", "."	
    }    
}
document {
    Key => {curve,(curve,Divisor),(curve,ZZ,ZZ),(curve,ZZ,ZZ,Ring)},
    Headline => "generates a random curve",
    {
    "The method ", TT "curve", " generates a random curve with given input."	
    },
    SYNOPSIS (
    	Usage => "C = curve(D)",
	Inputs => {"D" => Divisor},
	Outputs => {"C" => Curve},
	EXAMPLE {
	    "X = quadricSurface(ZZ/101[x_0..x_3]);",
	    "D = divisor({1,2},X);",
	    "C = curve D"    
	}	
    ),
    SYNOPSIS (
    	Usage => "C = curve(d,g)",
    	Inputs => {"d" => ZZ => "degree", "g" => ZZ => "genus"},
    	Outputs => {"C" =>  Curve},
    	EXAMPLE {
    	    "I = curve(5,2);",
	    "degree I, genus I"	
    	    }
    ),
    SYNOPSIS (
    	Usage => "C = curve(d,g,R)",
    	Inputs => {"d" => ZZ => "degree",
	    "g" => ZZ => "genus",
	    "R" => Ring => "ambient ring of P^3"},
    	Outputs => {"C" => Curve}	
    )     
}
document {
    Key => {isSmooth,(isSmooth,Ideal),(isSmooth, Curve)},
    Headline => "checks smoothness of an ideal or of a Curve",
    {
    "The method ", TT "isSmooth", " uses Jacobian criterion to check the smoothness of
     an ideal or a Curve." 	
    },
    SYNOPSIS (
    	Usage => "B = isSmooth(I)",
	Inputs => {"I"=>Ideal},
	Outputs => {"B" => Boolean}	
    ),
    SYNOPSIS (
    	Usage => "B = isSmooth(C)",
	Inputs => {"C"=>Curve},
	Outputs => {"B"=>Boolean},
	EXAMPLE {
	    "C = curve(5,2);",
	    "isSmooth C"    
	}	
    )  
}
document {
    Key => (degree, Curve),
    Headline => "computes the degree of a Curve",
    {"Computes the degree of a ", TT "Curve", "."},
    EXAMPLE {
    	"C = curve(5,2);",
	"degree C"	
    }    
}
document {
    Key => (genus, Curve),
    Headline => "computes the genus of a Curve",
    {"Computes the genus of a ", TT "Curve", "."},
    EXAMPLE {
    	"C = curve(5,2);",
	"genus C"	
    }    
}
document {
    Key => (degree, Divisor),
    Headline => "computes the degree of a Divisor",
    {"Computes the degree of a ", TT "Divisor", " using ",
	TT "IntersectionPairing", "."},
    EXAMPLE {
    	"R = ZZ/101[x,y,z,w];",
	"Q = quadricSurface(R);",
	"D = divisor({3,4},Q);",
	"degree D"	
    }    
}
document {
    Key => (genus, Divisor),
    Headline => "computes the genus of a Divisor",
    {"Computes the genus of a ", TT "Divisor", " using the adjunction formula."},
    EXAMPLE {
    	"R = ZZ/101[x,y,z,w];",
	"Q = quadricSurface(R);",
	"D = divisor({3,4},Q);",
	"genus D"	
    }     
}
document {
    Key => (symbol *,Divisor,Divisor),
    Headline => "intersection number of two Divisors",
    {
    "Returns the intersection number of two ", TT "Divisors", "."
    },
    EXAMPLE {
    	"Q = quadricSurface(ZZ/101[x,y,z,w]);",
    	"C = divisor({1,0},Q);",
	"D = divisor({0,1},Q);",
	"C*D"		
    }   
}
document {
    Key => (symbol +,Divisor,Divisor),
    Headline => "sum of two Divisors",
    {
    "Returns the sum of two ", TT "Divisors", "."
    } ,
    EXAMPLE {
    	"Q = quadricSurface(ZZ/101[x,y,z,w]);",
    	"C = divisor({1,0},Q);",
	"D = divisor({0,1},Q);",
	"C+D"		
    }  
}
document {
    Key => (symbol -,Divisor,Divisor),
    Headline => "difference of two Divisors",
    {
    "Returns the difference of two ", TT "Divisors", "."
    },
    EXAMPLE {
    	"Q = quadricSurface(ZZ/101[x,y,z,w]);",
    	"C = divisor({1,0},Q);",
	"D = divisor({0,1},Q);",
	"C-D"		
    }   
}
document {
    Key => (symbol *,ZZ,Divisor),
    Headline => "multiply a Divisor by an integer",
    {
    "Multiplies a ", TT "Divisors", " by an integer."
    },
    EXAMPLE {
    	"Q = quadricSurface(ZZ/101[x,y,z,w]);",
    	"C = divisor({1,0},Q);",
	"3*C"		
    }  
}
document {
    Key => {(ideal,QuadricSurface),(ideal,CubicSurface),(ideal,QuarticSurfaceRational)},
    Headline => "extracts the ideal of a surface",
    {
    "Extracts the key ", TT "Ideal", " of a ", TO "QuadricSurface", ", a ",
    TO "CubicSurface", " or a ", TO "QuarticSurfaceRational", "."
    },
    EXAMPLE {
    	"X = quadricSurface(ZZ/101[x,y,z,w]);",
	"ideal X"    
    }   
}
document {
    Key => {smoothDivisors,(smoothDivisors,ZZ,CubicSurface),
	(smoothDivisors,ZZ,QuadricSurface), (smoothDivisors,ZZ,QuarticSurfaceRational)},
    Headline => "produces a list of smooth divisors of a given degree on a surface",
    {
    "Produces a ", TT "List", " of ", TO "Divisor", " of a given degree on a 
    given surface. On the ", TO "CubicSurface", " and the ", TO "QuarticSurfaceRational",
    " the list is not exhaustive, but will include a divisor for each possible genus
    that a smooth curve can obtain."	
    },
    SYNOPSIS (
    Usage => "L = smoothDiviors(d,X)",
    Inputs => {"d" => ZZ => "degree", "X"=>QuadricSurface},
    Outputs => {"L"=> List => "of Divisors"},
    	EXAMPLE {
	    "X = quadricSurface(ZZ/101[x,y,z,w]);",
	    "L = smoothDivisors(5,X)"    
        }
    ),
    SYNOPSIS (
    Usage => "L = smoothDiviors(d,X)",
    Inputs => {"d" => ZZ =>"degree", "X"=>CubicSurface},
    Outputs => {"L"=>List => "of Divisors"},
    	EXAMPLE {
	    "X = cubicSurface(ZZ/101[x,y,z,w]);",
	    "L = smoothDivisors(5,X)"    
        }
    ),
    SYNOPSIS (
    Usage => "L = smoothDiviors(d,X)",
    Inputs => {"d" => ZZ =>"degree", "X"=>QuarticSurfaceRational},
    Outputs => {"L"=>List => "of Divisors"},
        EXAMPLE {
	    "X = quarticSurfaceRational(ZZ/101[x,y,z,w]);",
	    "L = smoothDivisors(5,X)"    
        }
    )    
}
document {
    Key => {(smoothDivisors,ZZ,ZZ),(smoothDivisors,ZZ,ZZ,Ring)},
    Headline => "produces a list of smooth divisors of given degree and genus",
    {"This method produces a list of Divisors of a given degree and genus, 
	one may also pass Ring as the ambient ring of P3."},
    Usage => "L = smoothdivisors(d,g)",
    Inputs => {"d" => ZZ => "degree", "g" => ZZ => "genus"},
    EXAMPLE {
    	"smoothDivisors(5,2)",
	"smoothDivisors(5,2,ZZ/101[x,y,z,w])"	
    }    
}
document {
    Key => (isPrime,Curve),
    Headline => "checks if the ideal of a Curve is prime",
    Usage => "B = isPrime(C)",
    Inputs => {"C" => Curve},
    Outputs => {"B" => Boolean},
    EXAMPLE {
    	"C = curve(5,2);",
	"isPrime C"       
    }    
}
document {
    Key => (ideal,Curve),
    Headline => "extracts the ideal of a Curve",
    {
    "Extracts the key ", TT "Ideal", " of a ", TO "Curve", "."	
    },
    EXAMPLE {
    	"C = curve(5,2);",
	"ideal C"	
    }    
}
document {
    Key => {dgTable,(dgTable,List)},
    Headline => "prints the table of (degree,genus) pairs",
    {
    TT "dgTable", " prints the table of (degree,genus) pairs, where the horizontal
    axis is the degree and the vertical is the genus. The input can be a ", TO "List",
    " of ", TO "Divisor", ", ", TO "Curve", ", ", TT "PostulationChar", " or ",
    TO "Ideal", "."
    } 
}
document {
  Key => {raoModule,(raoModule,Ideal),(raoModule, Curve)},
  Headline => "computes the Rao module of a curve",
  {"Given I the homogeneous ideal of a pure dimension one subscheme
       in P^3, the function computes its Rao module."
  },    
  Usage => "M = raoModule I",
  Inputs => {"I" => Ideal => "of a pure dimension one subscheme"},
  Outputs => {"M" =>  Module},
  EXAMPLE {
      "R = ZZ/101[x,y,z,w];",
      "I = monomialCurveIdeal(R,{1,3,4});",
      "M = raoModule I"    
  }
}
document {
    Key => {positiveChars,(positiveChars,ZZ),(positiveChars,ZZ,ZZ)},
    Headline => "generates all positive characters of a given degree",
    {"produces all positive characters of a given degree, equivalently, these are exactly
	 all the possible postulation characters of an ACM curve in P3 of that degree"	
    },
    SYNOPSIS (
    	Usage => "L = positiveChars(d)",
    	Inputs => {"d" => ZZ => "degree"},
    	Outputs => {"L" => List},
    	EXAMPLE {
    	"positiveChars(5)"	
    	}
    ),
    SYNOPSIS (
    	Usage => "L = positiveChars(d,s)",
    	Inputs => {"d" => ZZ => "degree", "s" => ZZ => "least degree surface"},
    	Outputs => {"L" => List},
    	EXAMPLE {
    	"positiveChars(5)"	
    	}
    )    
}
document {
    Key => {isACMBetti, (isACMBetti, BettiTally)},
    Headline => "checks whether a Betti table is that of an ACM curve",
    {
    	"Given a BettiTally, returns true or false depending on whether there 
	exists an ACM curve in P3 having that Betti table."	
    },
    Usage => "b = isACMBetti B",
    Inputs => {"B" => BettiTally},
    Outputs => {"b" => Boolean},
    EXAMPLE {
    	"B = generalACMBetti {-1,-1,2}",
	"isACMBetti B"	
    }   
}
document {
    Key => {isSmoothACMBetti, (isSmoothACMBetti, BettiTally)},
    Headline => "checks whether a Betti table is that of a smooth ACM curve",
    {
    	"Given a BettiTally, returns true or false depending on whether there 
	exists a smooth ACM curve in P3 having that Betti table."	
    },
    Usage => "b = isSmoothACMBetti B",
    Inputs => {"B" => BettiTally},
    Outputs => {"b" => Boolean},
    EXAMPLE {
    	"B = generalACMBetti {-1,-1,2}",
	"isSmoothACMBetti B"	
    }   
}
document {
    Key => {generalACMBetti, (generalACMBetti,List)},
    Headline => "the most general Betti table of an ACM curve with a given Hilbert function",
    { "Given a positive character, it outputs the Betti table of a general ACM curve that 
	has this character as its postulation character."	
    },
    Usage => "B = generalACMBetti gamma",
    Inputs => {"gamma" => List => "positive character"},
    Outputs => {"B" => BettiTally},
    EXAMPLE {
    	"generalACMBetti {-1,-1,2}"	
    }    
}
document {
    Key => {specializeACMBetti, (specializeACMBetti,BettiTally)},
    Headline => "lists all 1-specialization of a Betti table of an ACM curve",
    {
    	"Given a Betti table of an ACM curve, produces all possible Betti tables of 
	ACM curves with one more generator and syzygy of equal degree than the given one."	
    },
    Usage => "L = specializeACM B",
    Inputs => {"B" => BettiTally},
    Outputs => {"L" => List => "of BettiTally"},
    EXAMPLE {
	"B = generalACMBetti {-1,-1,-1,2,1}",
    	"specializeACMBetti B"	
    }    
}
document {
    Key => {allACMBetti, (allACMBetti,List)},
    Headline => "lists all Betti tables of ACM curves with a given Hilbert function",
    { "Given a positive character, it outputs all possible Betti tables of
	ACM curves that have this character as its postulation character."	
    },
    Usage => "L = allACMBetti gamma",
    Inputs => {"gamma" => List => "positive character"},
    Outputs => {"L" => List => "of BettiTally"},
    EXAMPLE {
    	"allACMBetti {-1,-1,-1,2,1}"	
    }    
}
document {
    Key => {degreeMatrix, (degreeMatrix,BettiTally)},
    Headline => "computes the Hilbert-Burch degree matrix from a Betti table of ACM curves",
    {
      "Given the Betti table of an ACM curve, this function returns the
       Hilbert-Burch degree matrix."	
    },
    Usage => "M = degreeMatrix B",
    Inputs => {"B" => BettiTally => "of an ACM curve"},
    Outputs => {"M" => Matrix},
    EXAMPLE {
	"B = generalACMBetti {-1,-1,2}",
	"degreeMatrix B"
    }    
}
document {
    Key => {randomDeterminantalIdeal, (randomDeterminantalIdeal,Ring,Matrix)},
    Headline => "produces a random determinantal ideal",
    {
    	"Given a ring and a degree matrix, we produce a random determinantal ideal
	with forms in prescribed dergee. Forms of non-positive degrees are taken as
	0 to ensure minimality of presentation."	
    },
    Usage => "I = randomDeterminantalIdeal(R,M)",
    Inputs => {"R" => Ring, "M" => Matrix => "of integers"},
    Outputs => {"I" => Ideal => "in the ring R"},
    EXAMPLE {
	"randomDeterminantalIdeal(ZZ/101[x,y,z],matrix{{1,1},{1,1},{1,1}})"	
    }    
}
document {
    Key => {minimalCurve, (minimalCurve, Module),
      (minimalCurve, Ideal),(minimalCurve,Curve)},
    Headline => "generates a minimal curve in the biliaison class",   
    "A finite length module M determines a unique
    biliaison class. Curves of minimal degrees in this class are called minimal curves.
    Given the ideal of a curve J,
     this function generates a random minimal curve
    in the biliaison class of J. 
    Given a finite length module M,
     this function generates a random minimal curve
    in the biliaison class specified by M.",
    SYNOPSIS (
  	Usage => "I = minimalCurve(M)",
    	Inputs => {"M" => Module},
    	Outputs => {"I" => Ideal},
    	EXAMPLE {
	"R = ZZ/101[x,y,z,w];",
    	"M = coker vars R;",
	"I = minimalCurve M"	
    	}	 
    ),
    SYNOPSIS (
  	Usage => "I = minimalCurve(J) ",
    	Inputs => {"J" => Ideal => "of a pure dimension one subscheme"},
    	Outputs => {"I" => Ideal => "of a minimal curve in the biliaison class"},
    	EXAMPLE {
    	"R = ZZ/101[x,y,z,w];",
	"J = monomialCurveIdeal(R,{1,3,4});",
	"I = minimalCurve J"	
    	}	 
    ),
    SeeAlso => {"minimalCurveBetti"}
}
document {
    Key => {minimalCurveBetti, (minimalCurveBetti, Module),
      (minimalCurveBetti, Ideal),(minimalCurveBetti,Curve)},
    Headline => "computes the Betti diagram of the minimal curve",   
    "A finite length module M determines a unique
    biliaison class. Curves of minimal degrees in this class are called minimal curves.
    Given the ideal of a curve J,
     this function returns the Betti tally of any minimal curve of J. 
    Given a finite length module M,
     this function returns the Betti tally of any minimal curve 
     in the biliaison class specified by M.",
    SYNOPSIS (
  	Usage => "T = minimalCurveBetti(M)",
    	Inputs => {"M" => Module},
    	Outputs => {"T" => BettiTally},
    	EXAMPLE {
	"R = ZZ/101[x,y,z,w];",
    	"M = coker vars R;",
	"I = minimalCurveBetti M"	
    	}	 
    ),
    SYNOPSIS (
  	Usage => "T = minimalCurve(J) ",
    	Inputs => {"J" => Ideal => "of a pure dimension one subscheme"},
    	Outputs => {"T" => BettiTally => "of a minimal curve in the biliaison class"},
    	EXAMPLE {
    	"R = ZZ/101[x,y,z,w];",
	"J = monomialCurveIdeal(R,{1,3,4});",
	"I = minimalCurveBetti J"	
    	}	 
    ),
    SeeAlso => {"degree(BettiTally)"} 
}
document {
    Key => (net,Curve),
    Headline => "displays the ideal of the curve"     
}
document {
    Key => (net,CubicSurface),
    Headline => "displays the ideal of the CubicSurface"    
}
document {
    Key => (net,QuadricSurface),
    Headline => "displays the ideal of the QuadricSurface"    
}
document {
    Key => (net, QuarticSurfaceRational),
    Headline => "displays the ideal of the QuarticSurfaceRational"    
}
document {
    Key => (net, Divisor),
    Headline => "displays the coordinates of the Divisor"    
}

end



-------------------------------------------------------------------------------------
restart
uninstallPackage "SpaceCurves"
installPackage "SpaceCurves"
viewHelp "SpaceCurves"


restart
needsPackage "SpaceCurves"
