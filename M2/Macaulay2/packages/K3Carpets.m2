newPackage(
	"K3Carpets",
    	Version => "0.1", 
    	Date => "November 17, 2016",
    	Authors => {{Name => "David Eisenbud, Frank-Olaf Schreyer", 
		  Email => "de@msri.org", 
		  HomePage => "http://www.msri.org/~de"}},
    	Headline => "K3 double structure on scrolls",
    	DebuggingMode => true,
	PackageExports => {"CompleteIntersectionResolutions"}
    	)

export {
    "carpet",
    "canonicalCarpet",
    "homotopyRanks",
    "canonicalHomotopies",
    "FineGrading",
    "Scrolls",
    "carpet1",
    "gorensteinDouble"
    }


carpet = method(Options =>{Characteristic => 32003,FineGrading=>false,Scrolls =>false})

carpet(ZZ,ZZ,Matrix) := opts -> (a1,a2,m) ->(
    --matrix m should be 2 x (a1+a2)
    --both a1,a2>=1
    --we'll always divide m into two parts, with the smaller part of size 2 x a coming "first"
    --check for bad entries:
    if a1+a2>numcols m then error"sum of the integers must be <= numcols of matrix";
    a := min(a1,a2);b := max(a1,a2);
    if a<1 then error("both integers should be >=1");
    
    xmat := m_{0..a-1};
    ymat := m_{a..a+b-1};
    if b == 1 then return ideal ((det m)^2);
    if a==1 then(  -- replace m by a new matrix, where, in effect, a1 = 2
    	xmat = map(target xmat,,matrix{{m_(0,0)^2,m_(0,0)*m_(1,0)},{m_(0,0)*m_(1,0),m_(1,0)^2}});
	a = 2);
    Ix := minors(2,xmat);
    Iy := minors(2,ymat);
    Imixed := ideal flatten apply(a-1, 
	    i-> apply(b-1,
		j->(det (xmat_{i}|ymat_{j+1})-det(xmat_{i+1}|ymat_{j}))
		));
    Ix+Iy+Imixed)

carpet(ZZ,ZZ) := opts -> (a1,a2) ->(
    --this version makes an appropriate ring and matrix, and then calls carpet(a,b,matrix)
    if opts.Characteristic == 0 then kk := QQ else kk = ZZ/opts.Characteristic;
    x := symbol x; y:=symbol y;
    a := min(a1,a2);
    b := max(a1,a2);
    if opts.FineGrading == false then
    S := kk[x_0..x_a, y_0..y_b] else (
    degreeString := apply(a+1, i->{1,0,i,a-i})|apply(b+1, i->{0,1,i,b-i});
    S = kk[x_0..x_a, y_0..y_b, Degrees=>degreeString]);
    if opts.FineGrading == false then (
	xmat := map(S^2, S^{a:-1}, (i,j) -> x_(i+j));
        ymat := map(S^2, S^{b:-1}, (i,j) -> y_(i+j))
	)
    else(	
        xmat = map(S^{{0,0,0,0},{0,0,1,-1}}, S^(apply(a,j->{ -1,0,-j,-a+j})), (i,j) -> x_(i+j));
        ymat = map(S^{{0,0,0,0},{0,0,1,-1}}, S^(apply(b,j->{ 0,-1,-j,-b+j})), (i,j) -> y_(i+j))
	);

    if b==1 then return ideal ((det(xmat|ymat))^2) -- in this case a == 1 as well
    else if a ==1 then (
	if opts.FineGrading == false then
    	xmat = map(S^2,S^{2:-2},(i,j)->x_i*x_j)
        else
        xmat = map(S^{{0,0,0,0},{0,0,1,-1}}, 
	           S^{{ -1,0,0,-2},{ -1,0,-1,-1}}, 
		   matrix{{x_0^2,x_0*x_1},{x_0*x_1,x_1^2}});
    	a = 2);

    	mat := xmat|ymat;
	if opts.Scrolls == false then return carpet(a,b,mat) else (carpet(a,b,mat),xmat,ymat)
	)        

///
restart
loadPackage "K3Carpets"
S = ZZ/101[x_0..x_9]
mat = genericMatrix(S,x_0,2,5)
betti res carpet(1,3,mat)
betti res carpet(1,3)
netList flatten apply(3, a-> apply(4,i->(betti(res carpet(a+1,a+1+i,Characteristic => 2,FineGrading=>true), Weights =>{1,1,0,0}))))
carpet(1,1)
betti(res canonicalCarpet(7,3,Characteristic => 2,FineGrading=>true), Weights =>{1,1,0,0})
--
restart
loadPackage "K3Carpets"
I = carpet(1,5,FineGrading=>true)
I = carpet(1,3,FineGrading=>false)
betti (res I, Weights =>{1,1,0,0})
isHomogeneous I
degrees  S
isHomogeneous mat
degrees target mat
degrees source mat
mat
--
///

--A different indexing, by genus and Clifford index (Cliff <= (g-1)//2))
canonicalCarpet = method(Options=>{Characteristic=>32003,FineGrading => false,Scrolls=>false})
canonicalCarpet(ZZ,ZZ) := opts -> (gen,cliff) -> 
     carpet(gen-cliff-1, cliff,Characteristic => opts.Characteristic, FineGrading => opts.FineGrading,Scrolls=>opts.Scrolls)

--Here's a structural approach that instead takes the kernel of the unique map of mainimal degree
--from the ideal of the scroll to the canonical module of the scroll. This code produces
--Gorenstein double structures on ACM varieties more generally. 
--computationally, the bare hands approach of carpet is much faster.
gorensteinDouble = method()
gorensteinDouble Ideal := I -> (
    --the script assumes that the "first" map I --> omega will be a surjection of the right degree
    c := codim I;
    F := res(I, LengthLimit => c);
    omega := coker transpose F.dd_c;
    ideal kernel (homomorphism (Hom(module I, omega))_{0})
    )

carpet1 = method(Options =>{Characteristic => 32003})
carpet1(ZZ,ZZ) := opts -> (a1,a2) ->(
    if opts.Characteristic == 0 then kk := QQ else
    kk = ZZ/opts.Characteristic;
    x := symbol x; y:=symbol y;
    S := kk[x_0..x_a1, y_0..y_a2];
    xmat := map(S^2, S^{a1:-1}, (i,j) -> x_(i+j));
    ymat := map(S^2, S^{a2:-1}, (i,j) -> y_(i+j));
    mat := xmat|ymat;
    I := minors(2, mat);
    gorensteinDouble I
    )

canonicalHomotopies = method(Options=>{Characteristic=>32003,FineGrading=>false})
--note: returns the pair: the resolution F of the canonical Carpet
--and the function that used to be called h0 such that h0(i,j) is the j-th homotopy 
--with source F_j that corresponds
--to the i-th quadric.
canonicalHomotopies(ZZ,ZZ):= opts -> (g,cliff) -> (
    F := res canonicalCarpet(g,cliff, Characteristic => opts.Characteristic, FineGrading => opts.FineGrading);
    ff := F.dd_1;
    H := makeHomotopies1(ff,F);
    if opts.FineGrading == false then
    h0:= (i,j) -> submatrixByDegrees(H#{i,j},j+3,j+3)
    else
    h0 = (i,j) ->(
	
	dlist := select(flatten degrees H#{i,j}, de->de_0+de_1 == j+3);
	hashTable apply(dlist, de -> (de,submatrixByDegrees(H#{i,j},de, de)))
	);
    (F,h0)
    )
///
restart
loadPackage "K3Carpets"
(F,h0) = canonicalHomotopies(7,3, FineGrading=>true);
degrees F_1
h0(0,2)
degrees F_2
homotopyRanks(7,3,Characteristic=>2)
///
homotopyRanks = method(Options=>{Characteristic=>32003})
homotopyRanks (ZZ,ZZ) := opts-> (g,cliff) ->(
(F,h0) := canonicalHomotopies(g,cliff,
    Characteristic =>opts.Characteristic);
print betti F;
ff := F.dd_1;
netList apply(numcols ff , i->{ff_i, apply(g-2, m->(rank h0(i,m+1)))})
)


beginDocumentation()
///
restart
uninstallPackage"K3Carpets"
installPackage"K3Carpets"
viewHelp K3Carpets
///

doc ///
Key
  K3Carpets
Headline
 The unique Gorenstein double structure on a surface scroll
Description
  Text
   There is a unique surjection from the ideal of a 2-dimensional rational normal scroll (other than the cone
   over a rational normal curve) onto the canonical module of the scroll 
   and the kernel
   of the this map is the ideal of a scheme that looks numerically like a K3 surface: a "K3 carpet".
   (Theorem 1.3 of "Degenerations of K3 surfaces in projective space",
   by Francisco Gallego and B.P. Purnaprajna,
   Trans. Amer. Math. Soc. 349 (1997), no. 6, 2477–2492.)

   The carpet lies on the intersection of the cones over two rational normal curves Ca and Cb
   of degrees a<=b. We write the ideal of Ca as the minors of a 2xa matrix X with entries x_i, i= 0..a,
   and similarly for Cb, with  a 2 x b matrix Y with entries y_j. We write Xi for the ith column of X, and
   similarly for Y.
   
   In the general case, where a,b are both >=2, the additional generators of the ideal of the Carpet are then given by the differences
   det(Xi,Yj)-det(X(i+1),Y(j-1)), or equivalently, by the minors of (Xi+Yj,X(i+1)+Y(j-1), 
       
   (In the case a=1=b the ideal is the square of the determinant of X|Y; if a=1, b>1 then for the mixed minors we replace the 1-column matrix
   by the symmetric quadratic matrix with entries x^2,xy,y^2.

   This package contains two routines for constructing this ideal: "carpet" uses the knowledge of the generators, as above
   (see ????) while "carpet1" calls "gorensteinDouble", computing the ideal from first principles. The first
   is much more efficient. 
   
   The hyperplane section of a K3 carpet is a "canonical ribbon" indexing by genus and clifford index
   of the hyperplane is done in the routine "canonicalCarpet", which calls "carpet".
///


doc ///
   Key
    carpet
    (carpet, ZZ, ZZ)
    (carpet, ZZ, ZZ, Matrix)    
    [carpet, Characteristic]
    [carpet, FineGrading]    
    [carpet, Scrolls]
   Headline
    Ideal of the unique Gorenstein double structure on a 2-dimensional scroll
   Usage
    I = carpet(a1,a2)
    I = carpet(a1,a2,m)    
    (I,xmat,ymat) = carpet(a1,a2,Scrolls=>true)
   Inputs
    a1:ZZ
    a2:ZZ
     a1 and a2 should be positive
    m:Matrix
     a 2xn matrix for some n >=a1+a2
   Outputs
    I:Ideal
   Consequences
    Item
     If no matrix m is present then the script creates a type a1,a2 K3-carpet over a new ring. If m is given,
     then an ideal made from certain minors and sums of minors of m is produced.
     The characteristic is given by the option, defaulting to 32003.
     If the option FineGrading is set to true, then the ideal is returned with the natural ZZ^4 grading
     (the default is FineGrading => false). This last may not work unless the matrix is of scroll type (or
     not given!) If Scrolls=>true, then a sequence of three items is returned, the second
     and third being the smaller and larger scroll matrices.
   Description
    Text
     The routine carpet(a1,a2,m) sets a = min(a1,a2), b = max(a1,a2), and forms
     two matrices from m:
     X:the 2 x a matrix that is the first a cols of m;
     Y:the 2 x b matrix that is the nex b cols of m--that is, cols a1..a1+a2-1 of m;
     Let Ix, Iy be the ideals of 2 x 2 minors of X and Y. If a,b\geq 2,the routine returns
     Ix+Iy+Imixed, where Imixed
     consists of the quadrics "outside minor - inside minor", that is,
     det(X_{\{i\}},Y_{\{j+1\}})-det(X_{\{i+1\}}|Y_{\{j\}}),
     for each pair of (i,i+1), (j,j+1) in the ranges a1 and a2.
     
     If m is usual ideal of the scroll of type (a,b), then carpet(a,b,m) produces the same ideal
     (over a different ring) as carpet(a,b). This is the ideal of the 2-dimensional
     rational normal scroll Scroll(a1,a2) is the ideal of 2 x 2 minors of X|Y.
     The ideal I to be constructed is the ideal of the unique (numerically) K3 scheme that is a double
     structure on the scroll S(a1,a2).
     
     When a,b > 1, the carpet ideal I is the sum Ix+Iy plus
     the ideal Imixed 
     
     When a = b = 1, I is the square of the det of X|Y.

     When a = 1, b>1 (or symmetrically), I is defined as in the case a,b>1, after replacing

     $$X = \begin{pmatrix}
     x_0\\
     x_1
     \end{pmatrix}$$
     
     by the 2 x 2 matrix
     
     $$\begin{pmatrix}
     x_0^2, x_0*x_1\\
     x_0*x_1, x_1^2
     \end{pmatrix}$$

     and changing a to 2.
    Example
     betti res carpet(2,5)
     S = ZZ/101[a..j]
     m = genericMatrix(S,a,2,5)
     I = carpet(2,3,m)
     L = primaryDecomposition I;
     betti res L_0
     betti res L_1
   Caveat
    We require a1,a2 >=1. If a1>a2 then the blocks are reversed, so that the smaller block always comes first.
   SeeAlso
    canonicalCarpet
    carpet1
    gorensteinDouble
///
 
 doc ///
   Key
    canonicalCarpet
    (canonicalCarpet, ZZ, ZZ)
    [canonicalCarpet, Characteristic]
    [canonicalCarpet, FineGrading]    
    [canonicalCarpet, Scrolls]
   Headline
    Carpet of given genus and Clifford index
   Usage
    I = canonicalCarpet(g,cliff)
    (I,xmat,ymat) = canonicalCarpet(g,cliff,Scrolls=>true)
   Inputs
    g:ZZ
     desired genus
    cliff:ZZ
     desired clifford index
   Outputs
    I:Ideal
     ideal of the K3 Carpet of (sectional) genus g, Clifford index cliff
   Description
    Text
     This is just a re-indexing of the carpet script:
     canonicalCarpet(g,cliff) = carpet(g-cliff-1, cliff).
     Here the natural choices for cliff are 1 \leq cliff \leq (g-1)//2.
   SeeAlso
    carpet
///
doc ///
   Key
    canonicalHomotopies
    (canonicalHomotopies, ZZ,ZZ)
    [canonicalHomotopies, Characteristic]
    [canonicalHomotopies, FineGrading]
   Headline
    resolution and homotopies of the canonical carpet
   Usage
    (F,h) = canonicalHomotopies(gen,cliff)    
   Inputs
    gen:ZZ
     genus of the Carpet
    cliff:ZZ
     Clifford index of the carpet
   Outputs
    F:ChainComplex
     Resolution of the Carpet
    h:Function
     values are the homotopies corresponding to the quadrics
   Description
    Text
     h0(i,j) is the homotopy for the i-th quadric acting on F_j,
     the j-th free module in the resolution of the scroll.
    Example
     (F,h) = canonicalHomotopies(6,2)
///
doc ///
   Key
    homotopyRanks
    (homotopyRanks, ZZ, ZZ)
    [homotopyRanks, Characteristic]
   Headline
    compute the ranks of the quadratic homotopies on a carpet
   Usage
    L = homotopyRanks(g,cliff)
   Inputs
    g:ZZ
     genus of the carpet
    cliff:ZZ
     Clifford index of the carpet
   Outputs
    L:Net
     netList showing the ranks
   Description
    Text
     Prints the Betti table of the canonical carpet
     and a list of pairs, with first element a quadric and second element
     the sequence of ranks of the homotopies for that quadric on F_i, for i = 1..g-2.
    Example
     homotopyRanks(7,3)
     homotopyRanks(7,3, Characteristic => 2)     
///


doc ///
   Key
    FineGrading
   Headline
    Option for carpet, canonicalCarpet
   Description
    Text
     The default is FineGrading => false. If the option FineGrading=>true is given, then the 
     ideal returned has the natural ZZ^4 grading, where x_i has degree \{1,0,i,a-i\}\ and 
     y_i has degree \{0,1,i,b-i\}. 
     (Note that after the call carpet(a1,a2) we have a = min(a1,a2), b = max(a1,a2).)
///
doc ///
   Key
    Scrolls
   Headline
    Option for carpet, canonicalCarpet
   Description
    Text
     The default is Scrolls => false. If the option Scrolls=>true is given, then 
     a triple is returned, with the ideal first followed by xmat and ymat, the smaller
     and larger of the scrolls containing the carpet.
///
doc ///
   Key
    carpet1
    (carpet1, ZZ, ZZ)
    [carpet1, Characteristic]
   Headline
    Ideal of the unique Gorenstein double structure on a 2-dimensional scroll
   Usage
    I = carpet1(a1,a2)
   Inputs
    a1:ZZ
    a2:ZZ
   Outputs
    I:Ideal
   Consequences
    Item
     Creates the carpet over a new ring. The characteristic is given by the option, defaulting to 32003.
   Description
    Text
     Creates a scroll and calls the routine gorensteinDouble to create the carpet. Even for modest size examples,
     this is much slower than the script "carpet", but gives a reassuring check that we have the
     righ computation.
   SeeAlso
    gorensteinDouble
///
doc ///
   Key
    gorensteinDouble
    (gorensteinDouble,Ideal)
   Headline
    attempts to produce a Gorenstein double structure J subset I
   Usage
    gorensteinDouble I
   Inputs
    I:Ideal
   Outputs
    J:Ideal
   Description
    Text
     Let S = ring I, and that I is an ideal of codimension c.
     Let F be the S-free resolution of S/I.
     Assuming that S is a polynomial ring and S/I is Cohen-Macaulay, 
     the canonical module of S/I is 
     omega = coker transpose F.dd_c. The script returns the ideal J that is the kernel of the first
     element of Hom(I, omega). In case I is the ideal of a scroll there is a unique
     element of minimal degree, and it represents a surjection, so S/J is Gorenstein.
   SeeAlso
    carpet1
///


TEST///
assert( betti res carpet1(2,5) == (new BettiTally from {(0,{0},0) => 1, (1,{2},2) => 15, (2,{3},3) => 35, (2,{4},4) => 14, (3,{4},4) => 35,
     (3,{5},5) => 35, (4,{5},5) => 14, (4,{6},6) => 35, (5,{7},7) => 15, (6,{9},9) => 1}) )
assert(false == (betti res canonicalCarpet(7,3,Characteristic =>0) == 
	        betti res canonicalCarpet(7,3,Characteristic =>2)))
assert(isHomogeneous carpet(2,4,FineGrading=>true))
assert(isHomogeneous canonicalCarpet(7,3,FineGrading=>true))
///

end--
restart
uninstallPackage "K3Carpets"
installPackage "K3Carpets"
check "K3Carpets"
viewHelp K3Carpets
viewHelp syzygyScheme
code syzygyScheme

