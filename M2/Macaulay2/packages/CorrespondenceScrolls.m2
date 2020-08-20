-*
restart
uninstallPackage"CorrespondenceScrolls"
restart
installPackage"CorrespondenceScrolls"
loadPackage("CorrespondenceScrolls", Reload => true)
check "CorrespondenceScrolls"
viewHelp CorrespondenceScrolls
*-

newPackage(
	"CorrespondenceScrolls",
    	Version => "0.6", 
    	Date => "April 23, 2018, updated June 27, 2020",
    	Authors => {{Name => "David Eisenbud", 
		  Email => "de@msri.org", 
		  HomePage => "http://www.msri.org/~de"},
	          {Name => "Frank-Olaf Schreyer", 
		  Email => "schreyer@math.uni-sb.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer"},
	          {Name =>"Alessio Sammartano",
		   -- Email => "",
		   HomePage => ""}},
    	Headline => "correspondence scrolls",
	PackageImports => { "Elimination" }
	)

export {    
    "hankelMatrix",
    "correspondenceScroll",
    "smallDiagonal",
    "productOfProjectiveSpaces",
    "irrelevantIdeal",
    "schemeInProduct",
    "multiHilbertPolynomial",
    "correspondencePolynomial",
    "carpet",
     --symbols
    "VariableName",
    "CoefficientField"
    }

hankelMatrix = method()
hankelMatrix(Matrix,ZZ,ZZ) := (rowmat,nrows,ncols) ->(
   --makes a Hankel matrix using the first ronum + colnum -1 entries of 
   --the (first) row of rowmat,
   --and having rownum rows, colnum columns, padding with zeros as necessary
   --result is homogeneous as long as the entries of the first row of rowmat all have the same
   --degree.
   if not instance(nrows,ZZ) or not instance(ncols,ZZ) or nrows < 0 or ncols < 0
   then error "expected nonnegative integers";
   S := ring rowmat;
   degs := {ncols:-((degree rowmat_0)_0)};
   map(S^nrows,S^degs,(i,j) ->if i+j<numcols rowmat then rowmat_(0,j+i) else 0_S)
   )

hankelMatrix(Ring,ZZ,ZZ) := (R,nrows,ncols) -> hankelMatrix(vars R, nrows, ncols)
hankelMatrix(Ring,RingElement, ZZ,ZZ) := (R,y,nrows,ncols) -> (
     j := position(gens R, i->i==y);
     v := (vars R)_{j..numgens R -1};
     hankelMatrix(v, nrows, ncols)
     )

hankelMatrix(ZZ,ZZ,String) := (nrows,ncols,s) -> (
    	  X := getSymbol s;
	  S := ZZ/32003[X_0..X_(nrows+ncols-2)];
          hankelMatrix(vars S,nrows,ncols)
	  )
      
hankelMatrix(ZZ,ZZ) := (nrows,ncols) -> hankelMatrix(nrows,ncols,"X")

-*    	  X := getSymbol "X";
	  S := ZZ/32003[X_0..X_(nrows+ncols-2)];
          hankelMatrix(vars S,nrows,ncols)
	  )
*-
correspondenceScroll = method(Options => 
    {VariableName=>"x"})
correspondenceScroll (Ideal,List) := o-> (I,scroll) ->(
    --I should be an ideal over a polynomial ring
    --S = kk[x_(i,j)], where j = 0,1, and i = 1..n, homogeneous in each pair
    --x_(i,0),x_(i,1) (say of degree e_i), and scroll should be 
    --a list of positive integers a_i.
    --The script interprets scroll as the degrees of rational normal curves,
    --and forms the ideal of the union of the planes spanned by the n-tuples of
    --points of P^1 defined by I.
    --The ordinary scroll should be the case when I is the (small) diagonal,
    --defined by the {n\choose 2} equations {x_(i,0)x_(i',1)-x_(i,1)x_(i',0).}
    
    --Currently, the script uses elimination to construct the ideal of the correspondence.
    --Once could speed it up by an option that would construct the ideals directly,
    --in the case of a product of projective spaces done here, by
    --multiplying the given equations to make them multi-homogeneous of degree
    --d_i in the i-th set of variables, where d_i is the smallest multiple of scroll_i
    --such that d_i \geq e_i.
S := ring I;
uS := unique (degrees vars S)_1;
SvarIdeals := apply(#scroll, i-> ideal basis(uS_i, S));
dims := apply(SvarIdeals, v -> numgens v -1);
kk := coefficientRing S;
--print dims;
--
y := getSymbol o.VariableName;
--var := apply(#scroll, i -> toList(y_(i,0)..y_(i,scroll_i)));
var := apply(#scroll, i -> toList(y_(i,0)..y_(i,binomial(dims_i+scroll_i, scroll_i)-1)));
--print var;
T := kk[flatten var];
varRings := apply(var, v -> kk[v]);
TvarIdeals := apply(varRings, vr -> ideal sub(vars vr, T));
tar := {};
scan(#scroll, i->tar = tar|((SvarIdeals_i)^(scroll_i))_*);
ker map(S/I,T,tar)
)

carpet = method(Options=>
    {VariableName => "x", 
     CoefficientField => ZZ/32003})
carpet List := o-> L ->(
    P:= productOfProjectiveSpaces(2,
	VariableName => o.VariableName,
	CoefficientField => o.CoefficientField);
    Delta := smallDiagonal P;
    correspondenceScroll(Delta^2,L))
-*
restart
loadPackage("CorrespondenceScrolls",Reload=>true)
carpet({1,3})
minimalBetti oo

scroll = {2,2}
S = productOfProjectiveSpaces{2,2}
vars S
I = ideal random(S^1, S^{{-5,-5}})
m = random(S^5, S^{5:{-1,-1}})
mm = m-transpose m
I = pfaffians(4,mm);
correspondenceScroll(I,scroll);
minimalBetti oo
*-

-*
---possible rewrite for speed, currently imcomplete.
Scrolls := apply(#scroll, i->hankelMatrix(T, y_(i,0), 2, scroll_i));
J0 := sum(#scroll, i->minors(2, Scrolls_i));
--
G := I_*;
Ge := flatten apply(G, f -> degree f);
--multiply up to make f have degree the smallest possible multiple of a_i in the i-th set of vars.
Gd := apply(#Ge, i-> (ceiling (Ge_i/scroll_i)*scroll_i));
--the following is the list of the bihomogeneous equations of correct degree in the S variables.
L := apply(#G, i-> product(#scroll, j->(SvarIdeals_j)^(Gd_j-Ge_j))*G_i)
--now need to translate these in terms of the T variables (and add J0)
)
*-




productOfProjectiveSpaces = method(Options=>{CoefficientField=>ZZ/32003, VariableName => "x"})
productOfProjectiveSpaces(List) := o -> L -> (
    kkk := o.CoefficientField;
    x := getSymbol o.VariableName;
    degs := flatten apply(#L, i -> toList (1+L_i:apply(#L, j-> if j==i then 1 else 0)));
    varlist := flatten apply(#L, i->apply(1+L_i, j->x_(i,j)));
    kkk[varlist, Degrees =>degs])
productOfProjectiveSpaces ZZ := opt -> n -> (productOfProjectiveSpaces(toList(n:1),CoefficientField=>opt.CoefficientField))

-*
restart
loadPackage("CorrespondenceScrolls", Reload=>true)
L={2,3}
S=productOfProjectiveSpaces L
S=productOfAffineSpaces L


S=productOfProjectiveSpaces (L, CoefficientField =>ZZ/2)
char S
vars S
n=2
S=productOfProjectiveSpaces n
S=productOfProjectiveSpaces (n, VariableName => "y")
vars S
*-

pp = productOfProjectiveSpaces


schemeInProduct = method(Options => {VariableName => "X"})
schemeInProduct(Ideal, List, Ring) := o->(I,Maps,PP) ->(
    --I defines a projective scheme in a polynomial ring S
    --Maps is a sequence of 1 x (1+m_i) matrices each consisting of forms of the 
    --same degree inside S/I, interpreted as rational maps from proj(S/I) to 
    --P^(m_i). PP is a multigraded ring corresponding to the product of the P^(m_i),
    --defined over the same field as S.
    --The routine forms the corresponding product of projective spaces, and
    --returns the multi-homogeneous ideal of the image.
    --
    --We should also have a version
    --inputting the product of projective spaces.
    S := ring I;
    dims := apply(Maps, f -> numcols f-1);
    degs := apply(Maps, f -> degree f_0);
    degslist := apply(#degs, i->toList(i:0)|degs_i|toList(1-i+dims_i:0));
    PPS := PP**(S/I);
    p := map(PPS,S);
    q := apply(#Maps,i-> select(gens PPS, u -> (degree u)_i == 1));
    lastdegs := toList (#Maps:0)|{1};
    Mats := apply(#Maps, i->
             map(PPS^{degslist_i,lastdegs}, PPS^(1+dims_i), 
	    (u,v) ->if u == 0 then p (Maps_i_v)_0 else q_i_v)
	);
    J := sum(Mats, m->minors(2,m));
    Jsat := saturate(J, irrelevantIdeal PPS);
    eliminate( (gens S)/p, Jsat)
    )

schemeInProduct(Ideal, List) := o-> (I,Maps) ->(
    --This version forms the corresponding product of projective spaces, and
    --returns the multi-homogeneous ideal of the image.
    S := ring I;
    kkk := coefficientRing S;
    dims := apply(Maps, f -> numcols f-1);
    PP := productOfProjectiveSpaces (dims, 
	         CoefficientField => kkk, 
	         VariableName => o.VariableName);
    schemeInProduct(I,Maps,PP)
)

-*
 restart
 loadPackage("CorrespondenceScrolls", Reload=>true)
 S = ZZ/101[a,b]
 I = ideal 0_S
 f0 = matrix"a,b"
 f1 = matrix"a,b"
 maps = {f0,f1}
 schemeInProduct(I, maps, Y) 
 *-

smallDiagonal = method()
smallDiagonal ZZ  := n ->(
    --returns the ring of (P^1)^n, and in it the ideal of the small diagonal
    S := productOfProjectiveSpaces n;
    smallDiagonal S)

smallDiagonal Ring  := S ->(
    --given an appropriate ring, as returned by 
    --productOfProjectiveSpaces n
    --returns the ideal of the small diagonal
    n := numgens S//2;
    v := genericMatrix(S,S_0,2,n);
    minors(2,v)
    )

degenerateK3=method(Options=>{CoefficientField=>ZZ/32003})
degenerateK3(ZZ,ZZ,Sequence):= opt -> (a,b,e) -> (
    S := productOfProjectiveSpaces(2,CoefficientField=>opt.CoefficientField);
    J := ideal( S_1^2*S_2^2-e_0*S_0*S_1*S_2*S_3+e_1*S_0^2*S_3^2);
    correspondenceScroll(J,{a,b})
    )

irrelevantIdeal = R ->(
u := unique ((gens R)/degree);
intersect(apply(u, d -> ideal basis(d,R))))

info Ideal := I-> (dim I, degree I)

multiHilbertPolynomial = method(Options => {VariableName => "s"})
multiHilbertPolynomial Module := o-> M ->(
s := getSymbol o.VariableName;
R := ring M;
u := unique ((gens R)/degree);
n := #u;
nums := apply(u, i-> #select(gens R, x -> degree x == i));
--form the hilbert polynomials of the factor rings
a := local a;
polys1 := apply(n, j-> (
  hilbertPolynomial(ZZ/2[a_0 .. a_(nums_j-1)], 
      Projective => false)));
--move them to the Cox ring
cox := QQ(monoid [s_0..s_(n-1)]);
polys := apply(n, j->
	  (map(cox, ring polys1_j, {cox_j})) polys1_j);
M1 := coker leadTerm gb presentation M;
F := res M1;
pd := length F;
Fdeglists := apply(1+pd, i->degrees F_i);
Fpolys := apply (1+pd, i -> 
    sum(Fdeglists_i, L-> 
	product(n, j -> 
	    sub(polys_j,{cox_j=>cox_j-L_j}))));
sum(1+pd, i-> (-1)^i*Fpolys_i)
)
///
S = productOfProjectiveSpaces{1,2}
I = ideal(x_(0,0),x_(1,0));
M = S^1/I;
g = multiHilbertPolynomial(M);

///
correspondencePolynomial = method(Options => {VariableName => "s"})
correspondencePolynomial(Module,List) := o-> (M,B)->(
    s := getSymbol o.VariableName;
    H1 := QQ[s];
    t := symbol t;
    f := multiHilbertPolynomial(M,VariableName => "t" );
    H := ring f;
    if numgens H != length B then 
         error"#B must equal degree length M";
    sub(f, apply(#B,i->H_i =>B_i*H1_0))
    )

beginDocumentation()

doc ///
   Key
    CorrespondenceScrolls
   Headline
    Package to compute and analyze examples of Correspondence Scrolls
   Description
    Text
     Correspondence Scrolls generalize rational normal scrolls and K3 Carpets, among other
     familiar constuctions.
     Suppose that Z is a subscheme of a product of projective spaces
     Z \subset P^{a_0} x .. x P^{a_{n-1}}
     The Correspondence Scroll C(Z;b), where b = (b_0,..,b_{n-1}) is the subscheme of
     P^{N-1} consisting set theoretically of the planes spanned by the points of the 
     Segre-Veronese embedding corresponding to Z.
     
     More generally, we treat the case of a multi-homogneous subscheme
     Z' \subset A^{a_0-1} x .. x A^{a_{n-1}-1}.
///

doc ///
   Key
    productOfProjectiveSpaces
    (productOfProjectiveSpaces,List)
    (productOfProjectiveSpaces,ZZ)
    [productOfProjectiveSpaces, CoefficientField]
    [productOfProjectiveSpaces, VariableName]    
   Headline
    Constructs the multi-graded ring of a product of copies of P^1 (pp is a synonym)
   Usage
    R = productOfProjectiveSpaces L
    R = productOfProjectiveSpaces n
   Inputs
    L:List
     of positive integers, the dimensions of the projective spaces
    n:ZZ
     positive integer, number of P^1 factors
    CoefficientField => Ring 
     the ground field
    VariableName => String
     name of variable to use.
   Outputs
    R:Ring
     ZZ^n - graded
   Description
    Text
     The variables are in 1+L_i -tuples, x_(i,0).. x_(i,L_i) with degree {0..0,1,0..0}, the 1 being in 
     the i-th place.     
    Example
     R = productOfProjectiveSpaces{1,3}
     v = gens R
     v/degree
     gens productOfProjectiveSpaces({1,1}, VariableName => "y")
     gens productOfProjectiveSpaces 2
///


-*
doc ///
   Key
    rationalOnP1n
    (rationalOnP1n, List)
   Headline
    Constructs the ideal of a general rational curve of type L on (P^1)^n

   Usage
    I  = rationalOnP1n L
   Inputs
    L:List
     List of positive integers - the degrees of the curve
   Outputs
    I:Ideal
     in a ring R = productOfProjectiveSpaces #L
   Description
    Text
     UNDER CONSTRUCTION!
     Chooses pairs of general forms of degree L_i for i = 1..n
     and forms the corresponding curve in (P^1)^n with n = #L
    Example
     I = rationalOnP1n({1,1})
     S = ring I;
     isHomogeneous I
///
*-

doc ///
   Key
    correspondenceScroll
    (correspondenceScroll, Ideal, List)
    [correspondenceScroll, VariableName]
   Headline
    Union of planes joining points of rational normal curves according to a given correspondence
   Usage
    G = correspondenceScroll(I,scroll)
   Inputs
    I:Ideal
     ideal of a correspondence; an arbitrary subscheme of (P^1)^{#scroll}
    VariableName:String
     name of the variable to use in the output
    scroll:List
     list of positive integers, the degrees of disjoint rational normal curves
   Outputs
    G:Ideal
     ideal of the generalized scroll
   Description
    Text
     Let L = {a_0,..a_{(m-1)}}, and let P = P^N with N = (#L-1+ sum L).
     Just as the ordinary scroll S(L) is  the union of planes joining rational normal curves C_i
     of
     degree a_i according to some chosen isomorphism among them (a (1,1,..,1) correspondence), 
     the generalized Scroll is the union of planes joining the points that correspond under
     an arbitrary correspondence, specified by I.

     Thus if I is the ideal of the small diagonal of (P^1)^m, then 
     generalized Scroll(I,L) is equal to S(L). If #L = 2, and I is the square of the ideal
     of the diagonal, we get a K3 carpet:
    Example
     L = {3,4}
     S = productOfProjectiveSpaces(#L) --creates the multi-graded ring of (P^1)^(#L)
     Delta = smallDiagonal S -- the ideal of the small diagonal of (P^1)^(#L)
     G = correspondenceScroll(Delta, L)
     minimalBetti G
     G = correspondenceScroll(Delta^2, L)
     minimalBetti G
    Text
     Here is how to make the generalized scroll corresponding to a general elliptic curve
     in (P^1)^3. First, the general elliptic curve, as a plane cubic through three given points:
    Example
     T = ZZ/32003[y_0,y_1,y_2]
     threepoints = gens intersect(ideal(y_0,y_1),ideal(y_0,y_2),ideal(y_1,y_2))
     f = threepoints*random(source threepoints, T^{-3}); -- general cubic through the three points
     L = {2,2,2}
     x = symbol x;
     S = productOfProjectiveSpaces(#L,VariableName =>"x") --creates the multi-graded ring of (P^1)^(#L)
     ST = (flattenRing(T**S))_0
     irrel = irrelevantIdeal ST;
    Text
     Here the irrelevant ideal is the 
     intersection of the 4 ideals of coordinats
     (P^2 and the three copies of P^1).
     Next, define the pairs of sections on the curve 
     giving the three projections:
    Example
--     ff = entries sub(transpose matrix {{y_0,y_1},{y_0,y_2},{y_1,y_2}}, ST) -- projections from the three points
     ff =  {{y_0,y_1},{y_0,y_2},{y_1,y_2}} -- projections from the three points
     ff =  apply(ff, f-> apply(f, p-> sub(p, ST)))
    Text
     And create the equations of the incidence variety
    Example
     D1 = det matrix{{x_(0,0),ff_0_1},{x_(0,1),ff_0_0}}
     D2 = det matrix{{x_(1,0),ff_1_1},{x_(1,1),ff_1_0}}
     D3 = det matrix{{x_(2,0),ff_2_1},{x_(2,1),ff_2_1}}
     J = sub(ideal f, ST)+ideal(D1,D2,D3)
    Text
     This must be saturated with respect to the irrelevant ideal, and then the y variables are eliminated,
     to get the curve in (P^1)^3.
    Example
     Js = saturate(J, irrel);
     I = eliminate({y_0,y_1,y_2}, Js);
     IS = (map(S,ST))I;
     codim I
    Text 
     Finally, we compute the ideal of the generalized Scroll:
    Example
     g = correspondenceScroll(IS, L);
     minimalBetti g
   Caveat
    The script currently uses an elimination method, but could be speeded up by replacing that
    with the easy direct description of the equations that come from the correspondence I.
   SeeAlso
    irrelevantIdeal
    pp
    smallDiagonal
///

doc ///
   Key
    smallDiagonal
    (smallDiagonal, Ring)
    (smallDiagonal, ZZ)
   Headline
    Ideal of the small diagonal in (P^1)^n
   Usage
    I = smallDiagonal n
    I = smallDiagonal S
   Inputs
    n:ZZ
     number of factors of P^1
    S:Ring
     ring with 2n vars, corresponding to (P^1)^n for some n
   Outputs
    I:Ideal
     ideal of the small diagonal
   Description
    Example
     smallDiagonal 3
     S = productOfProjectiveSpaces 3
     smallDiagonal S
   SeeAlso
    pp
///

doc ///
   Key
    irrelevantIdeal
   Headline
    returns the irrelevant ideal of a multi-graded ring
   Usage
    mm = irrelevantIdeal R
   Inputs
    R:Ring
     (multi)-graded ring
   Outputs
    mm:Ideal
     irrelevant ideal of R
   Description
    Text
     Returns the intersection of the ideals of variables in each single multi-degree.
    Example
     R = productOfProjectiveSpaces 3
     vars R
     (gens R)/degree     
     irrelevantIdeal R
   SeeAlso
    pp
///

doc ///
   Key
    hankelMatrix
    (hankelMatrix, Matrix, ZZ, ZZ)
    (hankelMatrix, Ring, RingElement, ZZ, ZZ)
    (hankelMatrix, Ring, ZZ, ZZ)    
    (hankelMatrix, ZZ,ZZ,String)
    (hankelMatrix, ZZ, ZZ)
   Headline
    matrix with constant anti-diagonal entries
   Usage
    m = hankelMatrix(r,p,q)
    m = hankelMatrix(R,x,p,q)    
    m = hankelMatrix(R,p,q)    
    m = hankelMatrix(p,q)        
    m = hankelMatrix(p,q,s)            
   Inputs
    r:Matrix
     matrix whose first row will be the entries of m
    R:Ring
     in this case, hankelMatrix will use the variables of R
    x:RingElement
     if present, hankelMatrix will use the variables of R starting with x
    p:ZZ
     number of rows
    q:ZZ
     number of columns
    s:String
     name of the variable to use in the method (hankelMatrix, p,q,s)
   Description
    Text
     A Hankel matrix (or catalecticant matrix) is a matrix with a repeated element on
     each anti-diagonal, that is m_(i,j) depends only on i+j. 
     If the matrix r is given, then we set m_(i,j) = r_(0,i+j) if i+j<numcols r, and 0 otherwise.
     The degrees of the rows of m are set to 0, and the degrees of the columns are set to
     the degree of r_(0,0); thus the Hankel matrix is homogeneous iff all the entries
     of the first row of r have the same degree.
    
     If no ring or matrix is given, hankelMatrix defines a new ring S with p+q-1 variables X_i,
     and then calls hankelMatrix(vars S, p,q).
    Example
     p = 2;q=3;
     S = ZZ/101[x_0..x_(p+q-2)]
     hankelMatrix(vars S, p,q)
     r = vars S ** transpose vars S
     hankelMatrix(r, p,q)
     hankelMatrix(S,p,q)
     hankelMatrix(r, p,q+2)
     hankelMatrix(p,q+2)
///
doc ///
   Key
    schemeInProduct
    (schemeInProduct, Ideal, List, Ring)    
    (schemeInProduct, Ideal, List)
    [schemeInProduct, VariableName]    
   Headline
    multi-graded Ideal of the image of a map to a product of projective spaces
   Usage
    J = schemeInProduct(I,maps,PP)
    J = schemeInProduct(I,maps)    
   Inputs
    I:Ideal
     defining the source scheme
    maps:List
     list of sequences of polynomials each of a single degree, in I
    PP:Ring
     multigraded ring of the product of projective spaces
    VariableName:String
     name of variable to use in constructing a ring PP if none is given
   Outputs
    J:Ideal
     multi-graded ideal of the image
   Description
    Example
     S = ZZ/101[a,b]
     I = ideal 0_S
     f0 = matrix"a,b"
     f1 = matrix"a,b"
     maps = {f0,f1}
     schemeInProduct(I, maps,VariableName =>"Y") 
   SeeAlso
    productOfProjectiveSpaces
///


doc ///
   Key
    multiHilbertPolynomial
    (multiHilbertPolynomial, Module)
    [multiHilbertPolynomial, VariableName]
   Headline
    Multi-graded Hilbert polynomial for a product of projective spaces
   Usage
    H = multiHilbertPolynomial M
    H = multiHilbertPolynomial (M, VariableName => "s")
   Inputs
    M:Module
     multigraded over a polynomial ring
    VariableName:String
   Outputs
    H:RingElement
     in QQ[h]
   Description
    Text
     Let M be a module over a polynomial ring
     P = kk[x_{0,0}..x_{0,a_0}..x_{n-1,0}..x_{n-1,a_{n-1}}]
     graded with degree x_{i,j} = e_i, the i-th unit vector.
     If M = P^{m} is free, then the Hilbert polynomial is the 
     product of the shifted binomial coefficients 
     binomial(a_i+m_i+t,a_i).
     In general,
     the routine computes a free resolution of the coker of
     the initial matrix of a presentation matrix, and then
     makes an alternating sum of the Hilbert polynomials of the
     free modules in the resolution. The polynomial returned
     has variables s_i (the default) or name_i if
     VariableName => "name" is given.
    Example
     P = productOfProjectiveSpaces{1,2}
     M1 = P^1
     multiHilbertPolynomial M1
   Caveat
    Because of the computation of a free resolution, this
    might be slow on large examples.
   SeeAlso
    correspondencePolynomial
///
doc ///
   Key
    correspondencePolynomial
    (correspondencePolynomial, Module, List)
    [correspondencePolynomial, VariableName]    
   Headline
    computes the Hilbert polynomial of a correspondence scroll
   Usage
    H = correspondencePolynomial(M,L)
    H = correspondencePolynomial(M,L,VariableName =>h)
   Inputs
    M:Module
    L:List
     of ZZ
    VariableName:String
   Outputs
    H:RingElement
     in QQ[h]
   Description
    Text
     Let M be a module over a polynomial ring
     P = kk[x_{0,0}..x_{0,a_0}..x_{n-1,0}..x_{n-1,a_{n-1}}]
     graded with degree x_{i,j} = e_i, the i-th unit vector,
     and let b = {b_0..b_{n-1}} be a list of integers.
     The code computes the multigraded Hilbert polynomial
     mH(h_0,..,h_{n-1}) and returns
     H(h) = mH(b_0*h_0, .., b_{n-1}*h_{n-1}).     
    Example
     P = productOfProjectiveSpaces {1,1}
     Delta = smallDiagonal P
     M = P^1/(Delta^2)
     correspondencePolynomial (M,{1,1})
     correspondencePolynomial (M,{2,2})     
   SeeAlso
    multiHilbertPolynomial
///
-*
 loadPackage("CorrespondenceScrolls", Reload => true)
 *-
doc ///
   Key
    carpet
    (carpet, List)
    [carpet, CoefficientField]
    [carpet, VariableName]
   Headline
    ideal of a K3 carpet
   Usage
    I = carpet L
   Inputs
    L:List
    CoefficientField: Ring
     the ground field
    VariableName: String
   Outputs
    I:Ideal
   Description
    Text
     the K3 carpet of type b1,b2 is the correspondence scroll
     of type b1,b2 with respect to the ideal Delta^2,
     where Delta \subset P^1 x P^1 is the ideal of the 
     diagonal.
    Example
     betti res carpet{1,3}
   SeeAlso
    correspondenceScroll
///

doc ///
   Key
    VariableName
   Headline
    symbol used to define the variable name in many routines
   SeeAlso
    carpet
    correspondenceScroll
    hankelMatrix
    productOfProjectiveSpaces
///
doc ///
   Key
    CoefficientField
   Headline
    symbol used to define the ground field in many routines
   SeeAlso
    carpet
    correspondenceScroll
    hankelMatrix
    productOfProjectiveSpaces
///

TEST///
I = carpet({2,3},CoefficientField => QQ, VariableName => z)
assert( (betti res I) === new BettiTally from {(0,{0},0) => 1, (2,{3},3) => 5, (2,{4},4) => 5, (4,{7},7)
     ---------------------------------------------------------------------------------------------------------
     => 1, (1,{2},2) => 6, (3,{5},5) => 6} )
///
TEST///
P = productOfProjectiveSpaces{1,2}
I = ideal( x_(0,0)*x_(1,0))
J = correspondenceScroll(I,{1,1}, VariableName =>"y")
use ring J
assert (J == ideal(y_(0,0)*y_(1,0)))
assert(toString correspondencePolynomial(P^1/I, {1,1}) == "(3/2)*s^2+(5/2)*s+1")
///
TEST///
assert(toString hankelMatrix(2,3,"t") == "matrix {{t_0, t_1, t_2}, {t_1, t_2, t_3}}")
///
TEST///
P = productOfProjectiveSpaces({1,2}, VariableName => "s")
assert(toString irrelevantIdeal P == "ideal(s_(0,1)*s_(1,2),s_(0,0)*s_(1,2),s_(0,1)*s_(1,1),s_(0,0)*s_(1,1),s_(0,1)*s_(1,0),s_(0,0)*s_(1,0))")
///
TEST///
     S = ZZ/101[a,b]
     I = ideal 0_S
     f0 = matrix"a,b"
     f1 = matrix"a,b"
     maps = {f0,f1}
assert(toString schemeInProduct(I, maps,VariableName =>"Y") == "ideal(Y_(0,1)*Y_(1,0)-Y_(0,0)*Y_(1,1))")
///
TEST///
I = smallDiagonal 3
assert(I ==ideal  {-x_(0,1)*x_(1,0)+x_(0,0)*x_(1,1),
       -x_(0,1)*x_(2,0)+x_(0,0)*x_(2,1), -x_(1,1)*x_(2,0)+x_(1,0)*x_(2,1)})
///       

end--


restart
loadPackage("CorrespondenceScrolls", Reload=>true)
P = productOfProjectiveSpaces{1,1}



I = ideal(P_0,P_1^9)
I = ideal(P_0,P_1^3,P_2^2)
I = ideal(P_0,P_1,P_2^2,P_3^2)

map(ZZ^8,ZZ^8, (i,j) -> degree correspondenceScroll(I,{i+1,j+1}))


degree correspondenceScroll(I,{4,5})
correspondenceScroll(I,{3,2})
degree correspondenceScroll(I,{3,2})
degree correspondenceScroll(I,{3,5})
degree correspondenceScroll(I^2,{1,1})
degree correspondenceScroll(I^2,{1,2})
degree correspondenceScroll(I^2,{2,1})
degree correspondenceScroll(I^2,{2,2})
degree correspondenceScroll(I^2,{3,5})
degree correspondenceScroll(I^2,{8,8})
degree correspondenceScroll(I^2,{8,8})

P = productOfProjectiveSpaces({1,1})
I = ideal(P_0^2*P_2^3)
degree correspondenceScroll(I,{4,5})

P = productOfProjectiveSpaces({3,2,1})
I = ideal(P_1^2*P_0^2*P_2^3)
I = ideal (x_(0,0)^2*x_(1,0)^3*x_(2,0)^1)
I = ideal 0_P
degree correspondenceScroll(I,{2,2,2})

-------------------
P = productOfProjectiveSpaces{1,1}
vars P
m1 =(ideal( x_(0,0),x_(0,1)))
P = ideal(x_(1,1))
degree correspondenceScroll(I,{1,1})
degree correspondenceScroll(I,{2,1})
degree correspondenceScroll(I,{6,6})
J = correspondenceScroll(ideal(0_P),{2,2})
sJ = ideal singularLocus(ring J/J)
dim oo
primaryDecomposition sJ

----complete intersection of two trilinear forms
P= productOfProjectiveSpaces{1,1,1}
I = ideal random(P^1, P^{2:{-1,-1,-1}})
degree correspondenceScroll(I,{2,2,3})
P = (primaryDecomposition I)_0
test = (a,b,c)->degree correspondenceScroll(I,{a,b,c})-a*b-a*c-b*c-2*(a+b+c)
for a from 1 to 5 do for b from a to 5 do for c from b to 5 do print test(1,1,1)
c = (a,b,c) -> correspondenceScroll(I,{a,b,c})
c(2,1,2)
betti res oo

--------
------------------- Singularity questions
P = productOfProjectiveSpaces{1,2}
I = minors(2,
    matrix{{(x_(0,0))^3,x_(0,0)*(x_(0,1))^2, (x_(0,1))^3},
	{x_(1,0),x_(1,1), x_(1,2)}})
J = correspondenceScroll(I,{1,2})
n = numgens ring J
dim I == dim J
codim minors(n-dim I,  jacobian J) == dim ring J

P = productOfProjectiveSpaces{1,2}
vars P
I =ideal( (x_(1,0))^2- x_(1,1)*x_(1,2))
J = correspondenceScroll(I,{1,1})
n = numgens ring J
dim I == dim J
codim minors(n-dim I,  jacobian J) == dim ring J


P = productOfProjectiveSpaces{1,2}
vars P
I1 = (ideal(x_(0,0),x_(0,1)))^2
I = minors(2, map(P^2, P^3, (i,j) -> if i==0 then I1_j else x_(1,j)))

lcmList = L -> (
    --assume L is a list of lists of ZZ
    n = #L_0;
    apply(n, i-> max(apply L, ell->ell_i)))

L =  degrees P
lcmList L
apply(2, i-> max apply(L, ell->ell_0))

randomMinor = (a,J)->(
    S:= ring J;
    m := random(S^a, (target J))*J*random((source J), S^a);
    minors(a,m))


J = jacobian I
randomMinor(2,J)
codim I
dim I
sing = (I+ ideal apply(10, s -> randomMinor(2,J)))
codim sing
rank J
randomMinor(
betti jacobian I
codim J
dim J
sJ = ideal singularLocus(ring J/J)
dim oo
primaryDecomposition sJ

--------closure of lines
restart
loadPackage "CorrespondenceScrolls"
L = {2,3}
A = ZZ/32003[x_(0,0)..x_(n-1,1)]
x_(0,0)

f = A^1/sum(apply(n, i->x_(i,0)))
f = homogenize(f,x_(0,1),{1,1,0,0})
f = homogenize(f,x_(1,1),{0,0,1,1})
P = productOfProjectiveSpaces{1,1}
(map(P,A))ideal presentation f



homogenize(f,x_(0,1),{1,0})
I = ideal apply

=

productOfAffineSpaces = method(Options=>{CoefficientField=>ZZ/32003, VariableName => "x"})
productOfAffineSpaces(List) := o -> L -> (
    kkk := ZZ/(o.CoefficientField);
    y := getSymbol o.VariableName;
    varlist := flatten apply(#L, i->apply(1+L_i, j->y_(i,j)));
    kkk[varlist])
p
productOfAffineSpaces ZZ := opt -> n -> (productOfProjectiveSpaces(toList(n:1),CoefficientField=>opt.CoefficientField))
 
L = {2,3}
A = productOfAffineSpaces L
vars A
k = coefficientRing A
lin = apply(sum L -1, s-> sum flatten apply(#L,i-> apply(L_i, j-> (random k)*x_(i, j))))



P = productOfProjectiveSpaces{2,2}
S = ZZ/101[a,b,c]
betti res (ideal random(S^1, S^{2:-3}), DegreeLimit => -1)
minimalBetti (ideal random(P^1, P^{2:{-3,-3}}), DegreeLimit => {-1,-1})
viewHelp DegreeLimit

