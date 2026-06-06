--needsPackage "CompleteIntersectionResolutions"
--needsPackage "PushForward"

Matrix Array := Matrix => (M,L) -> ((M^[L_0])_[L_1])

higherHomotopyFactorization = method();
higherHomotopyFactorization(List,Complex) :=  (L,C) -> (
    Q := ring L_0;
    t := local t;
    S := Q[t_1..t_(length L)];
    f := sum(1..length L,i->L_(i-1)*t_i);
    C = C**S;
    H := makeHomotopies(matrix{{f}}, (C));
    Ln := apply(keys H,i->(i_1+2*(i_0)_0-1,i_1));
    Hn := new HashTable from for i from 0 to length Ln-1 list Ln_i => H#((keys H)#i);
    (lo,hi) := concentration C;
    Lo := select(toList(lo-1..hi+1),odd);
    Le := select(toList(lo-1..hi+1),even);
    T1 := table(Lo,Le,(u,v) ->
	if Hn#?(u,v) then map(C_u,C_v,Hn#(u,v)) else 0);
    T2 := table(Le,Lo,(u,v) ->
	if Hn#?(u,v) then map(C_u,C_v,Hn#(u,v)) else 0);
    Co := directSum apply(Lo,i->i => C_i);
    Ce := directSum apply(Le,i->i => C_i);
    M1 := map(Ce,Co,matrix T2);
    M2 := map(Co,Ce,matrix T1);
    ZZdfactorization {M1,M2}
    )

higherHomotopyFactorization(RingElement,Complex) := (f,C) -> (
    H := makeHomotopies(matrix{{f}}, (C));
    Ln := apply(keys H,i->(i_1+2*(i_0)_0-1,i_1));
    Hn := new HashTable from for i from 0 to length Ln-1 list Ln_i => H#((keys H)#i);
    (lo,hi) := concentration C;
    Lo := select(toList(lo-1..hi+1),odd);
    Le := select(toList(lo-1..hi+1),even);
    T1 := table(Lo,Le,(u,v) ->
	if Hn#?(u,v) then map(C_u,C_v,Hn#(u,v)) else 0);
    T2 := table(Le,Lo,(u,v) ->
	if Hn#?(u,v) then map(C_u,C_v,Hn#(u,v)) else 0);
    Co := directSum apply(Lo,i-> i => C_i);
    Ce := directSum apply(Le,i-> i => C_i);
    M1 := map(Ce,Co,matrix T2);
    M2 := map(Co,Ce,matrix T1);
    ZZdfactorization {M1,M2}
    )
    
--do higher homotopy factorization coming from a non-resolution
 --do example that contradicts the h(F**F) <= h(F) beta(F)  
    
toBranchedCover = method();
toBranchedCover(ZZdFactorization,Symbol) := (C,z) -> (Q := ring C;
    --code assumes the input is a well-defined factorization
    if not((unique flatten degrees Q)=={0}) then error "Variables from ambient ring should have degree 0";
    d := period C;
    if d==2 then Q.rootOfUnity = -1;
    if Q.?rootOfUnity then t:=Q.rootOfUnity
    else  error "Need to adjoin dth root of unity";
    P := product(d,i->C.dd_i);
    f := sub(P_(0,0),Q);
    S := Q[z];
    zn := (S_*)_0;
    Sk := S/(zn^d+sub(f,S));
    use Sk;
    T := table(toList(0..d-1),toList(0..d-1),(u,v) -> 
	    if u==v then sub(t^(u),Sk)*zn*id_(sub(C_u,Sk)) 
	    else if u==(v-1)%d then sub(C.dd_u,Q)
	    else 0
	    );
    Cterms := directSum for i to d-1 list C_i**Sk;
    map(Cterms,Cterms,sub(matrix T,Sk))
    )

toBranchedCover(ZZdFactorization,RingElement) := (C,z) -> (toBranchedCover(C,getSymbol "z"))

--this redefines the ring to have all variables degree 0
zeroOutDegrees = method();
zeroOutDegrees(Ring) := R -> (n := length gens R;
    (baseRing R)[R_*,Degrees => toList(n:0)]
    )

zeroOutDegrees(ZZdFactorization) := C -> (Rn := zeroOutDegrees ring C;
    phi := map(Rn,ring C);
    phi(C)
    )

degreeSorter = method();
degreeSorter(ZZ,Module) := (d,M) -> (theDegs := degrees M;
    flatten for j to d-1 list positions(theDegs,i->(i_0%d == j))
    )

degreeSorter(ZZ,ZZ,Module) := (d,s,M) -> (theDegs := degrees M;
    flatten for j to d-1 list positions(theDegs,i->(i_0%d == (j+s)%d))
    )

degreeSorter(ZZ,Matrix) := (d,M) -> (L1 := degreeSorter(d,source M);
    L2 := degreeSorter(d,target M);
    (M_L1)^L2
    )

degreeSorter(ZZ,ZZ,Matrix) := (d,deg,M) -> (L1 := degreeSorter(d,source M);
    L2 := degreeSorter(d,deg,target M);
    (M_L1)^L2
    )
   

branchedToMF = method();
branchedToMF(Module) := ZZdFactorization =>  M -> (R := ring M;
    d := first degree (R.relations_(0,0));
    zn := (R_*)_0;
    phi := map(R,baseRing ambient R);
    T := matrix pushFwd(phi,zn*id_M);
    ZZdfactorization toList(d:T)
    )

branchedToMF(Module,Ring) := ZZdFactorization =>  (M,Q) -> (R := ring M;
    d := first degree (R.relations_(0,0));
    zn := (R_*)_0;
    phi := map(R, Q);
    T := sub(matrix pushFwd(phi,zn*id_M), Q);
    ZZdfactorization toList(d:T)
    )

branchedToMF(Matrix) := ZZdFactorization => M -> (R := ring M;
    d := first degree (R.relations_(0,0));
    zn := (R_*)_0;
    phi := map(R,baseRing ambient R);
    Mn := degreeSorter(d,-1,M);
    T := matrix pushFwd(phi,zn*id_(prune coker Mn));
    ZZdfactorization toList(d:T)
    )

branchedToMF(Matrix,Ring) := ZZdFactorization =>  (M,Q) -> (R := ring M;
    d := first degree (R.relations_(0,0));
    zn := (R_*)_0;
    phi := map(R, baseRing ambient R);
    Mn := degreeSorter(d,-1,M);
    T := sub(matrix pushFwd(phi,zn*id_(prune coker Mn)), Q);
    ZZdfactorization toList(d:T)
    )
   



mooreMF = method();
mooreMF(ZZ) := p -> (
    a := local a;
    x := local x;
    Q := if p==0 then QQ[a_0..a_2,x_0..x_2] else ZZ/p[a_0..a_2,x_0..x_2];
    M1 := matrix{{a_0*x_0,a_1*x_2,a_2*x_1},{a_1*x_1,a_2*x_0,a_0*x_2},{a_2*x_2,a_0*x_1,a_1*x_0}};
    M2 := matrix{{a_1*a_2*x_0^2-a_0^2*x_1*x_2,a_0*a_2*x_1^2-a_1^2*x_0*x_2,a_0*a_1*x_2^2-a_2^2*x_0*x_1},
	{a_0*a_2*x_2^2-a_1^2*x_0*x_1,a_0*a_1*x_0^2-a_2^2*x_1*x_2,a_1*a_2*x_1^2-a_0^2*x_0*x_2},
	{a_0*a_1*x_1^2-a_2^2*x_0*x_2,a_1*a_2*x_2^2-a_0^2*x_0*x_1,a_0*a_2*x_0^2-a_1^2*x_1*x_2}};
    ZZdfactorization {M1,M2}
    )


--constructs a rank 1, 2-generated maximal Cohen-Macaulay module based on
--input parameters specified by a list, and the integer d specifies the characteristic.
--if d is not specified the base field is chosen to be QQ
rk1MCM2gen = method()
rk1MCM2gen(List, ZZ) := ZZdFactorization =>  (L,d) -> (a := local a;
    b := local b;
    x := local x;
    Q := if d==0 then QQ[x_1..x_4,a,b] else ZZ/d[x_1..x_4,a,b]; 
    an := (Q_*)_4;
    bn := (Q_*)_5;
    S := Q/(an^2-an+1,bn^2-bn+1);
    use S;
    (i,j,s) := toSequence L;
    M1 := matrix{{x_1-an*x_s,-(x_i^2+bn*x_i*x_j+bn^2*x_j^2)},{x_i-bn*x_j,x_1^2+an*x_1*x_s+an^2*x_s^2}};
    M2 := matrix{{x_1^2+an*x_1*x_s+an^2*x_s^2,x_i^2+bn*x_i*x_j+bn^2*x_j^2},{-(x_i-bn*x_j),x_1-an*x_s}};
    ZZdfactorization {M1,M2}
    )

rk1MCM2gen(List) := ZZdFactorization => L -> rk1MCM2gen(L, 0)

-*
--need to fix this code
rk1MCM3gen = method()
rk1MCM3gen(ZZ,ZZ) := ZZdFactorization => (p,type) -> (a := local a;
    b := local b;
    c := local c;
    d := local d;
    e := local e;
    x := local x;
    Q := if p==0 then QQ[x_1..x_4,a,b,c,d,e,Degrees => {1,1,1,1,0,0,0,0,0}] else ZZ/p[x_1..x_4,a,b,c,d,e, Degrees => {1,1,1,1,0,0,0,0,0}]; 
    an := (Q_*)_4;
    bn := (Q_*)_5;
    cn := (Q_*)_6;
    dn := (Q_*)_7;
    en := (Q_*)_8;
    S := Q/(an^2-an+1,bn^2-bn+1,cn^2-cn+1,dn^2-dn+1,en^2+en+1,bn*cn*dn-en*an);
    use S;
    M1 := local M1;
    M2 := local M2;
    if type==1 then (
	M1 = matrix{{0,x_1-an*x_4,x_2-bn*x_3},
	             {x_1-cn*x_2,-bn^2*x_3-an*bn*cn^2*en^2*x_4,bn^2*cn^2*x_3-an*bn*cn*en^2*x_4},
		     {x_3-dn*x_4,cn^2*x_2+bn*cn^2*x_3+an*cn*x_4,-x_1-cn*x_2-an*x_4}};
        M2 = transpose M1;
	return ZZdfactorization {M1,M2};
	);
    if type==2 then (
	M1 = matrix{{0,x_1+x_2,x_3-an*x_4},
	             {x_1+en*x_2,-x_3+cn*x_4,0},
		     {x_3-bn*x_4,0,-x_1-en^2*x_2}};
	M2 = matrix{{0,x_1+x_3,x_2-an*x_4},
	             {x_1-an^2*bn*x_3,-x_2+cn*x_4,0},
		     {x_2-bn*x_4,0,-x_1+an*bn^2*x_3}};
        return ZZdfactorization {M1,M2};
	);
    )


rk1MCM3gen(ZZ) := ZZdFactorization => type -> rk1MCM3gen(0, type)
*-


classicalAdjoint = (G) -> (
n := rank target G;
m := rank source G;
matrix table(n, n, (i, j) -> (-1)^(i+j) * det(
submatrix(G, {0..j-1, j+1..n-1},
{0..i-1, i+1..m-1}))));

--constructs factorizations of determinant of a matrix using the adjoint
adjointFactorization = method()
adjointFactorization(Matrix) := ZZdFactorization => M -> (
    ZZdfactorization {M, classicalAdjoint M}
    )









