-- Paul Zinn-Justin 2024
-- factorisation in Weyl algebras
-- here it's assumed rings are created with makeWA in terms of ordering of variables

-- factorisation is up to:
-- * commuting *irreducible* factors
-- * commuting monomials and degree 0 factors into normal order

debug Core

debugPrint = seq -> if debugLevel>=seq#0 then print SPAN drop(seq,1);

-- rewrite degree zero element in terms of thetas
-- use lemma 1.3.1 of [STT]
θ:=local θ
thetaring = memoize ((F,n) -> F(monoid[θ_1..θ_n]) )
theta = a -> (
    R0:=ring a;
    n:=numgens R0//2;
    R:=thetaring(coefficientRing R0,n);
    if isPromotable(R,R0) then f:=map(R0,R) else (
        f=map(R0,R,apply(n,i->R0_i*R0_(i+n)));
        setupPromote f;
        );
    if a == 0 then return (toList(2*n:0),0_R);
    l:=listForm a;
    m:=min\(transpose(first\l));
    (
        m,
        sum(l,(e,c)->(
                e-=m;
                if e_{0..n-1}!=e_{n..2*n-1} then error "not homogeneous";
                c*product(n,i->if e#i==0 then 1_R else product(e#i,j->R_i-j))) -- annoying: we want 1_R, not 1_ZZ or whatever
            )
        )
    )
theta0 = a -> ( -- only for degree zero elements. doesn't try to pull monomials
    R0:=ring a;
    n:=numgens R0//2;
    R:=thetaring(coefficientRing R0,n);
    if isPromotable(R,R0) then f:=map(R0,R) else (
        f=map(R0,R,apply(n,i->R0_i*R0_(i+n)));
        setupPromote f;
        );
    if a == 0 then return (toList(2*n:0),0_R);
    l:=listForm a;
    sum(l,(e,c)->(
            if e_{0..n-1}!=e_{n..2*n-1} then error "not degree 0";
            c*product(n,i->if e#i==0 then 1_R else product(e#i,j->R_i-j))) -- annoying: we want 1_R, not 1_ZZ or whatever
        )
    )

sortByFirst := sortBy first

-- first, factorisation of homogeneous elements
-- strictly speaking this factorisation is not unique
-- because of θ_i and θ_i+1, as well as x_i and dx_i that can be pushed.
-- we choose a "normal form" x^a P(theta) dx^b 
fachom = a -> (
    R0:=ring a;
    n:=numgens R0//2;
    R:=thetaring(coefficientRing R0,n);
    (m,t):=theta a;
    (facs, exps) := rawFactor raw t;
    l:=toList apply(facs,exps,(f,e)->Power{promote(promote(f,R),R0),e});
    pre:=sortByFirst for i from 0 to n-1 list if m#i==0 then continue else Power{R0_i,m#i};
    if l#0#0 != 1 then pre=prepend(first l,pre);  -- move constant to the front
    l=drop(l,1);
    l=sortByFirst l;
    -- check for θ_i+1
    i:=0;
    while i<#l do (
        p:=position(n,j->l#i#0 == 1+R_j); -- ...
        if p=!=null then (
            l1:=take(l,i);
            l2:=drop(l,i+1);
            e:=l#i#1;
            l=l1 | splice apply(e,j->(Power{R0_(p+n),1},Power{R0_p,1})) | l2;
            i+=2*e;
            ) else i+=1;
        );
    post:=sortByFirst for i from n to 2*n-1 list if m#i==0 then continue else Power{R0_i,m#i};
    debugPrint_1("homogeneous, factored as ",toList join(pre,l,post));
    Product join(pre,l,post)
    )

-- helper function: all sequences 0<=s<=m
seqs = memoize ( m -> (
    if #m==0 then {()} else splice\(toList(0..m#0)**seqs(drop(m,1)))
    ))

-- decomposition of a homogeneous element into *two* factors
fachom2 = a -> (
    R0:=ring a;
    n:=numgens R0//2;
    (m,t):=theta a;
    R:=ring t;
    shift := e -> map(R,R,apply(n,i->R_i+e#i));
    (facs, exps) := rawFactor raw t;
    facs=apply(facs,x->promote(x,R));
    c:=(facs#0)^(exps#0);
    facs=drop(facs,1); exps=drop(exps,1); -- get rid of constant
    -- if any factor is θ_i+1 we need to split, as annoying as may be
    l:=apply(n,i->1+R_i);
    flatten apply(seqs (m_{0..n-1}+m_{n..2*n-1}), e -> (
	    shift1:=shift apply(n,i->min(e#i-m#i,0));
	    shift2:=shift apply(n,i->-max(e#i-m#i,0));
	    facs1:=shift1\facs;
	    facs2:=shift2\facs;
	    -- first the standard expressions
	    lst := apply(seqs exps, e' -> ( -- e, e' are the set of exponents of the first of the two factors
		    c*R0_(apply(n,i->min(m#i,e#i))) * product(#facs,i->facs1#i^(e'#i)) * R0_(toList(n:0)|apply(n,i->max(e#i-m#i,0))),
		    R0_(apply(n,i->max(m#i-e#i,0))) * product(#facs,i->facs2#i^(exps#i-e'#i)) * R0_(toList(n:0)|apply(n,i->m#(i+n)-max(e#i-m#i,0)))
		    ));
	    -- now check for th_i+1
	    scan(#facs, k -> (
		    if (p:=position(l,y->y==facs1#k))=!=null or (p=position(l,y->y==facs2#k))=!=null then (
			exps':=apply(#exps,j->if j!=k then exps#j else exps#k-1);
			lst = lst | apply(seqs exps', e' -> (
				c*R0_(apply(n,i->min(m#i,e#i))) * product(#facs,i->facs1#i^(e'#i)) * R0_(toList(n:0)|apply(n,i->max(e#i-m#i,0))) * R0_(n+p),
				R0_p * R0_(apply(n,i->max(m#i-e#i,0))) * product(#facs,i->facs2#i^(exps'#i-e'#i)) * R0_(toList(n:0)|apply(n,i->m#(i+n)-max(e#i-m#i,0)))
				));
			)
		    ));
	    lst
	    ))
    )

pullConstant = p -> (
    if coefficientRing ring p===QQ then (
        cfs:=flatten entries lift(last coefficients p,QQ); -- lift shouldn't be needed!
        num:=gcd(numerator\cfs);
        den:=lcm(denominator\cfs);
        if leadCoefficient p<0 then num*=-1;
        (num/den,den/num*p)
        ) else (
        c:=leadCoefficient p;
        (c,1/c*p)
        )
    )

-- combine two factorisations modulo 2 operations
-- sorting commuting irreducible factors
-- switching monomials/degree zero factors with normal ordering (x's)(theta)(dx's)
combineFac = (z,y) -> (
    debugPrint_2("combining ",z,y);
    if #z==0 then return y;
    R0:=if #y>0 then ring y#0#0;
    n:=numgens R0//2;
    R:=thetaring(coefficientRing R0,n);
    for f in y do if liftable(f#0,coefficientRing R0) then (
        -- constants are always on the left so let's cut this short
        z = if liftable(z#0#0,coefficientRing R0) then (cf:=f#0*z#0#0; if cf!=1 then replace(0,Power{cf,1},z) else drop(z,1)) else prepend(f,z)
        ) else (
        i:=#z-1;
        th:=if all(listForm f#0,c->c#0_{0..n-1}==c#0_{n..2*n-1}) then theta0 f#0; -- degree zero
        q:=position(n,j->f#0==R0_j); -- monomial
        while i>=0 do (
            if f#0*z#i#0==z#i#0*f#0 and f#0<z#i#0 then () -- commuting elements
            else if th=!=null and (p:=position(n,j->z#i#0==R0_(n+j)))=!=null then ( -- dx * theta -> theta * dx
                f=Power{promote(sub(th,R_p=>R_p+z#i#1),R0),f#1};
                )
            else if q =!= null and ( -- monomial
		th':=if all(listForm z#i#0,c->c#0_{0..n-1}==c#0_{n..2*n-1}) then theta0 z#i#0 -- degree zero
		) =!= null then (
		z=replace(i,Power{promote(sub(th',R_q=>R_q+f#1),R0),z#i#1},z); -- theta * x -> x * theta -- never happens in practice (?)
		)
	    else break;
            i-=1;
            );
        z = if i>=0 and f#0==z#i#0 then
        replace(i,Power{f#0,z#i#1+f#1},z)
        else insert(i+1,f,z);
        );
    debugPrint_2("combined ",z);
    z
    )


factorWeylAlgebracache = new CacheTable
factorWeylAlgebra = method(Options=>{StopAfter=>infinity})
factorWeylAlgebra RingElement := o -> a -> (
    if factorWeylAlgebracache#?a and factorWeylAlgebracache#a#0>=o.StopAfter then return factorWeylAlgebracache#a#1;
    debugPrint_1("factoring ",a);
    ret := x -> ( -- things to do before returning
        if class x =!= List then x = if class x === MutableHashTable then keys x else {x};
        factorWeylAlgebracache#a=(o.StopAfter,x);
	x
        );
    R0:=ring a;
    if liftable(a,coefficientRing R0) then return ret Product{Power{a,1}};
    (cf,a'):=pullConstant a;
    if cf!=1 then return ret apply(factorWeylAlgebra(a',o),x -> prepend(Power{promote(cf,R0),1},x));
    n:=numgens R0//2;
    l:=listForm a;
    m:=max \ (transpose(first\l)); -- max degrees
    if all(n,i->m#i==0 or m#(i+n)==0) then (
        (facs, exps) := rawFactor raw a;
        pr:=toList apply(facs,exps,(f,e)->Power{promote(f,R0),e});
        if pr#0#0 == 1 then pr=drop(pr,1);
        pr=sortByFirst pr;
        debugPrint_1("commutative, factored as ",pr);
        return ret Product pr;
        ); -- commutative case
    deg:=c-> (-c_{0..n-1}+c_{n..2*n-1});
    deg':=a->deg first first listForm a;
    p:=applyValues(partition(deg @@ first,l),d->sum(d,(e,c)->c*R0_e));
    if #p==1 then return ret fachom a; -- homogeneous case
    L:=new MutableHashTable; -- list of factorisations
    if o.StopAfter<infinity and (g:=commguess a)=!=null then ( -- last attempt before trying the hard way. slows down StopAfter==infinity so skip
        debugPrint_1("partially commutative, factored as ",g);
        L#(Product{})=true;
        scan(g,f->(
		f':=factorWeylAlgebra(f#0,o);
		LL:=new MutableHashTable;
		scan(keys L, b -> scan(f', f'' -> (
			    scan(f#1,i -> b=combineFac(b,f''));
			    LL#b=true;
			    )));
		L=LL;
		));
        if #L>=o.StopAfter then return ret L;
        );
    k:=sort keys p;
    deg1:=first k;
    deg2:=last k;
    a1:=p#deg1;
    a2:=p#deg2;
    debugPrint_2("top ",a1," bottom ",a2);
    f1:=fachom2 a1;
    f2:=fachom2 a2;
    debugPrint_2("factor as ",f1," ",f2);
    s:=(sortBy deg)(toList \ unique flatten apply(first\l,seqs));
    ss:=set s;
    lst:=sortByFirst flatten for fac1 in f1 list for fac2 in f2 list
    if any(first \ ( listForm (fac1#0*fac2#1) | listForm (fac1#1*fac2#0)), x -> not ss#?x) then continue else ( -- let's not waste time on impossible combos
        deg1:=apply(fac1,deg');
        deg2:=apply(fac2,deg');
        i:=0;
        while i<2 and leadCoefficient fac1#i*fac2#i!=leadCoefficient fac2#i*fac1#i do i+=1; -- annoying edge case: fac1#i ~ fac2#i
        if i<2 then (
            if liftable(fac1#i,coefficientRing R0) or deg1#(1-i)>=deg2#(1-i) then continue;
            (if o.StopAfter===infinity and i==0 then 100 else 0,fac1,fac2,i) -- empirically, i==0 slows down StopAfter==infinity... pulling out small factors on the left is not useful
            ) else (
            if deg1#0>=deg2#0 or deg1#1>=deg2#1 then continue;
            e0:=first\(listForm fac1#0|listForm fac2#0);
            e1:=first\(listForm fac1#1|listForm fac2#1);
            ma:=select(s,x->deg x>deg1#0 and deg x<deg2#0 and all(e1,y->ss#?(x+y))); -- are these bounds optimal?
            mb:=select(s,x->deg x>deg1#1 and deg x<deg2#1 and all(e0,y->ss#?(x+y)));
            (#ma+#mb+1,fac1,fac2,ma,mb)
            )
        );
    debugPrint_2(lst);
    L':={}; -- used for optimisation purposes. list second factor in decomposition into *two* factors
    process:= (pa',pb') -> ( -- returns true if needs to return
	debugPrint_1("factored as ",pa'," times ",pb');
	if any(L',y->pb'%y==0) then (
	    debugPrint_2(pb'," end already encountered");
	    return false;
	    ); -- note that pb' may have other factorisations, but at least it's not irreducible so we can safely ignore it
	-- get rid of constants
	local cfb;
	(cfb,pb')=pullConstant pb'; -- for optimisation purposes
	L'=append(L',pb');
	fa:=factorWeylAlgebra(cfb*pa',o);
	fb:=factorWeylAlgebra(pb',o);
	-- if (#fb==1 and #(fb#0)==1) then -- we can safely ignore factorisable pb' -- though doesn't seem to improve performance
	debugPrint_1("adding to list ",fa," ",fb);
	-- scan(fa, x -> L#(combineFac(precf x,fb#0))=true); -- same remark as above
	scan(fa, x -> scan(fb, y -> L#(combineFac(x,y))=true));
	#L>=o.StopAfter
	);
    for entry in lst do (
        fac1:=entry#1; fac2:=entry#2;
        debugPrint_2("trying with ",fac1#0,"+...+",fac2#0," and ",fac1#1,"+...+",fac2#1);
        if #entry==4 then if entry#3==1 then (
            -- a should be right divisible by fac1#1 = fac2#1
            if a%fac1#1==0 then if process(a//fac1#1,fac1#1) then return ret L;
            ) else (
            -- a should be left divisible by fac1#0 = fac2#0
            aa:=Dtransposition a;
            ff:=Dtransposition fac1#0;
            if aa%ff==0 then if process(fac1#0,Dtransposition(aa//ff)) then return ret L;
            ) else (
            ma:=entry#3; mb:=entry#4;
            debugPrint_2("possible monomials: ",#ma," ",#mb);
            u:=local u; v:=local v; w:=local w;
            vrs:=splice[u_1..u_(#ma),v_1..v_(#mb),w];
            S:=(coefficientRing R0) (monoid append(vrs,MonomialOrder=>Lex)); -- empirically better than grevlex, important that w be last!
            R1:=R0**S;
            scan(#vrs, i -> vrs#i <- R1_(numgens R0+i));
            g1:=map(R0,R1,gens R0 | {numgens S:0}); -- eww
            lift(R1,R0) := o -> (r,R0) -> g1 r;
            lift(Matrix,R1,R0) := o -> (m,R1,R0) -> g1 m;
            g2:=map(S,R1,{numgens R0:0}|gens S); -- eww
            lift(R1,S)  :=  o -> (r,S) -> g2 r;
            lift(Matrix,R1,S) := o -> (m,R1,S) -> g2 m;
            pa := fac1#0 + w*fac2#0 + sum(#ma,i->R0_(ma#i)*u_(i+1));
            pb := w*fac1#1 + fac2#1 + sum(#mb,i->R0_(mb#i)*v_(i+1));
            pp:=pa*pb-w*a;
            eqs:=lift(last coefficients(pp,Variables=>drop(gens R1,-(numgens S))),S);
            I := ideal apply(flatten entries eqs, x -> if x%w_S == 0 then x//w_S else x);
            debugPrint_2("solving ",I);
            -- rather than calling straight decompose, we're going to test a few things first,
            -- empirically faster in most situations
	    if not isMember(1_S,I) then
	    for c in if degree I == 1 then { I } else decompose I do if degree c == 1 then ( -- one might hope that testing unique(degree\c)=={{1}} would be enough, but M2 sucks at simplifying inhomogeneous ideals
		sol:=apply(#vrs,i->S_i%c);
		debugPrint_2("found solution ",sol);
		sol = apply(#vrs,i-> vrs#i => sol#i);
		pa':=lift(sub(pa,sol),R0);
		pb':=lift(sub(pb,sol),R0)*(lift(sub(w,sol),coefficientRing R0))^-1;
		if process(pa',pb') then return ret L;
		)
	    );
        );
    if #L==0 then ( -- a is irreducible
        debugPrint_1("irreducible ",a);
        ret Product{Power{a,1}}
        )
    else ret L -- a is reducible: return list of factorisations
    )

factorWeylAlgebra1 = a -> first factorWeylAlgebra(a,StopAfter=>1) -- shortcut

-- needed because of https://github.com/Macaulay2/M2/issues/3513
kernelFromRREF = m -> (
    r:=entries reducedRowEchelonForm m;
    -- TODO edge cases
    n:=#(r#0);
    s:=for row in r list (p:=position(row,i->i!=0); if p===null then continue else p); -- the columns with leading "1"s
    s':=select(n,i->not isMember(i,s)); -- the other columns
    k:=mutableMatrix(ring m,n,#s');
    --    print(n,s,s',r);
    scan(#s',j->(
	    scan(#s,i-> k_(s#i,j)=-r#i#(s'#j));
	    scan(#s',i-> k_(s'#i,j)=if i==j then 1 else 0);
	    ));
    matrix k
    )

-- guessing commutative subalgebra? works for p_2, p_10, p_12
commguess = a -> (
    l:=listForm a;
    R:=ring a;
    m:=max \ (transpose(first\l)); -- max degrees
    -- s:=drop(seqs(m//2),1); -- m//2 is too restrictive, cf Makar-Limonov ex
    -- s:=drop(drop(seqs m,1),-1); -- exclude "1", m. good but too much
    mx:=2*(sum m)//3; s:=select(drop(seqs m,1),x->plus x<=mx); -- some arbitrary choice in between
    if #s==0 then return;
    mons:=apply(s,e->R_(toList e));
    eqs:=matrix{apply(mons,x->x*a-a*x)};
    eqs1:=lift(last coefficients eqs,coefficientRing R);
    -* -- too slow, cf https://github.com/Macaulay2/M2/issues/3513
    k:=kernel eqs1;
    if rank k == 0 then return;
    lst:=apply(entries transpose gens k, r -> sum(r,mons,times));
    *-
    k:=kernelFromRREF eqs1;
    if rank source k == 0 then return;
    lst:=apply(entries transpose k, r -> sum(r,mons,times));
    debugPrint_1 ("list of guess building blocks ",lst);
    -- below we reimplement kernel because normal kernel strategy unhappy with Weyl algebra
    z:=local z;
    S:=(coefficientRing R)(monoid[z_1..z_(#lst)]);
    RS:=tensor(R,S,MonomialOrder=>Eliminate numgens R);
    I:=ideal apply(#lst,i->S_i-promote(lst#i,RS));
    G:=gb I;
    M:=lift(selectInSubring(1,generators G),S); -- that's the gens of kernel
    IM:=ideal M;
    debugPrint_2("kernel ",IM);
    IMp:=prune IM; -- remove pruning, eventually? but it helps with naive factorisation
    S':=ring IMp/IMp;
    J:=I+ideal promote(a,RS); -- now throw in a
    G':=gb J;
    M':=lift(selectInSubring(1,generators G'),S);
    K:=ideal promote(IM.cache.minimalPresentationMap M',S');
    g:=map(R,S,lst) * IM.cache.minimalPresentationMapInv * map(ring IMp,S');
    pos:=position(K_*,r->ideal r == K);
    if pos=!=null and g (b:=(leadCoefficient a/leadCoefficient g K_pos)*K_pos) == a then (
	debugPrint_2 ("guess ",b); -- one can just try to factor naively first -- certainly should if IMp is zero
	(facs, exps) := rawFactor raw b;
	pr:=toList apply(facs,exps,(f,e)->Power{g promote(f,S'),e});
	debugPrint_2("that is ",VerticalList pr);
	if product apply(pr,f->(f#0)^(f#1)) != a then return; -- should never happen
	if #pr>2 or pr#1#1>1 then (
	    if pr#0#0 == 1 then pr=drop(pr,1);
	    return Product pr; -- success
	    )
	);
    -- try harder
    pr={};
    cs:=decompose K; -- try to break a into pieces
    facs=flatten apply(cs,x->g \ x_*); -- potential factors; not quite right but close enough
    debugPrint_2 ("comps ",cs," facs ",facs);
    for fac in facs do (
	c:=0;
	while a%fac == 0 do ( c+=1; a//=fac; );
	if c>0 then pr=prepend(Power{fac,c},pr);
	);
    if a!=1 then pr=prepend(Power{a,1},pr);
    n:=sum(last\pr);
    if n<=1 or (n==2 and liftable(pr#0#0,coefficientRing R)) then return; -- failed to factor
    Product pr
    )

-- TESTS --------------------------------------------
tst = method()
tst RingElement := x -> ( f := factorWeylAlgebra x; f' := unique (value \ f); assert(#f' == 1 and f'#0 == x); #f )
TEST ///
importFrom(WeylAlgebras,"tst")
R1=makeWA(QQ[x_1])
A=(1+x_1)*(2+dx_1);
p_12=A*(A-1)*(A-2); -- 11 facs (could be fewer if commutation of arbitrary factors is allowed)
assert(tst p_12 == 11)
///

TEST ///
importFrom(WeylAlgebras,"tst")
R2=makeWA(QQ[x_1,x_2])
h_1=(dx_1+1)^2*(dx_1+x_1*dx_2) -- 2 factorisations
assert(tst h_1 == 2)
p_3=(x_1*dx_2+x_2*dx_1)*(3+x_1*dx_1)*(4+x_2*dx_2) -- should give unique fac
assert(tst p_3 == 1)
///

TEST ///
importFrom(WeylAlgebras,"tst")
R3=makeWA(QQ[x_1..x_3])
h_3=x_1*x_2^2*x_3^3*dx_1*dx_2^2+x_2*x_3^3*dx_2 -- homogeneous -- 60 factorisations but really all of them are identical up to permutation of commuting parts
assert(tst h_3 == 1)
///

-- DOCUMENTATION
beginDocumentation()
doc ///
 Node
  Key
    factorWeylAlgebra
    (factorWeylAlgebra,RingElement)
    [factorWeylAlgebra,StopAfter]
  Headline
    factor a Weyl algebra element
  Usage
    factorWeylAlgebra r
  Inputs
    r:
      the ring element
    StopAfter=>Number
      indicates how many factorisations should be computed, default infinity
  Outputs
    :List
      a list of factorisations
  Description
    Text
     Produces all the factorisations of the element @TT "r"@ of a Weyl algebra
     (unless @TT "StopAfter"@ is set to a finite value, in which case the algorithm stops after that
     many factorisations).
    Example
      R = makeWA(QQ[x])
      factorWA(x^5*dx^2+7*x^4*dx+8*x^3-x*dx^2+dx)
    Text
     To reduce their number, two factorisations are considered equivalent if
     they can be related by (1)	switching commuting irreducible factors or (2) switching
     monomials and degree 0 factors; a normal order is chosen where commuting factors are sorted, and
     monomials are pushed to the right/left if they're differential/not.
    Text
     @TT "factorWeylAlgebra"@ uses a variety of factorisation strategies, including the one
     in ``Factoring linear partial differential operators in n variables'',
     Mark Giesbrecht, Albert Heinle and Viktor Levandovskyy, Journal of Symbolic Computation
     Volume 75, July–August 2016, Pages 127-148.
  Caveat
    The ring of @TT "r"@ must have variables in the same order as those created by @TO "makeWeylAlgebra"@.
  SeeAlso
    factorWeylAlgebra1
 Node
  Key    
    factorWeylAlgebra1
  Headline    
    give one factorisation of a Weyl algebra element
  Usage
    factorWeylAlgebra1 r
  Inputs
    r:
      the ring element
  Description
    Text
     Produces one possible factorisation of the element @TT "r"@ of a Weyl algebra.
     This is a shortcut for @TO "factorWeylAlgebra"@ with optional argument @TT "StopAfter=>1"@.
     Note that alternate strategies are used if only a finite number of factorisation is required,
     so it may take much less time to compute, as on this example:
    Example
      R = makeWA(QQ[x,y])
      u=dx^3+x^2-8; v=1/2*dx; U=u^2+4*v; V=u^3+3*(u*v+v*u);  -- V^2==U^3+8
      factorWA1(U*V)     
  SeeAlso
    factorWeylAlgebra
///
