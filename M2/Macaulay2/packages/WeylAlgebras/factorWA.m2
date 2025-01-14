-- Paul Zinn-Justin 2023
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
    shft := e -> map(R,R,apply(n,i->R_i+e#i));
    (facs, exps) := rawFactor raw t;
    facs=apply(facs,x->promote(x,R));
    c:=(facs#0)^(exps#0);
    facs=drop(facs,1); exps=drop(exps,1); -- get rid of constant
    -- if any factor is θ_i+1 we need to split, as annoying as may be
    l:=apply(n,i->1+R_i);
    flatten apply(seqs (m_{0..n-1}+m_{n..2*n-1}), e -> (
	    shft1:=shft apply(n,i->min(e#i-m#i,0));
	    shft2:=shft apply(n,i->-max(e#i-m#i,0));
	    facs1:=shft1\facs;
	    facs2:=shft2\facs;
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
    )  -- used to be: L=L|(times\(fa**fb));


factorWAcache = new CacheTable
factorWA = method(Options=>{StopAfter=>infinity})
factorWA RingElement := o -> a -> (
    if factorWAcache#?a and factorWAcache#a#0>=o.StopAfter then return factorWAcache#a#1;
    debugPrint_1("factoring ",a);
    ret := x -> ( -- things to do before returning
        if class x =!= List then x = if class x === MutableHashTable then keys x else {x};
        factorWAcache#a=(o.StopAfter,x);
	x
        );
    R0:=ring a;
    if liftable(a,coefficientRing R0) then return ret Product{Power{a,1}};
    (cf,a'):=pullConstant a;
    if cf!=1 then return ret apply(factorWA(a',o),x -> prepend(Power{promote(cf,R0),1},x));
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
		f':=factorWA(f#0,o);
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
            --ma:=select(s,x->deg x>deg1a and deg x<deg2a);
            ma:=select(s,x->deg x>deg1#0 and deg x<deg2#0 and all(e1,y->ss#?(x+y))); -- are these bounds optimal?
            --mb:=select(s,x->deg x>deg1b and deg x<deg2b);
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
	fa:=factorWA(cfb*pa',o);
	fb:=factorWA(pb',o);
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

factorWA1 = a -> first factorWA(a,StopAfter=>1) -- shortcut

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
tst RingElement := x -> ( f := factorWA x; f' := unique (value \ f); assert(#f' == 1 and f'#0 == x); #f )
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
end
--


-- test cases
R1=makeWA(QQ[x_1])
-- the hs are ex from the paper, the ps and l are mine
h_0=(x_1^4-1)*x_1*dx_1^2+(1+7*x_1^4)*dx_1+8*x_1^3 -- "h" in the paper. 5 facs
p_0=dx_1*(x_1^2+1)
p_4=(x_1^4+1)*(1+31*dx_1) -- irreducible over QQ! shouldn't factor it
p_5=(x_1^2+5*x_1+1)*(x_1^2+3*x_1+1)*(1+13*dx_1) -- degree>1 solution
p_6=(dx_1+11)*(1-x_1^2) -- should give unique fac despite constant (here, sign) issues
A=(1+x_1)*(2+dx_1);
p_12=A*(A-1)*(A-2); -- 11 facs, would be a lot less if we allowed commutation of *arbitrary* factors, not just irreducible ones.
-- more importantly, shows that the commutative algebra approach won't give all factorisations, can only work with All=>false
-- also, w/o commguess factorWA(All=>false) gives a nicer answer :/
p_13=A^2*(A-3)^2; -- takes forever w/o commguess
u:=dx_1^3+x_1^2-8; v:=1/2*dx_1; U=u^2+4*v; V=u^3+3*(u*v+v*u);  -- Makar-Limanov example. V^2=U^3+8
p_14=U*V; -- takes forever w/o commguess
p_15=(U+1)*(U*V-1); -- takes forever w/o commguess
-- other, trickier ones: U^2, V^2

-- my RL examples:
l={x_1^2*dx_1^14-38*x_1^2*dx_1^13+541*x_1^2*dx_1^12+36*x_1*dx_1^13-3056*x_1^2*dx_1^11-1262*x_1*dx_1^12-1670*x_1^2*dx_1^10+16426*x_1*dx_1^11+306*dx_1^12+82684*x_1^2*dx_1^9-83404*x_1*dx_1^10-9886*dx_1^11-132318*x_1^2*dx_1^8-53512*x_1*dx_1^9+117414*dx_1^10-1060344*x_1^2*dx_1^7+1868604*x_1*dx_1^8-533480*dx_1^9+1610973*x_1^2*dx_1^6-2398308*x_1*dx_1^7-392388*dx_1^8+9644474*x_1^2*dx_1^5-18732144*x_1*dx_1^6+9825804*dx_1^7-881167*x_1^2*dx_1^4+21153468*x_1*dx_1^5-9535908*dx_1^6-46656232*x_1^2*dx_1^3+118955634*x_1*dx_1^4-76099656*dx_1^5-78118936*x_1^2*dx_1^2+7849898*x_1*dx_1^3+54718650*dx_1^4-52706752*x_1^2*dx_1-312216436*x_1*dx_1^2+322264866*dx_1^3-13176688*x_1^2-359131976*x_1*dx_1+103224478*dx_1^2-122354960*x_1-300797280*dx_1-193482184, x_1^3*dx_1^13-39*x_1^3*dx_1^12+4*x_1^2*dx_1^13+580*x_1^3*dx_1^11-123*x_1^2*dx_1^12-3636*x_1^3*dx_1^10+1142*x_1^2*dx_1^11+136*x_1*dx_1^12+1966*x_1^3*dx_1^9+1320*x_1^2*dx_1^10-4632*x_1*dx_1^11+80718*x_1^3*dx_1^8-80496*x_1^2*dx_1^9+58166*x_1*dx_1^10+1088*dx_1^11-213036*x_1^3*dx_1^7+351822*x_1^2*dx_1^8-277008*x_1*dx_1^9-36000*dx_1^10-847308*x_1^3*dx_1^6+745356*x_1^2*dx_1^7-309528*x_1*dx_1^8+447490*dx_1^9+2458281*x_1^3*dx_1^5-6745932*x_1^2*dx_1^6+6687912*x_1*dx_1^7-2304402*dx_1^8+7186193*x_1^3*dx_1^4-3000396*x_1^2*dx_1^5-7946052*x_1*dx_1^6+1044378*dx_1^7-8067360*x_1^3*dx_1^3+55356917*x_1^2*dx_1^4-63026568*x_1*dx_1^5+31570926*dx_1^6-38588872*x_1^3*dx_1^2+38515470*x_1^2*dx_1^3+77541576*x_1*dx_1^4-61768506*dx_1^5-39530064*x_1^3*dx_1-193088420*x_1^2*dx_1^2+342285776*x_1*dx_1^3-179603550*dx_1^4-13176688*x_1^3-315837144*x_1^2*dx_1-65652258*x_1*dx_1^2+320936574*dx_1^3-135531648*x_1^2-693812168*x_1*dx_1+625837114*dx_1^2-411838728*x_1-147286944*dx_1-362090008, 9*x_1^4*dx_1^12-369*x_1^4*dx_1^11-81*x_1^3*dx_1^12-764*x_1^2*dx_1^13+5958*x_1^4*dx_1^10+3582*x_1^3*dx_1^11+30092*x_1^2*dx_1^12-44640*x_1^4*dx_1^9-63270*x_1^3*dx_1^10-457650*x_1^2*dx_1^11-25976*x_1*dx_1^12+106974*x_1^4*dx_1^8+540000*x_1^3*dx_1^9+3064036*x_1^2*dx_1^10+940024*x_1*dx_1^11+512514*x_1^4*dx_1^7-1855926*x_1^3*dx_1^8-4293328*x_1^2*dx_1^9-12962476*x_1*dx_1^10-207808*dx_1^11-2942352*x_1^4*dx_1^6-2993652*x_1^3*dx_1^7-49278960*x_1^2*dx_1^8+76388114*x_1*dx_1^9+6901200*dx_1^10-1741068*x_1^4*dx_1^5+35570664*x_1^3*dx_1^6+161267724*x_1^2*dx_1^7-66869058*x_1*dx_1^8-86332220*dx_1^9+25606665*x_1^4*dx_1^4-20950272*x_1^3*dx_1^5+469532880*x_1^2*dx_1^6-1179842262*x_1*dx_1^7+451498716*dx_1^8+13462407*x_1^4*dx_1^3-265757625*x_1^3*dx_1^4-1558801548*x_1^2*dx_1^5+3096370086*x_1*dx_1^6-266728908*dx_1^7-99531054*x_1^4*dx_1^2+86516262*x_1^3*dx_1^3-4404117004*x_1^2*dx_1^4+8393378502*x_1*dx_1^5-5915806404*dx_1^6-148237740*x_1^4*dx_1+1063638198*x_1^3*dx_1^2+4499833366*x_1^2*dx_1^3-23012211558*x_1*dx_1^4+12326878092*dx_1^5-59295096*x_1^4+1047950064*x_1^3*dx_1+24960407724*x_1^2*dx_1^2-46738243594*x_1*dx_1^3+32319917700*dx_1^4+279534024*x_1^3+28571611880*x_1^2*dx_1+41320491758*x_1*dx_1^2-62419057092*dx_1^3+10689520912*x_1^2+119285406352*x_1*dx_1-112183579004*dx_1^2+58513234360*x_1+30960222720*dx_1+65835842576, 1492992*x_1^7*dx_1^11-50761728*x_1^7*dx_1^10+7464960*x_1^6*dx_1^11+633028608*x_1^7*dx_1^9-220962816*x_1^6*dx_1^10+18019584*x_1^5*dx_1^11-2974040064*x_1^7*dx_1^8+2149908480*x_1^6*dx_1^9-448436736*x_1^5*dx_1^10+32633856*x_1^4*dx_1^11+459044952*x_1^3*dx_1^12+4738995139*x_1^2*dx_1^13-3072577536*x_1^7*dx_1^7-3475685376*x_1^6*dx_1^8+2668640256*x_1^5*dx_1^9-761020416*x_1^4*dx_1^10-18759281664*x_1^3*dx_1^11-187175841597*x_1^2*dx_1^12+63511879680*x_1^7*dx_1^6-62947528704*x_1^6*dx_1^7+17963182080*x_1^5*dx_1^8+3775523328*x_1^4*dx_1^9+302240069472*x_1^3*dx_1^10+2858603072796*x_1^2*dx_1^11+161125834726*x_1*dx_1^12-43517730816*x_1^7*dx_1^5+274543312896*x_1^6*dx_1^6-241878016512*x_1^5*dx_1^7+34988613120*x_1^4*dx_1^8-2263075967040*x_1^3*dx_1^9-19285496328572*x_1^2*dx_1^10-5847457076934*x_1*dx_1^11-593446404096*x_1^7*dx_1^4+544553902080*x_1^6*dx_1^5+426960129024*x_1^5*dx_1^6-355245848064*x_1^4*dx_1^7+5447881445904*x_1^3*dx_1^8+28080085156650*x_1^2*dx_1^9+81016009335836*x_1*dx_1^10+1289006677808*dx_1^11+93713614848*x_1^7*dx_1^3-3402409328640*x_1^6*dx_1^4+3116569789440*x_1^5*dx_1^5+177916055040*x_1^4*dx_1^6+25737397791552*x_1^3*dx_1^7+308952241579098*x_1^2*dx_1^8-482560560969752*x_1*dx_1^9-42931824024900*dx_1^10+2889247076352*x_1^7*dx_1^2-4279003158528*x_1^6*dx_1^3-7513197576192*x_1^5*dx_1^4+5621071154688*x_1^4*dx_1^5-148782083764320*x_1^3*dx_1^6-1062443135708676*x_1^2*dx_1^7+471134764064876*x_1*dx_1^8+539767189042810*dx_1^9+3713722048512*x_1^7*dx_1+15008517070848*x_1^6*dx_1^2-22415260896000*x_1^5*dx_1^3-5558902304256*x_1^4*dx_1^4-86446131708096*x_1^3*dx_1^5-2774665124198820*x_1^2*dx_1^6+7216345506856628*x_1*dx_1^7-2856746878013338*dx_1^8+1405192126464*x_1^7+30125598547968*x_1^6*dx_1+28613790825984*x_1^5*dx_1^2-41871120238080*x_1^4*dx_1^3+1296882745242840*x_1^3*dx_1^4+10234646666917083*x_1^2*dx_1^5-19787230210618744*x_1*dx_1^6+1987107955738338*dx_1^7+14453404729344*x_1^6+93774867250176*x_1^5*dx_1+6875092965888*x_1^4*dx_1^2+682984943562432*x_1^3*dx_1^3+25491014527222475*x_1^2*dx_1^4-49671863784092080*x_1*dx_1^5+36129410931753358*dx_1^6+52691916662784*x_1^5+118558792912896*x_1^4*dx_1-5081048512516032*x_1^3*dx_1^2-31056497630110512*x_1^2*dx_1^3+144853059477715694*x_1*dx_1^4-79077432704220610*dx_1^5+80428971829248*x_1^4-7637776459993344*x_1^3*dx_1-148073678767841400*x_1^2*dx_1^2+275702978123931778*x_1*dx_1^3-190656682008507750*dx_1^4-3090584815118976*x_1^3-163560033575410864*x_1^2*dx_1-265561180783923220*x_1*dx_1^2+392704478479664806*dx_1^3-60345534451679568*x_1^2-714815358414141096*x_1*dx_1+659472804357356094*dx_1^2-342113266967900624*x_1-206021914401892848*dx_1-391912559053553160};
-- for example l#0 is
-- (dx_1-7)^6*(dx_1+1)^2*(dx_1+2)^3 -- silly prefactor times the thing we care about
-- *(-56*x_1 - 14*x_1^2 + 42*dx_1 - 18*x_1*dx_1 - 19*x_1^2*dx_1 + 14*x_1*dx_1^2 - 4*x_1^2*dx_1^2 + x_1^2*dx_1^3)
R2=makeWA(QQ[x_1,x_2])
h_1=(dx_1+1)^2*(dx_1+x_1*dx_2) -- 2 factorisations
scan(1..2,i->th_i=x_i*dx_i)
h_2=(th_1*dx_2+(th_1+3)*th_2+x_2)*((th_1+4)*x_1*dx_2+x_1+(th_1+1)*x_1*x_2) -- 3 factorisations because of monomial moving around
h_4=(x_1^2*dx_1+x_1*x_2*dx_2)*(dx_1*dx_2+dx_1^2*dx_2^2*x_1*x_2) -- same remark as h3
p_1=(x_1*dx_1+2*x_2)*(x_2*dx_2+3*x_1) -- sharing constants right, factorisation should be unique
p_2=(x_1*dx_1+x_2+dx_2)^3 -- example with non radical ideal of eqs
p_3=(x_1*dx_2+x_2*dx_1)*(3+x_1*dx_1)*(4+x_2*dx_2) -- should give unique fac
p_7=(11+3*x_1*dx_1+5*x_2*dx_2)*(1+31*x_1) -- tests edge case
p_8=x_1^5*dx_1+x_1^5-(307/84)*x_1^4*dx_1-(307/84)*x_1^4+(863/168)*x_1^3*dx_1+(863/168)*x_1^3-(24/7)*x_1^2*dx_1-(24/7)*x_1^2+(179/168)*x_1*dx_1+(179/168)*x_1-(5/42)*dx_1-5/42 -- one variable. 2 factorisations
p_9=x_1^4*(1+2*x_2*dx_1+3*x_1*dx_2)*dx_2^4 -- 1 fac. how to guess that here, monomials can be taken out w/o losing any factorisations?
p_10=3/5*(x_1*dx_1)^2*(x_2/13+1+7/31*dx_2)^2 -- this one is greatly improved by optimisation. 6 facs
p_11=(1+3*x_1+4*dx_2)^2*(1+5*x_2+3*dx_1)^2 -- 3 facs. didn't finish with earlier versions of the algo


R3=makeWA(QQ[x_1..x_3])
h_3=x_1*x_2^2*x_3^3*dx_1*dx_2^2+x_2*x_3^3*dx_2 -- homogeneous -- 60 factorisations but really all of them are identical up to permutation of commuting parts

tstall = () -> apply(apply(5,i->h_i) | apply(13,i->p_i),a->timing tst a) -- last should be {5, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 6, 3, 11}

-- random examples for benchmarking
(frame random)#0=(frame random)#0 ++ {Height=>100};
R2c=QQ[a_1..a_2,b_1..b_2]
f=map(R2,R2c,gens R2)
rnd=k->f sum(0..k,i->random(i,R2c)) -- really bad for this algo: unique lowest/highest degree terms
tot=10;
-- examples where algo should be better: lots of terms of same degree
use R2
rnd1=()->random QQ + random QQ * x_1 * dx_1 + random QQ * x_2 * dx_2 + random QQ * x_1 + random QQ * x_2
rnd2=()->random QQ + random QQ * x_1 * dx_1 + random QQ * x_2 * dx_2 + random QQ * x_1 + random QQ * x_2 + random QQ * x_1^2 + random QQ * x_2^2 + random QQ * x_1 * x_2
bnch = () -> (
    timing(for i from 1 to tot do tst (rnd 1*rnd 2)), -- anything beyond rnd 1*rnd 2 is too complicated
    timing(for i from 1 to tot do tst (rnd1()*rnd1()*rnd1())),
    timing(for i from 1 to tot do tst (dx_1*rnd2()*rnd2()))
    )



end

-- visualising polytope of degrees
needsPackage"VectorGraphics"
deg=c->(n:=#c//2; new Deg from (-c_{0..n-1}+c_{n..2*n-1}))
vis = H -> gList splice apply(pairs H, (a,p) -> (Circle{vector a,.1,"fill"=>"red"},GraphicsText{vector a,toString(#p),FontSize=>.5}))
-- ex of use
vis(partition(deg @@ first,listForm p_10))


