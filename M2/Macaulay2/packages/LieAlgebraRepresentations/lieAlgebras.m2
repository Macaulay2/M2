
-- DS: I commented out the following in order to try 
-- importFrom instead, in importsandexports
-- Access hasAttribute, getAttribute
-- debug Core



-- helper functions for semisimple Lie algebras
split = (w,L) -> ( -- split weight of semisimple algebra according to simple parts
    L=prepend(0,accumulate(plus,0,L)); -- why does accumulate suck
    apply(#L-1,i->w_(toList(L#i..L#(i+1)-1)))
    )
unsplit = (v,L,i) -> ( -- from a weight of one summand to the whole
    toList(sum(i,j->L#j):0) | v | toList(sum(i+1..#L-1,j->L#j):0)
    )

-----------------------------------------------------------------------
-- LieAlgebra= {
--   LieAlgebraRank => ZZ | Sequence, dim of Cartan subalgebra
--   RootSystemType => String | Sequence, type A through G
--   }

LieAlgebra = new Type of HashTable  
LieAlgebra.GlobalAssignHook = globalAssignFunction
LieAlgebra.GlobalReleaseHook = globalReleaseFunction

cartanMatrixQQ = a -> promote(cartanMatrix a,QQ)
characterRing = method()
characterRing (String,ZZ) := memoize( (type,m) -> (
    Q:=sum \ entries inverse cartanMatrixQQ(type,m);
    l:=lcm(denominator\Q);
    Q=apply(Q,q->lift(q*l,ZZ));
    x:=getSymbol "x";
    ZZ(monoid [x_1..x_m,Inverses=>true,MonomialOrder=>{Weights=>Q,Lex}])
    ))
characterRing (Sequence,Sequence) := memoize( (type,m) -> if #m == 0 then ZZ[Inverses=>true,MonomialOrder=>Lex] else ( -- tensor apply(type,m,characterRing))
	R := tensor apply(type,m,characterRing);
	vrs := split(gens R,m);
	R#"maps" = apply(#m, i -> map(R,characterRing(type#i,m#i),vrs#i)); -- ideally this should be generated automatically by tensor
	R
	))

characterRing LieAlgebra := g -> characterRing(g#"RootSystemType",g#"LieAlgebraRank")

-- helpers
new LieAlgebra from Sequence := (T,s) -> (
    emb:=if #s>2 then s#2 else hashTable {}; -- note that we can't include the Lie algebra itself because this would create a loop...
    subs:=new MutableList;
    for h in keys emb do (
        -- find possible sub/supalgebras from existing ones
        F:=emb#h;
        l:=h.cache#"Subalgebras";
        for k in l do if not emb#?k then (
            G:=k#"Embeddings"#h;
            -- g == k ?
            if F==G then return k;
            -- g < k ?
            H:=F//G;
            if F==G*H then emb=merge(emb,hashTable{k=>H},last);
            -* -- k < g ? not much we can do about it. give user a warning?
            H:=G//F;
            if G==F*H then print("embedding detected - please define your algebras in the opposite order");
            *-
            );
        );
    g := new LieAlgebra from {
	"RootSystemType"=>s#0,
	"LieAlgebraRank"=>s#1,
	"Embeddings"=>emb,
	cache => new CacheTable from { "Subalgebras" => subs }
	};
    scan(keys emb, h -> ( l:=h#cache#"Subalgebras"; l#(#l)=g )); -- we also record g in the bigger algebra
    g
    )
-- ...instead we define this (internally)
supalgebras = g -> hashTable append(pairs g#"Embeddings",g=>id_(ZZ^(plus g#"LieAlgebraRank"))) -- sup-Lie algebras including itself

simpleLieAlgebra = method(
    TypicalValue => LieAlgebra
    )
simpleLieAlgebra(String,ZZ) := (type,m) -> (
    if not isSimple(type,m) then (
    	if not member(type,{"A","B","C","D","E","F","G"}) then error "The simple Lie algebras over the complex numbers have types A, B, C, D, E, F, or G";
    	if type=="A" and m<= 0 then error "The rank for type A must be >= 1.";
    	if type=="B" and m<= 1 then error "The rank for type B must be >= 2.";
    	if type=="C" and m<= 1 then error "The rank for type C must be >= 2.";
    	if type=="D" and m<= 2 then error "The rank for type D must be >= 3.";
    	if type=="E" and not member(m,{6,7,8}) then error "The rank for type E must be 6, 7, or 8.";
    	if type=="F" and m!=4 then error "The rank for type F must be 4.";
    	if type=="G" and m!=2 then error "The rank for type G must be 2.";
	);
    new LieAlgebra from (type,m)
    )

fraktur := hashTable { ("A",𝔞),("B",𝔟),("C",𝔠),("D",𝔡),("E",𝔢),("F",𝔣),("G",𝔤) }
describe1 := (type,m) -> (hold fraktur#(type))_m
describe LieAlgebra := g -> Describe (
    if isSimple g then describe1(g#"RootSystemType",g#"LieAlgebraRank")
     else DirectSum apply(g#"RootSystemType",g#"LieAlgebraRank",describe1)
     )

expression LieAlgebra := g -> (
    if hasAttribute(g,ReverseDictionary) then expression getAttribute(g,ReverseDictionary)
    else unhold describe g
    )
net LieAlgebra := net @@ expression;
texMath LieAlgebra := texMath @@ expression;
toString LieAlgebra := toString @@ expression;
toExternalString LieAlgebra := toString @@ describe;

LieAlgebra ++ LieAlgebra := directSum
directSum LieAlgebra := identity
LieAlgebra.directSum = args -> if #args == 1 then args#0 else (
    subs := applyKeys(supalgebras args#0,sequence);
    scan(1..#args-1, i -> subs = combine(subs,supalgebras args#i,append,directSum,identity)); -- collisions shouldn't occur. we don't directSum yet the keys to avoid infinite loop
    new LieAlgebra from (
	join apply(args, g -> sequence g#"RootSystemType"),
	join apply(args, g -> sequence g#"LieAlgebraRank"),
	applyPairs(subs,(s,m)->if s!=args then (directSum s,m))
	)
    )

rank LieAlgebra := g -> plus sequence g#"LieAlgebraRank"

isSimple = method(TypicalValue => Boolean)
isSimple (String,ZZ) := (type,m) -> (
    (type=="A" and m>=1)
    or ((type=="B" or type=="C") and m>=2)
    or (type=="D" and m>=3)
    or (type=="E" and m>=6 and m<=8)
    or (type=="F" and m==4)
    or (type=="G" and m==2)
    )
isSimple LieAlgebra := g -> class g#"RootSystemType" === String and class g#"LieAlgebraRank" === ZZ and isSimple(g#"RootSystemType",g#"LieAlgebraRank") -- should we test each time?

dynkinDiagram = method(TypicalValue => Net)
dynkinA := (l,m,flag) -> stack ( -- flag = part of diagram
    (if flag then "---" else "") | demark("---",m-l+1:"o"),
    concatenate apply(l..m,i->if i==l and not flag then toString l else pad(4,toString i))
    )
dynkinDiagram (String,ZZ,ZZ) := (type,m,shift) -> if not isSimple(type,m) then error "can only draw simple Lie algebra Dynkin diagram" else (
    if type=="A" then dynkinA (1+shift,m+shift,false)
    else if type=="B" then dynkinA (1+shift,m-1+shift,false) | ("=>=o"||pad(4,toString(m+shift)))
    else if type=="C" then dynkinA (1+shift,m-1+shift,false) | ("=<=o"||pad(4,toString(m+shift)))
    else if type=="D" then dynkinA (1+shift,m-2+shift,false) | ((" o"|toString(m-1+shift))||"/"||""||"\\"||(" o"|toString(m+shift)))^2
    else if type=="E" then ("        o "|toString(2+shift))||"        |"|| (dynkinA (1+shift,1+shift,false)|dynkinA(3+shift,m+shift,true))
    else if type=="F" then dynkinA (shift+1,shift+2,false) | ("=>=o---o"||(pad(4,toString(3+shift))|pad(4,toString(4+shift))))
    else if type=="G" then "o≡<≡o"||(toString(shift+1)|pad(4,toString(shift+2)))
    )
dynkinDiagram (String,ZZ) := (type,m) -> dynkinDiagram(type,m,0)
dynkinDiagram LieAlgebra := g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if isSimple g then dynkinDiagram(type,m) else (
    	L:=prepend(0,accumulate(plus,0,m)); -- why does accumulate suck
    	horizontalJoin between("   ",apply(#m,i->dynkinDiagram(type#i,m#i,L#i)))
	)
    )

LieAlgebra == LieAlgebra := (g,h)-> g===h

-- helper function: gives the type of g in a canonical form
isomClass := g -> (
    l:=transpose {toList sequence g#"RootSystemType",toList sequence g#"LieAlgebraRank"};
    l=apply(l,x->if x=={"D",3} then {"A",3} else if x=={"C",2} then {"B",2} else x); -- low rank isomorphisms
    sort l
    )

isIsomorphic(LieAlgebra,LieAlgebra) := o -> (g,h) -> isomClass g === isomClass h

LieAlgebra _ ZZ := (g,n) -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if class m =!= Sequence or n<0 or n>=#m then error "invalid summand";
    r:=toList(sum(n,i->m#i)..sum(n+1,i->m#i)-1);
    new LieAlgebra from (
	type#n,
	m#n,
	applyValues(supalgebras g, e -> e_r)
	)
    )

LieAlgebra _* := g -> (
    m:=g#"LieAlgebraRank";
    if class m =!= Sequence then error "invalid summand";
    apply(#m,i->g_i)
    )

dualCoxeterNumber = method(
    TypicalValue => ZZ
    )
dualCoxeterNumber(String,ZZ) := memoize((type,m) -> (--see Appendix 13.A, [DMS]
    if type == "A" then return m+1;
    if type == "B" then return 2*m-1;
    if type == "C" then return m+1;
    if type == "D" then return 2*m-2;
    if type == "E" and m==6 then return 12;
    if type == "E" and m==7 then return 18;
    if type == "E" and m==8 then return 30;
    if type == "F" then return 9;
    if type == "G" then return 4
    ))
dualCoxeterNumber(LieAlgebra) := (g) -> (--see Appendix 13.A, [DMS]
    if not isSimple g then error "Lie algebra not simple";
    dualCoxeterNumber(g#"RootSystemType",g#"LieAlgebraRank")
    )


highestRoot = method(
    TypicalValue => List
    )
highestRoot(String,ZZ) := memoize((type, m) -> (--see Appendix 13.A, [DMS]
    if type == "A" and m==1 then return {2};
    if type == "A" and m >= 2 then return flatten {{1}, apply(m-2,i->0),{1}};
    if type == "B" and m==2 then return flatten {0,2};
    if type == "B" and m>=3 then return flatten {{0},{1}, apply(m-2,i->0)};
    if type == "C" then return flatten {{2}, apply(m-1,i->0)};
    if type == "D" and m==3 then return {0,1,1};
    if type == "D" and m>=4 then return flatten {{0},{1}, apply(m-2,i->0)};
    --July 2011: changed numbering of nodes in Dynkin diagram to match WeylGroups
    if type == "E" and m==6 then return {0,1,0, 0,0,0};
    if type == "E" and m==7 then return {1,0,0,0, 0,0,0};
    if type == "E" and m==8 then return {0,0,0,0, 0,0,0,1};
    if type == "F" then return {1,0,0,0};
    if type == "G" then return {0,1}
))

highestRoot(Sequence,Sequence):=memoize((type,m)-> join apply(type,m,highestRoot))

highestRoot(LieAlgebra) := (g) -> highestRoot(g#"RootSystemType",g#"LieAlgebraRank")

starInvolution = method()
starInvolution(String,ZZ,List) := (type, m, w) ->  ( N:=#w;
    if type == "A" then return reverse w;
    if type == "B" or type == "C" or type == "F" or type == "G" then return w;
    if type == "E" and m!= 6 then return w;
    if type == "D" and even(m) then return w;
    if type == "D" and odd(m) then (x:=w;
        return append(drop(x,{#x-2,#x-2}),w_(#w-2)));
    if type == "E" and m== 6 then return {w_5,w_1,w_4,w_3,w_2,w_0};
    )
starInvolution(Sequence,Sequence,List) := (type,m,w) -> (
    w = split(w,m);
    flatten apply(#w, i -> starInvolution(type#i,m#i,w#i))
)

starInvolution(LieAlgebra,List) := (g,v) -> starInvolution(g#"RootSystemType",g#"LieAlgebraRank",v)
starInvolution(LieAlgebra,Vector) := (g,v) -> starInvolution(g,entries v)

starInvolution(Vector,LieAlgebra) :=
starInvolution(List,LieAlgebra) := (v,g) -> starInvolution(g,v) -- for backwards compat

-- shorthand notation
scan(pairs fraktur, (let,sym) ->
    globalAssign(sym, new ScriptedFunctor from { subscript => n -> simpleLieAlgebra(let,n), symbol texMath => "\\mathfrak "|replace(".","\\L$&",let)})
    )

LieAlgebra#AfterPrint = g -> (
    if isSimple g then "simple ",
    class g,
    if #(g#"Embeddings")>0 then (
	lst := keys g#"Embeddings";
	mins := select(lst, h -> not any(lst, k -> k#"Embeddings"#?h)); -- find minimal elements
	", subalgebra of ",
	toSequence between(", ",mins)
	)
 )

blocks = C -> ( -- given a Cartan (or adjacency) matrix, decompose into irreducible blocks
    n:=numRows C;
    L:=toList(0..n-1);
    B:={};
    while #L>0 do (
	-- start a new block
	i:=first L; L=drop(L,1);
	b:={i}; j:=0;
	while j<#b do (
	    L':=select(L,k->C_(b#j,k)!=0); -- we're assuming undirected adjacency or Cartan
	    b=b|L';
	    scan(L',k->L=delete(k,L));
	    j=j+1;
	    );
	B=append(B,b);
	);
    B
)

lieTypeFromCartan := C -> ( -- used internally. returns (type,m,order) where order is permutation of rows/cols
    -- in principle one could conceive not permuting at all but it would require some rewrite (positiveRoots, etc)
    B:=blocks C;
    type':=(); m':=(); L:={}; -- L is permutation of rows/columns to match normal Cartan matrix
    scan(B, b -> (
	    c:=C^b_b;
	    n:=numRows c;
	    -- first pass, covers 99% of cases
	    t:=scan("A".."G",t->if c === (try cartanMatrix(t,n)) then break t);
	    if t === null then (
		-- let's try harder
		local c';
		t=scan("A".."G",t->(
			c'=try cartanMatrix(t,n);
			if c'=!=null and det c == det c' and sort sum entries c == sort sum entries c' -- fun fact: characterizes uniquely
			then break t;
			));
		if t === null then error ("not the Cartan matrix of a semi-simple Lie algebra");
		-- just try every permutation, damnit
		p:=scan(permutations n,p->if c_p^p==c' then break p);
		if p === null then error ("not the Cartan matrix of a semi-simple Lie algebra");
		b=b_p;
		);
	    type'=append(type',t); m'=append(m',n);
	    L=L|b;
	    ));
    (type',m',L)
    )

new LieAlgebra from Matrix := (T,C) -> ( -- define a Lie algebra based on its Cartan matrix
    if numColumns C == 0 then return new LieAlgebra from ((),());
    (type,m,L):=lieTypeFromCartan C;
    h:=directSum apply(type,m,simpleLieAlgebra); -- lazy though avoids unsequence, worrying about rings etc
    assert(cartanMatrix h == C_L^L);
    h
    )

subLieAlgebra = method ( TypicalValue => LieAlgebra )

subLieAlgebra (LieAlgebra, List) := (g,S) -> subLieAlgebra(g,if #S==0 then map(ZZ^(rank g),0,0) else matrix transpose apply(splice S,s ->
	if class s === ZZ and s>0 and s<=rank g then apply(rank g, j -> if j+1 == s then 1 else 0)
	else if class s === ZZ and s==0 then entries lift(-inverse cartanMatrixQQ g*vector highestRoot g,ZZ)
	else if instance(s,Vector) and rank class s == rank g then entries s
	else if class s === List and #s == rank g then s
	else error "wrong argument"))

-*
    -- identify the sub-Dynkin diagram
    S=deepSplice S;
    S=apply(S,i->i-1);
    C:=(cartanMatrix g)^S_S;
    h:=new LieAlgebra from C;
    )
*-

subLieAlgebra (LieAlgebra,Matrix) := (g,M) -> ( -- matrix of coroots
    if ring M =!= ZZ then try M=lift(M,ZZ) else error "matrix must be integer";
    -- in the simply laced case it'd be simply transpose M * cartanMatrix g * M. in general have to work harder
    if numRows M != rank g then error "wrong size of coroots";
    G := transpose M * inverse quadraticFormMatrix g * M; -- new inverse quadratic form <coroot_i|coroot_j>
    D := diagonalMatrix apply(numColumns M,i->2/G_(i,i)); -- inverse square norm of new simple coroots
    C := lift(D * G,ZZ);
    (type,m,L):=lieTypeFromCartan C;
    M=M_L; -- permuted matrix of coroots
    if M == id_(ZZ^(rank g)) then return g; -- better this way, no weirdness of defining a new g subalgebra of g
    new LieAlgebra from (
	unsequence type,
	unsequence m,
	applyValues(supalgebras g, A -> A*M)
	)
    )

subLieAlgebra(LieAlgebra,String) := (g,s) -> (
    if s =!= "principal" then error "only principal subalgebra predefined";
    if g#"RootSystemType"==="A" and g#"LieAlgebraRank"===1 then return g; -- better this way, no weirdness of defining a new g subalgebra of g
    M := lift(2*inverse promote(cartanMatrix g,QQ)*matrix apply(rank g,i->{1}),ZZ); -- 2 rho^v = 2 sum of fundamental coweights
    new LieAlgebra from (
	"A",
	1,
	applyValues(supalgebras g, A -> A*M)
	)
    )

embedding = method ( TypicalValue => Matrix )
embedding(LieAlgebra,LieAlgebra) := (g,h) -> (
    l:=supalgebras g;
    if l#?h then l#h else error "not a Lie subalgebra"
    )



