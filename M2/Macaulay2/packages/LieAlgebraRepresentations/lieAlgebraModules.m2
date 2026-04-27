

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- The LieAlgebraModule type
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


-- LieAlgebraModule= {
--   LieAlgebra => 
--   }
--Functions: weights, dimension, **

LieAlgebraModule = new Type of HashTable 
LieAlgebraModule.GlobalAssignHook = globalAssignFunction
LieAlgebraModule.GlobalReleaseHook = globalReleaseFunction
LL = new ScriptedFunctor from { subscript => w -> g -> irreducibleLieAlgebraModule(g,w) }
LL.texMath = ///{\mathcal L}///

describe LieAlgebraModule := M -> Describe (
    dec := M#"DecompositionIntoIrreducibles";
    g := Parenthesize expression M#"LieAlgebra";
    if #dec == 0 then expression 0
    else DirectSum apply(sort pairs dec,(v,mul) -> ((expression LL)_(unsequence toSequence v) g)^mul)
    )
expression LieAlgebraModule := M -> if hasAttribute(M,ReverseDictionary) then expression getAttribute(M,ReverseDictionary) else unhold describe M;

net LieAlgebraModule := net @@ expression
texMath LieAlgebraModule := texMath @@ expression
toString LieAlgebraModule := toString @@ expression;
toExternalString LieAlgebraModule := toString @@ describe;

-- helper
new LieAlgebraModule from Sequence := (T,s) -> new LieAlgebraModule from {
    "LieAlgebra" => s#0,
    "DecompositionIntoIrreducibles" => if class s#1 === VirtualTally then s#1 else new VirtualTally from s#1,
    cache => new CacheTable
    }

--simpleLieAlgebra LieAlgebraModule := M -> M#"LieAlgebra" -- no longer works now Lie algebras aren't always simple

LieAlgebraModule_ZZ := (M,i) -> irreducibleLieAlgebraModule(M#"LieAlgebra",(sort keys M#"DecompositionIntoIrreducibles")#i)
LieAlgebraModule_* := M -> apply(sort keys M#"DecompositionIntoIrreducibles", v -> irreducibleLieAlgebraModule(M#"LieAlgebra",v))
LieAlgebraModule_List := (V,w) -> (V#"DecompositionIntoIrreducibles")_w
LieAlgebraModule_Vector := (V,w) -> V_(entries w)
LieAlgebraModule_LieAlgebraModule := (V,W) -> (
	if not isIrreducible W then error "last module must be irreducible";
    	V_(first keys W#"DecompositionIntoIrreducibles")
    )

isIrreducible = method()
isIrreducible LieAlgebraModule := M -> values M#"DecompositionIntoIrreducibles" == {1}

LieAlgebraModule ^ ZZ :=
LieAlgebraModule ^ QQ := (M,q) -> (
    if q==1 then M
    else new LieAlgebraModule from (
	M#"LieAlgebra",
	if q==0 then {} else applyValues(M#"DecompositionIntoIrreducibles", a -> try lift(a*q,ZZ) else error "multiplicity not integer")
	)
)

LieAlgebraModule#AfterPrint = M -> (
    if isIrreducible M then "irreducible "
    else if any(values M#"DecompositionIntoIrreducibles",a->a<0) then "virtual ",
    class M,
    " over ",
    M#"LieAlgebra"
 )

trivialModule = method(TypicalValue => LieAlgebraModule)
trivialModule LieAlgebra := g -> irreducibleLieAlgebraModule(toList(rank g:0),g)

zeroModule = method(TypicalValue => LieAlgebraModule)
zeroModule LieAlgebra := g -> new LieAlgebraModule from (g,{})


LieAlgebraModule ^** ZZ := (M, n) -> M.cache#(symbol ^**, n) ??= (
	if n<0 then "error nonnegative powers only";
    	if n==0 then trivialModule M#"LieAlgebra"
    	else if n==1 then M
    	else M**(M^**(n-1)) -- order matters for speed purposes
    )

-*
-- the implementation below seems more reasonable but it's actually slower in most circumstances
LieAlgebraModule ^** ZZ := LieAlgebraModule => (M, n) -> BinaryPowerMethod(M, n, tensor,
    M -> trivialModule M#"LieAlgebra",
    M -> error "LieAlgebraModule ^** ZZ: expected non-negative integer")
*-

adjointWeight := (type,m) -> splice (
    if type == "A" then if m==1 then {2} else {1,m-2:0,1}
    else if type == "B" then if m==2 then {0,2} else {0,1,m-2:0}
    else if type == "C" then {2,m-1:0}
    else if type == "D" then if m==3 then {0,1,1} else {0,1,m-2:0}
    else if type == "E" then if m==6 then {0,1,4:0} else if m==7 then {1,6:0} else {7:0,1}
    else if type == "F" then {1,3:0}
    else if type == "G" then {0,1}
    )

adjointModule = method(TypicalValue => LieAlgebraModule)
adjointModule LieAlgebra := g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if isSimple g then irreducibleLieAlgebraModule(g,adjointWeight(type,m))
    else new LieAlgebraModule from (g, tally apply(#m, i -> unsplit(adjointWeight(type#i,m#i),m,i)))
    )

standardWeight :=(m) -> apply(m, i -> if i==0 then 1 else 0);

standardModule = method(TypicalValue => LieAlgebraModule)
standardModule LieAlgebra := g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if isSimple g then irreducibleLieAlgebraModule(g,standardWeight(m))
    else new LieAlgebraModule from (g, tally apply(#m, i -> unsplit(standardWeight(m#i),m,i)))
    )


dim LieAlgebra := g -> dim adjointModule g

starInvolution LieAlgebraModule := M -> (
    g:=M#"LieAlgebra";
    new LieAlgebraModule from (
    	g,
    	applyKeys(M#"DecompositionIntoIrreducibles", v -> starInvolution(g,v))
	)
    )
dual LieAlgebraModule := {} >> o -> lookup(starInvolution,LieAlgebraModule)



isIsomorphic(LieAlgebraModule,LieAlgebraModule) := o -> (V,W) -> V===W
LieAlgebraModule == LieAlgebraModule := (V,W) -> V===W

LieAlgebraModule == ZZ := (M,n) -> if n=!=0 then error "attempted to compare module to nonzero integer" else #(M#"DecompositionIntoIrreducibles") == 0

directSum LieAlgebraModule := identity
LieAlgebraModule.directSum = args -> (
    if not same apply(args, M -> M#"LieAlgebra") then error "modules must be over the same Lie algebra";
    new LieAlgebraModule from (
	(first args)#"LieAlgebra",
	sum(args,M->M#"DecompositionIntoIrreducibles")
	)
)
LieAlgebraModule ++ LieAlgebraModule := directSum

ωsub := i -> Subscript{symbol ω,i};
ω=new ScriptedFunctor from { subscript => ωsub }
irreducibleLieAlgebraModule = method(
    TypicalValue => LieAlgebraModule
    )
irreducibleLieAlgebraModule(LieAlgebra,List) := (g,v) -> (
    v = deepSplice v;
    if #v != rank g or not all(v, a -> class a === ZZ) then error "invalid highest weight";
    new LieAlgebraModule from (g,{v => 1})
    )
irreducibleLieAlgebraModule(LieAlgebra,VisibleList) := (g,v) -> irreducibleLieAlgebraModule(g,toList v)
irreducibleLieAlgebraModule(LieAlgebra,Vector) := (g,v) -> irreducibleLieAlgebraModule(g,entries v)
irreducibleLieAlgebraModule(LieAlgebra,ZZ) := (g,v) -> irreducibleLieAlgebraModule(g,{v})
irreducibleLieAlgebraModule(LieAlgebra,Expression) := (g,v) -> (
        ω.subscript = i -> apply(rank g,j->if j+1==i then 1 else 0 );
        irreducibleLieAlgebraModule(g,first(value v,ω.subscript=ωsub))
    )
irreducibleLieAlgebraModule(Thing,LieAlgebra) := (v,g) -> irreducibleLieAlgebraModule(g,v)

-*-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Private functions for LieAlgebraModule
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

We implement the Lie theoretic ingredients needed to compute the weights in an irreducible Lie algebra module and their multiplicities
We need: 
--a list of the positive roots
--the ability to compute casimirScalars
---->To get casimirScalars, we need the so-called quadratic form matrix, which can be looked up or computed from the Cartan matrix

Cartan matrices and the Killing form are also implemented in the WeylGroups package.  I am using my own 
implementations because I want the Cartan matrix over QQ (so I can invert it) and so that the Killing form is scaled to make 
(theta,theta) = 2, where theta is the highest root.  This is a popular convention in the conformal blocks literature that is not used in WeylGroups. 

To avoid shadowing, I have named my function cartanMatrixQQ

PZJ: actually there's so much shadowing already...

*-

cartanMatrix = method ( TypicalValue => Matrix )

cartanMatrix LieAlgebra := g -> cartanMatrix (g#"RootSystemType",g#"LieAlgebraRank")

cartanMatrix(Sequence,Sequence) := memoize((type,m) -> directSum apply(type,m,cartanMatrix))

cartanMatrix (String,ZZ) := memoize((type, m) -> (
    if not isSimple(type,m) then error "not simple type";
    M:={};
    if type=="A" then (
        return matrix apply(m, i-> apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0))
    );
    if type=="B" then (
        M = apply(m-2, i ->  apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0));
        M = append(M, apply(m, j -> if j==(m-2)-1 then -1 else if j==(m-2)then 2 else if j==(m-2)+1 then -2 else 0));
        M = append(M, apply(m, j -> if j==(m-1)-1 then -1 else if j==(m-1) then 2 else if j==(m-1)+1 then -1 else 0));
        return matrix M
    );
    if type=="C" then (
        M = apply(m-2, i -> apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0));
        M = append(M, apply(m, j -> if j==m-2-1 then -1 else if j==m-2 then 2 else if j==m-2+1 then -2 else 0));
        M = append(M, apply(m, j -> if j==m-1-1 then -1 else if j==m-1 then 2 else if j==m-1+1 then -1 else 0));
        return transpose matrix M
    );
    if type=="D" then (
        M = apply(m-3, i -> apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0));
        M = append(M,apply(m, j -> if j==m-3-1 then -1 else if j==m-3 then 2 else if j==m-3+1 then -1 else if j==m-3+2 then -1 else 0));
        M = append(M,apply(m, j -> if j==m-2 then 2 else if j==m-2-1 then -1 else 0));
        M = append(M,apply(m, j -> if j==m-1 then 2 else if j==m-1-2 then -1 else 0));
        return matrix M
    );
    if type=="E" and m==6 then (
        return matrix {{2, 0, -1, 0, 0, 0}, {0, 2, 0, -1, 0, 0}, {-1, 0, 2, -1, 0, 0}, {0, -1, -1, 2, -1, 0}, {0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, -1, 2}});
    if type=="E" and m==7 then (
	return matrix {{2, 0, -1, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0}, {0, -1, -1, 2, -1, 0, 0}, {0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, -1, 2}});
    if type=="E" and m==8 then (
	return matrix {{2, 0, -1, 0, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0, 0}, {0, -1, -1, 2, -1, 0, 0, 0}, {0, 0, 0, -1, 2, -1, 0, 0}, {0, 0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, 0, -1, 2}});
    if type == "F" then return matrix({{2,-1,0,0},{-1,2,-2,0},{0,-1,2,-1},{0,0,-1,2}});
    if type == "G" then return matrix({{2,-1},{-3,2}});
    ))


--We code what Di Francesco, Mathieu, and Senechal call the quadratic form matrix
--For types A,D,E, it is the inverse of the Cartan matrix.  See paragraph 1, [DMS] p. 498 and (13.51), [DMS] p. 499 
--For the other types Appendix 13.A, [DMS]

quadraticFormMatrix = method ( TypicalValue => Matrix )

quadraticFormMatrix LieAlgebra := g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    quadraticFormMatrix (type,m) 
    )

quadraticFormMatrix (Sequence,Sequence) := memoize((type,m) -> directSum apply(type,m,quadraticFormMatrix))

quadraticFormMatrix (String,ZZ) := memoize((type, m) -> ( M:={};
    if type=="A" or type =="D" or type=="E" then return (cartanMatrixQQ(type,m))^-1;
    if type =="B" then (
        M=apply(m-1, i -> append(apply(m-1, j -> if j+1<=i+1 then 2*(j+1) else 2*(i+1 )),i+1));
	M = append(M,append(apply(m-1,j->j+1),m/2));
	return (1/2)*matrix(M) 
	);
    if type =="C" then (
	M=apply(m, i -> apply(m, j -> if j+1<=i+1 then (j+1)/1 else (i+1 )));
	return (1/2)*matrix(M)
	);
    if type =="F" then return matrix {{2,3,2,1},{3,6,4,2},{2,4,3,3/2},{1,2,3/2,1}};
    if type =="G" then return matrix {{2/3,1},{1,2}}
    ))

killingForm = method(
    TypicalValue => QQ
    )
killingForm(Sequence,Sequence,List,List) :=
killingForm(String,ZZ,List,List) := memoize((type, m, v,w) -> (
    (matrix{v}*quadraticFormMatrix(type,m)*matrix transpose{w})_(0,0)
))
--killingForm(String,ZZ,Vector,Vector) := (type,m,v,w) -> (transpose matrix v *quadraticFormMatrix(type,m)*w)_0
killingForm(LieAlgebra,List,List) := (g,v,w) -> (matrix{v}*quadraticFormMatrix g*matrix transpose{w})_(0,0)
killingForm(LieAlgebra,Vector,Vector) := (g,v,w) -> (transpose matrix v *quadraticFormMatrix g*w)_0


--This function returns the weights in the Weyl alcove
weylAlcove = method(
    TypicalValue => List
    )     
weylAlcove(String,ZZ,ZZ) := memoize((type, m, l) -> ( pl:={};
    if l==0 then return {apply(m, i -> 0)};
    if m==1 then return apply(l+1,i->{i});
    if type=="A" or type == "C" then (
        pl={{append(apply(m-1, i -> 0),l)}};
        for k from 0 to l-1 do (
            pk:=weylAlcove(type,m-1,l-k);
            pk=apply(#pk, q -> append(pk_q,k));
            pl=append(pl,pk));
        return sort flatten pl
    );
    if type != "A" and type != "C" then (
        pl=weylAlcove("A",m,l);    
	Theta :=highestRoot(type,m);
	answer:=delete(null, apply(#pl, i -> if killingForm(type, m, pl_i, Theta) <= l then pl_i));
        return sort answer
    )
))

weylAlcove(LieAlgebra,ZZ) := (g,l)-> if not isSimple g then error "Lie algebra not simple" else weylAlcove(g#"RootSystemType",g#"LieAlgebraRank",l)

weylAlcove(ZZ,LieAlgebra) := (l,g) -> weylAlcove(g,l)

--For definitions and formulas of Casimir scalars, see (13.127), [DMS] p. 512
--For the definition and formula for rho, see: (13.46), [DMS] p. 499
    
casimirScalar = method(
    TypicalValue => QQ
    )
casimirScalar(Sequence,Sequence,List) :=
casimirScalar(String,ZZ,List) := (type, m, w) -> (
    rho:=apply(plus sequence m,h->1/1);
    killingForm(type,m,w,w) + 2*killingForm(type,m,w,rho)
)
casimirScalar(LieAlgebra,List) := (g, w) -> casimirScalar(g#"RootSystemType",g#"LieAlgebraRank",w)
casimirScalar(LieAlgebraModule) := (M) -> (
    if not isIrreducible M then error "Casimir scalar on irreducible modules only";
    g:=M#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    v:=first keys M#"DecompositionIntoIrreducibles";
    casimirScalar(type,m,v)
)

simpleRoots = method(
    TypicalValue => List
)
  
simpleRoots(String,ZZ) := memoize((type,m) -> (
    entries cartanMatrix(type,m)
))

simpleRoots(LieAlgebra):= memoize((g) -> entries cartanMatrix g)


-- Implement formula from de Graaf, page 96
-- Apply in de Graaf's algorithm to w = lambda-mu
level = (w,type,m) -> (
    -- Write lambda-mu in the basis of simple roots
    M:=matrix apply(simpleRoots(type,m), a -> 1/1*a);
    v:=(inverse transpose M)*(transpose matrix {w});
    -- Sum the coefficients
    lift(sum flatten entries v,ZZ)
);


-- May 2025: change positive roots to put in lex-level order
-- Start with the old function (now called "unorderedPositiveRoots")
-- then put the output in order
positiveRoots = method(
    TypicalValue => List
)

--In Freudenthal's formula, we need to sum over the positive roots
unorderedPositiveRoots = (type,m) -> (
    simpleroots:=simpleRoots(type,m);
    answer:={};
    answer1:={};
    es:={};
    es2:={};
    em:={};
    subs:={};
    eiplusej:={};
    if type=="A" then (
	return delete(null, flatten apply(m, i -> apply(m, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
    );
    if type=="B" then (
	answer1 = delete(null, flatten apply(m-1, i -> apply(m-1, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
        es=apply(m, i -> sum apply(m-i, k -> simpleroots_(m-1-k)));
        subs=subsets(es,2);
        eiplusej=apply(#subs,h -> sum subs_h);
        return flatten {answer1,es,eiplusej}
    );
    if type=="C" then (
	answer1 = delete(null, flatten apply(m-1, i -> apply(m-1, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
        twoes:=apply(m, i -> if i<m-1 then sum(apply(m-i-1, k -> 2*simpleroots_(m-2-k)))+ simpleroots_(m-1) else simpleroots_(m-1));
        subs=subsets(twoes,2);
        eiplusej=apply(#subs,h -> sum subs_h);
        eiplusej=apply(#eiplusej,h -> apply(m, t-> lift((1/2)*eiplusej_h_t,ZZ)));
        return flatten {answer1,twoes,eiplusej}
    );
    if type=="D" then (
        answer1 = delete(null, flatten apply(m-1, i -> apply(m-1, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
        em=(1/2)*(simpleroots_(m-1)-simpleroots_(m-2));
        em=apply(#em,k-> lift(em_k,ZZ));
        es={em};
        for i from 0 to m-2 do (
            es = append(es,es_(#es-1)+simpleroots_(m-2-i))
        );
        subs=subsets(es,2);
        eiplusej=apply(#subs,h -> sum subs_h);
        return flatten {answer1,eiplusej}
    );
    if type=="E" and m==6 then (
	return {{0, 0, 0, 0, -1, 2}, {0, 0, 0, -1, 1, 1}, {0, -1, -1, 1, 0, 1}, {-1, -1, 1, 0, 0, 1}, {1, -1, 0, 0, 0, 1}, {0, 1, -1, 0, 0, 1}, {-1, 1, 1, -1, 0, 1}, {1, 1, 0, -1, 0, 1}, {-1, 0, 0, 1, -1, 1}, {1, 0, -1, 1, -1, 1}, {-1, 0, 0, 0, 1, 0}, {1, 0, -1, 0, 1, 0}, {1, 0, -1, 1, 0, -1}, {0, 0, 1, 0, 0, -1}, {0, -1, -1, 2, -1, 0}, {-1, -1, 1, 1, -1, 0}, {0, 1, -1, 1, -1, 0}, {-1, 1, 1, 0, -1, 0}, {1, 0, 1, -1, 0, 0}, {0, 2, 0, -1, 0, 0}, {2, 0, -1, 0, 0, 0}, {-1, 0, 2, -1, 0, 0}, {1, 1, 0, 0, -1, 0}, {1, -1, 0, 1, -1, 0}, {-1, 0, 0, 1, 0, -1}, {1, 1, 0, -1, 1, -1}, {1, -1, 0, 0, 1, -1}, {-1, 1, 1, -1, 1, -1}, {-1, -1, 1, 0, 1, -1}, {0, 1, -1, 0, 1, -1}, {0, -1, -1, 1, 1, -1}, {0, 0, 0, -1, 2, -1}, {0, 1, 0, 0, 0, 0}, {0, -1, 0, 1, 0, 0}, {0, 0, 1, -1, 1, 0}, {0, 0, 1, 0, -1, 1}});
    if type=="E" and m==7 then (
	return {{0, 0, 0, 0, 0, -1, 2}, {0, 0, 0, 0, -1, 1, 1}, {0, 0, 0, -1, 1, 0, 1}, {0, -1, -1, 1, 0, 0, 1}, {-1, -1, 1, 0, 0, 0, 1}, {0, 1, -1, 0, 0, 0, 1}, {-1, 1, 1, -1, 0, 0, 1}, {-1, 0, 0, 1, -1, 0, 1}, {-1, 0, 0, 0, 1, -1, 1}, {-1, 0, 0, 0, 0, 1, 0}, {1, -1, 0, 0, 0, 0, 1}, {1, 1, 0, -1, 0, 0, 1}, {1, 0, -1, 1, -1, 0, 1}, {1, 0, -1, 0, 1, -1, 1}, {1, 0, -1, 0, 0, 1, 0}, {0, 0, 1, 0, -1, 1, -1}, {0, 0, 1, -1, 1, 0, -1}, {0, -1, 0, 1, 0, 0, -1}, {0, 1, 0, 0, 0, 0, -1}, {0, 0, 0, -1, 2, -1, 0}, {0, -1, -1, 1, 1, -1, 0}, {0, 1, -1, 0, 1, -1, 0}, {-1, -1, 1, 0, 1, -1, 0}, {-1, 1, 1, -1, 1, -1, 0}, {1, -1, 0, 0, 1, -1, 0}, {1, 1, 0, -1, 1, -1, 0}, {-1, 0, 0, 1, 0, -1, 0}, {1, -1, 0, 1, -1, 0, 0}, {1, 1, 0, 0, -1, 0, 0}, {-1, 0, 2, -1, 0, 0, 0}, {2, 0, -1, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0}, {1, 0, 1, -1, 0, 0, 0}, {-1, 1, 1, 0, -1, 0, 0}, {0, 1, -1, 1, -1, 0, 0}, {-1, -1, 1, 1, -1, 0, 0}, {0, -1, -1, 2, -1, 0, 0}, {0, 0, 1, 0, 0, -1, 0}, {1, 0, -1, 1, 0, -1, 0}, {1, 0, -1, 0, 1, 0, -1}, {-1, 0, 0, 0, 1, 0, -1}, {1, 0, -1, 1, -1, 1, -1}, {-1, 0, 0, 1, -1, 1, -1}, {1, 1, 0, -1, 0, 1, -1}, {-1, 1, 1, -1, 0, 1, -1}, {0, 1, -1, 0, 0, 1, -1}, {1, -1, 0, 0, 0, 1, -1}, {-1, -1, 1, 0, 0, 1, -1}, {0, -1, -1, 1, 0, 1, -1}, {0, 0, 0, -1, 1, 1, -1}, {0, 0, 0, 0, -1, 2, -1}, {1, 0, 0, 0, 0, 0, 0}, {-1, 0, 1, 0, 0, 0, 0}, {0, 0, -1, 1, 0, 0, 0}, {0, 1, 0, -1, 1, 0, 0}, {0, -1, 0, 0, 1, 0, 0}, {0, 1, 0, 0, -1, 1, 0}, {0, 1, 0, 0, 0, -1, 1}, {0, -1, 0, 1, -1, 1, 0}, {0, -1, 0, 1, 0, -1, 1}, {0, 0, 1, -1, 0, 1, 0}, {0, 0, 1, -1, 1, -1, 1}, {0, 0, 1, 0, -1, 0, 1}});
    if type=="E" and m==8 then (
	return {{0, 0, 0, 0, 0, 0, -1, 2}, {0, 0, 0, 0, 0, -1, 1, 1}, {0, 0, 0, 0, -1, 1, 0, 1}, {0, 0, 0, -1, 1, 0, 0, 1}, {0, -1, -1, 1, 0, 0, 0, 1}, {-1, -1, 1, 0, 0, 0, 0, 1}, {0, 1, -1, 0, 0, 0, 0, 1}, {-1, 1, 1, -1, 0, 0, 0, 1}, {-1, 0, 0, 1, -1, 0, 0, 1}, {-1, 0, 0, 0, 1, -1, 0, 1}, {-1, 0, 0, 0, 0, 1, -1, 1}, {1, -1, 0, 0, 0, 0, 0, 1}, {1, 1, 0, -1, 0, 0, 0, 1}, {1, 0, -1, 1, -1, 0, 0, 1}, {1, 0, -1, 0, 1, -1, 0, 1}, {1, 0, -1, 0, 0, 1, -1, 1}, {0, 0, 1, 0, -1, 0, 0, 1}, {0, 0, 1, -1, 1, -1, 0, 1}, {0, 0, 1, -1, 0, 1, -1, 1}, {0, -1, 0, 1, 0, -1, 0, 1}, {0, -1, 0, 1, -1, 1, -1, 1}, {0, 1, 0, 0, 0, -1, 0, 1}, {0, 1, 0, 0, -1, 1, -1, 1}, {0, -1, 0, 0, 1, 0, -1, 1}, {0, 0, 1, 0, -1, 0, 1, -1}, {0, 0, 1, -1, 1, -1, 1, -1}, {0, 0, 1, -1, 0, 1, 0, -1}, {0, -1, 0, 1, 0, -1, 1, -1}, {0, -1, 0, 1, -1, 1, 0, -1}, {0, 1, 0, 0, 0, -1, 1, -1}, {0, 1, 0, 0, -1, 1, 0, -1}, {0, -1, 0, 0, 1, 0, 0, -1}, {0, 1, 0, -1, 1, 0, 0, -1}, {0, 0, -1, 1, 0, 0, 0, -1}, {-1, 0, 1, 0, 0, 0, 0, -1}, {1, 0, 0, 0, 0, 0, 0, -1}, {0, 0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, -1, 1, 1, -1, 0}, {0, -1, -1, 1, 0, 1, -1, 0}, {-1, -1, 1, 0, 0, 1, -1, 0}, {1, -1, 0, 0, 0, 1, -1, 0}, {0, 1, -1, 0, 0, 1, -1, 0}, {-1, 1, 1, -1, 0, 1, -1, 0}, {1, 1, 0, -1, 0, 1, -1, 0}, {-1, 0, 0, 1, -1, 1, -1, 0}, {1, 0, -1, 1, -1, 1, -1, 0}, {-1, 0, 0, 0, 1, 0, -1, 0}, {1, 0, -1, 0, 1, 0, -1, 0}, {1, 0, -1, 1, 0, -1, 0, 0}, {0, 0, 1, 0, 0, -1, 0, 0}, {0, -1, -1, 2, -1, 0, 0, 0}, {-1, -1, 1, 1, -1, 0, 0, 0}, {0, 1, -1, 1, -1, 0, 0, 0}, {-1, 1, 1, 0, -1, 0, 0, 0}, {1, 0, 1, -1, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0, 0}, {2, 0, -1, 0, 0, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0, 0}, {1, 1, 0, 0, -1, 0, 0, 0}, {1, -1, 0, 1, -1, 0, 0, 0}, {-1, 0, 0, 1, 0, -1, 0, 0}, {1, 1, 0, -1, 1, -1, 0, 0}, {1, -1, 0, 0, 1, -1, 0, 0}, {-1, 1, 1, -1, 1, -1, 0, 0}, {-1, -1, 1, 0, 1, -1, 0, 0}, {0, 1, -1, 0, 1, -1, 0, 0}, {0, -1, -1, 1, 1, -1, 0, 0}, {0, 0, 0, -1, 2, -1, 0, 0}, {0, 1, 0, 0, 0, 0, -1, 0}, {0, -1, 0, 1, 0, 0, -1, 0}, {0, 0, 1, -1, 1, 0, -1, 0}, {0, 0, 1, 0, -1, 1, -1, 0}, {1, 0, -1, 0, 0, 1, 0, -1}, {1, 0, -1, 0, 1, -1, 1, -1}, {1, 0, -1, 1, -1, 0, 1, -1}, {1, 1, 0, -1, 0, 0, 1, -1}, {1, -1, 0, 0, 0, 0, 1, -1}, {-1, 0, 0, 0, 0, 1, 0, -1}, {-1, 0, 0, 0, 1, -1, 1, -1}, {-1, 0, 0, 1, -1, 0, 1, -1}, {-1, 1, 1, -1, 0, 0, 1, -1}, {0, 1, -1, 0, 0, 0, 1, -1}, {-1, -1, 1, 0, 0, 0, 1, -1}, {0, -1, -1, 1, 0, 0, 1, -1}, {0, 0, 0, -1, 1, 0, 1, -1}, {0, 0, 0, 0, -1, 1, 1, -1}, {0, 0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, 0, 1, -1}, {0, 0, 0, 0, 0, 1, -1, 0}, {0, 0, 0, 0, 1, -1, 0, 0}, {0, 0, 0, 1, -1, 0, 0, 0}, {0, 1, 1, -1, 0, 0, 0, 0}, {0, -1, 1, 0, 0, 0, 0, 0}, {1, 1, -1, 0, 0, 0, 0, 0}, {-1, 1, 0, 0, 0, 0, 0, 0}, {1, -1, -1, 1, 0, 0, 0, 0}, {-1, -1, 0, 1, 0, 0, 0, 0}, {1, 0, 0, -1, 1, 0, 0, 0}, {-1, 0, 1, -1, 1, 0, 0, 0}, {0, 0, -1, 0, 1, 0, 0, 0}, {1, 0, 0, 0, -1, 1, 0, 0}, {-1, 0, 1, 0, -1, 1, 0, 0}, {0, 0, -1, 1, -1, 1, 0, 0}, {0, 1, 0, -1, 0, 1, 0, 0}, {0, -1, 0, 0, 0, 1, 0, 0}, {1, 0, 0, 0, 0, -1, 1, 0}, {-1, 0, 1, 0, 0, -1, 1, 0}, {0, 0, -1, 1, 0, -1, 1, 0}, {0, 1, 0, -1, 1, -1, 1, 0}, {0, 1, 0, 0, -1, 0, 1, 0}, {0, -1, 0, 0, 1, -1, 1, 0}, {0, -1, 0, 1, -1, 0, 1, 0}, {0, 0, 1, -1, 0, 0, 1, 0}, {1, 0, -1, 0, 0, 0, 1, 0}, {-1, 0, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 0, 0, 1}, {1, 0, 0, 0, 0, 0, -1, 1}, {-1, 0, 1, 0, 0, 0, -1, 1}, {0, 0, -1, 1, 0, 0, -1, 1}, {0, 1, 0, -1, 1, 0, -1, 1}});
    if type=="F" and m==4 then (
	return {{0, 0, 0, 1}, {1, 0, 0, -1}, {-1, 1, 0, -1}, {0, -1, 2, -1}, {1, 0, 0, 0}, {-1, 1, 0, 0}, {0, -1, 2, 0}, {0,1,0,-2}, {1,-1,2,-2}, {-1, 0, 2, -2}, {-1, 0, 0, 2}, {1, -1, 0, 2}, {0, 1, -2, 2}, {2, -1, 0, 0}, {1, 1, -2, 0}, {-1, 2, -2, 0}, {0, 0, 1, -1}, {0, 1, -1, 0}, {1, -1, 1, 0}, {1, 0, -1, 1}, {-1, 0, 1, 0}, {-1, 1, -1, 1}, {0, -1, 1, 1}, {0, 0, -1, 2}});
    if type=="G" and m==2 then return {{-3, 2}, {-1, 1}, {0, 1}, {2, -1}, {3, -1}, {1, 0}};
)


positiveRoots(String,ZZ):= (type,m) -> (
    M:=matrix apply(simpleRoots(type,m), a -> 1/1*a);
    Mtinv:=inverse transpose M;
    PhiPlus := sort apply(unorderedPositiveRoots(type,m), a -> {level(a,type,m),reverse flatten entries (Mtinv*(transpose matrix {a})),a});
    apply(PhiPlus, p -> p_2)
)


positiveRoots(Sequence,Sequence):=memoize((type,m)->flatten toList apply(#m, i -> apply(positiveRoots(type#i,m#i),v->unsplit(v,m,i))))

positiveRoots(LieAlgebra):=memoize((g) -> positiveRoots(g#"RootSystemType",g#"LieAlgebraRank"))

positiveCoroots = method(
    TypicalValue => List
)

positiveCoroots(String,ZZ) :=
positiveCoroots(Sequence,Sequence) := memoize((type,m)->(
	pr:=positiveRoots(type,m);
	if all(sequence type, t -> t==="A" or t==="D" or t==="E") then return pr;
	apply(pr, v -> (
		r := 2 / killingForm(type,m,v,v);
		apply(v, k -> lift(r*k,ZZ))
		))
    ))

positiveCoroots(LieAlgebra):=(g) -> positiveCoroots(g#"RootSystemType",g#"LieAlgebraRank")


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Exported functions for Lie algebra modules 
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

multiplicity(List,LieAlgebraModule) := o -> (w,M) -> (
    W:=weightDiagram(M);
    W_w 
)
multiplicity(Vector,LieAlgebraModule) := o -> (w,M) -> multiplicity(entries w,M)

stdVars := memoize ( (type,m) -> (
	vrs := gens characterRing(type,m);
    	if type == "A" then apply(m+1, i -> (if i==m then 1 else vrs_i) * (if i==0 then 1 else vrs_(i-1)^-1))
    	else if type=="B" then apply(m, i -> (if i==m-1 then vrs_i^2 else vrs_i) * (if i==0 then 1 else vrs_(i-1)^-1))
    	else if type == "C" then apply(m, i -> vrs_i * (if i==0 then 1 else vrs_(i-1)^-1))
    	else if type == "D" then apply(m-2, i -> vrs_i*(if i==0 then 1 else vrs_(i-1)^-1)) | {vrs_(m-2)*vrs_(m-1)*vrs_(m-3)^-1,vrs_(m-1)*vrs_(m-2)^-1}
    	))

characterAlgorithms := new MutableHashTable;
-- Jacobi Trudi formulae
elemSym = memoize((L,i) -> (
    if i<0 or i>#L then 0
    else sum(subsets(L,i),product)
    ))
characterAlgorithms#"JacobiTrudi'" = (type,m,v) -> ( -- good for high rank algebras, small weights
    if type != "A" then return;
    z := stdVars(type,m);
    conj:=reverse splice apply(m,i -> v#i : i+1);
    if #conj == 0 then 1_(characterRing(type,m)) else det matrix table(#conj,#conj,(i,j)->elemSym(z,conj#i+j-i))
    )
completeSym = memoize((L,i) -> (
    if i<0 then 0
    else sum(compositions(#L,i),c->product(L,c,(v,k)->v^k))
    ))
characterAlgorithms#"JacobiTrudi" = (type,m,v) -> (
    if type != "A" then return;
    z := stdVars(type,m);
    pows := apply(m+1,j->sum(j..m-1,k->v#k));
    det matrix table(m+1,m+1,(i,j)->completeSym(z,pows#i+j-i))
    )

-- Weyl character formula
characterAlgorithms#"Weyl" = (type,m,v) -> ( -- good for low rank algebras
    z := stdVars(type,m);
    if type == "A" then (
	pows := apply(m+1,j->sum(j..m-1,k->1+v#k));
    	num := det matrix table(m+1,m+1,(i,j)->z_i^(pows#j));
    	den := product(m+1,j->product(j,i->z_i-z_j)); --  type A Weyl denominator formula
	)
    else if type=="B" then (
	pows = apply(m,j->sum(j..m-2,k->1+v#k)+(1+v#(m-1))//2);
	par := (1+v#(m-1)) % 2; -- shift of 1/2 to avoid half-integer powers
	num = (last gens characterRing(type,m))^(1-par)*det matrix table(m,m,(i,j)->z_i^(pows#j+par)-z_i^(-pows#j));
    	den = product(m,i->z_i-1)*product(m,j->product(j,i->(z_i^-1 - z_j)*(1-z_i*z_j^-1))); --  type B Weyl denominator formula
	)
    else if type == "C" then (
	pows = apply(m,j->sum(j..m-1,k->1+v#k));
    	num = det matrix table(m,m,(i,j)->z_i^(pows#j)-z_i^(-pows#j));
    	den = product(m,i->z_i-z_i^-1)*product(m,j->product(j,i->(z_i^-1 - z_j)*(1-z_i*z_j^-1))); --  type C Weyl denominator formula
	)
    else if type == "D" then (
	pows = append(apply(m-1,j->sum(j..m-3,k->1+v#k)+(2+v#(m-2)+v#(m-1))//2),(v#(m-1)-v#(m-2))//2);
	par = (v#(m-2)+v#(m-1)) % 2; -- shift of 1/2 to avoid half-integer powers
    	num1 := det matrix table(m,m,(i,j)->z_i^(pows#j+par)+z_i^(-pows#j));
	num2 := det matrix table(m,m,(i,j)->z_i^(pows#j+par)-z_i^(-pows#j));
    	den = product(m,j->product(j,i->(z_i^-1 - z_j)*(1-z_i*z_j^-1))); --  type D Weyl denominator formula
	num = (last gens characterRing(type,m))^(-par)*(num1+num2)//2;
	)
    else return;
    num//den
)

--In the next two functions we implement Freudenthal's recursive algorithm for computing the weights in a Lie algebra module and their multiplicities
--The function Freud computes the set of weights in a Lie algebra module without their multiplicities
Freud = memoize ((type,m,v) -> (
    simpleroots:=simpleRoots(type,m);
    if all(v, a -> a < 0) then return set{v};
    answer:=set {v};
    for i from 0 to #v-1 do
        for j from 1 to v_i do
            answer = answer + Freud(type,m,v-j*simpleroots_i);
    answer
))

-*
--the function weightsAboveMu computes the weights above mu=w in the weight diagram of lambda=v
weightsAboveMu = memoize( (type,m,v,w) -> (
    Omega:=Freud(type,m,v);
    if w==v then return {};
    simpleroots:=simpleRoots(type,m);
    answer:={};
    k:=0;
    for i from 0 to #simpleroots-1 do (
        k=0;
        while isSubset(set {w+k*(simpleroots_i)},Omega) do (
            if k>0 then answer = append(answer,w+k*(simpleroots_i));
            k=k+1;
    ));
    answer=unique answer;
    alllevels:={answer};
    for i from 0 to #answer-1 do (
        alllevels = append(alllevels,weightsAboveMu(type,m,v,answer_i))
    );
    unique flatten alllevels
))


multiplicityOfWeightInLieAlgebraModule = memoize((type,m,v,w) -> (
    rho:=toList(m:1);
    if v==w then return 1;
    Omega:=Freud(type,m,v);
    if not member(w,Omega) then return 0;
--    L:=weightsAboveMu(type,m,v,w);
    posroots:=positiveRoots(type,m);
    rhs:=0;
    local w';
    scan(posroots, a -> (
        w'=w+a;
        while member(w',Omega) do (
	    rhs=rhs+killingForm(type,m,w',a)*multiplicityOfWeightInLieAlgebraModule(type,m,v,w');
	    w'=w'+a;
	    )));
    lhs:=killingForm(type,m,v+rho,v+rho)-killingForm(type,m,w+rho,w+rho);
    lift(2*rhs/lhs,ZZ)
))


characterAlgorithms#"Freudenthal" = (type,m,v) -> (
    R := characterRing(type,m);
    sum(toList Freud(type,m,v), w -> multiplicityOfWeightInLieAlgebraModule(type,m,v,w) * R_w)
    )
*-

-- this is a rewrite of commented out multiplicityOfWeightInLieAlgebraModule
characterAlgorithms#"Freudenthal" = (type,m,v) -> (
    R:=characterRing(type,m);
    rho:=toList(m:1);
    Omega:=Freud(type,m,v);
    mults:=new MutableHashTable from Omega;
    posroots:=positiveRoots(type,m);
    -- sort
    Omega=apply(reverse sort apply(toList Omega,w->R_w),first @@ exponents);
    s:=R_v;
    for i from 1 to #Omega-1 do s+=(
	w:=Omega#i;
	rhs:=0;
	scan(posroots, a -> (
		w':=w+a;
		while mults#?w' do (
		    rhs=rhs+killingForm(type,m,w',a)*mults#w';
		    w'=w'+a;
		    )));
	lhs:=killingForm(type,m,v+rho,v+rho)-killingForm(type,m,w+rho,w+rho);
	mults#w = lift(2*rhs/lhs,ZZ)
	)*R_w;
    s
)

characterAlgorithms#"Picker" = (type,m,v) -> (
    if type != "A" and m>4 then characterAlgorithms#"Freudenthal"(type,m,v) -- forces Freudenthal for high rank not A
    else if type == "A" and m<=3 then characterAlgorithms#"Weyl"(type,m,v) -- forces Weyl for low rank A
    )

-- last strategy = first choice
scan({"JacobiTrudi","Freudenthal","Weyl","JacobiTrudi'","Picker"}, strat -> addHook(symbol character,characterAlgorithms#strat,Strategy=>strat))

character = method(
    Options=>{Strategy=>null},
    TypicalValue => RingElement
    )

character1 = memoize((type,m,v,o)->runHooks(symbol character,(type,m,v),o))
character2 = memoize((type,m,v,o) -> (
	v=split(v,m);
	R:=characterRing(type,m);
	product(#m,i->R#"maps"#i character1(type#i,m#i,v#i,o))
	))
character (String,ZZ,List) := o -> (type,m,v) -> character1(type,m,v,o) -- tricky to memoize a method with options
character (Sequence,Sequence,List) := o -> (type,m,v) -> character2(type,m,v,o) -- tricky to memoize a method with options
character (LieAlgebra,List) := o -> (g,v) -> if rank g == 0 then 1_(characterRing g) else character(g#"RootSystemType",g#"LieAlgebraRank",v,o) -- annoying special case, otherwise wrong ring
character (LieAlgebra,Vector) := o -> (g,v) -> character(g,entries v,o)
character LieAlgebraModule := o -> (cacheValue character) ((M) ->
    if #(M#"DecompositionIntoIrreducibles") == 0 then 0_(characterRing M#"LieAlgebra")
    else sum(pairs M#"DecompositionIntoIrreducibles",(v,a) -> a * character (M#"LieAlgebra",v,o)))

weightDiagram = method(
    Options=>{Strategy=>null},
    TypicalValue=>VirtualTally
    )

weightDiagram LieAlgebraModule := o -> (M) -> new VirtualTally from listForm character(M,o)
weightDiagram(LieAlgebra,Vector) := weightDiagram(LieAlgebra,List) := o -> (g,v) -> new VirtualTally from listForm character(g,v,o)

fac := memoize((type,m) -> ( -- possible denominator in Weyl product formula factors, ultimately coming from the choice of normalisation of the Killing form
    lcm append(apply(positiveCoroots(type,m), u -> numerator (killingForm(type,m,u,u)/2)),1) -- append is for g=0
    ))

qden := memoize((type,m,qnum) -> (
    rho:=toList(plus sequence m : 1);
    d:=fac(type,m);
    product(positiveRoots(type,m), a -> qnum lift(d*killingForm(type,m,rho,a),ZZ))
    ))

qdim1 = (M,qnum) -> ( -- used internally by dim and qdim: Weyl product formula
    g:=M#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    rho:=toList(plus sequence m : 1);
    d:=fac(type,m);
    (sum(pairs M#"DecompositionIntoIrreducibles", (w,mu) ->
	mu * product(positiveRoots g, a -> qnum lift(d*killingForm(g,w+rho,a),ZZ))
	))//qden(type,m,qnum)
    )

dim LieAlgebraModule := (cacheValue dim) (M -> qdim1(M,identity))

-- we use one q ring for everyone, to simplify
q:=getSymbol "q"
R:=ZZ(monoid[q,Inverses=>true,MonomialOrder=>Lex])
R':=ZZ(monoid[q]) -- annoying: can't take quotients with Inverses => true
-*
cyclotomic = memoize ( n -> ( -- clever but silly implementation
	facs := first \ toList factor (if odd n then R'_0^n-1 else R'_0^(n//2)+1);
	facs#(maxPosition(first\degree\facs))
	))
*-
cyclotomic = memoize ( n -> (
	P := R'_0^n - 1;
	scan(1..n//2, d -> if n%d==0 then P = P // cyclotomic d);
	P
	))

qring := memoize ( (n,d) -> R'/((map(R',R',{R'_0^d})) cyclotomic n ) )
qnum := n->sum(n,i->R_0^(2*i-n+1))

qdim = method()
qdim LieAlgebraModule := (cacheValue qdim) (M -> qdim1(M,qnum))
qdim (LieAlgebraModule,ZZ) := (M,l) -> (
    g:=M#"LieAlgebra";
    if not isSimple g then error "Lie algebra not simple";
    (map(qring(l+dualCoxeterNumber g,2*fac(g#"RootSystemType",g#"LieAlgebraRank")),R)) qdim M
    )


LieAlgebraModuleFromWeights = method(
    TypicalValue => LieAlgebraModule
    )
LieAlgebraModuleFromWeights(RingElement,LieAlgebra) := (c0,g) -> (
    if ring c0 =!= characterRing g then error "wrong ring";
    c:=c0;
    --find and peel off irreducibles
    decompositionData := while c!=0 list ( (v,mu) := first listForm leadTerm c ) do (
	if any(v,a->a<0) then error "not a valid weight diagram";
	c = c - mu*character(g,v);
    	);
    new LieAlgebraModule from {
    	"LieAlgebra" => g,
    	"DecompositionIntoIrreducibles" => new VirtualTally from decompositionData,
    	cache => new CacheTable from { character => c0 }
    	}
    )
-- another algorithm would be to apply the same Racah/Brauer/Klimyk algorithm as tensor product (with second weight = trivial one)
-- not clear which is faster

LieAlgebraModuleFromWeights(VirtualTally,LieAlgebra) := (W,g) -> (
    R := characterRing g;
    LieAlgebraModuleFromWeights(sum(pairs W,(w,a) -> a*R_w),g)
    )

adams = method( TypicalValue => LieAlgebraModule )
adams (ZZ,LieAlgebraModule) := (k,M) -> (
    g:=M#"LieAlgebra";
    if k==0 then new LieAlgebraModule from (g,{})
    else if k==1 then M
    else if k==-1 then starInvolution M
    else LieAlgebraModuleFromWeights(applyKeys(weightDiagram M, w -> k*w),g) -- primitive but works
)

symmetricPower(ZZ,LieAlgebraModule) := (n,M) -> M.cache#(symbol symmetricPower, n) ??= (
    if n<0 then error "nonnegative powers only";
    if n==0 then trivialModule M#"LieAlgebra"
    else if n==1 then M
    else (directSum apply(1..n, k -> adams(k,M) ** symmetricPower(n-k,M)))^(1/n)
    )

exteriorPower(ZZ,LieAlgebraModule) := o -> (n, M) -> M.cache#(symbol exteriorPower, n) ??= (
    if n<0 then error "nonnegative powers only";
    if n==0 then trivialModule M#"LieAlgebra"
    else if n==1 then M
    else (directSum apply(1..n, k -> (adams(k,M) ** exteriorPower(n-k,M))^((-1)^(k-1)) ))^(1/n)
    )

LieAlgebraModule @ LieAlgebraModule := (M,M') -> new LieAlgebraModule from (
    M#"LieAlgebra" ++ M'#"LieAlgebra",
    combine(M#"DecompositionIntoIrreducibles",M'#"DecompositionIntoIrreducibles",join,times,plus)
    )

---------------------------------------------------------
---------------------------------------------------------
--Tensor product decomposition
---------------------------------------------------------
--------------------------------------------------------- 
-*
--Action of word in Coxeter group or affine Coxeter group on weights
wordAction = (type,m,l,I,v) -> (
    simpleroots:=simpleRoots(type,m);
    w:=v;
    J:=reverse I; 
    for j from 0 to #J-1 do (     
        if J_j >0 then (
	    rho:=apply(#w, i-> 1);
            w=w+rho;
            w = w-(w_(J_j-1))*simpleroots_(J_j-1);
            w=w-rho);
        if J_j ==0 then (
            theta:=highestRoot(type,m);
            theta=apply(#theta, i -> lift(theta_i,ZZ));
            l0:=lift(l-killingForm(type,m,w,theta),ZZ);
            w = w+(l0+1)*theta);
    );
    w
)

squarefreeWordsOfLengthP = (L,p) -> (
    if p==0 then return {};
    if p==1 then return apply(#L, i -> {L_i});
    wlm1:=squarefreeWordsOfLengthP(L,p-1);
    answer:=delete(null, flatten apply(#L, i -> apply(#wlm1, j -> if L_i != wlm1_j_0 then prepend(L_i,wlm1_j))));
    answer
)

isIdentity = (type,m,l,w) -> (
    fdw:=apply(m, i -> apply(m, j -> if i==j then 1 else 0));
    apply(m, i -> wordAction(type,m,l,w,fdw_i)) == fdw      
)

*-

LieAlgebraModule ** LieAlgebraModule := (V,W) -> ( -- cf Humpheys' intro to LA & RT sec 24 exercise 9
    g:=V#"LieAlgebra";
    if g != W#"LieAlgebra" then error "V and W must be modules over the same Lie algebra";
    if V =!= W and dim W < dim V then (V,W)=(W,V); -- maybe should first test if characters already computed?
    wd:=weightDiagram V;
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    sr:=simpleRoots g;
    rho:=toList(rank g:1);
    ans := new MutableHashTable;
    add := (w,a) -> if ans#?w then ( s := ans#w+a; if s!=0 then ans#w = s else remove(ans,w) ) else ans#w = a;
    scanPairs(W#"DecompositionIntoIrreducibles", (w,a) -> -- loop over highest weights of W
    	scanPairs(wd, (v,b) -> ( -- loop over all weights of V
    		u:=v+w+rho;
		t:=a*b; i:=-1;
		while not any(u,zero) and ((i=position(u,j->j<0)) =!= null) do (
		    u-=u#i*sr#i;
		    t=-t;
	    	    );
		if i === null then add(u-rho,t);
		)));
    new LieAlgebraModule from (g,ans)
    )

tensorCoefficient = method(
    TypicalValue=>ZZ)
tensorCoefficient(LieAlgebraModule, LieAlgebraModule,LieAlgebraModule) := (U,V,W) -> (U**V)_W


---------------------------------------------------------
---------------------------------------------------------
--Fusion product decomposition
---------------------------------------------------------
--------------------------------------------------------- 

-*
fusionReflectionData = memoize( (type,m,l,maxwordlength,remainingWeights) -> (
    Pl:=weylAlcove(type,m,l);
    wl:=1;
    --initialize;
    remainingWeights=toList(set(remainingWeights)-set(Pl));
    found:= set Pl;
    answer:= set apply(#Pl, i -> {Pl_i,{}});
    fixed:={};
    S:=apply(m+1,i->i);
    while #remainingWeights >0 and wl<=maxwordlength do (
        words:=squarefreeWordsOfLengthP(S,wl);
        for i from 0 to #words-1 do (
            if isIdentity(type,m,l,words_i) then continue;
            newremainingWeights:={};
            for j from 0 to #remainingWeights-1 do (
                if wordAction(type,m,l,words_i,remainingWeights_j)==remainingWeights_j then (
                    answer = answer + set {{remainingWeights_j,reverse(words_i)}};
                    fixed = append(fixed,remainingWeights_j)) else newremainingWeights=append(newremainingWeights,remainingWeights_j)
            );
            remainingWeights=newremainingWeights;
            im:=apply(#Pl, j -> wordAction(type,m,l,words_i,Pl_j));
            if member(im,found) then continue else (
                found = found + set(im);
                remainingWeights=toList(set(remainingWeights)-set(im));
                answer=answer+set apply(#im, k -> {im_k,reverse(words_i)});
            )
        );
        wl=wl+1);
    if #remainingWeights==0 then return {sort toList(answer),sort fixed,true,remainingWeights} else return {sort toList(answer), sort fixed,false,remainingWeights}
))
*-

fusionProduct = method(
--    TypicalValue=>HashTable,Options=>{MaxWordLength=>10})
    TypicalValue=>LieAlgebraModule)

-- TODO: allow for arbitrary number of args just like tensor and directSum

-*
-- try to define abbreviated syntax? something like (except fusionProduct should output a fusion module)
FusionModule := new Type of LieAlgebraModule
LieAlgebraModule _ ZZ := (M,l) -> new FusionModule from merge(M,hashTable{"Level"=>l},last)
-- expression fusionModule := -- TODO
FusionModule ** LieAlgebraModule := (F,W) -> fusionProduct(F,W,F#"Level")
LieAlgebraModule ** FusionModule := (W,F) -> fusionProduct(W,F,F#"Level")
FusionModule ** FusionModule := (F,F') -> if F#"Level" != F'#"Level" then error "modules must have same level" else fusionProduct(F,F',F#"Level")
*-

fusionProduct(LieAlgebraModule,LieAlgebraModule,ZZ) := (V,W,l) -> (
    g:=V#"LieAlgebra";
    l = l + dualCoxeterNumber g;
    if g != W#"LieAlgebra" then error "V and W must be modules over the same Lie algebra";
    if not isSimple g then error "Lie algebra not simple";
    wd:=weightDiagram V;
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    sr:=simpleRoots(type,m);
    rho:=toList(m:1);
    pc:=positiveCoroots g;
    pr:=positiveRoots g;
--    Q:=quadraticFormMatrix g;
--    Q:=quadraticFormMatrix (type,m);
--    pr':=apply(pr, u -> entries(lift(Ci*vector u,ZZ))); -- possibly reinstate after non simply laced fix
--    pr':=apply(pr, u -> (2/killingForm(g,u,u))*entries(Q*vector u));
    ans := new MutableHashTable;
    add := (w,a) -> if ans#?w then ( s := ans#w+a; if s!=0 then ans#w = s else remove(ans,w) ) else ans#w = a;
    scanPairs(W#"DecompositionIntoIrreducibles", (w,a) -> -- loop over highest weights of W
    	scanPairs(wd, (v,b) -> ( -- loop over all weights of V
    		u:=v+w+rho;
		-- first recenter it using translations
		cnt:=0; i:=0;
        	while cnt < #pr do (
--		    s := sum(u,pr'#i,times);
		    s := killingForm(g,u,pr#i); -- is the same just more explicit
		    sn := numerator s; sd := denominator s; -- in non simply laced types, there can be a denominator
		    if sd == 1 and sn % l == 0 then break else if s < -l or s > l then (
			u=u-((sn+l*sd)//(2*l*sd))*l*pr#i;
			cnt=0;
			) else cnt=cnt+1;
		    i=i+1; if i==#pr then i=0;
            	    );
		if cnt == #pr then (
		    -- then end with usual algo
		    -- except the any(u,zero) not needed, filtered already
		    t:=1;
		    while (i=position(u,j->j<0)) =!= null do (
		    	u=u-u#i*sr#i;
		    	t=-t;
		    	);
		    add(u-rho,a*b*t);
		    )
		)));
    new LieAlgebraModule from (g,ans)
    )

-*
fusionProduct(LieAlgebraModule,LieAlgebraModule,ZZ) := memoize( opts-> (M,N,l) -> (
    wl:= opts.MaxWordLength;
    if M#"LieAlgebra" != N#"LieAlgebra" then error "The Lie algebra modules must be over the same Lie algebra.";
    g:=M#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if not isIrreducible M or not isIrreducible N then error "modules need to be irreducible";
    lambda:=first keys M#"DecompositionIntoIrreducibles";
    mu:=first keys N#"DecompositionIntoIrreducibles";
    wd:=pairs weightDiagram(g,lambda);
    wd=apply(#wd, i -> {wd_i_0+mu,wd_i_1});
    rd:=fusionReflectionData(type,m,l,wl,apply(#wd, i -> wd_i_0));
    if rd_2 == false then error "Need to allow longer words";
    fixed:=rd_1;
    rd=hashTable(rd_0);
    Pl:=weylAlcove(type,m,l);
    wtsinPl:=delete(null, apply(#wd, i -> if member(wd_i_0,Pl) and not member(wd_i_0,fixed) then wd_i));
    wdh:=new MutableHashTable from wtsinPl;
    for i from 0 to #wd-1 do (
        if member(wd_i_0,Pl) then continue;
        if member(wd_i_0,fixed) then continue;
        word:=rd#(wd_i_0);
        e:=#word;
        e=(-1)^e;
        im:=wordAction(type,m,l,word,wd_i_0);
        wdh#im = wdh#im + (e)*(wd_i_1)
    );
    wdh=pairs(wdh);
    newwdh:=delete(null, apply(#wdh, i -> if wdh_i_1 != 0 then wdh_i));
    if #newwdh == 1 and newwdh_0_1 == 1 then return irreducibleLieAlgebraModule(newwdh_0_0,simpleLieAlgebra(type,m));
    return new LieAlgebraModule from (simpleLieAlgebra(type,m),newwdh)
))
*-

fusionCoefficient=method(
--    TypicalValue=>ZZ,Options=>{MaxWordLength=>10})
    TypicalValue=>ZZ)
fusionCoefficient(LieAlgebraModule,LieAlgebraModule,LieAlgebraModule,ZZ) := (U,V,W,l) -> (
    if not isIrreducible W then error "third module must be irreducible";
    nu:=first keys W#"DecompositionIntoIrreducibles";
    fullFusionProduct:=(fusionProduct(U,V,l))#"DecompositionIntoIrreducibles";
    fullFusionProduct_nu
)

branchingRule = method ( TypicalValue => LieAlgebraModule )

branchingRule (LieAlgebraModule, String) :=
branchingRule (LieAlgebraModule, Matrix) :=
branchingRule (LieAlgebraModule, List) := (M,S) -> branchingRule(M,subLieAlgebra(M#"LieAlgebra",S))

branchingRule (LieAlgebraModule, LieAlgebra) := (M,h) -> ( -- here h must be a (known) subalgebra of that of M
    g:=M#"LieAlgebra";
    if g===h then return M; -- annoying special case
    S:=try h#"Embeddings"#g else error "not a Lie subalgebra";
    --    f:=if class S===List then a -> a_S else a -> entries(transpose S*vector a);
    f:=a -> entries(transpose S*vector a); -- lame but what we get for using Lists rather than vectors
    LieAlgebraModuleFromWeights(applyKeys(weightDiagram M,f,plus),h)
    )


