-- H_T first
-- input table of scalar products d<=3
scalar = matrix {{0,0,0,0,1,1,0,1,0,0,1,1,1,1,0,1,1,0,1,0,1,1,1,1,1,2,1},{1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,1,1,2,1,1,1,1},{1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,2,1,1,1,0,0,0,1,1,1,1,1},{1,1,1,0,1,1,1,0,0,0,1,0,0,1,0,1,1,1,1,0,0,1,1,1,2,1,0},{0,1,0,0,0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,1,1,1,1,2,1,1,1},{0,1,1,0,1,0,0,0,0,1,1,1,1,0,0,1,1,1,2,1,0,1,0,1,1,1,1},{1,0,1,0,1,1,0,1,0,1,0,0,1,0,1,1,2,1,1,0,0,1,1,0,1,1,1},{0,1,1,1,1,1,1,0,0,0,2,1,1,1,0,0,0,0,1,1,1,1,0,1,1,1,0},{1,0,1,1,1,2,1,1,0,0,1,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,0},{1,1,0,1,1,1,1,1,1,0,1,1,0,2,0,1,0,0,0,0,1,0,1,1,1,1,0},{1,1,1,0,0,0,1,0,1,1,0,0,0,0,1,1,1,2,1,1,0,1,1,1,1,0,1},{1,1,1,1,0,1,2,0,1,0,1,0,0,1,1,0,0,1,0,1,1,1,1,1,1,0,0},{1,2,1,1,1,0,1,0,1,1,1,1,0,1,0,1,0,1,1,1,0,0,0,1,1,0,0},{0,0,1,0,0,1,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,2,1,1,1,1,1},{1,1,1,1,2,1,0,1,0,1,1,1,1,1,0,1,1,0,1,0,0,0,0,0,1,1,0},{0,0,0,1,0,1,1,1,1,0,1,1,1,1,1,0,0,0,0,1,2,1,1,1,0,1,1},{0,1,0,1,1,0,0,1,1,1,1,2,1,1,0,1,0,0,1,1,1,0,0,1,0,1,1},{1,0,0,1,1,1,0,2,1,1,0,1,1,1,1,1,1,0,0,0,1,0,1,0,0,1,1},{1,1,0,1,0,0,1,1,2,1,0,1,0,1,1,1,0,1,0,1,1,0,1,1,0,0,1},{2,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,0,1,0,1,0,0},{1,1,2,1,1,1,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,1,0,0,1,0,0},{1,1,1,1,1,0,0,1,1,2,0,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1},{0,0,1,1,1,1,0,1,0,1,1,1,2,0,1,0,1,0,1,1,1,1,0,0,0,1,1},{1,0,1,1,0,1,1,1,1,1,0,0,1,0,2,0,1,1,0,1,1,1,1,0,0,0,1},{0,0,0,0,0,0,0,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,0,1,2},{0,1,1,1,0,0,1,0,1,1,1,1,1,0,1,0,0,1,1,2,1,1,0,1,0,0,1},{1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0}};
h:=FH_0_0; xbar:=FH_-1_1;
-- H_T fugacity in terms of scalar products d<=3
fug = matrix { { 1,0,0 },
    {h/(h-xbar),xbar/(h-xbar),0},
    {4*h^2/(h-xbar)/(4*h-xbar),h*xbar/(h-xbar)/(4*h-xbar),-xbar*(3*h-xbar)/(h-xbar)/(4*h-xbar)}
    };
fugacityH = p -> ( -- equivariant H
    states3:=makeStates 3;
    ind := x -> position(states3,y->y===x);
    n:=p.Length;
    defineFH n;
    product(n-1, i -> product(n-1-i, j -> (
                X := p#(i,j,1); W:=p#(i,j,0); U := p#(i+1,j,0);
                if p.Separation === null then (
                    X = ind X; W = ind W; U = ind U;
                    s := scalar_(U,X);
                    t := scalar_(W,X); -- print(i,j,X,W,U,s,t);
                    ) else if instance(p.Separation,QQ) then (
                    if X == W then ( s=1; t=1; ) else if X == U then (  s=1; t=0; ) else ( s=0; t=0; ); -- A_n scalar products ~ A_1 scalar products
                    ) else error "not implemented yet"; -- TODO D_n
                (map(FH_n,FH_-1,{FH_n_0,FH_n_(n-i)-FH_n_(j+1)})) fug_(s,t)
                ))))

q:=FK_0_0; zbar:=FK_-1_1;
--
fugacityK = p -> (
    d:=p.Steps;
    n:=p.Length;
    if p.Separation =!= null then (
	if instance(p.Separation,ZZ) then error "not implemented yet"; -- TODO D_n
	sep1 := a -> if a=="_" then p.Separation else first ascii a - 48;
    	tri := (a,b) -> if sep1 b < sep1 a then -q else 1;
	if p.Equivariant then (
	    defineFK n;
            product(n-1, i -> product(n-1-i, j ->
		    (
			z := FK_n_(n-i)/FK_n_(j+1);
			(a,b,c,d) := apply((p#(i+1,j,0),p#(i,j+1,1),p#(i,j,1),p#(i,j,0)),sep1); -- i,j,k,l
			if a==b then q*(1-z)/(1-q^2*z) else if a==d then 1
			else ((1-q^2)/(1-q^2*z)
			    * (if a<b then -q*z else -q^-1)
			    )
			)
                    )) * product(n,i->(
                    tri(p#(i,n-1-i,0),p#(i,n-1-i,1))
                    )
		)
	    ) else (
            product(n, i -> product(n-i, j -> tri(p#(i,j,0),p#(i,j,1))
		    * (if j+i==n-1 then 1 else (tri(p#(i+1,j,0),p#(i,j+1,1)))^(-1)
            )))
	)
    ) else if p.Equivariant then (
        (uptrifug,downtrifug) := try (myget ("fugacity-"|toString d|".m2"))(q) else error "K-fugacities not implemented for this value of d";
        --(uptrifug,downtrifug) := myget ("fugacity-"|toString d|".m2");
        rhfug := try (myget ("fugacity-equiv-"|toString d|".m2"))(q,zbar) else error "K-fugacities not implemented for this value of d";
	defineFK n;
        product(n-1, i -> product(n-1-i, j ->
                (map(FK_n,FK_-1,{FK_n_0,FK_n_(n-i)/FK_n_(j+1)})) rhfug#(p#(i+1,j,0),p#(i,j+1,1),p#(i,j,1),p#(i,j,0))
                )) * product(n,i->(
                uptrifug#(p#(i,n-1-i,0),p#(i,n-1-i,1),p#(i,n-1-i,2))
                )
            )
        ) else (
        (uptrifug,downtrifug) = try (myget ("fugacity-"|toString d|".m2"))(q) else error "K-fugacities not implemented for this value of d";
        product(n, i -> product(n-i, j ->
                uptrifug#(p#(i,j,0),p#(i,j,1),p#(i,j,2))
		* if j+i==n-1 then 1 else downtrifug#(p#(i+1,j,0),p#(i,j+1,1),p#(i,j,2))))
        )
    )

-- this is equivariant cohomology nongeneric d<=2 (phew)
-- missing almostsepdesc
fugacityH0 = p -> (
    n:=p.Length;
    defineFH n;
    product(n-1, i -> product(n-1-i, j -> if p#(i,j,2)=="" then FH_n_(n-i)-FH_n_(j+1) else 1))
    )

len := s -> #(replace("\\(|\\)","",s))
sign := (a,b,c) -> if a==b and a==c then (-1)^(len a-1) else if len a == len b+len c or len b==len a+len c or len c==len a+len b then 1 else -1

-- this is equivariant K-theory nongeneric d<=2 (phew)
-- missing sepdesc and almostsepdesc.
fugacityK0 = p -> (
    n:=p.Length;
    defineFK n;
    product(n-1, i -> product(n-1-i, j -> if p#(i,j,2)=="" then 1-FK_n_(j+1)*FK_n_(n-i)^-1 else (
		X := p#(i,j,1); W:=p#(i,j,0); U := p#(i+1,j,0); V := p#(i,j+1,1); C := p#(i,j,2);
		if p.Separation === null then (
		    (if (len X+len W>len U+len V) or (len X+len W==len U+len V and 
			    ((W=="2" and X=="20" and U=="1") or (W=="2" and X=="21" and U=="10") or (W=="20" and X=="0" and U=="21") or (W=="21" and X=="21" and U=="(21)0") or (W=="20" and X=="21" and U=="(21)0")))
		    	then FK_n_(j+1)*FK_n_(n-i)^-1 else 1)
		    *sign(C,W,X)*sign(U,V,C)
		    ) else ( -- X=m W=l U=i V=j
		    sep := toString (p.Separation-1/2);
		    if U==W and V==X and U!=X then 1
		    else if U!="_" and V!="_" and U<=sep and V>sep then -FK_n_(j+1)*FK_n_(n-i)^-1
		    else if U!="_" and V!="_" and V<U and (U<=sep or V>sep) then -1
		    else if (U=="_" and V>sep) or (U<=sep and V=="_") then FK_n_(j+1)*FK_n_(n-i)^-1
		    else 1
		))))
)

fugacity = true >> o -> p -> (
    if #o>0 then p = p ++ o; -- change options
    if p.Separation === null and p#Steps > 3 then error "fugacities not implemented yet for d>3";
    if not p.Equivariant and not p.Ktheory then return 1; -- ha
    if not p.Generic and not p.Equivariant then return (-1)^(inversion nwside p+inversion neside p-inversion bottom p); -- difference of inversion numbers -- careful with multinumber on bdry
    if not p.Generic and p.Separation === null and p#Steps>2 then error "cannot compute d>2 nongeneric equivariant fugacities";
    (if p.Generic then if p.Ktheory then fugacityK else fugacityH else if p.Ktheory then fugacityK0 else fugacityH0) p
    )

--tallyFugacities = true >> o -> L -> applyKeys(hashTable apply(L,p->p=>fugacity p),bottom,plus)
fugacityTally = true >> o -> L -> sum(L,p->new VirtualTally from {bottom p=>fugacity(p,o)})

fugacityVector = true >> o -> L -> (
    if #L === 0 then return 0; -- error "can't determine puzzle size";
    I := uniquePermutations new LabelList from sort(bottom(first L),ord);
    t := fugacityTally(L,o);
    vector apply(I,i->t_i)
    )

end

needsPackage "CotangentSchubert"
(M,FF,I)=setupCotangent(1,2,3,Ktheory=>true)
segreCls = segreClasses();
T=table(I,I,(i,j)->segreCls^(-1)*(segreClass i @ segreClass j));
TT=table(I,I,(i,j)->fugacityVector puzzle(i,j,Generic=>true,Equivariant=>true,Ktheory=>true));
T==TT

(M,FF,I)=setupCotangent(1,2,3,4,Ktheory=>true)
segreCls = segreClasses();
segreInv = segreCls^(-1);
