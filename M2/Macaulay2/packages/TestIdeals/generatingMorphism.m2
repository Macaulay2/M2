
generatingMorphism = method();

generatingMorphism(ZZ,Ideal) := (i,A) -> (
generatingMorphism (i,A)
)


generatingMorphism(Ideal) := (A) -> (
generatingMorphism (A)
)



generatingMorphism= (i,I) ->(
    	local A,U;
    	R:=ring(I);
	p:=char(R);
	f:=inducedMap(coker gens I, coker gens frobeniusPower(p,I));
	resf:=res f;
	G:=Hom(resf, R^1);
	E:=prune (HH^i G);
	if (E==0) then
	{
	    A=matrix{{1_R}};
	    U=matrix{{0_R}};
	}
    	else
	{
	    A=matrix entries relations source E;
	    U=matrix entries E;
	};
	(A,U)
)


generatingMorphism= (I) ->(
    	local A,U;
	answer:=new MutableHashTable;
    	R:=ring(I);
	p:=char(R);
	f:=inducedMap(coker gens I, coker gens frobeniusPower(p,I));
	resf:=res f;
	G:=Hom(resf, R^1);
	for i from 0 to length source resf do
	{ 
	    E:=prune(HH^i G);
	    if (E==0) then
	    {
	    	A=matrix{{1_R}};
	    	U=matrix{{0_R}};
	    }
    	    else
	    if (E!=0) then
	    {
	    	A=matrix entries relations source E;
	    	U=matrix entries E;
	    	answer#i=(A,U);
	    };
	};
	answer
)



isFInjectiveViaExt= (I) ->(
    local i;
    answer:=true;
    g:=generatingMorphism (I);
    for i in keys(g) do
    if answer then
    {
	A:=(g#i)#0;
	U:=(g#i)#1;
	print(i,A,U);
	M:=frobeniusRoot(1,compress U);
	M=M| A;
	M=coker M;
	if (M!=0) then answer=false;
    };
    answer
)


