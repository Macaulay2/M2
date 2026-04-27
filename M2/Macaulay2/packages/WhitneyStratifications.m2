newPackage(
	"WhitneyStratifications",
	Version => "2.23", 
    	Date => "June 21, 2025",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "martin.helmer@swansea.ac.uk", 
		  HomePage => "http://martin-helmer.com/"}},
    	Headline => "Compute Whitney Stratifications",
    	DebuggingMode => false,
	PackageImports=>{"Elimination","PrimaryDecomposition","Saturation","SegreClasses", "Msolve"},
	Keywords => {"Algebraic Geometry"}
	);
export{
    "conormal",
    "conormalRing",
    "whitneyStratify",
    "polarVars", 
    "eulerObsMatrix",
    "mapStratify", 
    "isProper",
    "nonProperSet",
    "coordProj",
    "StratsToFind",
    "AssocPrimes",
    "polarSequence",
    "minCoarsenWS",
    "whitneyStratifyPol",
    "mapStratifyPol",
    "multCC"
    }


--multCatchErr=method(TypicalValue=>List,Options=>{Print=>false});
multCatchErr=(X,Y)->(
    mult:=0;
    try mult= multiplicity(X,Y) then(return mult;) else (return multCatchErr(X,Y));
    return mult;
    )

eulerObsMatrix=method(TypicalValue=>List,Options=>{Print=>false,Algorithm=>"saturate"});
eulerObsMatrix(MutableHashTable):=opts->V->(
    dimsV:=keys V;
    strata:= flatten for k in keys(V) list (for w in V#k list {k,w});
    dimV:=max dimsV;
    R:=ring last last strata;
    kk:=ZZ/32749;
    R2:=kk[gens R];
    m:=#strata;
    Eu:=mutableMatrix(QQ,m,m);
    for b from 1 to m  do(
	Zb:=sub(last strata_(m-b),R2);
	if opts.Print then <<"big= "<<last strata_(m-b)<<endl;
	db:=first strata_(m-b);
	polsZb:=polarVars(Zb,Print=>opts.Print, Algorithm=>opts.Algorithm);
	Eu_(m-b,m-b)=1;--(-1)^db;
	for a from 0 to m-b-1 do(
	    if opts.Print then <<"little= "<<last strata_a<<endl;
	    psaZ:=polarSequence(sub(last strata_a,R2),Zb,polsZb,Print=>opts.Print, Algorithm=>opts.Algorithm);
	    da:=first strata_a;
	    if opts.Print then <<"Current Polar Sequence= "<<psaZ<<endl;
	    --Eu_(a,m-b)=(-1)^(db)*sum(0..#psaZ-1,k->(-1)^(#psaZ-1-k)*psaZ_k);
	    Eu_(a,m-b)=sum(0..#psaZ-1,k->(-1)^(#psaZ-1-k)*psaZ_k);
	    );
	if opts.Print then <<"Current Eu= "<<matrix Eu<<endl;
	);
    --polsZ:=polarVars(sub(Z,R2));
    use R;
    return {lift(matrix Eu,ZZ), strata};
    );
multCC=method(TypicalValue=>List,Options=>{Print=>false,Algorithm=>"saturate"});
multCC(MutableHashTable):=opts->V->(
    Eu:= eulerObsMatrix(V, Print=>opts.Print, Algorithm=>opts.Algorithm);
    A:=mutableMatrix lift(first Eu,ZZ);
    strata:=last Eu;
    dimV:=max(for s in strata list first s);
    for i from 0 to #strata-1 do(
	if (dimV-first(strata_i))%2==1 then columnMult(A,i,-1);
	);
    if opts.Print then <<"Eu scaled= "<<matrix(A)<<endl;
    if opts.Print then <<"Eu scaled= "<<toString matrix(A)<<endl;
    if opts.Print then <<"Eu scaled= "<<tex matrix(A)<<endl;
    b:=transpose matrix {for i from 1 to #strata list 1};
    sol:=flatten entries (solve(matrix(A),b));
    if opts.Print then <<"CC mults= "<<sol<<endl;
    strataCCmult:=for i from 0 to #strata-1 list {sol_i,strata_i};
    return {matrix A, strataCCmult};
    );

whitneyStratifyPol=method(TypicalValue=>MutableHashTable,Options=>{Print=>false,Algorithm=>"saturate"});
whitneyStratifyPol (Ideal):=opts->(I)->(
    c:=codim(I);
    R:=ring I;
    n:=numgens(R);
    dimI:=n-codim(I);
    V:=new MutableHashTable;
    for i from 0 to dimI-1 do V#i=new MutableList;
    --<<"decompose I"<<endl;
    V#dimI=decompose I;
    level:=n-c;
    J:=ideal mingens(I+minors(codim I,jacobian I));
    PolsIa:={};
    PolsIa2:={};
    pols2:={};
    tempJ:=0;
    if dimI<2 then(
	if opts.Print then <<"decompose sing(I)"<<endl;
	tempJ=decompose(J);
	for K in tempJ do(
	    for i from 0 to dimI-1 do(
		if i==(n-codim(K)) and (any(toList(V#i),jj->K==jj)==false) then (V#i)#(#(V#i))=K;
		);
	    );
	for a from 1 to level do V#(dimI-a)=unique toList(V#(dimI-a)); 
	use R;
	return V;
	);
    if codim(J)==n then V#0= decompose J;
    if codim(J)>=n then return V;
    for a from 0 to level-1 do(
	--add singularities arising from intersections of current dimensional pieces 
        intse:=unique flatten for s in subsets(toList(V#(dimI-a)),2) list decompose ideal mingens(sum(s));
        for th in intse do(
            Ti:=n-codim(th);
	    (V#Ti)#(#(V#Ti))=th;
	    );
	--now find singularities and Whitney points of current dim pieces 
	for Ia in V#(dimI-a) do(
	    PolsIa={};
	    pols2={};
	    dimIa:=dimI-a;
	    if opts.Print then<<"sing minors below dim "<<dimI-a<<endl;
	    J=radical ideal mingens(Ia+minors(codim Ia,jacobian Ia));
	    mu:=n-codim(J);
	    ell:=0;
	    for K in decompose(J) do(
		ell=n-codim(K);
		(V#ell)#(#(V#ell))=K;
		);
	--	);
	    if mu>0 then(
		if opts.Print then<<"find pols "<<dimI-a<<endl;
		PolsIa=polarVars(Ia,Print=>opts.Print, Algorithm=>opts.Algorithm);
		);
	    if opts.Print then<<"decompose pols+ sing"<<endl;
		pols2=flatten for p in PolsIa list decompose(J+p);
	    --add all singularities of Ia
	    for K in pols2 do(
		for i from 0 to dimIa-1 do(
		    if i==(n-codim(K)) and (any(toList(V#i),jj->K==jj)==false) then (V#i)#(#(V#i))=K;
		    );
		);
	    --now take polar vars

	    );
	);
    for a from 1 to level do V#(dimI-a)=unique toList(V#(dimI-a));
    use R;
    return V;
    );

mapStratifyPol=method(TypicalValue=>MutableHashTable,Options=>{isProper=>true,StratsToFind=>"all",Print=>false,AssocPrimes=>true, Algorithm=>"saturate"});
mapStratifyPol (List,Ideal,Ideal):=opts->(F,X,Y)->(
    R:=ring X;
    S:=ring Y;
    kk:=coefficientRing R;
    if opts.Print then <<"R= "<<R<<"S= "<<S<<endl;
    conRingX:=conormalRing(X);
    conRingY:=conormalRing(Y);
    if opts.Print then<<"conRingX= "<<gens(conRingX)<<endl;
    n:=numgens(R);
    m:=numgens(S);
    if opts.Print then<<"isProper= "<<opts.isProper<<endl;
    V:=new MutableHashTable;
    VcheckedBeforeInd:=new MutableHashTable;
    W:=new MutableHashTable;
    WcheckedBeforeInd:=new MutableHashTable;
    fIsCoordProjection:=false; 
    if opts.Print then<<"F= "<<F<<", gens S= "<<gens S<<endl;
    graphR:=0;
    elimIm:=0;
    elimInv:=0;
    Gam:=0;
    y:=0;
    if all(F, i->any(gens R,j->i===j)) then(
	if opts.Print then <<"the map is a coordinate projection"<<endl;
	fIsCoordProjection=true;
	graphR=R;
	elimIm=toList(set(gens(R))-set(F));
	elimInv={};
	Gam=ideal(0_graphR); 
	y=F;
	)
    else (
	graphR=kk[gens R, gens S];
	--forward map
	elimIm=(gens graphR)_{0..numgens(R)-1};
    	--inverse map
    	elimInv=(gens graphR)_{numgens(R)..numgens(graphR)-1};
    	y=(gens graphR)_{numgens(R)..numgens(graphR)-1};
    	Gam=ideal for i from 0 to numgens(S)-1 list y_i-sub(F_i,graphR);
	);
    if opts.Print then <<"graph R= "<<gens(graphR)<<endl;
    if opts.Print then <<"elimIm= "<<elimIm<<endl;
    if opts.Print then <<"elimInv= "<<elimInv<<endl;
    dimX:=n-codim(X);
    tempSubsetsV:=0;
    tempSubsetsW:=0; 
    dimY:=m-codim(Y);
    fX:=ideal mingens(sub(eliminate(elimIm,Gam+sub(X,graphR)),S)+Y);
    dimfX:=m-codim(fX);
    for i from 0 to dimX-1 do V#i=new MutableList;
    for i from 0 to dimfX-1 do W#i=new MutableList;
    
    for i from 0 to dimX do VcheckedBeforeInd#i=0;
    for i from 0 to dimfX do WcheckedBeforeInd#i=0;
    if opts.Print then print "decompose X";
    V#dimX=decompose X;  
    if opts.Print then print "decompose Y";
    W#(dimY)=decompose Y;
    if fX!=Y then(
	Y=fX;
	dimY=dimfX;
	W#(dimY)=decompose Y;
	);
    if opts.isProper==false then(
	if opts.Print then<<"Computing Jolenck flag"<<endl;
	JelY:=new MutableList from {};
	JelX:=new MutableList from {};
	JelY#0=Y;
	JelX#0=X;
	for i from 0 to dimY-1 do(
	    if opts.Print then<<"Jel#"<<i<<" ="<<JelY#i<<endl;
	    JelY#(i+1)=nonProperSet(F,JelX#i,Y);
	    JelX#(i+1)=ideal mingens(sub(eliminate(elimInv,Gam+sub(JelY#(i+1),graphR)),R)+X);
	    );
	for jel in unique toList(JelY) do(
	    for comp in decompose (jel+Y) do(
		dimjel:=m-codim(comp);
		if (any(toList(W#dimjel),jj->comp==jj)==false) then (W#dimjel)#(#(W#dimjel))=comp;	   
		);
	    );
	for jel in unique toList(JelX) do(
	    for comp in decompose (jel+X) do(
		dimjel:=n-codim(comp);
		if (any(toList(V#dimjel),jj->comp==jj)==false) then (V#dimjel)#(#(V#dimjel))=comp;	   
		);
	    );
	);
    if opts.Print then print "past Jel check/computation";	
    if opts.Print then print "V";
    if opts.Print then for ke in keys V do print toList(V#ke);
    if opts.Print then print "W";
    if opts.Print then for ke in keys W do print toList(W#ke);
    aX:=0;
    aY:=0;
    JY:=0;dimYa:=0;muY:=0;ellY:=0;dimKinv:=0;Kdim:=0;Kinv:=0;ell:=0;Ti:=0;intse:=0;CYa:=0;CpullYa:=0;PolsYa:=0;
    JX:=0;muX:=0;ellX:=0;Kpf:=0;AX:=0;WhitXa:=0;rXa:=0;IiXf:=0;Cpull:=0;CXa:=0;PolsXa:=0;KDim:=0;WhitYa:=0;
    Ya:=0;
    Xa:=0;
    tYind:=0;
    tXind:=0;
    YaNew:=0;YaOld:=0;
    XaNew:=0;XaOld:=0;
    while (aX<dimX) or (aY<dimY) do(
	if opts.Print then <<"{aY, aX}= "<<{aY,aX}<<endl;
	-- Y at codim>a 
	if (dimY-aY)>0 then(
	    if opts.Print then print "In Y";
	    	--add subset intersections of Y at level aY
		tYind=WcheckedBeforeInd#(dimY-aY);
		YaNew=(toList(W#(dimY-aY)))_(toList(tYind..(#(W#(dimY-aY))-1)));
		YaOld=(toList(W#(dimY-aY)))_(toList(0..tYind-1));
		tempSubsetsW=join(flatten for a1 in YaOld list(for b1 in YaNew list {a1,b1}),subsets(YaNew,2));
		--TODO need to add intersections of the form 1 from before tYind and 1 from after tYind
		if opts.Print then<<"starting subset sing at "<<dimY-aY<<", num subsets W= "<<#tempSubsetsW<<endl;
		intse=unique flatten for s in tempSubsetsW list decompose ideal  mingens(sum(s));
		for th in intse do(
		    Ti=m-codim(th);
		    if opts.Print then <<"Y Whit= "<<th<<endl;
		    if (any(toList(W#Ti),jj->th==jj)==false) then (
			(W#Ti)#(#(W#Ti))=th;
			if opts.Print then<<"added to Y strat= "<<th<<endl;
			);
		    --Pullback to X, add to X strat, V
		    Kinv=ideal mingens(sub(eliminate(elimInv,Gam+sub(th,graphR)),R)+X);
		    for comp in decompose Kinv do(
			dimKinv=n-codim(comp);
			if opts.Print then <<"Y Whit in X= "<<comp<<endl;
			if (any(toList(V#dimKinv),jj->comp==jj)==false) then(
			    (V#dimKinv)#(#(V#dimKinv))=comp;
			    					--have unprocessed components above current dim, go back up
					if dimKinv>(dimX-aX) then aX=dimX-dimKinv;
			    );
			);
		    );
	    for j2 from WcheckedBeforeInd#(dimY-aY) to #(W#(dimY-aY))-1 do(
		Ya=(W#(dimY-aY))#j2;
		WcheckedBeforeInd#(dimY-aY)=WcheckedBeforeInd#(dimY-aY)+1;
	    	dimYa=dimY-aY;
	    	JY=radical ideal mingens(Ya+minors(codim Ya,jacobian Ya));
	    	muY=m-codim(JY);
	    	ellY=0;
	    	WhitYa=0;
		if muY==0 then (
		   --<<"adding dim 0 sings"<<endl;
		    for K in decompose(JY) do(
		    --Add to Y strat, W
		    KDim=m-codim(K);
		    if (any(toList(W#KDim),jj->(K)==jj)==false) then (W#KDim)#(#(W#KDim))=ideal mingens(K);
		    --Pullback to X, add to X strat, V
		    Kinv=ideal mingens(sub(eliminate(elimInv,Gam+sub(K,graphR)),R)+X);
		    for comp in decompose Kinv do(
			dimKinv=n-codim(comp);
			if (any(toList(V#dimKinv),jj->comp==jj)==false) then(
			    (V#dimKinv)#(#(V#dimKinv))=comp;	   	
			    --have unprocessed components above current dim, go back up
			    if dimKinv>(dimX-aX) then aX=dimX-dimKinv;
			    );
			);
		    );
		);	
		if muY>0 and (opts.StratsToFind!="singularOnly") then (

		    PolsYa=polarVars(ideal mingens Ya, Print=>opts.Print, Algorithm=>opts.Algorithm);
		    WhitYa=flatten for p in PolsYa list decompose(JY+p);
			    if opts.Print then <<"done assoc primes "<<endl;
			    for K in WhitYa do(
				--Add to Y strat, W
			    	KDim=m-codim(K);
				if (any(toList(W#KDim),jj->(K)==jj)==false) then (W#KDim)#(#(W#KDim))=ideal mingens(K);
				--<<"added Whit Strata Y"<<endl;
				--Pullback to X, add to X strat, V
				Kinv=ideal mingens(sub(eliminate(elimIm,Gam+sub(K,graphR)),R)+X);
				for comp in decompose Kinv do(
				    dimKinv=n-codim(comp);
				    if (any(toList(V#dimKinv),jj->comp==jj)==false) then(
					(V#dimKinv)#(#(V#dimKinv))=comp;
					--have unprocessed components above current dim, go back up
					if dimKinv>(dimX-aX) then(
					    aX=dimX-dimKinv;
					    );
					);	   
				    );
			    	);
		    );
	    	);
	    --now go down a level in Y, if there are levels left
		aY=aY+1;
	 --end Y block   
	 );
	--X at codim> a
	--TODO, change X block to look like Y block above with new index check etc. 
	if (dimX-aX)>0 then(
	    if opts.Print then print "In X";
	    	--subset intersections at level aX of X
		tXind=VcheckedBeforeInd#(dimX-aX);
		XaNew=(toList(V#(dimX-aX)))_(toList(tXind..(#(V#(dimX-aX))-1)));
		XaOld=(toList(V#(dimX-aX)))_(toList(0..tXind-1));
		tempSubsetsV=join(flatten for a1 in XaOld list(for b1 in XaNew list {a1,b1}),subsets(XaNew,2));
	    	--Add subset intersections of Xa, push forward
		if opts.Print then<<"starting subset sing at "<<dimX-aX<<", num subsets V= "<<#tempSubsetsV<<endl;
		    intse=unique flatten for s in tempSubsetsV list decompose ideal  mingens(sum(s));
		    for th in intse do(
		    	Ti=n-codim(th);
		    	if opts.Print then<<"X whit subsets= "<<th<<endl;
		    	if (any(toList(V#Ti),jj->th==jj)==false) then (V#Ti)#(#(V#Ti))=th;
		    	--Pushforward to Y, add to Y strat, W
		    	Kinv=ideal mingens(sub(eliminate(elimIm,Gam+sub(th,graphR)),S)+Y);
		    	for comp in decompose Kinv do(
			    dimKinv=m-codim(comp);
			    if opts.Print then<<"X whit in Y "<<comp<<endl;
			    if (any(toList(W#dimKinv),jj->comp==jj)==false) then (
				(W#dimKinv)#(#(W#dimKinv))=comp;
				--have unprocessed components above current dim, go back up
				if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
				if opts.Print then<<"Added to Y Sing = "<<comp<<endl;
				if opts.Print then<<"Dim th= "<<dim(th)<<", From X part= "<<th<<endl;
				if opts.Print then<<", intse="<<intse<<endl;
				);
			    );
		    	);
	    for j2 from VcheckedBeforeInd#(dimX-aX) to #(V#(dimX-aX))-1 do(
		Xa=(V#(dimX-aX))#j2;
		VcheckedBeforeInd#(dimX-aX)=VcheckedBeforeInd#(dimX-aX)+1;
		if opts.Print then<<"Xa= "<<Xa<<endl;
	    	dimXa:=dimX-aX;
	    	JX=radical ideal mingens(Xa+minors(codim Xa,jacobian Xa));
		if opts.Print then<<"JX= "<<JX<<endl;
	    	muX=n-codim(JX);
	    	ellX=0;
	    	WhitXa=0;
		if muX==0 then (
		    for K in decompose(JX) do(
			Ti=n-codim(K);
			if opts.Print then<<"X Whit= "<<K<<endl;
			if (any(toList(V#Ti),jj->K==jj)==false) then (V#Ti)#(#(V#Ti))=ideal mingens(K);
			);
		    );
		AX=jacobian(Xa+ideal(F));
		if opts.Print then<<"AX= "<<AX<<endl;
		--todo, use the tighter bound here
		rXa=min(numrows(AX), numcols(AX));
		for i from 0 to rXa do(
		    IiXf=ideal mingens(Xa+minors(rXa-i,AX));
		    if opts.Print then<<"IiXf= "<<IiXf<<endl;
		    if IiXf==ideal(1_R) then break;
		    --<<" In TB starting decompose of IiXf"<<endl;
		    for K in decompose(IiXf) do(
			--add to X strat, V
			Ti=n-codim(K);
			if opts.Print then<<"X Tb 1= "<<K<<endl;
			if opts.Print then<<"V#"<<Ti<<" ="<<toList(V#Ti)<<", ring= "<<(for v in toList(V#Ti) list gens(ring(v)))<<endl;
			if (any(toList(V#Ti),jj->K==jj)==false) then (
			    (V#Ti)#(#(V#Ti))=K;
			    if opts.Print then<<"Added to X TB = "<<K<<endl;
			    );
			--add image to Y strat, W
			Kpf=ideal mingens(sub(eliminate(elimIm,Gam+sub(K,graphR)),S)+Y);
			--<<"TB X decomposing in Y"<<endl;
		       	for comp in decompose Kpf do(
		            dimKinv:=m-codim(comp);
			    if opts.Print then<<"Y TB= "<<comp<<endl;
			    if (any(toList(W#dimKinv),jj->comp==jj)==false) then(
				(W#dimKinv)#(#(W#dimKinv))=comp;
				--Unprocessed components detected above, go back up
				if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
				if opts.Print then<<"Added to Y TB = "<<comp<<endl;
				);
			    );
			--<<"done inner decompose"<<endl;
		    	);
		    );
		--<<"done TB"<<", muX="<<muX<<endl;
		if opts.Print then<<"done TB"<<", muX="<<muX<<endl;
		if muX>0 and (opts.StratsToFind!="singularOnly") then (
		    PolsXa=polarVars(ideal mingens Xa,Print=>opts.Print, Algorithm=>opts.Algorithm);
		    --<<"done polar vars, start decompose"<<endl;
		    WhitXa=flatten for p in PolsXa list decompose(JX+p);
			    for K in WhitXa do(
			    	--only add elements that are new
			    	Ti=n-codim(K);
				--<<"X whit= "<<K<<endl;
				if (any(toList(V#Ti),jj->K==jj)==false) then(
				    (V#Ti)#(#(V#Ti))=ideal mingens(K);
				   -- <<"added Whit Strata X"<<endl;
			    	    if opts.Print then<<"X whit= "<<K<<endl;
				    --<<"X whit= "<<K<<endl;
		       	    	    Kpf=ideal mingens(sub(eliminate(elimIm,Gam+sub(K,graphR)),S)+Y);
		       	    	    for comp in decompose Kpf do(
		       	    	    	dimKinv=m-codim(comp);
				    	if opts.Print then<<"X whit in Y "<<comp<<endl;
	       		 	    	if (any(toList(W#dimKinv),jj->comp==jj)==false) then(
					    (W#dimKinv)#(#(W#dimKinv))=comp;
					    if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
					    );
     				    	);
			    	    );
			    	);

		    );
		);
	    aX=aX+1;
		    if opts.Print then<<"|W|= "<<toString(sum for k in keys W list #(W)#k)<<endl;
		    if opts.Print then<<"|V|= "<<toString(sum for k in keys V list #(V)#k)<<endl;
	    	);
    	);
    for a in keys(V) do V#a=unique toList(V#a); 
    for a in keys(W) do W#a=unique toList(W#a);
    use R;  
    return {V,W};
    );

polarSequence=method(TypicalValue=>ZZ,Options=>{Print=>false, Algorithm=>"saturate"});
polarSequence(Ideal,Ideal):=opts->(X,Z)->(
    R:=ring X;
    kk:=ZZ/32749;
    R2:=kk[gens R];
    polsZ:=polarVars(sub(Z,R2),Print=>opts.Print, Algorithm=>opts.Algorithm);
    return polarSequence(sub(X,R2),sub(Z,R2),polsZ,Print=>opts.Print, Algorithm=>opts.Algorithm); 
    );
polarSequence(Ideal,Ideal,List):=opts->(X,Z,polsZ)->(
    R:=ring X;
    kk:=coefficientRing(R);
    R1:=kk[gens R];
    isHom:=false;
    LX:=0;
    temp:=0;
    if (not isHom) then(
	R1=kk[getSymbol "thhhh",gens R];
	LX=ideal for i from 1 to dim(X) list random(1,R1)-random(kk);
	)
    else(
	LX=ideal for i from 1 to (numgens(R1)-codim(X)) list random(1,R1);
	);
    x0:= first gens R1;
    polSeq:=new MutableList from {};
    ind:=0;
    for p in polsZ do(
	temp=saturate(homogenize(sub(p+X,R1)+LX,x0));
	if temp==ideal(1_R1) then(
	    polSeq#ind=0;
	    )
	else(
	    --if opts.Print then
	    if opts.Print then <<"mult calc started, coeff ring="<<coefficientRing(R1)<<endl;
	    polSeq#ind=multCatchErr(temp,homogenize(sub(p,R1),x0));
	    );
	ind=ind+1;
	);
    if opts.Print then print "done pol seq";
    if opts.Print then <<"pol seq= "<<toList(polSeq)<<endl;
    use R;
    return toList polSeq;
    );
minCoarsenWS=method(TypicalValue=>MutableHashTable,Options=>{Print=>false, Algorithm=>"saturate"});
minCoarsenWS(MutableHashTable):=opts->(Vin)->(
    V:=new MutableHashTable from Vin;
    W:=new MutableHashTable;
    polHash:=new MutableHashTable;
    polMultHash:=new MutableHashTable;
    kV:=rsort keys V;
    W#(kV_0)=V#(kV_0);
    PassedSing:=false;
    I:=intersect V#(kV_0);
    R:=ring I;
    R1:=R;
    R2:=ZZ/65521[gens R1];
    kk:=coefficientRing R;
    isHom:=isHomogeneous(I);
    Z:=0;
    Y:=0;
    X:=0;
    Zlist:={};
    Ylist:={};
    Yj:=0;
    contX:={};
    contY:={};
    polXZ:={};
    polYZ:={};
    Xadded:=false;
    if opts.Print then<<"kV= "<<kV<<endl;
    if (not isHom) then R1=kk[getSymbol "wthhhh",gens R];
    x0:=first gens R1;
    R3:=ZZ/65521[gens R1];
    curL:={};
    for i from 1 to #kV-1 do(
	if opts.Print then<<"i= "<<i<<endl;
	if PassedSing then(
	    curL:=new MutableList from {};
	    for m from 0 to #V#(kV_i)-1 do(
		X=(V#(kV_i))_m;
		if opts.Print then<<"checking X= "<<X<<endl;
		Xadded=false;
		for j from 1 to i-1 do(
		    if opts.Print then<<"i= "<<i<<", j= "<<j<<endl;
		    if V#(kV_(i-j))!={} then(
			Ylist=V#(kV_(i-j));
			Yj=i-j;
			contX=positions(Ylist, K->isSubset(K,X));
			if opts.Print then<<"cont X= "<<contX<<endl;
			break;
			);
		    );
		if (#contX>1) then(
		    curL#(#curL)=X;
		    Xadded=true;
		    );
		if #contX==1 then(
		    if Ylist== {} then (print "something went wrong"; return {};);
		    Y=Ylist_(first contX);
		    if isHom then(
			if multCatchErr(sub(X,R2),sub(Y,R2))>1 then(
			    if opts.Print then<<"X is singular in higher piece "<<endl;
			    curL#(#curL)=X;
			    Xadded=true;
			    );
			)
		    else(
			if multCatchErr(sub(homogenize(sub(X,R1),x0),R3),sub(homogenize(sub(X,R1),x0),R3))>1 then(
			    curL#(#curL)=X;
			    Xadded=true;
			    );
			);
		    );
		if opts.Print then<<"starting polar test, i= "<<i<<", Yj= "<<Yj<<endl;
		if (not Xadded) and (not (#contX==0)) then(
		    for l from 1 to Yj do(
			Zlist=V#(kV_(Yj-l));
			contY=positions(Zlist, K->isSubset(K,X));
		        if opts.Print then<<"l= "<<l<<", Zlist= "<< Zlist_contY<<endl;
			for b from 0 to #contY-1 do(
			    Z=Zlist_b;
			    if not polHash#?(kV_(Yj-l),b) then(
				if opts.Print then<<"finding polar for Z= "<<Z<<endl;
				polHash#(kV_(Yj-l),b)=polarVars(sub(Z,R2),Print=>opts.Print, Algorithm=>opts.Algorithm);
				) else (if opts.Print then print "using cached polar var";);
			    --
			    if opts.Print then<<"starting pol sequence: X, Z= "<<X<<", "<<Z<<endl;
			    if not polMultHash#?(kV_i,m,kV_(Yj-l),b) then(
				if opts.Print then print "pol seq not cached";
				polMultHash#(kV_i,m,kV_(Yj-l),b)=polarSequence(sub(X,R2),sub(Z,R2),polHash#(kV_(Yj-l),b),Print=>opts.Print);
				) else (if opts.Print then print "using cached polar seq";);
			    polXZ=polMultHash#(kV_i,m,kV_(Yj-l),b);
			    if opts.Print then<<"starting pol sequence: Y, Z= "<<Y<<", "<<Z<<endl;
			    if not polMultHash#?(kV_(Yj),first contX,kV_(Yj-l),b) then(
				if opts.Print then print "pol seq not cached";
				polMultHash#(kV_(Yj),first contX,kV_(Yj-l),b)=polarSequence(sub(Y,R2),sub(Z,R2),polHash#(kV_(Yj-l),b), Print=>opts.Print);
				)else (if opts.Print then print "using cached polar seq";);
			    polYZ=polMultHash#(kV_(Yj),first contX,kV_(Yj-l),b);
			    if opts.Print then<<"Y= "<<Y<<endl;
			    if opts.Print then<<"Z= "<<Z<<endl;
			    if opts.Print then<<"polXZ= "<<polXZ<<", polYZ= "<<polYZ<<endl;
			    if opts.Print then<<"Y= "<<Y<<endl;
			    if opts.Print then<<"Z= "<<Z<<endl;
			    if opts.Print then<<"polXZ= "<<polXZ<<", polYZ= "<<polYZ<<endl;
			    if polXZ!=polYZ then(
				curL#(#curL)=X;
				Xadded=true;
				);
			    );
			);
		    );
		if opts.Print then print "done pol test";
		--if (not Xadded) then V#(kV_i)=delete(X,V#(kV_i));
		);
	    W#(kV_i)=toList curL;
	    if opts.Print then print "cur L added";
	    );
	if opts.Print then<<"PassedSing= "<<PassedSing<<endl;
	if not PassedSing then (
	    W#(kV_i)=V#(kV_i);
	    if (V#(kV_i)!={}) then PassedSing = true;
	    );
	);
    return W;
    );
nonProperSet=method(TypicalValue=>Ideal);
nonProperSet (List,Ideal,Ideal):=(F,X,Y)->(
    --<<"non Properness"<<endl;
    R:= ring X;
    S:=ring Y;
    S1:=ring Y;
    if #F!= numgens(S) then (print "map does not match given image space"; return 0;);
    gR:=0;
    GamH:=0;
    kk:=coefficientRing R;
    n:=numgens(R);
    m:=numgens(S);
    isCordProj:=false; 
    --if all(F, i->isMember(i,gens R)) and (Y==ideal(0_S)) then isCordProj=true; 
    if all(F, i->any(gens R,j->i===j)) and (Y==ideal(0_S)) then isCordProj=true;
    if isCordProj then(
	--<<"coord proj"<<endl;
	tttt:=symbol tttt;
	S=kk[tttt_0..tttt_(numgens(S)-1)];
	Y=ideal(0_S);
	);
    gR=kk[gens R,local u,gens S,Degrees=>join(for i from 0 to n list {1,0},for i from 1 to m list {0,1})];
    y:=(gens gR)_{n+1..numgens(gR)-1};
    Gam:=ideal groebnerBasis(ideal(for i from 0 to numgens(S)-1 list y_i-sub(F_i,gR))+sub(X,gR));
    GamH=ideal for i from 0 to numgens(Gam)-1 list sum(terms Gam_i,t->u^(first(degree(Gam_i))-first(degree(t)))*t);
    IGam:=intersect for i from 0 to n-1 list ideal mingens(eliminate((gens gR)_{0..n},sub(GamH+ideal(u),gR_i=>1)));
    if isCordProj then(
	return sub(sub(IGam, for i from 0 to #F-1 list y_i=>sub(F_i,gR)),S1);
	)
    else(
	return sub(IGam,S)+Y;
	);
    );


saturateHelper = (S,K,J)-> (
    JsatPoly:=sum(flatten entries gens J, f->(-1)^(random(ZZ))*random(1,2000)*f);
    K2:=sub(K,S)+ideal(1-S_0*sub(JsatPoly,S));
    return sub(ideal (selectInSubring(1,groebnerBasis(K2, Strategy=>"F4"))),ring J);
    );

polarVars=method(TypicalValue=>Ideal,Options=>{Print=>false,Algorithm=>"saturate"});
polarVars(Ideal):=opt->(I)->(
    R:=ring I;
     kk:=coefficientRing R;
     t3t:=symbol t3t;
     S:=kk[t3t, gens R, MonomialOrder => Eliminate 1];
     n:=numgens(R)-1;
     J:=radical ideal mingens(I+ minors(codim I, jacobian I));
     pol:=new MutableList from {};
     M:=0;
     K:=0;
     K3:=0;
     for l from 1 to n+1+(1-codim(I)) do(
	 M=(jacobian I)|(transpose matrix for j from 1 to l list (for i from 1 to numgens R list ((-1)^(random(ZZ))*random(1,9155))));
	 K=I+minors(codim(I)+l,M);
	 if opt.Algorithm=="M2F4" then(
	     if opt.Print then<<"M2 F4"<<endl;
	     K3=saturateHelper(S,K,J);
	     )
	 else if(opt.Algorithm=="msolve") then(
	     if opt.Print then<<"msolve"<<endl;
	     JsatPoly:=sum(flatten entries gens J, f->(-1)^(random(ZZ))*random(1,2000)*f);
	     K2:=sub(K,S)+ideal(1-S_0*sub(JsatPoly,S));
	     if char(kk)==0 then(
		 K3=sub(msolveEliminate(S_0, K2),R);
		 )
	     else(
		 K5:=msolveEliminate(S_0, K2);
		 K3=sub(ideal(selectInSubring(1,gens sub(K5,S))),R);
		 );

	     )
	 else(
	     if opt.Print then<<"doing saturate"<<endl;
	     K3=saturate(K,J);
	     );
	 pol#(#pol)=K3;
	 );
     return toList pol;
     );


conormalRing=method(TypicalValue=>Ring);
conormalRing(Ideal):=(I)->(
     return conormalRing(ring I);
    );
conormalRing(Ring):=(R)->(
    n:=numgens(R)-1;
    kk:=coefficientRing R;
    degs:=join(for i from 0 to n list {1,0},for i from 0 to n list {0,1});
    v:=symbol v;
    S:=kk[gens(R),v_0..v_n,Degrees=>degs];
    return S;
    );
conormalRing(Ideal,List,List):=(I,Alist,alpha)->(
    R:=ring I;
    n:=numgens(R)-1;
    kk:=coefficientRing R;
    degs:=join(for i from 0 to n list {1,0},for i from 0 to n list {0,1});
    indset:=for vert in alpha list position(Alist,i->i==vert);
    v:=symbol v;
    S:=kk[gens(R),for i in indset list v_i,Degrees=>degs];
    return S;

    );

conormal=method(TypicalValue=>Ideal);
conormal (Ideal):=(I)->(
    return conormal(I,conormalRing(I));
    );
conormal (Ideal,Ring):=(I,S)->(
    gbI:= groebnerBasis(I,Strategy=>"MGB");
    c:=codim ideal leadTerm gbI;
    J1:=jacobian(I);
    J:=minors(c, J1);
    IS:=sub(I,S);
    vs:=select(gens(S),i->degree(i)=={0,1});
    K:=minors(c+1,matrix{vs}||sub((transpose J1),S));
    N:=saturate(ideal mingens(IS+K),sub(ideal mingens(J),S));
    return N;
    );
whitneyStratify=method(TypicalValue=>MutableHashTable,Options=>{Projective=>false,AssocPrimes=>true});
whitneyStratify (Ideal):=opts->(I)->(
    c:=codim(I);
    R:=ring I;
    n:=numgens(R);
    if opts.Projective then n=n-1;
    return whitneyStratify(I,n-c,c, Projective=>opts.Projective, AssocPrimes=>opts.AssocPrimes);
    );
whitneyStratify (Ideal,ZZ):=opts->(I,level)->(
    c:=codim(I);
    return whitneyStratify(I,level,c, Projective=>opts.Projective, AssocPrimes=>opts.AssocPrimes);
    );
whitneyStratify (Ideal,ZZ,ZZ):=opts->(I,level,Icodim)->(
    c:=Icodim;
    R:=ring I;
    n:=numgens(R);
    S:=conormalRing(I);
    if opts.Projective then n=n-1;
    V:=new MutableHashTable;
    dimI:=n-codim(I);
    for i from 0 to dimI-1 do V#i=new MutableList;
    V#dimI=decompose I;
    Wdim:=0;
    W:={};
    Kdim:=0;
    if level==0 then return V;
    J:=ideal mingens(I+minors(codim I,jacobian I));
    if dimI<2 then(
	for K in decompose(J) do(
	    for i from 0 to dimI-1 do(
		if i==(n-codim(K)) and (any(toList(V#i),jj->K==jj)==false) then (V#i)#(#(V#i))=K;
		);
	    );
	for a from 1 to level do V#(dimI-a)=unique toList(V#(dimI-a)); 
	use R;
	return V;
	);
    --print J;
    if codim(J)==n then V#0= decompose J;
    if codim(J)>=n then return V;
    for a from 0 to level-1 do(
	--add singularities arising from intersections of current dimensional pieces 
        intse:=unique flatten for s in subsets(toList(V#(dimI-a)),2) list decompose ideal mingens(sum(s));
        for th in intse do(
            Ti:=n-codim(th);
	    (V#Ti)#(#(V#Ti))=th;
	    );
	--now find singularities and Whitney points of current dim pieces 
	for Ia in V#(dimI-a) do(
	    dimIa:=dimI-a;
	    J=ideal mingens(Ia+minors(codim Ia,jacobian Ia));
	    mu:=n-codim(J);
	    ell:=0;
	    --add all singularities of Ia
	    for K in decompose(J) do(
		for i from 0 to dimIa-1 do(
		    if i==(n-codim(K)) and (any(toList(V#i),jj->K==jj)==false) then (V#i)#(#(V#i))=K;
		    );
		);
	    if mu>dimI-level then (
		--compute the conormal to current ideal
		CIa:=conormal(ideal mingens Ia,S);
		Cpull:=0;
		for j from 0 to mu do(
		    	ell=mu-j;
			for Z in V#ell do(
			    --if the variety of Z is contained in the variety of Ia check 
			    --if there are any Whitney points for Ia in Z.
			    if isSubset(Ia, Z) then(
				-- No primary decomp way
				if opts.AssocPrimes==false then (
				    W=decompose fiberContAssocPrimes(Z, CIa);
				    for K in W do(
					Kdim=n-codim(K);
				    	if Kdim>=0 then(
					    if (any(toList(V#Kdim),jj->K==jj)==false) then (V#Kdim)#(#(V#Kdim))=K;
					    );
					);
				    )--end if
				----- associated primes way
			    	else (Cpull=ideal mingens (CIa+sub(Z,S));
			    	    W=for q in associatedPrimes Cpull list sub(eliminate((gens S)_{(numgens(R))..numgens(S)-1},q),R);
			    	    for K in W do(
				    	for i from 0 to ell-1 do(
				    	    Ztemp:=ideal mingens(K+Z);
				    	    if i==(n-codim(K)) and (any(toList(V#i),jj->Ztemp==jj)==false) then (V#i)#(#(V#i))=Ztemp;
				    	    );
				    	);
				    );
			    	-----
				
				);
			    );
			);
		    );
		);
	);
	for a from 1 to level do V#(dimI-a)=unique toList(V#(dimI-a)); 
	use R;
    return V;
    );

mapStratify=method(TypicalValue=>MutableHashTable,Options=>{isProper=>true,StratsToFind=>"all",Print=>false,AssocPrimes=>true});
mapStratify (List,Ideal,Ideal):=opts->(F,X,Y)->(
    R:=ring X;
    S:=ring Y;
    kk:=coefficientRing R;
    if opts.Print then <<"R= "<<R<<"S= "<<S<<endl;
    conRingX:=conormalRing(X);
    conRingY:=conormalRing(Y);
    if opts.Print then<<"conRingX= "<<gens(conRingX)<<endl;
    n:=numgens(R);
    m:=numgens(S);
    if opts.Print then<<"isProper= "<<opts.isProper<<endl;
    V:=new MutableHashTable;
    VcheckedBeforeInd:=new MutableHashTable;
    W:=new MutableHashTable;
    WcheckedBeforeInd:=new MutableHashTable;
    fIsCoordProjection:=false; 
    if opts.Print then<<"F= "<<F<<", gens S= "<<gens S<<endl;
    graphR:=0;
    elimIm:=0;
    elimInv:=0;
    Gam:=0;
    y:=0;
    if all(F, i->any(gens R,j->i===j)) then(
	if opts.Print then <<"the map is a coordinate projection"<<endl;
	fIsCoordProjection=true;
	graphR=R;
	elimIm=toList(set(gens(R))-set(F));
	elimInv={};
	Gam=ideal(0_graphR); 
	y=F;
	)
    else (
	graphR=kk[gens R, gens S];
	--forward map
	elimIm=(gens graphR)_{0..numgens(R)-1};
    	--inverse map
    	elimInv=(gens graphR)_{numgens(R)..numgens(graphR)-1};
    	y=(gens graphR)_{numgens(R)..numgens(graphR)-1};
    	Gam=ideal for i from 0 to numgens(S)-1 list y_i-sub(F_i,graphR);
	);
    if opts.Print then <<"graph R= "<<gens(graphR)<<endl;
    if opts.Print then <<"elimIm= "<<elimIm<<endl;
    if opts.Print then <<"elimInv= "<<elimInv<<endl;
    dimX:=n-codim(X);
    tempSubsetsV:=0;
    tempSubsetsW:=0; 
    dimY:=m-codim(Y);
    fX:=ideal mingens(sub(eliminate(elimIm,Gam+sub(X,graphR)),S)+Y);
    dimfX:=m-codim(fX);
    for i from 0 to dimX-1 do V#i=new MutableList;
    for i from 0 to dimfX-1 do W#i=new MutableList;
    
    for i from 0 to dimX do VcheckedBeforeInd#i=0;
    for i from 0 to dimfX do WcheckedBeforeInd#i=0;
    if opts.Print then print "decompose X";
    V#dimX=decompose X;  
    if opts.Print then print "decompose Y";
    W#(dimY)=decompose Y;
    if fX!=Y then(
	Y=fX;
	dimY=dimfX;
	W#(dimY)=decompose Y;
	);
    if opts.isProper==false then(
	if opts.Print then<<"Computing Jolenck flag"<<endl;
	JelY:=new MutableList from {};
	JelX:=new MutableList from {};
	JelY#0=Y;
	JelX#0=X;
	for i from 0 to dimY-1 do(
	    if opts.Print then<<"Jel#"<<i<<" ="<<JelY#i<<endl;
	    JelY#(i+1)=nonProperSet(F,JelX#i,Y);
	    JelX#(i+1)=ideal mingens(sub(eliminate(elimInv,Gam+sub(JelY#(i+1),graphR)),R)+X);
	    );
	for jel in unique toList(JelY) do(
	    for comp in decompose (jel+Y) do(
		dimjel:=m-codim(comp);
		if (any(toList(W#dimjel),jj->comp==jj)==false) then (W#dimjel)#(#(W#dimjel))=comp;	   
		);
	    );
	for jel in unique toList(JelX) do(
	    for comp in decompose (jel+X) do(
		dimjel:=n-codim(comp);
		if (any(toList(V#dimjel),jj->comp==jj)==false) then (V#dimjel)#(#(V#dimjel))=comp;	   
		);
	    );
	);
    if opts.Print then print "past Jel check/computation";	
    if opts.Print then print "V";
    if opts.Print then for ke in keys V do print toList(V#ke);
    if opts.Print then print "W";
    if opts.Print then for ke in keys W do print toList(W#ke);
    aX:=0;
    aY:=0;
    JY:=0;dimYa:=0;muY:=0;ellY:=0;dimKinv:=0;Kdim:=0;Kinv:=0;ell:=0;Ti:=0;intse:=0;CYa:=0;CpullYa:=0;
    JX:=0;muX:=0;ellX:=0;Kpf:=0;AX:=0;WhitXa:=0;rXa:=0;IiXf:=0;Cpull:=0;CXa:=0;KDim:=0;WhitYa:=0;
    Ya:=0;
    Xa:=0;
    tYind:=0;
    tXind:=0;
    YaNew:=0;YaOld:=0;
    XaNew:=0;XaOld:=0;
    while (aX<dimX) or (aY<dimY) do(
	if opts.Print then <<"{aY, aX}= "<<{aY,aX}<<endl;
	-- Y at codim>a 
	if (dimY-aY)>0 then(
	    if opts.Print then print "In Y";
	    	--add subset intersections of Y at level aY
		tYind=WcheckedBeforeInd#(dimY-aY);
		YaNew=(toList(W#(dimY-aY)))_(toList(tYind..(#(W#(dimY-aY))-1)));
		YaOld=(toList(W#(dimY-aY)))_(toList(0..tYind-1));
		tempSubsetsW=join(flatten for a1 in YaOld list(for b1 in YaNew list {a1,b1}),subsets(YaNew,2));
		--TODO need to add intersections of the form 1 from before tYind and 1 from after tYind
		if opts.Print then<<"starting subset sing at "<<dimY-aY<<", num subsets W= "<<#tempSubsetsW<<endl;
		intse=unique flatten for s in tempSubsetsW list decompose ideal  mingens(sum(s));
		for th in intse do(
		    Ti=m-codim(th);
		    if opts.Print then <<"Y Whit= "<<th<<endl;
		    if (any(toList(W#Ti),jj->th==jj)==false) then (
			(W#Ti)#(#(W#Ti))=th;
			if opts.Print then<<"added to Y strat= "<<th<<endl;
			);
		    --Pullback to X, add to X strat, V
		    Kinv=ideal mingens(sub(eliminate(elimInv,Gam+sub(th,graphR)),R)+X);
		    for comp in decompose Kinv do(
			dimKinv=n-codim(comp);
			if opts.Print then <<"Y Whit in X= "<<comp<<endl;
			if (any(toList(V#dimKinv),jj->comp==jj)==false) then(
			    (V#dimKinv)#(#(V#dimKinv))=comp;
			    					--have unprocessed components above current dim, go back up
					if dimKinv>(dimX-aX) then aX=dimX-dimKinv;
			    );
			);
		    );
		<<"for j2...."<<endl;
	    for j2 from WcheckedBeforeInd#(dimY-aY) to #(W#(dimY-aY))-1 do(
		Ya=(W#(dimY-aY))#j2;
		WcheckedBeforeInd#(dimY-aY)=WcheckedBeforeInd#(dimY-aY)+1;
	    	dimYa=dimY-aY;
	    	JY=ideal mingens(Ya+minors(codim Ya,jacobian Ya));
	    	muY=m-codim(JY);
	    	ellY=0;
	    	WhitYa=0;
		<<"loop over components of JY="<<JY<<endl;
	    	for K in decompose(JY) do(
		    --Add to Y strat, W
		    <<"at K= "<<K<<endl;
		    KDim=m-codim(K);
		    if (any(toList(W#KDim),jj->(K)==jj)==false) then (W#KDim)#(#(W#KDim))=ideal mingens(K);
		    --Pullback to X, add to X strat, V
		    Kinv=ideal mingens(sub(eliminate(elimInv,Gam+sub(K,graphR)),R)+X);
		    for comp in decompose Kinv do(
			dimKinv=n-codim(comp);
			if (any(toList(V#dimKinv),jj->comp==jj)==false) then(
			    (V#dimKinv)#(#(V#dimKinv))=comp;	   	
			    --have unprocessed components above current dim, go back up
			    if dimKinv>(dimX-aX) then aX=dimX-dimKinv;
			    );
			);
		    );
		if muY>0 and (opts.StratsToFind!="singularOnly") then (
		    CYa=conormal(ideal mingens Ya,conRingY);
		    CpullYa=0;
		    for j from 0 to muY do(
		    	ell=muY-j;
		    	for Z in W#ell do(
			    CpullYa=ideal mingens (CYa+sub(Z,conRingY));
			    if opts.AssocPrimes==false then(
				WhitYa=decompose fiberContAssocPrimes(Z, CYa);
				)
			    else(
				if (opts.StratsToFind=="all") then(
				    if opts.Print then <<"doing Y assoc. primes with "<<CpullYa<<endl;
				    WhitYa=for q in associatedPrimes CpullYa list sub(eliminate((gens conRingY)_{(numgens(S))..numgens(conRingY)-1},q),S);
				    )
			    	else(
			            if opts.Print then <<"doing Y min. primes with "<<CpullYa<<endl;
				    WhitYa=for q in decompose CpullYa list sub(eliminate((gens conRingY)_{(numgens(S))..numgens(conRingY)-1},q),S);
				    );
			    	);
			    if opts.Print then <<"done assoc primes "<<endl;
			    for K in WhitYa do(
			    	KDim=m-codim(K);
				if (any(toList(W#KDim),jj->(K+Z)==jj)==false) then (W#KDim)#(#(W#KDim))=ideal mingens(K+Z);
				--Pullback to X, add to X strat, V
				Kinv=ideal mingens(sub(eliminate(elimIm,Gam+sub(K,graphR)),R)+X);
				for comp in decompose Kinv do(
				    dimKinv=n-codim(comp);
				    if (any(toList(V#dimKinv),jj->comp==jj)==false) then(
					(V#dimKinv)#(#(V#dimKinv))=comp;
					--have unprocessed components above current dim, go back up
					if dimKinv>(dimX-aX) then(
					    aX=dimX-dimKinv;
					    );
					);	   
				    );
			    	);
			    );
		    	);
		    );
	    	);
	    --now go down a level in Y, if there are levels left
		aY=aY+1;
	 --end Y block   
	 );
	--X at codim> a
	--TODO, change X block to look like Y block above with new index check etc. 
	if (dimX-aX)>0 then(
	    if opts.Print then print "In X";
	    	--subset intersections at level aX of X
		tXind=VcheckedBeforeInd#(dimX-aX);
		XaNew=(toList(V#(dimX-aX)))_(toList(tXind..(#(V#(dimX-aX))-1)));
		XaOld=(toList(V#(dimX-aX)))_(toList(0..tXind-1));
		tempSubsetsV=join(flatten for a1 in XaOld list(for b1 in XaNew list {a1,b1}),subsets(XaNew,2));
	    	--Add subset intersections of Xa, push forward
		--tempSubsetsV=subsets(toList(V#(dimX-a)),2);
		if opts.Print then<<"starting subset sing at "<<dimX-aX<<", num subsets V= "<<#tempSubsetsV<<endl;
		    intse=unique flatten for s in tempSubsetsV list decompose ideal  mingens(sum(s));
		    for th in intse do(
		    	Ti=n-codim(th);
		    	if opts.Print then<<"X whit subsets= "<<th<<endl;
		    	if (any(toList(V#Ti),jj->th==jj)==false) then (V#Ti)#(#(V#Ti))=th;
		    	--Pushforward to Y, add to Y strat, W
		    	Kinv=ideal mingens(sub(eliminate(elimIm,Gam+sub(th,graphR)),S)+Y);
		    	for comp in decompose Kinv do(
			    dimKinv=m-codim(comp);
			    if opts.Print then<<"X whit in Y "<<comp<<endl;
			    if (any(toList(W#dimKinv),jj->comp==jj)==false) then (
				(W#dimKinv)#(#(W#dimKinv))=comp;
				--have unprocessed components above current dim, go back up
				if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
				if opts.Print then<<"Added to Y Sing = "<<comp<<endl;
				if opts.Print then<<"Dim th= "<<dim(th)<<", From X part= "<<th<<endl;
				if opts.Print then<<", intse="<<intse<<endl;
				);
			    );
		    	);
	    for j2 from VcheckedBeforeInd#(dimX-aX) to #(V#(dimX-aX))-1 do(
		Xa=(V#(dimX-aX))#j2;
		VcheckedBeforeInd#(dimX-aX)=VcheckedBeforeInd#(dimX-aX)+1;
	    	--dima=dimY-aY;	    
	    --for Xa in V#(dimX-a) do(
		if opts.Print then<<"Xa= "<<Xa<<endl;
	    	dimXa:=dimX-aX;
	    	JX=ideal mingens(Xa+minors(codim Xa,jacobian Xa));
		if opts.Print then<<"JX= "<<JX<<endl;
	    	muX=n-codim(JX);
	    	ellX=0;
	    	WhitXa=0;
	    	for K in decompose(JX) do(
		     Ti=n-codim(K);
		     if opts.Print then<<"X Whit= "<<K<<endl;
		     if (any(toList(V#Ti),jj->K==jj)==false) then (V#Ti)#(#(V#Ti))=ideal mingens(K);
		    );
		AX=jacobian(Xa+ideal(F));
		if opts.Print then<<"AX= "<<AX<<endl;
		--todo, use the tighter bound here
		rXa=min(numrows(AX), numcols(AX));
		for i from 0 to rXa do(
		    IiXf=ideal mingens(Xa+minors(rXa-i,AX));
		    if opts.Print then<<"IiXf= "<<IiXf<<endl;
		    if IiXf==ideal(1_R) then break; 
		    for K in decompose(IiXf) do(
			--add to X strat, V
			Ti=n-codim(K);
			if opts.Print then<<"X Tb 1= "<<K<<endl;
			if opts.Print then<<"V#"<<Ti<<" ="<<toList(V#Ti)<<", ring= "<<(for v in toList(V#Ti) list gens(ring(v)))<<endl;
			if (any(toList(V#Ti),jj->K==jj)==false) then (
			    (V#Ti)#(#(V#Ti))=K;
			    if opts.Print then<<"Added to X TB = "<<K<<endl;
			    );
			--add image to Y strat, W
			Kpf=ideal mingens(sub(eliminate(elimIm,Gam+sub(K,graphR)),S)+Y);
		       	for comp in decompose Kpf do(
		            dimKinv:=m-codim(comp);
			    if opts.Print then<<"Y TB= "<<comp<<endl;
			    if (any(toList(W#dimKinv),jj->comp==jj)==false) then(
				(W#dimKinv)#(#(W#dimKinv))=comp;
				--Unprocessed components detected above, go back up
				if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
				if opts.Print then<<"Added to Y TB = "<<comp<<endl;
				);
			    );
		    	);
		    );
		if opts.Print then<<"done TB"<<", muX="<<muX<<endl;
		if muX>0 and (opts.StratsToFind!="singularOnly") then (
		    CXa=conormal(ideal mingens Xa,conRingX);
		    Cpull=0;
		    for j from 0 to muX do(
		    	ell=muX-j;
		    	for Z in V#ell do(
	     	    	    if opts.AssocPrimes==false then(
				WhitXa=decompose fiberContAssocPrimes(Z, CXa);
				)
			    else(
				Cpull=ideal mingens (CXa+sub(Z,conRingX));
			    	if (opts.StratsToFind=="all") then(
				    if opts.Print then<<"doing X assoc. primes with "<<Cpull<<endl;
				    WhitXa=for q in associatedPrimes(Cpull) list sub(eliminate((gens conRingX)_{(numgens(R))..numgens(conRingX)-1},q),R);
				    )
			    	else(
				    if opts.Print then<<"doing X min. primes with "<<Cpull<<endl;
				    pList:= decompose Cpull; 
				    WhitXa= for q in pList list sub(eliminate((gens conRingX)_{(numgens(R))..numgens(conRingX)-1},q),R);
				    );
			    	);
			    if opts.Print then<<"done associated/min primes X"<<endl;
			    for K in WhitXa do(
			    	--only add elements that are new
			    	Ti=n-codim(K+Z);
				if (any(toList(V#Ti),jj->K==jj)==false) then(
				    (V#Ti)#(#(V#Ti))=ideal mingens(K+Z);
			    	    if opts.Print then<<"X whit= "<<K<<endl;
		       	    	    Kpf=ideal mingens(sub(eliminate(elimIm,Gam+sub(K+Z,graphR)),S)+Y);
		       	    	    for comp in decompose Kpf do(
		       	    	    	dimKinv=m-codim(comp);
				    	if opts.Print then<<"X whit in Y "<<comp<<endl;
	       		 	    	if (any(toList(W#dimKinv),jj->comp==jj)==false) then(
					    (W#dimKinv)#(#(W#dimKinv))=comp;
					    if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
					    );
     				    	);
			    	    );
			    	);
			    );
		    	);
		    );
		);
	    aX=aX+1;
		    if opts.Print then<<"|W|= "<<toString(sum for k in keys W list #(W)#k)<<endl;
		    if opts.Print then<<"|V|= "<<toString(sum for k in keys V list #(V)#k)<<endl;
	    	);
    	);
    for a in keys(V) do V#a=unique toList(V#a); 
    for a in keys(W) do W#a=unique toList(W#a);
    use R;  
    return {V,W};
    );
fiberContAssocPrimes=(IY,ConX)->(
    R:=ring IY;
    S:=ring ConX;
    kk:=coefficientRing R;
    -- if we want to use Msolve here 
    --can replace I with monomialIdeal leadTerm(I), and use Msolve for leadTerm
    mis:= independentSets(IY);
    u:=support(last random mis);
    xMinusU:=rsort toList(set(gens R) - set(u));
    els:=(#xMinusU)+numgens(R);
    S1:=kk[xMinusU, (gens S)_(toList(numgens(R)..numgens(S)-1)),u,MonomialOrder => {els,numgens(S)-els}];
    gBCXY:=ideal groebnerBasis(sub(IY,S1)+sub(ConX,S1));
    S2:=(kk[u])[xMinusU, (gens S)_(toList(numgens(R)..numgens(S)-1))];
    G:=flatten entries gens sub(gBCXY,S2);
    h:=sub(lcm for g in G list leadCoefficient g,S);
    hProj:=sub(eliminate(ideal mingens (ConX+sub(IY,S)+h),(gens S)_(toList(numgens(R)..numgens(S)-1))),R);
    return hProj;
    );

beginDocumentation()
multidoc ///

Node 
     Key
     	  WhitneyStratifications
     Headline
     	  Computes Whitney Statifications of real and complex varieties and of algebraic maps between them.
     Description
     	  Text
	      This package computes Whitney stratifications of real and complex algebraic varieties using the algorithms described in [1, 2, 4]. For varieties considered over the complex numbers the output is indexed by the strata dimension. When wishing to treat the variety over the reals, the same output may be used, but the dimensions of the strata may differ (and some strata may be empty), see [2] for more details. This post processing in the real case is currently left to the user.    
	      
	      A method is also provided to stratify polynomial maps $f:X\to Y$ between algebraic varieties, the output is a Whitney stratification of both $X$ and $Y$, such that for each (open, connected) strata $M$ of $X$ there is an (open, connected) strata $N$ of $Y$ such that $f(M) \subset N$ and such that the restriction of $f$ to $M$ is a submersion. This in particular is sufficient to guarantee that Thom's (first) isotopy lemma holds; namely that the stratified homeomorpism type of $f^{-1}(q)$ is fixed for all $q$ in a given strata of the codomain.   
	      
	      Using the methods of [1] or [4] computing the Conormal variety of a variety is an important step in these algorithms, so a method for this is also provided. An alternative method based on [2] is also provided, this requires the computation of polar varieties, hence a method for this is provided as well.

	      Finally, based on work in [3], we use Whitney stratification to compute the characteristic cycle of a constructable function and of the associated annhilator ideal of a D-module.  
	      
	      References:
	      
	      [1] Martin Helmer and Vidit Nanda. "Conormal Spaces and Whitney Stratifications", Foundations of Computational Mathematics, DOI: 10.1007/s10208-022-09574-8.
	      
	      [2] Martin Helmer, Anton Leykin, and Vidit Nanda. "Effective Whitney Stratification of Real Algebraic Varieties". Arxiv: 2307.05427.

	      [3] Martin Helmer, Felix Tellander. "Spectral Decomposition of Euler-Mellin Integrals". Arxiv: 2505.12458.

	      [4] Martin Helmer and Rafael Mohr. A New Algorithm for Whitney Stratification of Varieties . Arxiv: 2406.1712.

	      
	      
	      
Node 
    Key
    	whitneyStratify
	(whitneyStratify, Ideal)
    Headline
    	Computes a Whitney stratification of the real and complex varieties using conormal methods.
    Usage
    	whitneyStratify(I)
    Inputs
    	I:Ideal
	    an ideal defining a closed subvariety of affine or projective space over a field.
    Outputs
        WS:MutableHashTable
	    a hash table indexed by (complex) dimension, with the entry of (complex) dimension $i$ consisting of a list of prime ideals. The strata of $V(I)$ are the connected components of the difference between the variety defined by WS#i and that defined by WS#(i-1).    
    Description 
    	Text
	    For a variety $X$ this command computes a Whitney stratification WS where WS#i is a list of strata closures in (complex) dimension $i$; for a prime ideal $P$ in WS#i the associated open (connected) strata is given by the connected components of $V(P)-Z$ where $Z$ is the union of the varieties defined by the entries of WS#(i-1). We demonstrate the method for the Whitney umbrella below.
    	Example
	    R=QQ[x..z]
	    I=ideal(y^2*z-x^2)
	    WS=whitneyStratify I
	    peek WS
	Text
	    Now the projective version; using the option Projective=>true will treat the input as a projective variety. If this option is omitted the input is treated as an affine cone over the projective variety.
	Example 
	    S=QQ[x..z,w]
	    I=ideal(y^2*z-x^2*w)
	    WS=whitneyStratify(I,Projective=>true)
	    peek WS
	Text 
	    Another projective example.
	Example
	    R=QQ[a_0..a_4]
	    I=ideal(a_0^2*a_4-a_1*a_2^2+a_3^3-a_3*a_0^2-a_4^2*a_3)
	    V=whitneyStratify(I,Projective=>true)
	    peek V
	Text 
	    Note that as with the Whitney umbrella simply taking successive singular loci will not yield the correct stratification, in particular one would miss the two points defined by the second entry of V#0. 
	Example 
	    J=radical (I+minors(codim I, jacobian I))
	    J2=radical (J+minors(codim J, jacobian J))
	Text
	    By default whitneyStratify will use the algorithm of [1] (Martin Helmer and Vidit Nanda. "Conormal Spaces and Whitney Stratifications", Foundations of Computational Mathematics, DOI: 10.1007/s10208-022-09574-8), which as a step requires the computation of the associated primes of a certain ideal arising from a conormal variety. This can in practice be very costly. The algorithm of [4] (Martin Helmer and Rafael Mohr. A New Algorithm for Whitney Stratification of Varieties . Arxiv: 2406.1712) also uses conormal varieties but avoids the need to compute associated primes. This algorithm is often much faster (though it is slightly more likely to produce non-minimal Whitney stratifications, it is accessed with the option AssocPrimes=>false. Both algorithms are deterministic and are guaranteed to produce a correct Whitney stratification (though not necessarily the unique minimal one). 

	Example
	    R=QQ[x..z]
	    I=ideal(y^2+x^3-x^2*z^2)
	    WS=whitneyStratify(I,AssocPrimes=>false)
	    peek WS
Node 
    Key
    	whitneyStratifyPol
	(whitneyStratifyPol, Ideal)
    Headline
    	Computes a Whitney stratification of the real and complex varieties.
    Usage
    	whitneyStratifyPol(I)
    Inputs
    	I:Ideal
	    an ideal defining a closed subvariety of affine or projective space over a field.
    Outputs
        WS:MutableHashTable
	    a hash table indexed by (complex) dimension, with the entry of (complex) dimension $i$ consisting of a list of prime ideals. The strata of $V(I)$ are the connected components of the difference between the variety defined by WS#i and that defined by WS#(i-1).    
    Description 
    	Text
	    For a variety $X$ this command computes a Whitney stratification WS where WS#i is a list of strata closures in (complex) dimension $i$; for a prime ideal $P$ in WS#i the associated open (connected) strata is given by the connected components of $V(P)-Z$ where $Z$ is the union of the varieties defined by the entries of WS#(i-1).

	    This function uses an algorithm based on the computation of polar varieties and is probabilistic (unlike the conormal based whitneyStratify which is deterministic).

	    There are several options which can be used to change how the polar varieties are computed and which may be faster on some examples. 

	    We demonstrate the method for the Whitney umbrella below. Note that the polar variety method always generates unnecessary strata (which are random) and need not be present in a minimal Whitney stratification. 
    	Example
	    R=QQ[x..z]
	    I=ideal(y^2+x^3-x^2*z^2)
	    WS1=whitneyStratifyPol I
	    peek WS1
	Text 
	    Note that since the "extra" strata are random we can generate the minimal stratification by taking the common strata closures which appear in two different calculations (with high probability). We do this as follows:
	Example 
	    WS2=whitneyStratifyPol I
	    peek WS2
	    WS=new MutableHashTable from for k in keys WS1 list k=>toList((set(WS1)#k)*(set(WS2)#k))
	    peek WS
	Text 
	    There are also several different options to perform the underlying polar variety calculations. The default algorithm uses the M2 saturate command to compute the polar variteies, this option is Algorithm=>. The other options are: Algorithm=>"msolve" and  Algorithm=>"M2F4". The Algorithm=>"msolve" only works in versions 1.25.06 and above of Macaulay2. The  Algorithm=>"M2F4" is mostly beneficial when working over a finite field. Note that over a finite field we can still sometimes obtain useful information about the stratification, but the coefficients appearing in the resulting polynomials may (or likely will) be incorrect. 
	Example 
	    WS3=whitneyStratifyPol(I,Algorithm=>"msolve")
	    peek WS3
	    R=ZZ/32749[x..z]
	    I=ideal(y^2+x^3-x^2*z^2)
	    WS3=whitneyStratifyPol(I,Algorithm=>"M2F4")
	Text 
	    On harder examples the whitneyStratifyPol can be much faster than other methods and often the option Algorithm=>"msolve" will be the fastest (the msolve option is not currently set as default since the msolve package is a new addition to M2). 
	    
	    
Node 
    Key
    	mapStratify
	(mapStratify, List, Ideal, Ideal)
    Headline
    	Computes a Whitney stratification of a polynomial map $f:X\to Y$ between real or complex varieties.
    Usage
    	mapStratify(f,X,Y)
    Inputs
        f:List
	    a list of polynomials in the same ring as the ideal X
    	X:Ideal
	    defining an affine or projective variety. 
	Y:Ideal
	    defining an affine or projective variety.     
    Outputs
        MS:List
	    a List containing two elements: a Whitney stratification of $X$ and of $Y$. Each stratification is stored in a hash table indexed by (complex) dimension, with the entry of (complex) dimension $i$ consisting of a list of prime ideals. The strata of $V(I)$ are the connected components of the successive difference between the varieties in the hash tables.    
    Description 
    	Text
	    For a polynomial map $f:X\to Y$ this command computes a Whitney stratification of $f$, that is a Whitney stratification of $X$ and of $Y$ (in the same output format as the whitneyStatify command), such that for each (open, connected) strata $M$ of $X$ there is an (open, connected) strata $N$ of $Y$ such that $f(M) \subset N$ and such that the restriction of $f$ to $M$ is a submersion. When $f$ is a proper map this stratification turns $f$ into a locally trivial fiber bundle by Thom's isotopy lemma. More generally when $f$ is a dominant map between varieties of the same dimension a stratification which further stratifies by non-properness locus is also implemented (as described in Section 3.3 of [2], M. Helmer, V. Nanda, Effective Whitney Stratification of Real Algebraic Varieties . Arxiv: 2307.05427). This can be used to, for example, solve the real root classification problem (see Section 5 of [2]), that is to stratify the parameter space of a parametric system of polynomials with generically finitely many complex solutions into regions where the number of real solutions is fixed. 
	    
	    We illustrate this on the quadratic equation $ax^2+bx+c$ by stratifying the parameter space, $(a,b,c)\in \RR^3$, into regions where the number of real roots is constant.  
	    
	    Here we stratify the projection map $X\to \RR^3$ where $X=V(ax^2+bx+c)$.
        Example 	    
	    R=QQ[a,b,c,x]
	    I=ideal(a*x^2+b*x+c)
	    S=QQ[(gens(R))_(toList(0..2))]
	    gens S
	    F={R_0,R_1,R_2}
	    ms=mapStratify(F,I,ideal(0_S),isProper=>false)
	    peek last ms
	Text
	    For this and other root classification examples the option isProper=>false should be used, as the corresponding projection maps are in general not proper. Note that the top dimensional strata in this case are the four connected components of $\RR^3-V(a(b^2-4ac))$, and so on for the lower dimensional strata. 
	
	    Another root classification example, this time we find the regions of the parameter space $\{(a,b)\in \RR^2\}$ where the number of real roots to the system $x^2-y^2+b = -ax+x^2+by=0$ is constant; this is Example 1.2 in Reference [2]. 
	Example
	    R=QQ[a,b,x,y];
	    I=ideal(x^2-y^2+b, -a*x+x^2+b*y);
	    S=QQ[(gens(R))_(toList(0..1))];
	    F={R_0,R_1};
	    ms=mapStratify(F,I,ideal(0_S),isProper=>false)
	    peek last ms
        Text
	    Another broad class of interesting examples is parameterized projective varieties, as the projection map to the parameter space is induced by the projection $\PP^n\times \CC^m\to \CC^m$ and is hence always proper. Again we seek to understand how the topology of the variety changes with parameters. The following example is a special case of an example from the study of Feynman integrals in mathematical physics (in particular is arises from the "one loop bubble" for the case of equal masses). Note that for any fixed value of parameters M1, P, the resulting polynomial Gh is homogeneous.  
	Example
	    R=QQ[M1,P,X_0..X_2];
	    U = (X_1+X_2);
	    F =U*(M1*X_1+M1*X_2)-P*X_1*X_2;
	    Gh = U*X_0+F;
	    Xh=ideal (X_0*X_1*X_2*Gh);
	    F={M1,P};
	    S=QQ[(gens(R))_(toList(0..1))];
	    ms=mapStratify(F,Xh,ideal(0_S));
	    peek last ms 
        Text
	    Finally we remark that the option: StratsToFind, may be used with this function, but should only be used with care. The default setting is StratsToFind=>"all", and this is the only value of the option which is guaranteed to compute the complete stratification, the other options may fail to find all strata but are provided to allow the user to obtain partial information on larger examples which may take too long to run on the default "all" setting. The other possible values are StratsToFind=>"singularOnly", and StratsToFind=>"most". The option  StratsToFind=>"singularOnly" is the fastest, but also the most likely to return incomplete answers, and hence the output of this command should be treated as a partial answer only. The option StratsToFind=>"most" will most often get the full answer, but can miss strata, so again the output should be treated as a partial answer. In the example below all options return the complete answer, but only the output with StratsToFind=>"all" should be considered complete; StratsToFind=>"all" is run when no option is given. 	                     
    	Example
	    time ms=mapStratify(F,Xh,ideal(0_S),StratsToFind=>"singularOnly")
	    peek last ms
	    time ms=mapStratify(F,Xh,ideal(0_S),StratsToFind=>"most")
	    peek last ms
	    time ms=mapStratify(F,Xh,ideal(0_S),StratsToFind=>"all")
	    peek last ms


Node 
    Key
    	mapStratifyPol
	(mapStratifyPol, List, Ideal, Ideal)
    Headline
    	Computes a Whitney stratification of a polynomial map $f:X\to Y$ between real or complex varieties.
    Usage
    	mapStratifyPol(f,X,Y)
    Inputs
        f:List
	    a list of polynomials in the same ring as the ideal X
    	X:Ideal
	    defining an affine or projective variety. 
	Y:Ideal
	    defining an affine or projective variety.     
    Outputs
        MS:List
	    a List containing two elements: a Whitney stratification of $X$ and of $Y$. Each stratification is stored in a hash table indexed by (complex) dimension, with the entry of (complex) dimension $i$ consisting of a list of prime ideals. The strata of $V(I)$ are the connected components of the successive difference between the varieties in the hash tables.    
    Description 
    	Text
	    For a polynomial map $f:X\to Y$ this command computes a Whitney stratification of $f$, that is a Whitney stratification of $X$ and of $Y$ (in the same output format as the whitneyStatify command), such that for each (open, connected) strata $M$ of $X$ there is an (open, connected) strata $N$ of $Y$ such that $f(M) \subset N$ and such that the restriction of $f$ to $M$ is a submersion. When $f$ is a proper map this stratification turns $f$ into a locally trivial fiber bundle by Thom's isotopy lemma. More generally when $f$ is a dominant map between varieties of the same dimension a stratification which further stratifies by non-properness locus is also implemented (as described in Section 3.3 of [2], M. Helmer, V. Nanda, Effective Whitney Stratification of Real Algebraic Varieties . Arxiv: 2307.05427). This can be used to, for example, solve the real root classification problem (see Section 5 of [2]), that is to stratify the parameter space of a parametric system of polynomials with generically finitely many complex solutions into regions where the number of real solutions is fixed. 

	    This method is used in an exactly identical way to mapStratify. The main difference is that it uses the underlying Whitney stratification step based on polar variety computation, and is hence probabilistic (while mapStratify uses conormal method for the Whitney stratification step and is hence deterministic).

	    The mapStratifyPol method is often faster than mapStratify. Similar to the whitneyStratifyPol method it also produces unnecessary components (which are random). 

	    We carry out the example of the quadratic equation $ax^2+bx+c$ by stratifying the parameter space, $(a,b,c)\in \RR^3$, into regions where the number of real roots is constant. To remove unnecessary strata we do the calculation once and take the common elements as the result. 
	    
	    Here we stratify the projection map $X\to \RR^3$ where $X=V(ax^2+bx+c)$.
        Example 	    
	    R=QQ[a,b,c,x]
	    I=ideal(a*x^2+b*x+c)
	    S=QQ[(gens(R))_(toList(0..2))]
	    gens S
	    F={R_0,R_1,R_2}
	    ms1=mapStratifyPol(F,I,ideal(0_S),isProper=>false)
	    ms2=mapStratifyPol(F,I,ideal(0_S),isProper=>false)
	    ms={new MutableHashTable from for k in keys first ms1 list k=>toList((set(first ms1)#k)*(set(first ms2)#k)),new MutableHashTable from for k in keys last ms1 list k=>toList((set(last ms1)#k)*(set(last ms2)#k))}
	    peek last ms


	    
Node 
    Key
    	conormal
	(conormal, Ideal)
    Headline
    	Computes the conormal variety
    Usage
    	conormal(I)
    Inputs
    	I:Ideal
	    in a polynomial ring $k[x_1,\dots, x_n]$ for a field $k$, defining a closed variety in $k^n$ (or in $\PP^n$ if the ideal is homogeneous).
    Outputs
        C:
	    an ideal defining the conormal variety in $k^n \times \PP^{n-1}$.
    Description 
    	Text
	    For a complex projective variety $X=V(I)\subset \PP^n$ this command computes the ideal of the conormal variety $Con(X)$ in $k^n \times \PP^{n-1}$.
	Example 
	    S=QQ[x..z]
	    I=ideal(y^2*z-x^2)
	    conormal I
Node 
    Key
    	minCoarsenWS
	(minCoarsenWS, MutableHashTable)
    Headline
    	Coarsens a given Whitney stratification to the unique minimal one. 
    Usage
    	minCoarsenWS(WS)
    Inputs
    	WS:MutableHashTable
	    a MutableHashTable storing a Whitney stratification (produced by either whitneyStatify or whitneyStatifyPol).
    Outputs
        minWS:
	    a MutableHashTable storing the unique minimal Whitney stratification obtained by coarsening the given one.
    Description 
    	Text
	    For a given Whitney stratification, represented in the form output by the whitneyStratify and whitneyStratifyPol commands, this command identifies any strata which are unnecessary and removes them to compute a coarsening.

	    In the example below we use whitneyStratifyPol to compute a non-minimal Whitney stratification and then use minCoarsenWS to obtain the unique minimal Whitney stratification from this output. 
	Example 
	    R=QQ[x..z]
	    I=ideal(y^2*z-x^2)
	    WS= whitneyStratifyPol I
	    peek WS
	    mWS=minCoarsenWS WS
	    peek mWS
Node 
    Key
    	polarVars
	(polarVars, Ideal)
    Headline
    	Computes the list of all polar varieties of a variety
    Usage
    	polarVars(I)
    Inputs
    	I:Ideal
	    in a polynomial ring $k[x_1,\dots, x_n]$ for a field $k$, defining a closed variety in $k^n$ (or in $\PP^n$ if the ideal is homogeneous).
    Outputs
        pols:
	    a list of ideals defining polar varieties to $X=V(I)$ relative to a random flag of linear subspaces. The list starts with a dimension 0 polar variety and ends with the $dim(X)$ polar variety, which is $X$, hence the last ideal in the list is I.
    Description 
    	Text
	    For a complex variety $X=V(I)$ this command computes a list of ideals defining polar varieties to $X=V(I)$ relative to a random flag of linear subspaces. For the relevant definitions see, e.g., [2]: Martin Helmer, Anton Leykin, and Vidit Nanda. "Effective Whitney Stratification of Real Algebraic Varieties". Arxiv: 2307.05427. 
	Example 
	    S=QQ[x..z]
	    I=ideal(y^2*z-x^2)
	    polarVars I
	Text 
	    There are also several different options to perform the polar variety calculations. The default algorithm uses the M2 saturate command to compute the polar varieties, this option is Algorithm=>. The other options are: Algorithm=>"msolve" and  Algorithm=>"M2F4". The Algorithm=>"msolve" only works in versions 1.25.06 and above of Macaulay2. The  Algorithm=>"M2F4" is mostly beneficial when working over a finite field. Note that over a finite field we can still sometimes obtain useful information, but the coefficients appearing in the resulting polynomials may (or likely will) be incorrect. 
Node 
    Key
    	polarSequence
	(polarSequence, Ideal, Ideal)
	(polarSequence, Ideal, Ideal,List)
    Headline
    	Computes the list of multiplcities of a subvareity in the polar varieties of another vareity. 
    Usage
    	polarSequence(I)
    Inputs
    	IX:Ideal
	    in a polynomial ring $k[x_1,\dots, x_n]$ for a field $k$.
	IZ:Ideal
	    in a polynomial ring $k[x_1,\dots, x_n]$ for a field $k$ such that $V(IX)\subset V(IZ)$.
	polsZ:List
	    a list of ideals defining the polar varieties of $Z=V(IZ)$; this is an optional argument to avoid recomputing polar varieties for multiple polar sequence calculations. This is meant to be the output of polarVars(IZ).    
    Outputs
        polarMultiplcities:
	    a list of integers giving the (Hilbert-Samuel) multiplicity sequence of $X=V(IX)$ in each of the polar varieties of  $Z=V(IZ)$ which contain it. 
    Description 
    	Text
	    Below we compute the polar multiplicity sequence for the origin and the z-axis in the Whitney umbrella. 
	Example 
	    R=QQ[x..z]
	    IZ=ideal(y^2*z-x^2)
	    IX=ideal(x,y,z)
	    polarSequence(IX, IZ)
	    polarSequence(ideal(x,y), IZ)
	Text 
	    Note that for any polar varieties of $Z$ which do not contain $X$ we obtain multiplicity 0.  
	    
Node 
    Key
    	eulerObsMatrix
	(eulerObsMatrix, MutableHashTable)
    Headline
    	Computes a matrix containing the strata-wise values of the local Euler obstruction function.
    Usage
    	eulerObsMatrix(V)
    Inputs
    	V:MutableHashTable
	    in the format output by whitneyStratify and whitneyStratifyPol: the keys are the dimensions of strata, each gives a list of ideals defining all (closures of) strata of the corresponding dimension. Note that the input needs to represent a Whitney stratification to ensure correct results (though it need not be minimal). 
    Outputs
        {Eu, StratList}:
	    a list consisting of two elements: the first is a matrix Eu which gives the value of the local Euler obstruction function and the second is an ordered list of lists with each element consisting of the dimension and the equations of the closure of a Whitney strata.  
    Description 
    	Text
	    Below we find the value of the Euler obstruction function (also called the local Euler obstruction function) for all strata pairs. We begin by computing a Whitney stratification to input into the eulerObsMatrix function. We will use the Whitney cusp as our example variety, which we denote $X$. 
	Example 
	    S=QQ[x..z]
	    Y=ideal(y^2+x^3-x^2*z^2)
	    V=whitneyStratify Y
	    peek V
	Text
	    Now we are ready to compute the Euler obstructions of strata. 
	Example 
	    EuL=eulerObsMatrix V
	    Eu=first EuL
	    stratList=last EuL
	Text
	    The strata (closures) are represented as two element lists, the first element being the dimension and the second the defining equations. The order of this list is used in the matrix of Euler obstruction values.

	    First fix some notation. Suppose $V_i\subset V_j$ are two strata closures appearing in the list at positions $i$ and $j$, $i\leq j$. Let  $Z$ be the open strata corresponding to $V_i$; $Z$ consists of all points in $V_i$ which are not in any strata closure of dimension strictly less than that of $V_i$.

	    With this notation in hand, the entry in row i and column j of the matrix Eu is $Eu_{V_j}(Z)$, which is the value of the local Euler obstruction function associated to $V_j$, $Eu_{V_j}: V_j\to \mathbb{Z}$, at any point in $Z$.

	    For more discussion see Remark 4.2 of reference [3] (Martin Helmer, Felix Tellander. "Spectral Decomposition of Euler-Mellin Integrals". Arxiv: 2505.12458.) Note that $Eu_{V_j}$ is defined to have value 0 outside $V_j$, meaning if some strata closure $V_l$ is not contained in $V_j$ the corresponding matrix row and column will be zero.

	    Note, this algorithm implementation contains steps which are probabilistic. To be confident of the answer it is advised to confirm by running twice. 

	    Lets look at our specific example. Consider the last column of the matrix Eu. Since the last entry in our ordered list is the defining equation of the Whitney cusp $Y$, this column gives the the values of $Eu_Y$, the Euler obstruction function of $Y$.

	Example 
	    Eu_2
	Text
	    The first entry of the column
	Example
	    Eu_(0,2)
	Text
	    is the value of $Eu_Y$ at any point in $Z=Y-V(x,y)=\{(x,y,z)|y^2+x^3-x^2*z^2=0, \; x\neq 0,\;y\neq 0\} $.

	    The second entry in of the column
	Example
	    Eu_(1,2)
	Text
	    is the value of $Eu_Y$ at any point in $Z=V(x,y)-V(x,y,z)=\{(x,y,z)|x=y=0, \; z\neq 0\} $. And the final entry of the column is the value of $Eu_Y$ at the point $(0,0,0)$.

	Text 
	    There are also several different options to perform the underlying polar variety calculations. The default algorithm uses the M2 saturate command to compute the polar varieties, this option is Algorithm=>. The other options are: Algorithm=>"msolve" and  Algorithm=>"M2F4". The Algorithm=>"msolve" only works in versions 1.25.06 and above of Macaulay2. The  Algorithm=>"M2F4" is mostly beneficial when working over a finite field. Note that over a finite field we can still sometimes obtain useful information about the stratification, but the coefficients appearing in the resulting polynomials may (or likely will) be incorrect. 	    

Node 
    Key
    	multCC
	(multCC, MutableHashTable)
    Headline
    	Computes a list containing the coefficient associated to each term in the characteristic cycle of the constructable function corresponding to the input.
    Usage
    	multCC(V)
    Inputs
    	V:MutableHashTable
	    in the format output by whitneyStratify and whitneyStratifyPol: the keys are the dimensions of strata, each gives a list of ideal defining all (closures of) strata of the corresponding dimension. Note that the input needs to represent a Whitney stratification to ensure correct results (though it need not be minimal). 
    Outputs
        {Eu, CClist}:
	    a list consisting of two elements: the first is a matrix Eu which gives the value of the local Euler obstruction function (see also the \(eulerObsMatrix\) function documentation) and the second is an ordered list of lists, CClist. The list CClist consists of lists, the first entry in each list is the multiplicity of the (conormal variety of) the strata closure in the characteristic cycle, and the second entry is a list with the dimension and the equations of the closure of the strata.  
    Description 
    	Text
	    Below we find the multiplicities of the elements of the formal sum making up the characteristic cycle of the constructable function associated to the input. When $V$ corresponds to a hypersurface this also gives the characteristic cycle arising from the D-module annihilating the associated rational function. For further background see Remark 4.2 and Example 4.5 of reference [3] (Martin Helmer, Felix Tellander. "Spectral Decomposition of Euler-Mellin Integrals". Arxiv: 2505.12458.) Note that $Eu_{V_j}$ is defined to have value 0 outside $V_j$, meaning if some strata closure $V_l$ is not contained in $V_j$ the corresponding matrix row and column will be zero.

	    This multiplicity also directly gives the value of the the Euler characteristic of the complex link of any stratum to the variety corresponding to our Whitney stratification, as we will see on our example below. 

	    Note, this algorithm implementation contains steps which are probabilistic. To be confident of the answer it is advised to confirm by running twice. 

	    We begin by computing a Whitney stratification to input into the multCC function. We will use the Whitney cusp as our example variety, which we denote $X$. 
	Example 
	    R=QQ[x..z]
	    Y=ideal(y^2+x^3-x^2*z^2)
	    V=whitneyStratify Y
	    peek V
	Text
	    Now we are ready to compute the Euler obstructions of strata. 
	Example 
	    CCmults=multCC V
	    Eu=first CCmults
	    CCmultiplcityList=last CCmults
	Text
	    Now we can extract the multiplicity with which the conormal variety of each strata will appear in the characteristic cycle.
	Example    
	    m=for s in CCmultiplcityList list first s
	Text
	    Using this we can immediately obtain the the value of the the Euler characteristic, $\chi(CL_{Y}(S))$ of the complex link, $CL_{Y}(S)$ of any stratum $S$ of $Y$ to the variety Y (for any variety $Y$). See (7) and Remark 4.2 of reference  [3] (Martin Helmer, Felix Tellander. "Spectral Decomposition of Euler-Mellin Integrals". Arxiv: 2505.12458.) for a definition.

	    Note that the block of code below can be used identically on any other example as well. 
	Example
	    d=dim Y
	    dStrats=for s in CCmultiplcityList list first last s
	    EulerCharComplexLink=for i from 0 to #CCmultiplcityList-1 list (1-(-1)^(d-dStrats_i)*m_i)
	Text 
	    There are also several different options to perform the underlying polar variety calculations. The default algorithm uses the M2 saturate command to compute the polar varieties, this option is Algorithm=>. The other options are: Algorithm=>"msolve" and  Algorithm=>"M2F4". The Algorithm=>"msolve" only works in versions 1.25.06 and above of Macaulay2. The  Algorithm=>"M2F4" is mostly beneficial when working over a finite field. Note that over a finite field we can still sometimes obtain useful information about the stratification, but the coefficients appearing in the resulting polynomials may (or likely will) be incorrect. 	    	    
///	      
TEST ///
-*  
    restart
    installPackage "WhitneyStratifications"
    needsPackage "WhitneyStratifications"
*-

--Feynman example
R=QQ[M1,P,X_0..X_2]
U = (X_1+X_2);
F =U*(M1*X_1+M1*X_2)-P*X_1*X_2;
Gh = U*X_0+F;
Xh=ideal (X_0*X_1*X_2*Gh);
params={M1,P};
S=QQ[(gens(R))_(toList(0..1))];
ansD1={ideal P, ideal M1, ideal(4*M1-P)};
ms=mapStratify(params,Xh,ideal(0_S));
assert((last ms)#1==ansD1);	    
--root counting example
R=QQ[a,b,x_1,x_2]
I=ideal(x_1^2-x_2^2+b, -a*x_1+x_1^2+b*x_2)
S=QQ[(gens(R))_(toList(0..1))]
ansD1={ideal b, ideal(a^6-3*a^4*b^2+3*a^2*b^4-b^6+a^4*b-20*a^2*b^3-8*b^5-16*b^4)}
gens S
F={R_0,R_1}
ms=mapStratify(F,I,ideal(0_S),isProper=>false)
assert(((last ms)#1)==ansD1)
--Whitney Umbrella, affine
R=QQ[x_0..x_2];
I=ideal(x_1^2*x_2-x_0^2);
V=whitneyStratify(I);
assert((first V#0)==ideal(gens R));
--Whitney Cusp, affine 
R=QQ[x_1..x_3];
I=ideal(x_2^2+x_1^3-x_1^2*x_3^2);
V=whitneyStratify I;
assert((first V#0)==ideal(gens R));
--test polar variety stratification and multCC
WS1=whitneyStratifyPol I;
WS2=whitneyStratifyPol I;
WS=new MutableHashTable from for k in keys WS1 list k=>toList((set(WS1)#k)*(set(WS2)#k));
assert((first WS#0)==ideal(gens R));
assert(#(WS#0)==1);
CCmults=multCC WS;
m=for s in last CCmults list first s;
assert(m=={1,1,1});
///	      

