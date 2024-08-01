newPackage(
	"WhitneyStratifications",
	Version => "2.03", 
    	Date => "March 30, 2024",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "mhelmer@ncsu.edu", 
		  HomePage => "http://martin-helmer.com/"}},
    	Headline => "Compute Whitney Statifications Algebraically & Stratifies Maps",
    	DebuggingMode => false,
	PackageImports=>{"Elimination","PrimaryDecomposition","Saturation"},
	Keywords => {"Algebraic Geometry"}
    	);
export{
    "conormal",
    "conormalRing",
    "whitneyStratify",
    "mapStratify", 
    "isProper",
    "nonProperSet",
    "StratsToFind"
    }

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

conormalRing=method(TypicalValue=>Ring);
conormalRing(Ideal):=(I)->(
     R:=ring I;
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
    print S;
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

whitneyStratify=method(TypicalValue=>MutableHashTable,Options=>{Projective=>false,Print=>false});
whitneyStratify (Ideal):=opts->(I)->(
    c:=codim(I);
    R:=ring I;
    n:=numgens(R);
    if opts.Projective then n=n-1;
    return whitneyStratify(I,n-c,c, Projective=>opts.Projective, Print=>opts.Print);
    );
whitneyStratify (Ideal,ZZ):=opts->(I,level)->(
    c:=codim(I);
    return whitneyStratify(I,level,c, Projective=>opts.Projective, Print=>opts.Print);
    );
whitneyStratify (Ideal,ZZ,ZZ):=opts->(I,level,Icodim)->(
    c:=Icodim;
    R:=ring I;
    n:=numgens(R);
    S:=conormalRing(I);
    if opts.Projective then n=n-1;
    V:=new MutableHashTable;
    --print "test";
    dimI:=n-codim(I);
    for i from 0 to dimI-1 do V#i=new MutableList;
    V#dimI=decompose I;
    Wdim:=0;
    if dimI<2 then return V;
    if level==0 then return V;
    J:=ideal mingens(I+minors(codim I,jacobian I));
    if codim(J)>n then return V;
    for a from 0 to level-1 do(
	--add singularities arising from intersections of current dimensional pieces 
        intse:=unique flatten for s in subsets(toList(V#(dimI-a)),2) list decompose ideal mingens(sum(s));
        for th in intse do(
            Ti:=n-codim(th);
	    if (any(toList(V#Ti),jj->th==jj)==false) then (
		(V#Ti)#(#(V#Ti))=th;
		if opts.Print==true then <<"Strata found= "<<th<<endl;
		);
	    );
	--now find singularities and Whitney points of current dim pieces 
	for Ia in V#(dimI-a) do(
	    dimIa:=dimI-a;
	    J=ideal mingens(Ia+minors(codim Ia,jacobian Ia));
	    mu:=n-codim(J);
	    ell:=0;
	    W:=0;
	    --add all singularities of Ia
	    for K in decompose(J) do(
		for i from 0 to dimIa-1 do(
		    if i==(n-codim(K)) and (any(toList(V#i),jj->K==jj)==false) then(
			(V#i)#(#(V#i))=K;
			if opts.Print==true then <<"Strata found= "<<K<<endl;
			);
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
				----- associated primes way
			    	Cpull=ideal mingens (CIa+sub(Z,S));
			    	    W=for q in associatedPrimes Cpull list sub(eliminate((gens S)_{(numgens(R))..numgens(S)-1},q),R);
			    	    for K in W do(
				    	for i from 0 to ell-1 do(
				    	    Ztemp:=ideal mingens(K+Z);
				    	    if i==(n-codim(K)) and (any(toList(V#i),jj->Ztemp==jj)==false) then(
						(V#i)#(#(V#i))=Ztemp;
						if opts.Print==true then <<"Strata found= "<<Ztemp<<endl;
						);
				    	    );
				    	);
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

mapStratify=method(TypicalValue=>MutableHashTable,Options=>{Projective=>false,isProper=>true,StratsToFind=>"all",Print=>false});
mapStratify (List,Ideal,Ideal):=opts->(F,X,Y)->(
    R:=ring X;
    S:=ring Y;
    kk:=coefficientRing R;
    if opts.Print==true then <<"R= "<<R<<"S= "<<S<<endl;
    conRingX:=conormalRing(X);
    conRingY:=conormalRing(Y);
    if opts.Print==true then<<"conRingX= "<<gens(conRingX)<<endl;
    n:=numgens(R);
    m:=numgens(S);
    if opts.Print==true then<<"isProper= "<<opts.isProper<<endl;
    V:=new MutableHashTable;
    VcheckedBeforeInd:=new MutableHashTable;
    W:=new MutableHashTable;
    WcheckedBeforeInd:=new MutableHashTable;
    if opts.Projective then(
	<<"Projective Option not implemented yet"<<endl;
	n=n-1;
	m=m-1;
	);
    fIsCoordProjection:=false; 
    if opts.Print==true then<<"F= "<<F<<", gens S= "<<gens S<<endl;
    graphR:=0;
    elimIm:=0;
    elimInv:=0;
    Gam:=0;
    y:=0;
    if all(F, i->any(gens R,j->i===j)) then(
	if opts.Print==true then <<"the map is a coordinate projection"<<endl;
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
    if opts.Print==true then <<"graph R= "<<gens(graphR)<<endl;
    if opts.Print==true then <<"elimIm= "<<elimIm<<endl;
    if opts.Print==true then <<"elimInv= "<<elimInv<<endl;
    dimX:=n-codim(X);
    tempSubsetsV:=0;
    tempSubsetsW:=0; 
    dimY:=m-codim(Y);
    --
    --print "starting Jel";
    fX:=ideal mingens(sub(eliminate(elimIm,Gam+sub(X,graphR)),S)+Y);
    --fX:=Y;
    dimfX:=m-codim(fX);
    for i from 0 to dimX-1 do V#i=new MutableList;
    for i from 0 to dimfX-1 do W#i=new MutableList;
    
    for i from 0 to dimX do VcheckedBeforeInd#i=0;
    for i from 0 to dimfX do WcheckedBeforeInd#i=0;
    if opts.Print==true then print "decompose X";
    V#dimX=decompose X;  
    --V#dimX={X};
    if opts.Print==true then print "decompose Y";
    W#(dimY)=decompose Y;
    if fX!=Y then(
	Y=fX;
	dimY=dimfX;
	W#(dimY)=decompose Y;
	);
    if opts.isProper==false then(
	if opts.Print==true then<<"Computing Jolenck flag"<<endl;
	JelY:=new MutableList from {};
	JelX:=new MutableList from {};
	JelY#0=Y;
	JelX#0=X;
	for i from 0 to dimY-1 do(
	    if opts.Print==true then<<"Jel#"<<i<<" ="<<JelY#i<<endl;
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
    if opts.Print==true then print "past Jel check/computation";	
    if opts.Print==true then print "V";
    if opts.Print==true then for ke in keys V do print toList(V#ke);
    if opts.Print==true then print "W";
    if opts.Print==true then for ke in keys W do print toList(W#ke);
    --level:=max(dimX,dimY);
    --levelX:=dimX;
    --lelvelY:=dimY;
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
    if opts.Print==true then<<"dim X= "<<dimX<<", dim Y= "<<dimY<<endl;
    while (aX<dimX) or (aY<dimY) do(
	if opts.Print==true then<<"aX= "<<aX<<", aY="<<aY<<endl;
    --for a from 0 to level-1 do(
	if opts.Print==true then <<"{aY, aX}= "<<{aY,aX}<<endl;
	-- Y at codim>a 
	if (dimY-aY)>0 then(
	    if opts.Print==true then print "In Y";
	    	--add subset intersections of Y at level aY
		tYind=WcheckedBeforeInd#(dimY-aY);
		YaNew=(toList(W#(dimY-aY)))_(toList(tYind..(#(W#(dimY-aY))-1)));
		YaOld=(toList(W#(dimY-aY)))_(toList(0..tYind-1));
		tempSubsetsW=join(flatten for a1 in YaOld list(for b1 in YaNew list {a1,b1}),subsets(YaNew,2));
		--TODO need to add intersections of the form 1 from before tYind and 1 from after tYind
		if opts.Print==true then<<"starting subset sing at "<<dimY-aY<<", num subsets W= "<<#tempSubsetsW<<endl;
		intse=unique flatten for s in tempSubsetsW list decompose ideal  mingens(sum(s));
		for th in intse do(
		    Ti=m-codim(th);
		    if opts.Print==true then <<"Y Whit= "<<th<<endl;
		    if (any(toList(W#Ti),jj->th==jj)==false) then (
			(W#Ti)#(#(W#Ti))=th;
			if opts.Print==true then<<"added to Y strat= "<<th<<endl;
			);
		    --Pullback to X, add to X strat, V
		    Kinv=ideal mingens(sub(eliminate(elimInv,Gam+sub(th,graphR)),R)+X);
		    for comp in decompose Kinv do(
			dimKinv=n-codim(comp);
			if opts.Print==true then <<"Y Whit in X= "<<comp<<endl;
			if (any(toList(V#dimKinv),jj->comp==jj)==false) then(
			    (V#dimKinv)#(#(V#dimKinv))=comp;
			    					--have unprocessed components above current dim, go back up
					if dimKinv>(dimX-aX) then aX=dimX-dimKinv;
			    );
			);
		    );
	    --for Ya in W#(dimY-a) do(
	    for j2 from WcheckedBeforeInd#(dimY-aY) to #(W#(dimY-aY))-1 do(
		Ya=(W#(dimY-aY))#j2;
		WcheckedBeforeInd#(dimY-aY)=WcheckedBeforeInd#(dimY-aY)+1;
	    	dimYa=dimY-aY;
	    	JY=ideal mingens(Ya+minors(codim Ya,jacobian Ya));
	    	muY=m-codim(JY);
	    	ellY=0;
	    	WhitYa=0;
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
		if muY>0 and (opts.StratsToFind!="singularOnly") then (
		    if opts.Print==true then <<"doing Y conormal with "<<Ya<<endl;
		    CYa=conormal(ideal mingens Ya,conRingY);
		    CpullYa=0;
		    for j from 0 to muY do(
		    	ell=muY-j;
		    	for Z in W#ell do(
			    CpullYa=ideal mingens (CYa+sub(Z,conRingY));
			    if opts.Print==true then <<"doing Y assoc. primes with "<<CpullYa<<endl;
			    if (opts.StratsToFind=="all") then(
				WhitYa=for q in associatedPrimes CpullYa list sub(eliminate((gens conRingY)_{(numgens(S))..numgens(conRingY)-1},q),S);
				)
			    else(
				WhitYa=for q in decompose CpullYa list sub(eliminate((gens conRingY)_{(numgens(S))..numgens(conRingY)-1},q),S);
				);
			    if opts.Print==true then <<"done assoc primes "<<endl;
			    for K in WhitYa do(
				--Add to Y strat, W
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
	    if opts.Print==true then print "In X";
	    	--subset intersections at level aX of X
		tXind=VcheckedBeforeInd#(dimX-aX);
		XaNew=(toList(V#(dimX-aX)))_(toList(tXind..(#(V#(dimX-aX))-1)));
		XaOld=(toList(V#(dimX-aX)))_(toList(0..tXind-1));
		tempSubsetsV=join(flatten for a1 in XaOld list(for b1 in XaNew list {a1,b1}),subsets(XaNew,2));
	    	--Add subset intersections of Xa, push forward
		--tempSubsetsV=subsets(toList(V#(dimX-a)),2);
		if opts.Print==true then<<"starting subset sing at "<<dimX-aX<<", num subsets V= "<<#tempSubsetsV<<endl;
		    intse=unique flatten for s in tempSubsetsV list decompose ideal  mingens(sum(s));
		    for th in intse do(
		    	Ti=n-codim(th);
		    	if opts.Print==true then<<"X whit subsets= "<<th<<endl;
		    	if (any(toList(V#Ti),jj->th==jj)==false) then (V#Ti)#(#(V#Ti))=th;
		    	--Pushforward to Y, add to Y strat, W
		    	Kinv=ideal mingens(sub(eliminate(elimIm,Gam+sub(th,graphR)),S)+Y);
		    	for comp in decompose Kinv do(
			    dimKinv=m-codim(comp);
			    if opts.Print==true then<<"X whit in Y "<<comp<<endl;
			    if (any(toList(W#dimKinv),jj->comp==jj)==false) then (
				(W#dimKinv)#(#(W#dimKinv))=comp;
				--have unprocessed components above current dim, go back up
				if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
				if opts.Print==true then<<"Added to Y Sing = "<<comp<<endl;
				if opts.Print==true then<<"Dim th= "<<dim(th)<<", From X part= "<<th<<endl;
				if opts.Print==true then<<", intse="<<intse<<endl;
				);
			    );
		    	);
	    for j2 from VcheckedBeforeInd#(dimX-aX) to #(V#(dimX-aX))-1 do(
		Xa=(V#(dimX-aX))#j2;
		VcheckedBeforeInd#(dimX-aX)=VcheckedBeforeInd#(dimX-aX)+1;
	    	--dima=dimY-aY;	    
	    --for Xa in V#(dimX-a) do(
		if opts.Print==true then<<"Xa= "<<Xa<<endl;
	    	dimXa:=dimX-aX;
	    	JX=ideal mingens(Xa+minors(codim Xa,jacobian Xa));
		if opts.Print==true then<<"JX= "<<JX<<endl;
	    	muX=n-codim(JX);
	    	ellX=0;
	    	WhitXa=0;
	    	for K in decompose(JX) do(
		     Ti=n-codim(K);
		     if opts.Print==true then<<"X Whit= "<<K<<endl;
		     if (any(toList(V#Ti),jj->K==jj)==false) then (V#Ti)#(#(V#Ti))=ideal mingens(K);
		    );
		AX=jacobian(Xa+ideal(F));
		if opts.Print==true then<<"AX= "<<AX<<endl;
		--todo, use the tighter bound here
		rXa=min(numrows(AX), numcols(AX));
		for i from 0 to rXa do(
		    IiXf=ideal mingens(Xa+minors(rXa-i,AX));
		    if opts.Print==true then<<"IiXf= "<<IiXf<<endl;
		    if IiXf==ideal(1_R) then break; 
		    for K in decompose(IiXf) do(
			--add to X strat, V
			Ti=n-codim(K);
			if opts.Print==true then<<"X Tb 1= "<<K<<endl;
			if opts.Print==true then<<"V#"<<Ti<<" ="<<toList(V#Ti)<<", ring= "<<(for v in toList(V#Ti) list gens(ring(v)))<<endl;
			if (any(toList(V#Ti),jj->K==jj)==false) then (
			    (V#Ti)#(#(V#Ti))=K;
			    if opts.Print==true then<<"Added to X TB = "<<K<<endl;
			    );
			--add image to Y strat, W
			Kpf=ideal mingens(sub(eliminate(elimIm,Gam+sub(K,graphR)),S)+Y);
		       	for comp in decompose Kpf do(
		            dimKinv:=m-codim(comp);
			    if opts.Print==true then<<"Y TB= "<<comp<<endl;
			    if (any(toList(W#dimKinv),jj->comp==jj)==false) then(
				(W#dimKinv)#(#(W#dimKinv))=comp;
				if dimKinv>(dimY-aY) then aY=dimY-dimKinv;
				if opts.Print==true then<<"Added to Y TB = "<<comp<<endl;
				);
			    );
		    	);
		    );
		if opts.Print==true then<<"done TB"<<", muX="<<muX<<endl;
		if muX>0 and (opts.StratsToFind!="singularOnly") then (
		    if opts.Print==true then<<"doing X conormal with "<<Xa<<endl;
		    CXa=conormal(ideal mingens Xa,conRingX);
		    Cpull=0;
		    for j from 0 to muX do(
		    	ell=muX-j;
		    	for Z in V#ell do(
			    Cpull=ideal mingens (CXa+sub(Z,conRingX));
			    if opts.Print==true then<<"doing X assoc. primes with "<<Cpull<<endl;
			    if (opts.StratsToFind=="all") then(
				WhitXa=for q in associatedPrimes Cpull list sub(eliminate((gens conRingX)_{(numgens(R))..numgens(conRingX)-1},q),R);	
				)
			    else(
				WhitXa=for q in decompose Cpull list sub(eliminate((gens conRingX)_{(numgens(R))..numgens(conRingX)-1},q),R);
				);
			    if opts.Print==true then<<"done associated primes X"<<endl;
			    for K in WhitXa do(
			    	--only add elements that are new
			    	Ti=n-codim(K+Z);
			    	if (any(toList(V#Ti),jj->K==jj)==false) then (V#Ti)#(#(V#Ti))=ideal mingens(K+Z);
			    	if opts.Print==true then<<"X whit= "<<K<<endl;
		       	    	Kpf=ideal mingens(sub(eliminate(elimIm,Gam+sub(K+Z,graphR)),S)+Y);
		       	    	for comp in decompose Kpf do(
		       	    	    dimKinv=m-codim(comp);
				    if opts.Print==true then<<"X whit in Y "<<comp<<endl;
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
	    aX=aX+1;
		    if opts.Print==true then<<"|W|= "<<toString(sum for k in keys W list #(W)#k)<<endl;
		    if opts.Print==true then<<"|V|= "<<toString(sum for k in keys V list #(V)#k)<<endl;
	    	);
    	);
    for a in keys(V) do V#a=unique toList(V#a); 
    for a in keys(W) do W#a=unique toList(W#a);
    use R;  
    return {V,W};
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
	      This package computes Whitney stratifications of real and complex algebraic varieties using the algorithms described in [1, 2]. For varieties considered over the complex numbers the output is indexed by the strata dimension. When wishing to treat the variety over the reals, the same output may be used, but the dimensions of the strata may differ (and some strata may be empty), see [2] for more details. This post processing in the real case is currently left to the user.    
	      
	      A method is also provided to stratify polynomial maps $f:X\to Y$ between algebraic varieties, the output is a Whitney stratification of both $X$ and $Y$, such that for each (open, connected) strata $M$ of $X$ there is an (open, connected) strata $N$ of $Y$ such that $f(M) \subset N$ and such that the restriction of $f$ to $M$ is a submersion. 
	      
	      Computing the Conormal variety of a variety is an important step in these algorithms, so a method for this is also provided.  
	      
	      References:
	      
	      [1] Martin Helmer and Vidit Nanda. "Conormal Spaces and Whitney Stratifications", Foundations of Computational Mathematics, DOI: 10.1007/s10208-022-09574-8.
	      
	      [2] Martin Helmer and Vidit Nanda "Effective Whitney stratification of real algebraic varieties", arXiv:2307.05427v2, 2023.
	      
	      
Node 
    Key
    	whitneyStratify
	(whitneyStratify, Ideal)
    Headline
    	Computes a Whitney stratification of the real and complex varieties.
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
	    Now the projective version; using the option Projective=>true will treat the input as a projective vareity. If this option is omitted the input is treated as an affine cone over the projective vareity.
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
V=whitneyStratify I
assert((first V#0)==ideal(gens R));
      ///
