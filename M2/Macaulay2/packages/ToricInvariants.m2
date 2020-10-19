newPackage(
	"ToricInvariants",
	Version => "3.01", 
    	Date => "July 12, 2018",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "m.helmer@math.ku.dk", 
		  HomePage => "http://martin-helmer.com/"}},
    	Headline => "Euclidean distance degrees, polar degrees, degree and codimension of the dual, and Chern-Mather classes of toric varieties X_A from the polytope conv(A) or from its Gale dual",
	Keywords => {"Toric Geometry"},
    	DebuggingMode => false,
	PackageImports => {"LLLBases", "Polyhedra" }
    	);

export{"polarDegrees",
       "edDeg",
       "cmVolumes",
       "cmClass",
       "dualDegCodim",
       "Output",
       "TextOutput",
       "ForceAmat"
    }


volB=method(TypicalValue=>ZZ)
volB (MutableHashTable):=(alpha)->(
    Balpha:=alpha#"Balpha";
    vec:=first Balpha;
    primVec:=0;
    vol:=0;
    tempDet:=0;
    if allInrelLine(Balpha) then(
	    primVec=(1/gcd(vec))*vec;
	    ind:=position(vec,i->i!=0); 
	    for i from 0 to #Balpha-1 do(
		tempDet=det(matrix{primVec,Balpha_i});
		<<"primVec= "<<primVec<<", vec="<<Balpha_i<<endl;
		if tempDet>0 then vol=vol+tempDet;
		);
	    return vol;
	)
    else return 1;
    );
indZ=method(TypicalValue=>ZZ)
indZ (List) :=(Aalpha)->(
    alpMat:=matrix Aalpha;
    tM:=groebnerBasis transpose groebnerBasis (alpMat);
    if numrows(tM)==numColumns(tM) then(
	dM:=det(tM);
	if dM!=0 then return dM else (error("Face of polytope seems invalid...."); return 1;);
	)
    else(
	print "Index Error";
	return 1; 
	)
    );
allInrelLine=method(TypicalValue=>Boolean)
allInrelLine (List) :=(Balpha)->(
    if #Balpha==1 then return true; 
    W:=for b in Balpha list (1/gcd(b))*b;
    v:=#W;
    --<<"scaled vector= "<<W<<endl;
    for i from 0 to v-1 do(
	for j from i to v-1 do(
	    if not (W_j==W_i or W_j==-W_i) then return false;
	    );
	);
    return true; 
    );
muA=method(TypicalValue=>ZZ)
muA (Matrix,MutableHashTable,MutableHashTable):=(A,alpha,beta)->(
    return mu(A,alpha,beta);
    );
muB=method(TypicalValue=>ZZ)
muB (List,MutableHashTable,MutableHashTable):=(B,BalphaHash,BbetaHash)->(
    Balpha:=BalphaHash#"Balpha";
    Bbeta:=BbetaHash#"Balpha";
    --print "Find mu with";
    --<<"Bbeta= "<<Bbeta<<endl;
    --<<"Balpha= "<<Balpha<<endl;
    w:=-sum(Bbeta);
    detSum:=0;
    temp:=-1;
    muBalbe:=0;
    Am:=0;
    Amz:=0;
    P:=0;
    primVec:=0;
    vec:=0;
    vminus:=0;
    vplus:=0;
    lambda:=0;
    tempDegB:=0;
    tempMat:=0;
    lams:={};
    gcdlams:=1;
    if matrix(B)==matrix(Balpha) then(
	--print "alpha=big mat";
	if allInrelLine(Bbeta) then(
	    --print "all in rel line";
	    vec=first Bbeta;
	    --<<"vec= "<<vec<<", gcd vec= "<<gcd(vec)<<endl;
	    primVec=(1/gcd(vec))*vec;
	    ind:=position(vec,i->i!=0);
	    --<<"primVec= "<<primVec<<", vec="<<vec<<endl; 
	    for i from 0 to #Bbeta-1 do(
		lambda=((Bbeta_i)_ind)/(primVec_ind);
		lams=append(lams,lambda);
		--<<"lambda= "<<lambda<<endl;
		if lambda<0 then vminus=vminus+abs(lambda) else vplus=vplus+abs(lambda);
		);
	    gcdlams=gcd(lams);
	    if vplus==0 then muBalbe=(1/gcdlams)*vminus else if vminus==0 then muBalbe=(1/gcdlams)*vplus else muBalbe=(1/gcdlams)*min(vplus,vminus);
	    )
	else(
	    --<<"NOT all in rel line"<<endl;
	    for b in Bbeta do(
	    	temp=det(matrix{w}||matrix{b});
	    	if temp>0 then detSum=detSum+temp;
	    	);
	    tempDegB=degB(append(Bbeta,w));
	    muBalbe=(1/indZ(append(Bbeta,w)))*(tempDegB-detSum);
	    );
	)
    else(
	if allInrelLine(Bbeta) then(
	    muBalbe=1/BbetaHash#"index";
	    )
    	else (
	    if not allInrelLine(Balpha) then (
		--check square....
		muBalbe=1/BbetaHash#"index";
		    ) 
		else(
		    vec=first Balpha;
	    	    primVec=(1/gcd(vec))*vec;
	    	    for i from 0 to #Bbeta-1 do(
			lambda=det(matrix{Bbeta_i}||matrix{primVec});
			lams=append(lams,lambda);
			if lambda<0 then vminus=vminus+abs(lambda) else vplus=vplus+abs(lambda);
			);
	    	    gcdlams=indZ(BbetaHash#"Aalpha");
	    	    if vplus==0 then muBalbe=(1/gcdlams)*vminus else if vminus==0 then muBalbe=(1/gcdlams)*vplus else muBalbe=(1/gcdlams)*min(vplus,vminus);
		    );
	    
	    );
	);
    muRet:=1;
    if matrix(B)==matrix(Balpha) then(muRet=muBalbe;) else muRet=BalphaHash#"index"*muBalbe;
    return muRet;
    );
degB=method(TypicalValue=>ZZ)
degB (List):=(Balpha)->(
    B:=matrix Balpha;
    beta1:=sum select(flatten entries (B_{0}),i->i>0);
    beta2:=sum select(flatten entries (B_{1}),i->i>0);
    v:={};
    m:=length(Balpha);
    for i from 0 to m-2 do(
	for j from i+1 to m-1 do(
	    if ( (((Balpha)_i)_0<0) and (((Balpha)_j)_0<0) ) or ( (((Balpha)_i)_1<0) and (((Balpha)_j)_1<0) ) or ( (((Balpha)_i)_0>0) and (((Balpha)_j)_0>0) ) or ( (((Balpha)_i)_1>0) and (((Balpha)_j)_1>0) ) then(
		v=append(v,0);
		)
	    else(
		v=append(v,min( abs((((Balpha)_i)_0)*(((Balpha)_j)_1)), abs((((Balpha)_i)_1)*(((Balpha)_j)_0))  ));
		);
	    );
	);
    return beta1*beta2-sum(v);
    );
faceLatticeAB=method(TypicalValue=>List)
faceLatticeAB (Matrix):=(A)->(
    B:=gens ker A;
    Bc:=B;
    Ac:=A;
    Acl:=entries transpose Ac;
    P:=convexHull(A);
    VPT:=vertices P;
    m:=dim(P);
    FacePoset:={};
    ifaces:={};
    tempFace:={};
    curFace:=0;
    curVol:=0;
    Acols:=entries transpose(A);
    notFcols:=0;
    interiorCols:={};
    faceCounts:={};
    n:=numColumns(A);
    indsSet:=toList(0..n-1);
    for i from 0 to m do(
	ifaces={};
	--tempFace=faces(i,P);
	tempFace=for fc in faces(i,P) list convexHull(VPT_(fc#0));
	faceCounts=append(faceCounts, #tempFace);
	for f in tempFace do(
	    curFace=new MutableHashTable;
	    curFace#"dim"=m-i;
	    curFace#"verticesMat"=lift(vertices(f),ZZ);
	    curFace#"verticesList"=entries transpose curFace#"verticesMat";
	    if i==m then(
		  curVol=1;
		 )
	     else(
		 curVol=((m-i)!)*volume(f);
		 );
	    curFace#"volume"=curVol;
	    notFcols=toList(set(Acols)-set(curFace#"verticesList"));
	    interiorCols={};
	    for c in notFcols do(
		if contains(f,transpose matrix{c}) then(
		    interiorCols=append(interiorCols,c);
		    );
		);
	    curFace#"interiorCol"=interiorCols;
	    curFace#"Aalpha"=join(interiorCols,curFace#"verticesList");
	    if i==0 then (
		L:=for wl in curFace#"Aalpha" list position(entries transpose A,ll->ll==wl);
		Ac=A_L;
		Bc=B^L;
		curFace#"Balpha"=entries Bc;
		Acl=entries transpose Ac;
		)
	    else(
		L2:=for wl in curFace#"Aalpha" list position(Acl,ll->ll==wl);
		Bcalpha:=Bc^(indsSet-set(L2));
		curFace#"Balpha"=entries Bcalpha;
		);
	    ifaces=append(ifaces,curFace);	    
	    );
	FacePoset=append(FacePoset,ifaces);
	);
    return FacePoset; 
    );
cmClass=method(TypicalValue=>RingElement,Options => {TextOutput=>"Quiet",Output=>RingElement,ForceAmat=>false})
cmClass (Matrix,QuotientRing):=opts->(A,Chring)->(
    if opts.Output===HashTable then(
	return polarDegrees(A,Chring,Output=>HashTable,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat);
	);
    return (polarDegrees(A,Chring,Output=>HashTable,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat))#"CM class";
    );
cmClass (Matrix):=opts->(A)->(
        if opts.Output===HashTable then(
	    return polarDegrees(A,Output=>HashTable,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat);
	);
    return (polarDegrees(A,Output=>HashTable,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat))#"CM class"
    );
polarDegrees=method(TypicalValue=>List,Options => {TextOutput=>"Quiet",Output=>List,ForceAmat=>false})
polarDegrees (Matrix):=opts->(A)->(
    n:=numColumns(A);
    h:=symbol h;
    Chring:=ZZ[h]/(h^n);
    return polarDegrees(A,Chring,Output=>opts.Output,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat);
    );
polarDegrees (Matrix,QuotientRing):=opts->(A,Chring)->(
    EV:=0;
    TempEV:=0;
    FacePoset:={};
    if opts.Output===HashTable then(
	TempEV= cmVolumes(A,TextOutput=>opts.TextOutput,Output=>opts.Output,ForceAmat=>opts.ForceAmat);
	EV=first TempEV;
	FacePoset=last TempEV;
	) else EV=cmVolumes(A,Output=>opts.Output,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat);
    m:=#EV-1;
    delta:={};
    temp:=0;
    for i from 0 to m do(
	temp=(-1)^(m-i)*sum(i..m,j->(-1)^(j-i)*binomial(j+1,i+1)*(EV_(m-j)));
	delta=append(delta, temp);
	);
    EVList:=reverse EV;
    A=transpose matrix unique entries transpose A;
    n:=numColumns(A);
    h:=Chring_0;
    cmClass:=sum(#(EVList),i->EVList_i*h^(n-1-i));
    FirstNonZero:=position (delta,i->(not i==0) );
    if opts.Output===List then(
	<<"The toric variety has degree = "<<last(EVList)<<endl;
	<<"The dual variety has degree = "<<delta_FirstNonZero<<", and codimension = "<<FirstNonZero+1<<endl;
    	<<"Chern-Mather Volumes: (V_0,..,V_(d-1)) = "<<EVList<<endl;
    	<<"Polar Degrees: "<<delta<<endl;
    	<<"ED Degree = "<<sum(delta)<<endl;
    	<<endl;
    	<<"Chern-Mather Class: "<<cmClass<<endl;
	);
    if opts.Output===HashTable then(
	delHash:=hashTable{"degree"=>last(EVList),"dual codim"=>FirstNonZero+1,"dual degree"=>delta_FirstNonZero,"hyperplane class"=>Chring_0,"Chow ring"=>Chring,"ED"=>sum(delta),"polar degrees"=>delta,"CM class"=>cmClass,"CM volumes"=>EVList,"FacePoset"=>FacePoset};
	return delHash;
	);
    return delta;
    );
dualDegCodim=method(TypicalValue=>HashTable,Options => {ForceAmat=>false})
dualDegCodim (Matrix):=opts->(A)->(
    pdh:=polarDegrees(A,Output=>HashTable,ForceAmat=>opts.ForceAmat);
    pd:=pdh#"polar degrees";
    FirstNonZero:=position (pd,i->(not i==0) );
    return hashTable{"dualCodim"=>FirstNonZero+1,"dualDegree"=>pd_FirstNonZero};
    );
edDeg=method(TypicalValue=>ZZ,Options => {TextOutput=>"Quiet",Output=>ZZ,ForceAmat=>false})
edDeg (Matrix):=opts->(A)->(
    if opts.Output===HashTable then(
	return polarDegrees(A,Output=>opts.Output,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat);
	);
    return sum polarDegrees(A,TextOutput=>opts.TextOutput,ForceAmat=>opts.ForceAmat);    
    );
cmVolumes=method(TypicalValue=>List,Options => {TextOutput=>"Quiet",Output=>List,ForceAmat=>false})
cmVolumes (Matrix):=opts->(A)->(
    BMatCD2:=false;
    AinNum:=numColumns(A);
    A=transpose matrix unique entries transpose A;
    if opts.TextOutput!="Quiet" and AinNum!=numColumns(A) then (
	print "Warning: Input matrix contains duplicate columns, duplicate columns have been removed";
	);
    if minors(numRows(A),A)!=ideal(1) then (
	if opts.TextOutput!="Quiet" then print "Input matrix does not generate the integer lattice, attempting to build a new matrix which generates the lattice and defines the same toric ideal";
	betterA:= transpose gens kernel transpose gens kernel A;
	if minors(numRows(betterA),betterA)==ideal(1) then(
	    if opts.TextOutput!="Quiet" then print "New matrix generated";
	    A=transpose hermite transpose betterA;
	    )
	else(
	    print "Input matrix does not generate the integer lattice, attempting to build a new matrix which generates the lattice and defines the same toric ideal";
	    error"Matrix generation failed, please enter a matrix whose columns span the full integer lattice";
	    return 0; 
	    );
	);
    if rank(A)!=min(numRows(A),numColumns(A)) then (
	error "Input matrix expected to have maximal rank";
	return 0;
	);
    if rank(A)<rank(A||matrix{toList(numColumns(A):1)}) then(
	if opts.TextOutput!="Quiet" then print "Adding a row of ones";
	A=(A||matrix{toList(numColumns(A):1)});
	);

    if opts.TextOutput!="Quiet" and ((numRows(A)+numColumns(A))<35) then <<"Proceeding with A-matrix="<<entries(A)<<endl;
    P:=convexHull(A);
    VPT:=vertices P;
    m:=dim(P);
    FacePoset:={};
    ifaces:={};
    tempFace:={};
    curFace:=0;
    curVol:=0;
    Acols:=entries transpose(A);
    notFcols:=0;
    interiorCols:={};
    faceCounts:={};
    B:=gens ker A;
    --<<"variety has codim= "<<numColumns(B)<<", "<<endl;
    if numColumns(B)==2 then(
	    if not opts.ForceAmat then(
        BMatCD2=true;
		--print "using B Matrix method";
		);
	    );
    --<<BMatCD2<<", B= "<<B<<endl;
    Bc:=B;
    Ac:=A;
    Acl:=entries transpose Ac;
    n:=numColumns(A);
    indsSet:=toList(0..n-1);
    HP:=0;
    HS:=0;
    --end defs of B mat stuff
    timList:={};
    timList2:={};
    for i from 0 to m do(
	ifaces={};
	--print "find faces";
	--time 
			
	tim1:=elapsedTiming(
	--tempFace=faces(i,P)
	tempFace=for fc in faces(i,P) list convexHull(VPT_(fc#0));
	);
	timList2=append(timList2,first tim1);
	faceCounts=append(faceCounts, #tempFace);
	for f in tempFace do(
	    curFace=new MutableHashTable;
	    curFace#"dim"=m-i;
	    curFace#"verticesMat"=lift(vertices(f),ZZ);
	    curFace#"verticesList"=entries transpose curFace#"verticesMat";
	    notFcols=toList(set(Acols)-set(curFace#"verticesList"));
	    interiorCols={};	    
	    tim2:=elapsedTiming(
	    if i>0 and i<m then(
		HP = hyperplanes f;
		HS = halfspaces f;
		HP0:=HP_0;
		HS0:=HS_0;
		HP1:=HP_1;
		HS1:=HS_1;
	    	for c in notFcols do(
		    --<<"mat c "<<transpose matrix{c}<<endl;
		     HP2:=(HP0*(transpose matrix{c}))-HP1;
	    	     HS2:=(HS0 * (transpose matrix{c}))-HS1;
		    if all(flatten entries HP2, e -> e == 0) and (all(flatten entries HS2, e -> e <= 0)) then(  
		    	interiorCols=append(interiorCols,c);
		    	);
		    );
	    	);
	    --print "done int check";
	    );
	    timList=append(timList,first tim2);
	    curFace#"interiorCol"=interiorCols;
	    curFace#"Aalpha"=join(interiorCols,curFace#"verticesList");
	    if i==0 then curFace#"Aalpha"=Acols;
	    if BMatCD2==true then curFace#"index"=indZ(curFace#"Aalpha");
	    if BMatCD2==true then(
		if i==0 then (
		    L:=for wl in curFace#"Aalpha" list position(entries transpose A,ll->ll==wl);
		    Ac=A_L;
		    Bc=B^L;
		    if opts.TextOutput!="Quiet" then <<"Canonical Forms: Ac= "<<Ac<<", Bc="<<Bc<<endl;
		    curFace#"Balpha"=entries Bc;
		    curFace#"subscripts"=L;
		    Acl=entries transpose Ac;
		    )
		else(
		    L2:=for wl in curFace#"Aalpha" list position(Acl,ll->ll==wl);
		    Bcalpha:=Bc^(indsSet-set(L2));
		    curFace#"Balpha"=entries Bcalpha;
		    curFace#"subscripts"=L2;
		    );
	    		
		);
	    if i==m then(
		curVol=1;
		)
	    else(
		if BMatCD2==true then (
		    if i==0 then(
			curVol=((m-i)!)*volume(f)
			)
		    else(
			if allInrelLine(curFace#"Balpha") then (
			    v1:=first curFace#"Balpha";
			    v1=1/(gcd(v1))*v1;
			    PosSum:=0;
			    for b in entries(B^(curFace#"subscripts")) do(
 				tempdet1:=det(matrix({v1,b}));
				if tempdet1>0 then PosSum=PosSum+tempdet1;
				); 
			    curVol=PosSum;
 			    ) 
			else curVol=(curFace#"index");
			);
		     )
		else(
		    curVol=((m-i)!)*volume(f);
		    );
		);
	    curFace#"volume"=curVol;
	    ifaces=append(ifaces,curFace);	    
	    );
	FacePoset=append(FacePoset,ifaces);
	);
    --<<"time to build face lattice= "<<sum(timList2)+sum(timList)<<endl;
    V:={};
    tempEL:=0;
    tempVL:=0;
    tempAlpha:=0;
    tempBeta:=0;
    tempsum:=0;
    volBeta:=0;
    tempEV:=0;
    tempMuBeta:={};
    tempMu:=0;
    ((FacePoset_0)_0)#"EulerOb"=1;
    V=append(V,((FacePoset_0)_0)#"EulerOb"*((FacePoset_0)_0)#"volume");
    for i from 1 to m do(
	 tempBeta=FacePoset_i;
	 tempEL={};
	 tempVL={};
	 tempEV=0;
         for j from 0 to #(tempBeta)-1 do(
	     tempsum=0;
		 for l from 0 to i-1 do(
		     tempAlpha=FacePoset_l;
		     for b from 0 to (#(tempAlpha)-1) do(
			 if isSubset((tempBeta_j)#"verticesList",(tempAlpha_b)#"verticesList") then(
			      if opts.TextOutput!="Quiet" then(
			     	 print "*****************************************";
			     	 print "Find mu with";
				 if(tempAlpha_b)#?"Balpha" and (tempBeta_j)#?"Balpha" then(
				     <<"Balpha= "<<((tempAlpha_b)#"Balpha")<<endl;
				     <<"Bbeta= "<<((tempBeta_j)#"Balpha")<<endl;
				     );
			     	 <<"Aalpha= "<<transpose matrix((tempAlpha_b)#"Aalpha")<<", Abeta= "<<transpose matrix((tempBeta_j)#"Aalpha")<<endl;
				 );
			     if BMatCD2==true then(
				 tempMu=muB(entries(Bc),(tempAlpha_b),(tempBeta_j));
				 )
			     else (
				 tempMu=muA(A,tempAlpha_b,tempBeta_j);
				 );
			     if opts.TextOutput!="Quiet" then(
			     	 <<"mu= "<<tempMu<<endl; 
			     	 print "*****************************************";
			     	 );
			     tempMuBeta=append(tempMuBeta,{tempMu,(tempAlpha_b)#"verticesMat"});
			     tempsum=tempsum+(-1)^(i-l-1)*tempMu*((tempAlpha_b)#"EulerOb");
			     );
			 );
		     );
		 ((FacePoset_i)_j)#"EulerOb"=tempsum;
		 ((FacePoset_i)_j)#"Mu"=tempMuBeta;
		 tempMuBeta={};
		 tempEL=append(tempEL,tempsum);
		 tempEV=tempEV+tempsum*((tempBeta_j)#"volume");
	     );
	 V=append(V,tempEV);
	);
    --);
    <<endl;
    if opts.Output===HashTable then(
	return {V,FacePoset};
	) else return V;
    );
normalizedVolume=method(TypicalValue=>ZZ)
normalizedVolume (Polyhedron,ZZ):=(P,n)->(
    if n>dim(P) then return 0;
    if n==0 then (
	    return 1;
	    ) 
	else (
	    vertices(P);
	    return latticeVolume(P);
	    --return ((dim(P))!)*vP;
	    );
        );    
normalizedVolume (Polyhedron):=(P)->(
    if dim(P)==0 then return 1 else return latticeVolume(P);
        );      
---------------------------------------------------
--Internal Functions
--      
--mu=method(TypicalValue=>ZZ)
--------------------------------------------------
mu =(A,alpha,beta)->(
    dBeta:=beta#"dim";
    dAlpha:=alpha#"dim";
    r:=dAlpha-dBeta;
    d:=numRows(A);
    n:=numColumns(A);
    vbeta:=beta#"verticesMat";
    mbeta:=beta#"verticesList";
    valpha:=alpha#"verticesMat";
    malpha:=alpha#"verticesList";
    Atemp:=0;
    Asort3:=beta#"Aalpha";
    Asort2:=toList(set(alpha#"Aalpha")-set(beta#"Aalpha"));
    Asort1:=toList(set(entries transpose(A))-set(alpha#"Aalpha")); 
    M:=transpose(matrix(join(Asort1,Asort2,Asort3)));
    if numColumns(A)!=numColumns(M) then (
	print "There seems to be an error somewhere";
	);
    W:=(M);
    Anew:=transpose hermite(transpose(W));
    cs:=n-(#Asort2+#Asort3);
    ce:=n-#Asort3-1;
    rowInds:={};
    inc:=0;
    dind:=d-1;
    for w from 0 to dind do(
	if flatten(entries((Anew^{dind-w})_(toList((ce+1)..(n-1)))))==toList((#Asort3):0) then(
	    inc=inc+1;
	    rowInds=append(rowInds,dind-w);
	    );
	if inc==r then break;    
	);
    C:=(Anew_{cs..ce})^rowInds;
    if rank(C)==0 then return 1;
    big:=convexHull((transpose(matrix{toList(numRows(C):0)})|C));
    C1:=transpose matrix delete(toList(numRows(C):0), entries transpose C);
    little:=convexHull(C1);
    vol1:=normalizedVolume(big,r);
    vol2:=normalizedVolume(little,r);
    vol:=(vol1-vol2);
    if numRows(C)!=r then(
	print "Something may be wrong...we seem to have picked a C matrix with the wrong number of rows";
    );
    return vol;
    );
beginDocumentation()
multidoc ///

Node 
     Key
     	  ToricInvariants
     Headline
     	  Given a projective toric variety, the package computes the degree and codimension of the dual, the Euclidean distance degree, polar degrees, and Chern-Mather class 
     Description
     	  Text
	      Given a projective toric variety X_A defined by a full rank integer matrix A with the vector (1,1,...,1) in its row space, the package computes the degree and codimension of the dual (i.e. the A-discriminant variety), the Euclidean distance degree of X_A, the polar degrees of X_A, and the Chern-Mather class of X_A.
	      Note that we do not require that X_A is normal. This package uses the algorithms described in [1] and [2]. For definitions of the objects computed by the package see [1,2].
	      
	      References: \break
	      [1] Martin Helmer and Bernd Sturmfels. "Nearest points on toric varieties." Mathematica Scandinavica 122, no. 2 (2018): 213-238. Arxiv version: https://arxiv.org/abs/1603.06544.\break
	      [2] Martin Helmer and Bernt Ivar Utstol Nodland. "Polar degrees and closest points in codimension two." Journal of Algebra and Its Applications (2017): 1950095. Arxiv version: https://arxiv.org/abs/1711.02381. 
Node 
    Key
    	polarDegrees
	(polarDegrees,Matrix)
    Headline
    	Computes the polar degrees of a projective toric variety 
    Usage
    	polarDegrees(A)
    Inputs
    	ForceAmat=>Boolean
	    if A defines a codimension two toric variety a faster method will be used by default, setting this to true forces the general purpose method
	Output=>List
	    this can be set to HashTable to return a HashTable with all computed values
	A:Matrix
 	    a full rank integer matrix with the vector (1,1,...,1) in its row space defining a projective toric variety X_A
    Outputs
        pd:List
	    the polar degrees of the projective toric variety X_A.
    Description 
    	Text
	    This function computes the polar degrees of the projective toric variety X_A, we do not assume that X_A is normal. The default output is a list of polar degrees; other values of interest computed by the program are also output. To suppress text output use the option Output =>HashTable. 
    	Example
	    A=matrix{{0, 0, 0, 1, 1,5}, {7,0, 1, 3, 0, -2},{1,1, 1, 1, 1, 1}}
	    polarDegrees(A)
	    A=matrix{{3, 0, 0, 1, 1,2},{3,5,0,2,1,3},{0, 1, 2, 0, 2,0},{1, 1, 1, 1, 1,1}}
	    pdh=polarDegrees(A,Output=>HashTable);
	    pdh#"polar degrees"
	    pdh#"dual degree"
	    pdh#"dual codim"
	    pdh#"ED"
	    pdh#"degree"
Node 
    Key
    	dualDegCodim
	(dualDegCodim,Matrix)
    Headline
    	Computes the degree and codimension of the dual to a projective toric variety 
    Usage
    	dualDegCodim(A)
    Inputs
    	ForceAmat=>Boolean
	    if A defines a codimension two toric variety a faster method will be used by default, setting this to true forces the general purpose method
	A:Matrix
 	    a full rank integer matrix with the vector (1,1,...,1) in its row space defining a projective toric variety X_A
    Outputs
        degCodim:HashTable
	    the polar degrees of the projective toric variety X_A.
    Description 
    	Text
	    This function computes the degree and codimension of the projective toric variety X_A, we do not assume that X_A is normal. This function uses @TO polarDegrees@ internally and this information can also be obtained from the @TO polarDegrees@ function. 
    	Example
	    A=matrix{{0, 0, 0, 1, 1,5},{7,0, 1, 3, 0, -2},{1,1, 1, 1, 1, 1}}
	    dc=dualDegCodim(A)
	    dc#"dualCodim"
	    dc#"dualDegree"
	    pd=polarDegrees(A);
Node 
    Key
    	cmClass
	(cmClass,Matrix)
    Headline
    	Computes the Chern-Mather class of a projective toric variety 
    Usage
    	cmClass(A)
    Inputs
    	ForceAmat=>Boolean
	    if A defines a codimension two toric variety a faster method will be used by default, setting this to true forces the general purpose method
	Output=>List
	    this can be set to HashTable to return a HashTable with all computed values
	A:Matrix
 	    a full rank integer matrix with the vector (1,1,...,1) in its row space defining a projective toric variety X_A
    Outputs
        cm:RingElement
	    the Chern-Mather class of the projective toric variety X_A pushedforward to the Chow ring of the ambient projective space.
    Description 
    	Text
	    This function computes the Chern-Mather class of the projective toric variety X_A pushedforward to the Chow ring of the ambient projective space, we do not assume that X_A is normal. 
    	Example
	    A=matrix{{0, 0, 0, 1, 1,5},{7,0, 1, 3, 0, -2},{1,1, 1, 1, 1, 1}}
	    cmClass(A)
	    A=matrix{{3, 0, 0, 1, 1,2}, {3,5,0,2,1,3},{0, 1, 2, 0, 2,0},{1, 1, 1, 1, 1,1}}
	    cmh=cmClass(A,Output=>HashTable);
	    cmh#"CM class"
	    cmh#"polar degrees"
	    cmh#"dual degree"
	    cmh#"dual codim"
	    cmh#"ED"
	    cmh#"degree"	    	     	        
Node 
    Key
    	cmVolumes
	(cmVolumes,Matrix)
    Headline
    	Computes the Chern-Mather volumes of a projective toric variety 
    Usage
    	cmVolumes(A)
    Inputs
    	ForceAmat=>Boolean
	    if A defines a codimension two toric variety a faster method will be used by default, setting this to true forces the general purpose method
	Output=>List
	    this can be set to HashTable to return a HashTable with all computed values
	A:Matrix
 	    a full rank integer matrix with the vector (1,1,...,1) in its row space defining a projective toric variety X_A
    Outputs
        cmv:List
	    the Chern-Mather volumes of the projective toric variety X_A, ordered from dimension X_A to dimension zero.
    Description 
    	Text
	    This function computes the Chern-Mather volumes of the projective toric variety X_A, these are the coefficients of the Chern-Mather class ordered from dimension X_A to dimension zero. We do not assume that X_A is normal. 
    	Example
	    A=matrix{{0, 0, 0, 1, 1,5}, {7,0, 1, 3, 0, -2},{1,1, 1, 1, 1, 1}}
	    cmVolumes(A)
	    A=matrix{{3, 0, 0, 1, 1,2}, {3,5,0,2,1,3},{0, 1, 2, 0, 2,0},{1, 1, 1, 1, 1,1}}
	    cm=cmVolumes(A)
Node 
    Key
    	edDeg
	(edDeg,Matrix)
    Headline
    	Computes the (generic) Euclidean distance degree of a projective toric variety 
    Usage
    	edDeg(A)
    Inputs
    	ForceAmat=>Boolean
	    if A defines a codimension two toric variety a faster method will be used by default, setting this to true forces the general purpose method
	Output=>List
	    this can be set to HashTable to return a HashTable with all computed values
	A:Matrix
 	    a full rank integer matrix with the vector (1,1,...,1) in its row space defining a projective toric variety X_A
    Outputs
        ED:ZZ
	    the (generic) Euclidean distance degree of the projective toric variety X_A.
    Description 
    	Text
	    This function computes (generic) Euclidean distance degree the projective toric variety X_A, we do not assume that X_A is normal. The default output is a list of polar degrees; other values of interest computed by the program are also output. To suppress text output use the option Output =>HashTable. This function uses @TO polarDegrees@ internally.
    	Example
	    A=matrix{{0, 0, 0, 1, 1,5}, {7,0, 1, 3, 0, -2},{1,1, 1, 1, 1, 1}}
	    edDeg(A)
	    A=matrix{{3, 0, 0, 1, 1,2}, {3,5,0,2,1,3},{0, 1, 2, 0, 2,0},{1, 1, 1, 1, 1,1}}
	    time edDeg(A)
	    time edDeg(A,ForceAmat=>true)
///



TEST ///
-*  
    restart
    installPackage "ToricInvariants"
    needsPackage "ToricInvariants"
*-  
   A=matrix{{3, 5, 2, 1, 1,2},{3,-2,0,2,-1,3},{0, 1, 2, 4, 2,0},{1, 1, 1, 1, 1,1}};
   pd1=polarDegrees(A);
   assert(pd1=={121, 278, 236, 74});
   pd2= polarDegrees(A,ForceAmat=>true);
   assert(pd1==pd2);
   assert(edDeg(A)==709);
   assert(cmVolumes(A)=={74, 60, 14, 23}); 
   dc=dualDegCodim A;
   assert(dc#"dualCodim"==1);
   assert(dc#"dualDegree"==121);  
///



TEST ///
-*  
    restart
    needsPackage "ToricInvariants"
*-  
   A=matrix{{3, 4, 3, 13, 1,2},{7, 1, 2, -4, -2,10},{1, 1, 1, 1, 1,1}};
   pd1=polarDegrees(A);
   assert(pd1=={226, 362, 146});
   assert(edDeg(A)==734);
   dc=dualDegCodim A;
   assert(dc#"dualCodim"==1);
   assert(dc#"dualDegree"==226);  
///

TEST ///
-*  
    restart
    needsPackage "ToricInvariants"
*-     
   A=matrix{{0, -4,5, 3, 3, -1,2},{4, 1, 4, -4, -9,7,11},{1,1, 1, 1, 1, 1,1}};
   pdh=polarDegrees(A,Output=>HashTable);
   assert(pdh#"polar degrees"=={363, 477, 189});
   assert(pdh#"ED"==1029);
///

TEST ///
-*  
    restart
    needsPackage "ToricInvariants"
*-  
   A=matrix{{3, 0, 0, 1, 1,2,1,2},{3,5,0,2,1,3,12,11},{5, 1, 9, 10, 12,3,7,9},{3, 1, 2, 19, 7,1,1,2},{0, 1, 2, 0, 2,0,5,7},{1, 1, 1, 1, 1,1,1,1}};
   time pd=polarDegrees(A,Output=>HashTable);
   assert(pd#"polar degrees"=={30840, 119341, 202791, 183622, 87616, 16924});
   assert(pd#"ED"==641134);
   A=transpose(matrix{{0,0,1},{0,7,1},{3,0,1},{5,0,1}})
   ed=edDeg(A,ForceAmat=>true);
   assert(ed==10);
///

