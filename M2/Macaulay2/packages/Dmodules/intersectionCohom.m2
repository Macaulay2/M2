-- Copyright 2020 by Andras C. Lorincz

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Computes the intersection cohomology D-module corresponding to an irreducible affine subvariety of the affine space. 
-- Special algorithm to be added for the case when ideal is a complete intersection via Barlet-Kashiwara.
-- Computes the intersection cohomology groups of an irreducible affine variety.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

ICmodule = method( Options => {Strategy => (Walther,null,OTW)})  -- Strat 1,2 for LocalCohom, Strat 3 for the localization after
ICmodule(Ideal) := options -> I -> (
    R := ring I;
    if class R =!= PolynomialRing
    then error "expected an ideal in a polynomial ring";
    primes := minimalPrimes I;
    if #primes > 1 then error "The variety defined by the ideal must be irreducible";
    P := primes#0;
    c:=codim I; --to add here CI case
    	S := flatten entries mingens(ideal singularLocus P);
    	f:=1_R;
    	for i from 0 to #S-1 do (
	    if ((S_i % P) != 0) then (
	    	f=S_i; 
	    	break;
	    	);
	    ); --How to choose smallest??
    	H := localCohom(c, I, Strategy => options.Strategy#0, LocStrategy => options.Strategy#1);
    	if (first degree f) == 0 then H   --when smooth
    	else (      	    	    	  --general case
    	    G := Ddual H;
    	    f = sub(f,ring G);
    	    T := DlocalizeAll(G,f, Strategy => options.Strategy#2);
    	    image T#LocMap
       	    )    		 
    );

ICcohom = method( Options => {Strategy => (Walther,null,OTW,Schreyer)})
ICcohom(Ideal) := options -> I -> (
    IC:=minimalPresentation ICmodule(I, Strategy => drop(options.Strategy,-1));
    n:=numgens (ring I);
    d:=dim I;
    w:=toList(n:1);
    integrationTable := Dintegration(IC,w, Strategy => options.Strategy#3);
    new HashTable from (for i from 0 to d list i=>integrationTable#(d-i)) 
    )

ICcohom(ZZ,Ideal) := options -> (k,I) -> (
    R:= ring I;
    if class R =!= PolynomialRing
    then error "expected an ideal in a polynomial ring";
    d:=dim I;
    if k>d then 0
    else (
    	IC:=minimalPresentation ICmodule(I, Strategy => drop(options.Strategy,-1));
    	n:=numgens R;
    	w:=toList(n:1);
    	Dintegration(d-k,IC,w, Strategy => options.Strategy#3)
      	)
    )