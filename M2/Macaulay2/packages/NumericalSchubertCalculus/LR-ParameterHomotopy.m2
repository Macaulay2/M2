if version#"VERSION" == "1.8.2.1" then needsPackage "SLPexpressions"
parametricSchubertProblem = method()
p := symbol p;
x := symbol x; 
parametricSchubertProblem (List,ZZ,ZZ) := (conds,k,n) -> (
    twoconds := take(conds,2);
    c1 := first twoconds;
    c2 := last twoconds;
    all'but2 := drop(conds,2);
    P := {};
    remaining'conditions'flags := apply(#all'but2, nc->( 
	    c := conds#nc;
	    (c, matrix table(n,n,(i,j)->(
		    pInput := inputGate (symbol p)_("f"|toString nc,i,j);
		    P = P | {pInput};
		    pInput
		    )))
	));   
    (PX,X) := skewSchubertVariety((k,n),c1,c2,Inputs=>symbol x);
    (X,P,makeSquareSystem(transpose PX,remaining'conditions'flags))
    )
