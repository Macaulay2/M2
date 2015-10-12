if version#"VERSION" == "1.8.2.1" then needsPackage "SLPexpressions"
parametricSchubertProblem = method()
p := symbol p;
parametricSchubertProblem (List,ZZ,ZZ) := (SchPblm,k,n) -> (
    twoconds := take(SchPblm,2);
    c1 := first first twoconds;
    c2 := first last twoconds;
    remaining'conditions'flags := drop(SchPblm,2);   
    (PX,P) := skewSchubertVariety((k,n),c1,c2,Inputs=>symbol p);
    (P,makeSquareSystem(transpose PX,remaining'conditions'flags))
    )
