-- bare mode

applyMethod := (m,x) -> if x === null then x else (
     method := lookup(m,class x);
     if method === null then x else method x
     )

Print Thing := x -> (
     x = applyMethod(AfterEval,x);
     y := applyMethod(BeforePrint,x);
     << name y << flush;
     )

BeforePrint Net := BeforePrint String := identity

NoPrint Thing := x -> ()
