--		Copyright 1994 by Daniel R. Grayson

fold = (v,f) -> (
     if # v === 0 then error "expected a nonempty list";
     x := v#0;
     scan(drop(v,1),y->(x=f(x,y)));
     x)

document { quote fold,
     TT "fold({a,b,c,d},f)", " -- yields f(f(f(a,b),c),d)"
     }

demark = (s,v) -> concatenate between(s,v)
document { quote demark,
     TT "demark(s,x)", " -- given a list of strings ", TT "x", " and
     a string ", TT "s", " provides the string obtained by concatenating
     the elements of ", TT "x", " with a copy of ", TT "x", " inserted
     between each successive pair.",
     PARA,
     EXAMPLE "demark(\"+\",{\"a\",\"b\",\"c\"})"
     }
