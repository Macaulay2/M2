--		Copyright 1994 by Daniel R. Grayson

accumulate = method()
accumulate(Function,Thing,List) := (f,x,v) -> apply(v, y -> x = f(x,y))
accumulate(List,Thing,Function) := (v,x,f) -> reverse apply(reverse v, w -> x = f(w,x))
accumulate(Function,List) := (f,v) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(f,v#0,drop(v,1)))
accumulate(List,Function) := (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     accumulate(drop(v,-1),v#-1,f))     

document { quote accumulate,
     TT "accumulate(f,x0,{x1,...,xn})", " -- computes the list 
     ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     BR,NOINDENT,
     TT "accumulate({xn,...,x1},x0,f)", " -- computes the list 
     ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     BR,NOINDENT,
     TT "accumulate(f,{x0,x1,...,xn})", " -- computes the list 
     ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     BR,NOINDENT,
     TT "accumulate({xn,...,x1,x0},f)", " -- computes the list 
     ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     PARA,
     EXAMPLE {
	  "accumulate(plus,1,{10,100,1000})",
	  "accumulate(toList,{a,b,c,d})",
	  },
     SEEALSO {"fold"}
     }

TEST ///
     assert( accumulate(toList,a,{b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c},d,toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
     assert( accumulate(toList,{a,b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c,d},toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
///     

fold = method()
fold(Function,Thing,List) := (f,x,v) -> (scan(v, y -> x = f(x,y)); x)
fold(List,Thing,Function) := (v,x,f) -> (scan(reverse v, w -> x = f(w,x)); x)
fold(Function,List) := (f,v) -> (
     if # v === 0 then error "expected a nonempty list";
     fold(f,v#0,drop(v,1)))
fold(List,Function) := (v,f) -> (
     if #v === 0 then error "expected a nonempty list";
     fold(drop(v,-1),v#-1,f))     

document { quote fold,
     TT "fold(f,x0,{x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     BR,NOINDENT,
     TT "fold({xn,...,x1},x0,f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     BR,NOINDENT,
     TT "fold(f,{x0,x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     BR,NOINDENT,
     TT "fold({xn,...,x1,x0},f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     EXAMPLE {
	  "fold(toList, {a,b,c,d,e})"
	  },
     SEEALSO {"accumulate"}
     }

TEST ///
     assert( fold(toList, a, {b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c}, d, toList) === {a, {b, {c, d}}} )
     assert( fold(toList, {a,b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c,d}, toList) === {a, {b, {c, d}}} )
///

demark = (s,v) -> concatenate between(s,v)
document { quote demark,
     TT "demark(s,x)", " -- given a list of strings ", TT "x", " and
     a string ", TT "s", " provides the string obtained by concatenating
     the elements of ", TT "x", " with a copy of ", TT "x", " inserted
     between each successive pair.",
     PARA,
     EXAMPLE "demark(\"+\",{\"a\",\"b\",\"c\"})"
     }
