this code could be installed in functions like gb that have lots of optional arguments to provide a hierarchy of optional arguments

o = x -> if not instance(x,OptionTable) then new OptionTable from x else x
g := (def,opts) -> (
     opts = first override(def,opts);
     if opts === def then opts else merge(def,opts,(i,j) -> if i =!= j and instance(i,OptionTable) then g(i,o j) else j));
def = o { a => o { b => 3, c => o { d => 44 }}}
f = method(Options => def)
f ZZ := opts -> arg -> (
     opts = g(def,opts);
     (opts, arg))
f (a => o { b => 55} , 5)
f (a => o { c => { d => 66}} , 5)

