--		Copyright 1997 by Daniel R. Grayson

AbstractSet = new Type of Type
document { quote AbstractSet,
     TT "AbstractSet", " -- the class of all abstract sets.",
     PARA,
     "An abstract set ", TT "X", " is a ", TO "class", " which makes no demands on
     its instances.  Anything whose class is ", TT "X", " is to be regarded as
     an element of the corresponding set.",
     PARA,
     "There are two types of sets: those in the class ", TO "Set", ", each of
     which is finite and incorporates an enumeration of its elements;
     and those in the class ", TT "AbstractSet", ", each of which may 
     be infinite, and does not incorporate an enumeration of its elements.",
     PARA,
     "Abstract free modules may be based on abstracts sets.",
     SEEALSO "AbstractFreeModule"
     }

AbstractFreeModule = new Type of BasicModule
document { quote AbstractFreeModule,
     TT "AbstractFreeModule", " -- the class of all abstract free modules.",
     PARA,
     "An abstract free module is a free module based on an abstract set.
     Given a ring ", TT "R", " and an abstract set ", TT "X", ", the
     abstract free module is formed by typing ", TT "R X", ".",
     EXAMPLE "X = new AbstractSet of SelfNamer",
     EXAMPLE "x = new X",
     EXAMPLE "y = new X",
     EXAMPLE "M = ZZ X",
     EXAMPLE "m = 2*x - y",
     SEEALSO ("AbstractSet", "SelfName", "AbstractFreeModuleElement",
	  "AbstractModuleMap", "MixedModuleMap")
     }

AbstractFreeModuleElement = new Type of HashTable
document { quote AbstractFreeModuleElement,
     TT "AbstractFreeModuleElement", " -- the class of all elements of
     abstract free modules."
     }

clean := m -> select(m, r -> r != 0)

use AbstractFreeModule := M -> (
     if not M.?use then error("no 'use' method for ", name M);
     M.use M; 
     M)

Ring AbstractSet := (R,X) -> (
     M := new AbstractFreeModule of AbstractFreeModuleElement;
     M.ring = R;
     M.set = X;
     M#0 = new M;
     one := 1_R;
     expression M := (m) -> sum(pairs m, (x,r) -> expression r * expression x);
     net M := m -> net expression m;
     name M := m -> name expression m;
     M + M := (m,n) -> clean merge(m,n,plus);
       - M :=   (n) -> clean apply(n,s -> -s);
     M - M := (m,n) -> m + -n;
     R * M := (r,m) -> clean apply(m,s -> r*s);
     X + M := (x,m) -> one*x + m;
     M + X := (m,x) -> m + one*x;
     X - M := (x,m) -> one*x - m;
     M - X := (m,x) -> m - one*x;
     R * X := (r,x) -> new M from { x => r };
     M.use = M -> (
	    - X :=   (y) ->       - one*y;
	  X + X := (x,y) -> one*x + one*y;
	  X - X := (x,y) -> one*x - one*y;
	  scan(R.baseRings, A -> (
		    A * X := (i,x) -> (i * one) * x;
		    )
	       );
	  );
     M
     )

AbstractModuleMap = new Type of ModuleMap
document { quote AbstractModuleMap,
     TT "AbstractModuleMap", " -- the class of all maps between abstract free
     modules.",
     PARA,
     "Such a map ", TT "f", " is defined either by a table ", TT "f.table", "
     or by a function ", TT "f.function", ", either of which is to be used to
     take a basis element of the source abstract free module and produce
     an element of the target abstract free module.",
     SEEALSO ("function", "table")
     }
document { quote function,
     TT "function", " -- a key used in instances of the
     class ", TO "AbstractModuleMap", "."
     }
AbstractModuleMap AbstractFreeModuleElement := (f,m) -> (
     -- this assumes the source and target are abstract modules
     if source f =!= class m then error "module element not in source of map";
     if f.?table then (
	  F := f.table;
	  sum(pairs m, (x,r) -> r * F#x))
     else if f.?function then (
	  G := f.function;
	  sum(pairs m, (x,r) -> r * G x))
     else error "no table or function in abstract module map"
     )
AbstractModuleMap * AbstractModuleMap := (f,g) -> (
     if source f != target g then error "maps not composable";
     h := new AbstractModuleMap;
     h.ring = ring source f;
     h.source = source g;
     h.target = target f;
     h.function = (
	  if g.?table then (
	       gt := g.table;
	       x -> f gt#x
	       )
	  else if g.?function then (
	       gfn := g.function;
	       x -> f gfn x
	       )
	  else error "no table or function in abstract module map"
	  );
     h
     )
     
MixedModuleMap = new Type of ModuleMap
document { quote MixedModuleMap,
     TT "MixedModuleMap", " -- the class of all maps from a module to an
     abstract free module.",
     PARA,
     SEEALSO "AbstractFreeModule"
     }

