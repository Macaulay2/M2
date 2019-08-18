--		Copyright 1998 by Daniel R. Grayson

end -- disable this test

Descent = new Type of MutableHashTable
stars = n -> (
     d := depth n + height n;
     if d == 0 then n else (stack ( d : " " )) ^ (height n - 1) | n
     )
net Descent := x -> stars stack apply(values x.index, k -> "  " | k || net x#k )
new Descent := Descent -> (
     r := new MutableHashTable;
     r.index = new MutableHashTable;
     r)
enter = method()
enter(Descent, String) := (r,s) -> (
     x := new Descent;
     x.parent = r;
     r.index#(#r.index) = s;
     r#s = x;
     x)

currentDescent = world = new Descent
reachable = new MutableHashTable

reach1 = method(Dispatch => Thing)
reach2 = method(Dispatch => Thing)
reach3 = method(Dispatch => Thing)

reach1 Thing := identity
reach1 Sequence :=
reach1 Hypertext := x -> scan(x,reach1)
reach1 TO := identity
reach1 TOH := identity
reach1 UL := x -> scan(x,reach2)
-- reach1 NOCONTENTS := x -> scan(x,reach3)

reach3 Thing := reach
reach3 UL := x -> scan(x,reach1)

reach2 Thing := reach
reach2 TO  := 
reach2 TOH := x -> (
     s := toString x#0;
     if not reachable#?s or not reachable#s then (
	  reachable#s = true;
	  currentDescent = enter(currentDescent, formatDocumentTag s);
	  reach1 documentation s;
	  currentDescent = currentDescent.parent;
	  ))

DocumentationProvided = set apply(topicList(), toString)
scan(keys DocumentationProvided, s -> reachable#s = false)
reach2 TO "Macaulay2"

o = "docStructure.out" << world << endl
-- unreachable = applyPairs(new HashTable from reachable, (k,v) -> if not v then (k,true))
-- scan(
--      sort keys unreachable, 
--      s -> o << "documentation for '" << s << "' not reachable through main menus" << endl
--      )
o << close
<< "Documentation structure written to file " << o << endl
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test docStructure.out"
-- End:
