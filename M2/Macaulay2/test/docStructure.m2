--		Copyright 1998 by Daniel R. Grayson

Descent = new Type of MutableHashTable
stars = n -> (
     d := depth n + height n;
     if d == 0 then n else (verticalJoin ( d : " " )) ^ (height n - 1) | n
     )
net Descent := x -> stars verticalJoin apply(values x.index, k -> "  " | k || net x#k )
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

reach1 = method(SingleArgumentDispatch=>true)
reach2 = method(SingleArgumentDispatch=>true)

reach1 Thing := identity
reach1 Sequence :=
reach1 MarkUpList := x -> scan(x,reach1)
reach1 TO := identity
reach1 MENU := reach2

reach2 Thing := identity
reach2 Sequence :=
reach2 MarkUpList := x -> scan(x,reach2)
reach2 SHIELD := x -> null
reach2 TO := x -> (
     s := getDocumentationTag x#0;
     if not reachable#?s or not reachable#s then (
	  reachable#s = true;
	  currentDescent = enter(currentDescent, formatDocumentTag s);
	  reach1 doc s;
	  currentDescent = currentDescent.parent;
	  ))

DocumentationProvided = set apply(topicList(), getDocumentationTag)
scan(keys DocumentationProvided, s -> reachable#s = false)
reach2 TO "Macaulay 2"

fn = "docStructure.out"
o = openOut fn
o << world << endl

unreachable = applyPairs(new HashTable from reachable, (k,v) -> if not v then (k,true))
scan(sort keys unreachable, s -> (
	  o << "documentation for '" << s << "' not reachable through main menus" << endl
	  )
     )

o << close

<< "Documentation structure written to file " << fn << endl


