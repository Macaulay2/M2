--		Copyright 1998 by Daniel R. Grayson

Descent = new Type of MutableHashTable
world = new Descent
stars = n -> (
     d := depth n + height n;
     if d == 0 then n else (verticalJoin ( d : " " )) ^ (height n - 1) | n
     )
net Descent := x -> stars verticalJoin sort apply( pairs x, (k,v) -> " " | k || net v )

reachable = new MutableHashTable

reach1 := method(SingleArgumentDispatch=>true)
reach2 := method(SingleArgumentDispatch=>true)

reach1 Thing := identity
reach1 Sequence :=
reach1 HtmlList := x -> scan(x,reach1)
reach1 TO := identity
reach1 MENU := reach2

reach2 Thing := identity
reach2 Sequence :=
reach2 HtmlList := x -> scan(x,reach2)
reach2 SHIELD := x -> null
reach2 TO := x -> (
     s := getDocumentationTag x#0;
     if not reachable#?s then (
	  reachable#s = true;
	  t := currentDescent;
	  currentDescent = currentDescent#(formatDocumentTag s) = new Descent;
	  reach1 doc s;
	  currentDescent = t;
	  ))

reachable#(getDocumentationTag "Macaulay 2") = true

topName = "Macaulay 2"
currentDescent = world#topName = new Descent
reach1 doc topName

fn = "docStructure.out"
(
     fn
     -- << "-- -*- mode: Outline; truncate-lines: 1; -*- --" << endl
     << world << endl
     << close
     )
<< "Documentation structure written to file " << fn << endl


