--		Copyright 2009 by Daniel R. Grayson

needs "code.m2"
needs "indeterminates.m2"
needs "monoids.m2"
needs "variables.m2"

-- this code should go after the last method installed for baseName
scan(join(apply(methods baseName,last),{MonoidElement}), X -> if X =!= Symbol and X =!= IndexedVariable and X =!= Thing and not ancestor(Expression,X) then (
	  err1 := lookup(symbol .., Thing, Thing);
	  err2 := lookup(symbol ..<, Thing, Thing);
	  X .. X := (a,z) -> (
	       a' := try baseName a else err1(a,z);
	       z' := try baseName z else err1(a,z);
	       if a === a' and z === z' then err1(a,z);
	       r := a' .. z';
	       if value a' =!= a or value z' =!= z then return r;
	       r' := apply(r,value);
	       if same apply(r', class) then r' else r);
	  X ..< X := (a,z) -> (
	       a' := try baseName a else err1(a,z);
	       z' := try baseName z else err1(a,z);
	       if a === a' and z === z' then err1(a,z);
	       r := a' ..< z';
	       if value a' =!= a or value z' =!= z then return r;
	       r' := apply(r,value);
	       if same apply(r', class) then r' else r);
	  X .. Thing := (a,z) -> (
	       a' := try baseName a else err1(a,z);
	       if a === a' then err1(a,z);
	       r := a' .. z;
	       if value a' =!= a then return r;
	       r' := apply(r,value);
	       if same apply(r', class) then r' else r);
	  X ..< Thing := (a,z) -> (
	       a' := try baseName a else err1(a,z);
	       if a === a' then err1(a,z);
	       r := a' ..< z;
	       if value a' =!= a then return r;
	       r' := apply(r,value);
	       if same apply(r', class) then r' else r);
	  Thing .. X := (a,z) -> (
	       z' := try baseName z else err1(a,z);
	       if z === z' then err1(a,z);
	       r := a .. z';
	       if value z' =!= z then return r;
	       r' := apply(r,value);
	       if same apply(r', class) then r' else r);
	  Thing ..< X := (a,z) -> (
	       z' := try baseName z else err1(a,z);
	       if z === z' then err1(a,z);
	       r := a ..< z';
	       if value z' =!= z then return r;
	       r' := apply(r,value);
	       if same apply(r', class) then r' else r);
	  ))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
