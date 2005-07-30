Function \\ VisibleList := VisibleList => (f,v) -> f v
sublists := (x,f,g,h) -> (
     p := positions(toSequence x, f);
     mingle(
	  apply( prepend(-1,p), append(p,#x), (i,j) -> h take(x,{i+1,j-1})),
	  apply( p, i -> g x#i)))
sign := new HashTable from { "+" => 1 , "-" => -1 }
alpha := set characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
digit := set characters "0123456789"

macaulayNumber = method()
macaulayNumber String := macaulayNumber @@ characters
macaulayNumber List := u -> first sublists( u, c -> not digit#?c,
     c -> error("invalid character ",format c),
     v -> if #v==0 then 1 else value concatenate v)
--macaulayNumber "234"
--macaulayNumber "11"
--macaulayNumber ""

macaulayMonomial = method()
macaulayMonomial String := macaulayMonomial @@ characters
macaulayMonomial List := t -> (w -> w#0 * (product \\ power @@ toSequence 
	  \ pack_2 drop(w,1))) if #t === 0 then {0} else flatten sublists( t, c -> alpha#?c, value, macaulayNumber)

macaulayPoly = method()
macaulayPoly String := macaulayPoly @@ characters
macaulayPoly List := s -> sum \\ times @@ toSequence \ pack_2 prepend_1 sublists( s, c -> sign#?c, c -> sign#c, macaulayMonomial)

macaulayList = method()
macaulayList String := macaulayList @@ characters
macaulayList List := s -> (
     inside := false;
     (s -> take(s,{1,#s-2})) splice sublists(s,
	  c -> (
	       if not inside
	       then if c === "{" then inside = true else error("illegal character: ",format c)
	       else if c === "," then true else if c === "}" then (inside = false; true) else false
	       ),
	  c -> (), macaulayPoly))

macaulayTable = method()
macaulayTable String := macaulayTable @@ characters
macaulayTable List := s -> (
     inside := 0;
     (s -> take(s,{1,#s-2})) splice sublists(s,
	  c -> (
	       if inside == 0 then (
		    if c == "{"
		    then (inside = 1; true)
		    else error("illegal character: ",format c)
		    )
	       else if inside == 1 then (
	       	    if c == "," then true else if c == "}" then (inside = 0; true) else if c == "{" then (inside = 2; false) else false
		    )
	       else (
		    if c == "}" then inside = 1;
		    false
		    )
	       ),
	  c -> (), macaulayList))

ideal String := (s) -> ideal macaulayList ("{"|s|"}")
matrix String := o -> (s) -> matrix macaulayTable("{"|s|"}")
end

debug Macaulay2Core
S = ZZ[x,y,z,t]
macaulayMonomial "x3y4zt"
macaulayMonomial "322xyz"
macaulayMonomial "xy"
macaulayPoly "x3y4zt+322xyz-xy+300y2zt11"
macaulayPoly "-x3y4zt+322xyz-xy+300y2zt11"

macaulayList "{x,y,z,y}"
macaulayList "{-x3y4zt+322xyz-xy+300y2zt11,x,y2z3,z}"

matrix macaulayTable "{{-x3y4zt+322xyz-xy+300y2zt11,x,y2z3},{x,y,z},{yz,xz,xy}}"
matrix macaulayTable "{{x,y,z},{yz,xz,xy}}"
matrix macaulayTable "{{x},{y},{z}}"

ideal"x2y4-32z223t,xy"
matrix"{x,y2,z3-3,4},{x,y,z,t}"

matrix"{x,y2,z3-3,4},{x,y,z,t}"

-- WANT matrix"x,y2,z3-3,4;x,y,z,t" to work

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
