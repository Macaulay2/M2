
sign := new HashTable from { "+" => 1 , "-" => -1 }
alpha := set characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
digit := set characters "0123456789"

macaulayNumber = method()
macaulayNumber String := u -> if u === "" then 1 else if match("[[:digit:]]*",u) then value u else error("non-digit in number ",u)
macaulayNumber List := u -> macaulayNumber concatenate u

macaulayRational = method()
macaulayRational String := u -> if not match("/",u) then macaulayNumber u else (
     p := separate_"/" u;
     if #p != 2 then error "too many slashes in number";
     macaulayNumber p#0 / macaulayNumber p#1)     

macaulayMonomial = method()
macaulayMonomial String := macaulayMonomial @@ characters
macaulayMonomial List := t -> (w -> w#0 * (product \\ power @@ toSequence 
	  \ pack_2 drop(w,1))) if #t === 0 then {0} else flatten sublists( t, c -> alpha#?c, value, macaulayRational)

macaulayPoly = method()
macaulayPoly String := macaulayPoly @@ characters
macaulayPoly List := s -> sum \\ times @@ toSequence \ pack_2 prepend_1 sublists( s, c -> sign#?c, c -> sign#c, macaulayMonomial)

macaulayList = row -> apply(separate_"," row, macaulayPoly)
macaulayTable = t -> apply(separate_";" t, row -> apply(separate_"," row, macaulayPoly))

ideal String := (s) -> ideal macaulayList s
matrix String := o -> (s) -> matrix macaulayTable s

end

debug Macaulay2Core
S = QQ[x,y,z,t]

macaulayNumber "234"
macaulayNumber "11"
macaulayNumber ""

macaulayMonomial "x3y4zt"
macaulayMonomial "322xyz"
macaulayMonomial "xy"
macaulayPoly "x3y4zt+322xyz-xy+300y2zt11"
macaulayPoly "-x3y4zt+322xyz-xy+300y2zt11"

macaulayList "x,y,z,y"
macaulayList "-x3y4zt+322xyz-xy+300y2zt11,x,y2z3,z"

matrix macaulayTable "-x3y4zt+322xyz-xy+300y2zt11,x,y2z3;x,y,z;yz,xz,xy"
matrix macaulayTable "x,y,z;yz,xz,xy"
matrix macaulayTable "x;y;z"

ideal "x2y4-32/3z223t,xy"
matrix "x,y2,z3-3,4;x,y,,"
matrix "x"
matrix "x;y2"
matrix ""
ideal ""

to do:
	      parenthesized subexpressions with optional exponents and multiplication etc
	      allow white space, which is ignored : includes space and newline and tab at least
	      rational coefficients like 2/3
	      bracketed subscripts, each of which is a comma separated list of natural numbers

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
