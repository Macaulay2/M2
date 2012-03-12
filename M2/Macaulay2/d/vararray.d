--		Copyright 1994 by Daniel R. Grayson

use arithmetic;

export vararrayint := {
     ints:array(int),
     size:int
     };
export newvararrayint(i:int):vararrayint := vararrayint(
     new array(int) len i do provide 0, 
     0);
needatleast(i:int,v:vararrayint):void := (
     if length(v.ints) < i then (
     	  v.ints = new array(int) len 2*i do (
	       foreach c in v.ints do provide c;
	       while true do provide 0
	       );
     	  );
     );
export (v:vararrayint) << (c:int) : vararrayint := (
     needatleast(v.size + 1,v);
     v.ints.(v.size) = c;
     v.size = v.size + 1;
     v
     );
export (v:vararrayint) << (s:array(int)):vararrayint := (foreach c in s do v << c;v);
export toarrayint(v:vararrayint):array(int) := (
     new array(int) len v.size do foreach c in v.ints do provide c
     );
export toreversearrayint(v:vararrayint):array(int) := (
     new array(int) len v.size do for i from v.size-1 to 0 by -1 do provide v.ints.i
     );
export empty(v:vararrayint):void := v.size = 0;
export takearrayint(v:vararrayint):array(int) := (
     s := toarrayint(v);
     empty(v);
     s);
export takereversearrayint(v:vararrayint):array(int) := (
     s := toreversearrayint(v);
     empty(v);
     s);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
-- End:
