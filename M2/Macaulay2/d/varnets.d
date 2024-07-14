--		Copyright 1994-2006 by Daniel R. Grayson
use nets;
-- varnets:
-- a varnet is a net whose strings are varstrings
-- you can make a new empty varnet
-- you can convert a varnet to a net
-- you can push nets, strings, or characters onto the end of a varnet
-- pushing N spaces is quick - our routine accepts an integer for that purpose

export varnet := {
     height:int,			  -- number of strings above the baseline
     width:int,				  -- width of body (strings may be shorter, and must be not longer)
     body:array(varstring)		  -- one string for each row, read-only
     };
export hash(v:varnet):int := (
     h := 33 * v.height + 47 * v.width;
     foreach s in v.body do h = 51 * h + hash(s);
     h & 0x7fffffff );
export newvarnet():varnet := varnet(1,0,array(varstring)(newvarstring(10)));
export toNet(x:varnet):Net := Net(x.height,x.width,new array(string) len length(x.body) do foreach s in x.body do provide tostring(s));
accommodate(x:varnet,height:int,bodylen:int):void := (
     a := if height > x.height then height - x.height else 0;
     xdepth := length(x.body) - x.height;
     depth := bodylen - height;
     b := if depth > xdepth then depth - xdepth else 0;
     if a > 0 || b > 0 then (
	  x.height = x.height + a;
	  x.body = new array(varstring) len a + length(x.body) + b do (
	       for a do provide newvarstring(3);
	       foreach s in x.body do provide s;
	       for b do provide newvarstring(3)));
     x);
export (x:varnet) << (s:string) : varnet := (
     accommodate(x,1,1);
     t := x.body.(x.height-1);
     t.width = x.width;
     t << s;
     x.width = t.width;
     x);
export (x:varnet) << (y:Net) : varnet := (
     accommodate(x,y.height,length(y.body));
     a := x.height - y.height;
     foreach s at i in y.body do (
	  t := x.body.(i+a);
	  t.width = x.width;
	  t << s);
     x.width = x.width + y.width;
     x);
export (x:varnet) << (n:int) : varnet := (x.width = x.width + n; x);

-- vaNet

export vaNet := { nets:array(Net), size:int };
export toarray(v:vaNet):array(Net) := new array(Net) len v.size do foreach n in v.nets do provide n;
export newvaNet(i:int):vaNet := vaNet( new array(Net) len i do provide Net(0,0,array(string)()), 0);
export newvaNet():vaNet := newvaNet(0);
export (v:vaNet) << (x:Net) : vaNet := (
     if length(v.nets) == v.size then v.nets = new array(Net) len 2*length(v.nets) + 3 do (
	       foreach n in v.nets do provide n;
	       provide x;
	       while true do provide dummyNet)
     else v.nets.(v.size) = x;
     v.size = v.size + 1;
     v);
export hash(v:vaNet):hash_t := (
     h := hash_t(323);
     for i from 0 to v.size-1 do h = 41 * h + hash(v.nets.i);
     h & 0x7fffffff);

-- netfile

export NetFile := {+ v:vaNet, x:varnet };
export hash(n:NetFile):hash_t := 0x7fffffff & (hash(n.v) + 43 * hash(n.x));
export newNetFile():NetFile := NetFile(newvaNet(),newvarnet());
export (n:NetFile) << (i:int) : NetFile := ( n.x << i; n );
export (n:NetFile) << (s:string) : NetFile := ( n.x << s; n );
export (n:NetFile) << (y:Net) : NetFile := ( n.x << y; n );
export endlnetfile(n:NetFile):NetFile := ( n.v << toNet(n.x); n.x = newvarnet(); n);
export popnet(n:NetFile):Net := ( r := toNet(n.x); n.x = newvarnet(); r);
export tonets(n:NetFile):array(Net) := toarray(n.v);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d varnets.o "
-- End:
