--		Copyright 1996 by Daniel R. Grayson

-- nets are 2 dimensional strings of characters

use varstrin;
use ctype;

export Net := {+
     height:int,			  -- number of strings above the baseline
     width:int,				  -- width of body (strings may be shorter)
     body:array(string)			  -- one string for each row, read-only
     };

export toString(n:Net):string := (
     b := n.body;
     if length(b) == 0 then return "";
     m := length(b) - 1;
     foreach s in b do m = m + length(s);
     new string len m do (
	  foreach s in b do (
	       foreach c in s do provide c;
	       provide '\n';
	       )));

anytabs(s:string):bool := (
     foreach c in s do if c == '\t' then return true;
     false);
lengthUntabified(s:string):int := (
     n := 0;
     foreach c in s do n = if c == '\t' then ((n+8)/8)*8 else n+1;
     n);
untabify(s:string):string := (
     if anytabs(s) then (
	  n := 0;
	  new string len lengthUntabified(s) do
	  foreach c in s do if c == '\t' 
	  then (
	       i := ((n+8)/8)*8-n;
	       for i do provide ' ';
	       n = n+i;
	       )
	  else (
	       provide c;
	       n = n+1;
	       )
	  )
     else s);
export lines(s:string):array(string) := (
     nlines := 1;
     i := 0;
     while true do (
	  j := index(s,i);				    -- search for \n
	  if j == -1 then break;
	  i = j + 1;
	  nlines = nlines + 1;	    -- count the bit after the last newline even if it's empty
	  );
     i = 0;
     new array(string) len nlines do (
	  while true do (
	       j := index(s,i);				    -- search for \n
	       if j == -1 then (
		    provide untabify(substr(s,i));
		    break;
		    )
	       else (
		    nexti := j+1;
     	  	    if j > i && s.(j-1) == '\r' then j = j-1;	    -- handle MS-DOS line ending \r\n
		    provide untabify(substr(s,i,j-i));
		    i = nexti;
		    ))));
export toNet(s:string):Net := (
     v := if length(s) > 0 then lines(s) else array(string)(s);
     wid := 0;
     foreach s in v do if wid < utf8width(s) then wid = utf8width(s);
     Net(1,wid,v));
export toNet(c:char):Net := toNet(string(c));
export RaiseNet(n:Net,i:int):Net := Net(n.height+i,n.width,n.body);
export HorizontalJoin(v:array(Net)):Net := (
     if length(v) == 0 then return Net(0,0,array(string)());
     if length(v) == 1 then return v.0;
     width := 0;
     foreach n in v do width = width + n.width;
     height := v . 0 . height;
     for i from 1 to length(v)-1 do (
	  if height < v.i.height then height = v.i.height);
     leng := height - v . 0 . height + length(v . 0 . body);
     for i from 1 to length(v)-1 do (
	  thislen := height - v . i . height + length(v.i.body);
	  if leng < thislen then leng = thislen;
	  );
     lengths := new array(int) len leng at row do (
	  l := 0;
	  for j from length(v)-1 to 0 by -1 do (
	       n := v.j;
	       k := row + n.height - height;
	       body := n.body;
	       if 0 <= k && k < length(body) then (
		    if l > 0 then l = l + length(body.k) + n.width - utf8width(body.k)
		    else l = length(body.k);
		    ) else if l > 0 then l = l + n.width;
	       );
	       provide l
	  );
     Net(height,width, 
	  new array(string) len leng at row do
	  provide new string len lengths.row do (
	       foreach n in v do (
	       	    k := row + n.height - height ;
		    if 0 <= k && k < length(n.body) then (
			 s := n.body.k;
		    	 foreach c in s do provide c;
			 for n.width - utf8width(s) do provide ' ')
		    else for n.width do provide ' '))));

export VerticalJoin(v:array(Net)):Net := (
     if length(v) == 0 then return Net(0,0,array(string)());
     if length(v) == 1 then return v.0;
     leng := 0;
     width := 0;
     foreach n in v do (
	  leng = leng + length(n.body);
	  if width < n.width then width = n.width;
	  );
     Net( v . 0 . height,
	  width,
	  new array(string) len leng do (
	       foreach n in v do foreach s in n.body do provide s
	       )));

export (x:Net) === (y:Net) : bool := (
     if x.height != y.height
     || x.width != y.width
     || length(x.body) != length(y.body) then return false;
     foreach s at i in x.body do if !(s === y.body.i) then return false;
     true);

export NetList := {
     previous:NetList,
     item:Net
     };

export HorizontalJoin(v:NetList):Net := (
     i := 0;
     p := v;
     while p.previous != p do (
	  i = i+1;
	  p = p.previous;
	  );
     p = v;
     s := new array(Net) len i do ( provide p.item; p = p.previous; );
     for j from 0 to (i-1)/2 do (			    -- now reverse the list
	  k := i-1-j;
	  t := s.j;
	  s.j = s.k;
	  s.k = t;
	  );
     HorizontalJoin(s));

export dummyNet := Net(0,0,array(string)());
export dummyNetList := NetList(self,dummyNet);

export netcmp(s:Net, t:Net):int := (
     if s.height > t.height then return 1;
     if s.height < t.height then return -1;
     sbody := s.body;
     tbody := t.body;
     slen := length(sbody);
     tlen := length(tbody);
     i := 0;
     while i<slen && i<tlen do (
	  r := strnumcmp(sbody.i,tbody.i);
	  if r != 0 then return r;
	  i = i+1;
	  );
     if slen > tlen then return 1;
     if slen < tlen then return -1;
     0);

export netcmp(s:Net, t:string):int := (
     if s.height > 1 then return 1;
     if s.height < 1 then return -1;
     sbody := s.body;
     slen := length(sbody);
     if slen == 0 then return -1;
     r := strnumcmp(sbody.0,t);
     if r != 0 then return r;
     if slen > 1 then return 1;
     0);
     
export netcmp(s:string, t:Net):int := (
     if 1 > t.height then return 1;
     if 1 < t.height then return -1;
     tbody := t.body;
     tlen := length(tbody);
     if tlen == 0 then return 1;
     r := strnumcmp(s,tbody.0);
     if r != 0 then return r;
     if tlen > 1 then return -1;
     0);

use vararray;

blankcolumn(i:int, t:Net):bool := (
     if i >= 0 then foreach s in t.body do if length(s) > i && s.i != ' ' then return false;
     true
     );

splitcolumn(i:int, t:Net):bool := (			    -- whether we can split between column i and column i-1
                                                            -- we don't want to split: identifiers; M2 operators
     if i <= 0 then return false;			    -- shouldn't happen!
     foreach s in t.body do if length(s) > i && (
          x := s.(i-1);	    	     y := s.i;
	  a := isalnum(x);           b := isalnum(y);
	  a && b
	  ||
	  (!a && x != ' ') && (!b && y != ' ')		    -- not between two punctuation characters
	  ||
	  (x == '.' && isdigit(y))     			    -- not between . and digit
	  ||
	  (y == '.' && isdigit(x))			    -- not between . and digit
	  ) then return false;
     true);

splitcolumnUTF(i:int, t:Net):bool := (			    -- whether we can split between column i and column i-1
                                                            -- we don't want to split: unicode characters encoded in utf-8
     if i <= 0 then return false;			    -- shouldn't happen!
     foreach s in t.body do if length(s) > i && (
          x := s.(i-1);	    	     y := s.i;
	  ((int(x) & 0x80) != 0) && ((int(y) & 0xc0) == 0x80)
	  ) then return false;
     true);

verticalTrim(t:Net):Net := (
     a := 0;
     b := length(t.body)-1;
     while a<b && length(t.body.a)==0 do a = a+1;
     while a<b && length(t.body.b)==0 do b = b-1;
     h := t.height - a;
     if h < 1 then h = 1;
     Net(h,t.width,new array(string) len b-a+1 do for i from a to b do provide t.body.i));

subnet(t:Net,startcol:int,wid:int):Net := (
     if startcol < 0 then startcol = 0;			  -- shouldn't happen
     if wid > t.width-startcol then wid = t.width-startcol;
     if startcol == 0 && wid == t.width then return t;
     Net( t.height, 
	  wid, 
	  new array(string) len length(t.body) do foreach s in t.body do provide utf8substr(s,startcol,wid)
	  ));

export wrap(wid:int, sep:char, t:Net):Net := (
     if t.width <= wid then return t;
     if wid <= 0 then return t;
     breaks := newvararrayint(t.width/wid + 5);
     minwid := wid/3;
     if minwid == 0 then minwid = 1;
     leftbkpt := 0;
     nextleftbkpt := 0;
     rightbkpt := 0;
     while true do (
	  breaks << leftbkpt;
	  n := leftbkpt + wid;
     	  -- find a good break point where there is a blank column
	  nextleftbkpt = n;
	  rightbkpt = n;
	  found := false;
	  if n >= t.width then (
	       found = true;
	       rightbkpt = t.width;
	       nextleftbkpt = t.width;
	       )
	  else if blankcolumn(n,t)
	  then (
	       found = true;
	       rightbkpt = n;
	       nextleftbkpt = n+1;
	       )
	  else for i from n to leftbkpt + minwid by -1 do (
	       if blankcolumn(i-1,t) then (
	       	    found = true;
		    rightbkpt = i-1;
		    nextleftbkpt = i;
		    break;
		    ));
	  while rightbkpt>leftbkpt && blankcolumn(rightbkpt-1,t) do rightbkpt = rightbkpt-1;
	  if !found then (
	       -- find a good break point where we don't split any identifiers or any operators
	       nextleftbkpt2 := n;
	       rightbkpt2 := n;
	       found2 := false;
	       if n >= t.width then (
		    found2 = true;
		    rightbkpt2 = t.width;
		    nextleftbkpt2 = t.width;
		    )
	       else if splitcolumn(n,t)
	       then (
		    found2 = true;
		    rightbkpt2 = n;
		    nextleftbkpt2 = n;
		    )
	       else for i from n to leftbkpt + minwid by -1 do (
		    if splitcolumn(i,t) then (
			 found2 = true;
			 rightbkpt2 = i;
			 nextleftbkpt2 = i;
			 break;
			 ));
	       while rightbkpt2>leftbkpt && blankcolumn(rightbkpt2-1,t) do rightbkpt2 = rightbkpt2-1;
	       if found2 then (
	       	    rightbkpt = rightbkpt2;
	       	    nextleftbkpt = nextleftbkpt2;
		    )
	       else (
		    -- find a good break point where we don't split any utf8 characters, which is always possible within about 4 bytes
		    nextleftbkpt3 := n;
		    rightbkpt3 := n;
		    -- found3 := false;
		    if n >= t.width then (
			 -- found3 = true;
			 rightbkpt3 = t.width;
			 nextleftbkpt3 = t.width;
			 )
		    else if splitcolumnUTF(n,t)
		    then (
			 -- found3 = true;
			 rightbkpt3 = n;
			 nextleftbkpt3 = n;
			 )
		    else for i from n to leftbkpt + minwid by -1 do (
			 if splitcolumnUTF(i,t) then (
			      -- found3 = true;
			      rightbkpt3 = i;
			      nextleftbkpt3 = i;
			      break;
			      ));
		    while rightbkpt3>leftbkpt && blankcolumn(rightbkpt3-1,t) do rightbkpt3 = rightbkpt3-1;
	       	    rightbkpt = rightbkpt3;
	       	    nextleftbkpt = nextleftbkpt3;
		    );
	       );
	  -- record the break point for future use
	  breaks << rightbkpt;
	  leftbkpt = nextleftbkpt;
	  while leftbkpt < t.width && blankcolumn(leftbkpt,t) do leftbkpt = leftbkpt+1;
	  if leftbkpt >= t.width then break;
	  );
     j := 0;
     VerticalJoin(
	  if int(sep) == 0 then (
	       new array(Net) len breaks.size/2 do (
		    a := breaks.ints.j;
		    b := breaks.ints.(j+1);
		    provide verticalTrim(subnet(t,a,b-a));
		    j = j+2;
		    ))
	  else (
	       sepline := toNet(new string len wid do provide sep);
	       new array(Net) len breaks.size - 1 do (
		    a := breaks.ints.j;
		    b := breaks.ints.(j+1);
		    provide verticalTrim(subnet(t,a,b-a));
		    j = j+2;
		    provide sepline;
		    ))));

export hash(n:Net):int := (
     h := n.height * 3457 + n.width * 7753;
     foreach s in n.body do h = h * 77 + hash(s);
     h);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
-- End:
