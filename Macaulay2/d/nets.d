--		Copyright 1996 by Daniel R. Grayson

-- nets are 2 dimensional strings of characters

use C;
use system;
use strings;
use varstrin;

export Net := {
     height:int,			  -- number of strings above the baseline
     width:int,				  -- width of body (strings may be shorter)
     body:array(string)			  -- one string for each row, read-only
     };

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
	  j := index(s,i);
	  if j == -1 then (
	       break;
	       );
	  i = j + if j+1 < length(s) && s.j == '\r' && s.(j+1) == '\n' then 2 else 1;
	  nlines = nlines + 1;	    -- count the bit after the last newline even if it's empty
	  );
     i = 0;
     new array(string) len nlines do (
	  while true do (
	       j := index(s,i);
	       if j == -1 then (
		    provide untabify(substr(s,i));
		    break;
		    )
	       else (
		    provide untabify(substr(s,i,j-i));
		    if j+1 < length(s) && s.j == '\r' && s.(j+1) == '\n'
		    then i = j+2
		    else i = j+1;
		    ))));
export toNet(s:string):Net := (
     v := if length(s) > 0 then lines(s) else array(string)(s);
     wid := 0;
     foreach s in v do if wid < length(s) then wid = length(s);
     Net(1,wid,v));
export toNet(c:char):Net := toNet(string(c));
export RaiseNet(n:Net,i:int):Net := Net(n.height+i,n.width,n.body);
export HorizontalJoin(v:array(Net)):Net := (
     if length(v) == 0 then return Net(0,0,array(string)());
     if length(v) == 1 then return v.0;
     width := 0;
     accumwids := new array(int) len length(v) do (
	  foreach n in v do (
	       o := width;
	       width = width + n.width;
	       provide o));
     height := v . 0 . height;
     for i from 1 to length(v)-1 do (
	  if height < v.i.height then height = v.i.height);
     leng := height - v . 0 . height + length(v . 0 . body);
     for i from 1 to length(v)-1 do (
	  thislen := height - v . i . height + length(v.i.body);
	  if leng < thislen then leng = thislen;
	  );
     widths := new array(int) len leng at row do (
	  j := length(v)-1;
	  while true do (
	       n := v.j;
	       k := row + n.height - height ;
	       body := n.body;
	       if 0 <= k && k < length(body) then (
		    l := length(body.k);
		    if l > 0 then (
		    	 provide l + accumwids.j;
			 break;
			 );
		    );
	       j = j-1;
	       if j < 0 then (
	  	    provide 0;
		    break;
		    );
	       );
	  );
     Net(height,width, 
	  new array(string) len leng at row do
	  provide new string len widths.row do (
	       foreach n in v do (
	       	    k := row + n.height - height ;
		    if 0 <= k && k < length(n.body) then (
			 s := n.body.k;
		    	 foreach c in s do provide c;
		    	 for n.width - length(s) do provide ' ')
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
     this:Net
     };

export HorizontalJoin(v:NetList):Net := (
     i := 0;
     p := v;
     while p.previous != p do (
	  i = i+1;
	  p = p.previous;
	  );
     p = v;
     s := new array(Net) len i do ( provide p.this; p = p.previous; );
     for j from 0 to (i-1)/2 do (			    -- now reverse the list
	  k := i-1-j;
	  t := s.j;
	  s.j = s.k;
	  s.k = t;
	  );
     HorizontalJoin(s));

export dummyNet := Net(0,0,array(string)());
export dummyNetList := NetList(self,dummyNet);

min(x:int,y:int):int := if x<y then x else y;

export (s:Net) < (t:Net) : bool := (
     if s.height != t.height then return s.height < t.height;
     n := min(length(s.body),length(t.body));
     for i from 0 to n-1 do (
	  if !(s.body.i === t.body.i) then return s.body.i < t.body.i;
	  );
     return length(s.body) < length(t.body);
     );
export (s:Net) >= (t:Net) : bool := !(s<t);
export (s:Net) > (t:Net) : bool := t<s;
export (s:Net) <= (t:Net) : bool := !(t<s);

use vararray;

blankcolumn(i:int, t:Net):bool := (
     if i >= 0 then foreach s in t.body do if length(s) > i && s.i != ' ' then return false;
     true
     );

subnet(t:Net,startcol:int,wid:int):Net := (
     if startcol < 0 then startcol = 0;			  -- shouldn't happen
     if wid > t.width-startcol then wid = t.width-startcol;
     if startcol == 0 && wid == t.width then return t;
     Net( t.height, 
	  wid, 
	  new array(string) len length(t.body) do foreach s in t.body do provide substr(s,startcol,wid)
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
     while (
	  while leftbkpt < t.width && blankcolumn(leftbkpt,t) do leftbkpt = leftbkpt+1;
	  leftbkpt < t.width
	  )
     do (
	  breaks << leftbkpt;
	  n := leftbkpt + wid;
	  nextleftbkpt = n;
	  rightbkpt = n;
	  if n >= t.width then (
	       rightbkpt = t.width;
	       nextleftbkpt = t.width;
	       )
	  else if blankcolumn(n,t)
	  then (
	       rightbkpt = n;
	       nextleftbkpt = n;
	       )
	  else for i from n to leftbkpt + minwid by -1 do (
	       if blankcolumn(i-1,t) then (
		    nextleftbkpt = i;
		    rightbkpt = i-1;
		    break;
		    ));
	  while rightbkpt>leftbkpt && blankcolumn(rightbkpt-1,t) do rightbkpt = rightbkpt-1;
	  breaks << rightbkpt;
	  leftbkpt = nextleftbkpt;
	  );
     j := 0;
     VerticalJoin(
	  if length(t.body) > 1 then (
     	       sepline := toNet(new string len wid do provide sep);
     	       new array(Net) len breaks.size - 1 do (
	  	    provide subnet(t,breaks.ints.j,breaks.ints.(j+1)-breaks.ints.j);
		    j = j+2;
	  	    provide sepline;
	  	    ))
	  else (
     	       new array(Net) len breaks.size/2 do (
		    provide subnet(t,breaks.ints.j,breaks.ints.(j+1)-breaks.ints.j);
	       	    j = j+2;
		    ))));

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
