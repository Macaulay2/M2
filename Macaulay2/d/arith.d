--		Copyright 1994 by Daniel R. Grayson

use err;
use system;
use stdio;
use strings;
use varstrin;
use C;

valn2(x:int):int := (
     if x == 0 then fatal("zero passed to valn2");
     i := 0;
     while (x&1)==0 do (x=x>>1; i=i+1);
     i);

export min(x:int,y:int):int := if x<y then x else y;
export max(x:int,y:int):int := if x<y then y else x;

gcd(x:int,y:int):int := (
     -- this doesn't work if an arg is -2^63
     if x < 0 then x = -x;
     if y < 0 then y = -y;
     if x == 0 then return(y);
     if y == 0 then return(x);
     a := valn2(x);
     b := valn2(y);
     x = x >> a;
     y = y >> b;
     while true do (
	  if x>y then (w:=x;x=y;y=w);
	  y = y-x;
	  if y == 0 then return(x << min(a,b));
     	  y = y >> valn2(y);
	  ));	  

gcd(x:ushort,y:ushort):ushort := ushort(gcd(int(x),int(y)));

intpower(x:int,n:int):int := (
     if n == 0 then return(1);
     if n < 0 then return(0);	-- put rational numbers in eventually
     y := x;
     z := 1;
     while true do (
	  if (n & 1) != 0 then z = z * y;
	  n = n >> 1;
	  if n == 0 then break;
	  y = y * y;
	  );
     z
     );

export Integer := {
     negative:bool,
     body:array(ushort)
     -- The corresponding integer is defined to be
     -- (if negative then -1 else 1) * sum(i -> body_i 2^16^i, length body)
     -- The body is always normalized so that the high order bigit is
     -- nonzero
     };

dump(o:file, x:Integer):file := (
     stderr << (if x.negative then "-" else "+") << "(";
     foreach b at i in x.body by -1 do stderr << "+" << x.body.i << "*2^" << i*16 ;
     stderr << ")";
     o);

zerobody := array(ushort)();

getbit(i:ushort,n:int):bool := 0 != (int(i) & (1 << n));
getbit(x:Integer,n:int):bool := getbit(x.body.(n/16),n%16);

negsmall := -100;
possmall := 300;
smallints := new array(Integer) len possmall - negsmall + 1 do (
     i := negsmall;
     while i <= possmall do (
	  if i < 0
	  then provide Integer(true,array(ushort)(ushort(-i)))
	  else if i == 0
	  then provide Integer(false,array(ushort)())
	  else provide Integer(false,array(ushort)(ushort(i)));
	  i = i+1;
	  );
     );
export toInteger(x:int):Integer := (
     if x >= negsmall && x <= possmall
     then smallints.(x-negsmall)
     else (
	  neg := x < 0;
	  z := if neg then uint(-x) else uint(x);
	  if z < uint(0x10000)
	  then Integer(neg,array(ushort)(ushort(z)))
	  else Integer(neg,array(ushort)(ushort(z),ushort(z>>16)))
	  ));
export toInteger(x:uint):Integer := (
     if x <= uint(possmall)
     then smallints.(int(x)-negsmall)
     else (
	  if x < uint(0x10000)
	  then Integer(false,array(ushort)(ushort(x)))
	  else Integer(false,array(ushort)(ushort(x),ushort(x>>16)))
	  ));
toInteger(x:ushort):Integer := (
     if x <= ushort(possmall)
     then smallints.(int(x)-negsmall)
     else Integer(false,array(ushort)(x)));
zeroInteger := toInteger(0);
numbits(i:ushort):int := (
     n := 0;
     if i >= ushort(256) then (n = n+8; i = i >> 8);
     if i >= ushort( 16) then (n = n+4; i = i >> 4);
     if i >= ushort(  4) then (n = n+2; i = i >> 2);
     if i >= ushort(  2) then (n = n+1; i = i >> 1);
     if i >= ushort(  1) then  n = n+1;
     n);
numbits(i:int):int := (
     n := 0;
     if i >= 65536 then (n = n+16; i = i >> 16);
     if i >=   256 then (n = n+ 8; i = i >>  8);
     if i >=    16 then (n = n+ 4; i = i >>  4);
     if i >=     4 then (n = n+ 2; i = i >>  2);
     if i >=     2 then (n = n+ 1; i = i >>  1);
     if i >=     1 then  n = n+ 1;
     n);
numbits(x:Integer):int := (
     n := length(x.body) - 1;
     16 * n + numbits(x.body.n));

bumpone(x:Integer):Integer := (		  -- add 1 to body, ignore sign
     e := 1;
     foreach b in x.body do (
	  if b != ushort(0xffff) then (
	       e = 0;
	       break;
	       ));
     s := uint(1);
     Integer(
	  x.negative,
	  new array(ushort) len length(x.body) + e do (
	       foreach c in x.body do (
		    s = s + uint(c);
		    provide ushort(s);
		    s = s >> 16;
		    );
	       provide ushort(s);
	       )));
normalize(body:array(ushort)):array(ushort) := (
     n := length(body);
     if n == 0
     then zerobody
     else (
	  n = n - 1;
	  if body.n == ushort(0)
	  then (
	       while (
		    n = n-1;
		    n >= 0
		    )
	       do (
		    if body.n != ushort(0)
		    then return(new array(ushort) len n+1 do (
			      foreach b in body do provide b
			      )));
	       zerobody)
	  else body));
normalize(i:Integer):void := (
     body := i.body;
     n := length(body);
     if n == 0
     then (
	  i.negative = false;
	  i.body = zerobody;
	  )
     else (
	  n = n - 1;
	  if body.n == ushort(0)
	  then (
	       while (
		    n = n-1;
		    n >= 0
		    )
	       do (
		    if body.n != ushort(0)
		    then (
			 i.body = new array(ushort) len n+1 do (
			      foreach b in body do provide b
			      );
			 return ();
			 );
		    );
	       i.negative = false;
	       i.body = zerobody;
	       )));
mantissa(x:Integer):double := (
     n := length(x.body);
     m := (if n == 0 then 0.
	  else if n == 1 then double(x.body.0) / 65536.
	  else if n == 2 then (
	       double(x.body.0) / 65536. 
	       + double(x.body.1)) / 65536.
	  else if n == 3 then (
	       (
		    double(x.body.0) / 65536. 
		    + double(x.body.1)) / 65536.
	       + double(x.body.2)) / 65536.
	  else if n == 4 then (
	       (
		    (
			 double(x.body.0) / 65536. 
			 + double(x.body.1)) / 65536.
		    + double(x.body.2)) / 65536.
	       + double(x.body.3)) / 65536.
	  else (
	       (
		    (
			 (
			      double(x.body.(n-5)) / 65536. 
			      + double(x.body.(n-4))) / 65536.
			 + double(x.body.(n-3))) / 65536.
		    + double(x.body.(n-2))) / 65536.
	       + double(x.body.(n-1))) / 65536.);
     if x.negative then m = -m;
     m);
leftshift(x:Integer,n:int):Integer := (
     if n < 0 then fatal("negative argument to leftshift");
     if n == 0 then return(x);
     nb := n / 16;
     nx := length(x.body);
     if nx == 0 then return(x);
     nr := n % 16;
     ne := (
	  if (x.body.(nx - 1) >> (16 - nr)) == ushort(0)
	  then 0
	  else 1
	  );
     y := new array(ushort) len nx + nb + ne do (
	  for nb do provide ushort(0);
	  s := uint(0);
	  foreach b in x.body do (
	       s = s | (uint(b) << nr);
	       provide ushort(s);
	       s = s >> 16;
	       );
	  provide ushort(s);
	  );
     Integer(x.negative,y));
rightshift(x:Integer,n:int):Integer := (
     if n < 0 then fatal("negative argument to rightshift");
     if n == 0 then return(x);
     nb := (n-1) / 16;
     nx := length(x.body);
     if nx == 0 then return(x);
     nr := 16 * (nb + 1) - n;
     ny := nx - nb - 1;
     if (x.body.(nx - 1) >> (16 - nr)) != ushort(0) then ny = ny + 1;
     if ny <= 0 then return(zeroInteger);
     y := new array(ushort) len ny do (
	  s := uint(x.body.nb) >> (16 - nr);
	  for i from nb + 1 to nx - 1 do (
	       s = s | (uint(x.body.i) << nr);
	       provide ushort(s);
	       s = s >> 16;
	       );
	  provide ushort(s);
	  );
     Integer(x.negative,y));
export (x:Integer) << (n:int) : Integer := (
     if n >= 0 then leftshift(x,n) else rightshift(x,-n)
     );
export (x:Integer) >> (n:int) : Integer := (
     if n >= 0 then rightshift(x,n) else leftshift(x,-n)
     );     
export hash(x:Integer):int := (
     h := 0;
     foreach b in x.body do h = h * 3737 + int(b);
     if x.negative then -h else h);
export isInt(x:Integer):bool := (
     n := length(x.body);
     n <= 1 
     || 
     n == 2 && int(x.body.1) <= if x.negative then 0x8000 else 0x7fff
     );
export toInt(x:Integer):int  := (
     n := length(x.body);
     if n==0
     then 0
     else if n==1
          then if x.negative
	       then - int(x.body.0)
	       else   int(x.body.0)
	  else if x.negative
	       then -(int(x.body.0) + (int(x.body.1) << 16))
	       else   int(x.body.0) + (int(x.body.1) << 16)
	       );
expt(x:double,n:int):double := (
     if n == 0 then return(1.);
     if n < 0 then (x = 1./x; n = -n);
     z := 1.;
     while true do (
	  if (n&1) != 0 then z = z * x;
	  n = n >> 1;
	  if n == 0 then break;
	  x = x * x;
	  );
     z);
logtwo := log(2.);
export Floor(x:double):Integer := (
     if x < 2147483648. && x > -2147483648.
     then toInteger(int(floor(x)))
     else (
	  neg := x < 0.;
	  if neg then x = -x;
	  n := int(floor(log(x)/logtwo)) / 16;
	  b := new array(ushort) len n+1 do provide ushort(0);
	  x = x/expt(65536.,n);
	  i := n;
	  while i >= 0 && x != 0. do (
	       c := ushort(floor(x));
	       b.i = c;
	       x = (x - double(c)) * 65536.;
	       i = i - 1;
	       );
	  Integer(neg,b)));
export Round(x:double):Integer := Floor(x + 0.5);
export toDouble(x:Integer):double := (
     -- could be faster...
     y := 0.;
     foreach b at i in x.body by -1 do y = 0x10000 * y + int(b);
     if x.negative then -y else y);
compare(x:array(ushort),y:array(ushort)):int := (
     if length(x) > length(y)
     then 1
     else if length(x) < length(y)
     then -1
     else (
	  for i from length(x)-1 to 0 by -1 do (
	       if x.i > y.i
	       then return(1)
	       else if x.i < y.i
	       then return(-1);
	       );
	  0));
compare(x:Integer,y:Integer):int := (
     if x.negative
     then if y.negative
     	  then compare(y.body,x.body)
	  else -1
     else if y.negative
     	  then 1
	  else compare(x.body,y.body));
add(x:array(ushort), y:array(ushort)) : array(ushort) := (
     if length(x) < length(y) then (z := x; x = y; y = z);
     normalize( new array(ushort) len length(x)+1 do (
	  s := uint(0);
	  for i from 0 to length(y) - 1 do (
	       s = s + uint(x.i) + uint(y.i);
	       provide ushort(s);
	       s = s >> 16;
	       );
	  for i from length(y) to length(x)-1 do (
	       s = s + uint(x.i);
	       provide ushort(s);
	       s = s >> 16;
	       );
	  provide ushort(s);
	  )));
subtract(x:array(ushort), y:array(ushort)) : array(ushort) := (
     normalize( new array(ushort) len length(x) do (
	  s := 0;
	  for i from 0 to length(y) - 1 do (
	       s = s + int(x.i) - int(y.i);
	       provide ushort(s);
	       s = s >> 16;
	       );
	  for i from length(y) to length(x)-1 do (
	       s = s + int(x.i);
	       provide ushort(s);
	       s = s >> 16;
	       );
	  )));
times(x:array(ushort),y:array(ushort)):array(ushort) := (
     z := new array(ushort) len length(x)+length(y) do provide ushort(0);
     foreach xb at i in x do (
	  s := uint(0);
	  foreach yb at j in y do (
	       s = s + uint(xb) * uint(yb) + uint(z.(i+j));
	       z.(i+j) = ushort(s);
	       s = s >> 16;
	       );
	  if s != uint(0)
	  then (
	       j := i + length(y);
	       while true do (
		    s = s + uint(z.j);
		    z.j = ushort(s);
		    s = s >> 16;
		    if s == uint(0) then break;
		    j = j + 1;
		    );
	       );
	  );
     normalize(z));
export (x:Integer) % (y:ushort) : ushort := (
     if y == ushort(0) then fatal("division by zero");
     if length(x.body) == 0 then ushort(0)
     else if (int(y) & (int(y)-1)) == 0 
     then (
	  z := x.body.0 % y;
     	  if x.negative && z != ushort(0) then z = ushort(uint(y)-uint(z));
	  z
	  )
     else (
     	  m := uint(y);
     	  z := uint(0);
     	  foreach xb at i in x.body by -1 do z = ((z << 16) + uint(xb)) % m;
     	  if x.negative && z != uint(0) then z = m-z;
     	  ushort(z)));
(x:Integer) // (y:ushort) : Integer := (
     if y == ushort(0) then fatal("division by zero");
     n := length(x.body);
     if n==0 then return(x);
     m := uint(y);
     b := new array(ushort) len n do provide ushort(0);
     w := uint(0);
     foreach xb at i in x.body by -1 do (
	  a := (w << 16) + uint(xb);
	  b.i = ushort(a / m);
	  w = a % m;
	  );
     if x.negative && w != uint(0) then (
	  j := 0;
	  v := uint(1);
	  while true do (
	       v = v + uint(b.j);
	       b.j = ushort(v);
	       v = v >> 16;
	       if v == uint(0) then break;
	       j = j + 1;
	       );
	  );
     Integer(x.negative,normalize(b)));
export (x:Integer) + (y:Integer) : Integer := (
     if x.negative
     then if y.negative
     	  then Integer(true,add(x.body,y.body))
	  else (
	       c := compare(x.body,y.body);
	       if c == 1
	       then Integer(true,subtract(x.body,y.body))
	       else if c == 0
	       then zeroInteger
	       else Integer(false,subtract(y.body,x.body)))
     else if y.negative
     	  then (
	       c := compare(x.body,y.body);
	       if c == 1
	       then Integer(false,subtract(x.body,y.body))
	       else if c == 0
	       then zeroInteger
	       else Integer(true,subtract(y.body,x.body)))
	  else Integer(false,add(x.body,y.body)));
export             - (y:Integer) : Integer := (
     if length(y.body) == 0
     then y
     else Integer(!y.negative,y.body));
export (x:Integer) - (y:Integer) : Integer := x + -y;
export (x:Integer) * (y:Integer) : Integer := (
     i := Integer(x.negative ^^ y.negative, times(x.body,y.body));
     normalize(i);
     i
     );
twopower64 := expt(2.,64);
twopower(n:int):Integer := toInteger(1) << n;
roundshift(x:Integer,n:int):Integer := (
     if n==0 then return(x);
     if length(x.body) == 0 then return(x);
     m := numbits(x);
     if m < n then zeroInteger
     else if m == n then if x.negative then toInteger(-1) else toInteger(1)
     else if getbit(x,n-1) then bumpone(x >> n) else x >> n);
export (x:Integer) === (y:Integer) : bool := (
     x == y || (
     	  x.negative == y.negative
     	  &&
     	  length(x.body) == length(y.body)
     	  && (
	       i := 0;
	       n := length(x.body);
	       while i < n && x.body.i == y.body.i do i = i+1;
	       i==n)));
export (x:Integer) === (y:int) : bool := (
     !(x.negative ^^ (y<0))
     && 
     (
	  z := if y<0 then uint(-y) else uint(y);
	  n := length(x.body);
	  if n==0
	  then y==0
	  else if n==1 
	  then z == uint(x.body.0)
	  else if n==2
	  then z == uint(x.body.0) + (uint(x.body.1) << 16)
	  else false
	  ));
export (x:int) === (y:Integer) : bool := y === x;
export (x:Integer) > (y:Integer) : bool :=  1 == compare(x,y);
export (x:Integer) < (y:Integer) : bool := -1 == compare(x,y);
export (x:Integer) >= (y:Integer) : bool := -1 != compare(x,y);
export (x:Integer) <= (y:Integer) : bool :=  1 != compare(x,y);
export (x:Integer) > (y:int) : bool := (
     if x.negative
     then if y < 0
     	  then (
	       z := uint(-y);
	       n := length(x.body);
	       -- n>0 because x<0
	       if n==1
	       then uint(x.body.0) < z
	       else if n==2
	       then uint(x.body.0) + (uint(x.body.1) << 16) < z
	       else true)
	  else false
     else if y < 0
     	  then true
	  else (
	       n := length(x.body);
	       z := uint(y);
	       if n==0
	       then false
	       else if n==1 
	       then uint(x.body.0) > z
	       else if n==2
	       then uint(x.body.0) + (uint(x.body.1) << 16) > z
	       else true
	       ));
export (x:Integer) < (y:int) : bool := (
     if x.negative
     then if y < 0
     	  then (
	       z := uint(-y);
	       n := length(x.body);
	       -- n>0 because x<0
	       if n==1
	       then uint(x.body.0) > z
	       else if n==2
	       then uint(x.body.0) + (uint(x.body.1) << 16) > z
	       else true)
	  else true
     else if y < 0
     	  then false
	  else (
	       n := length(x.body);
	       z := uint(y);
	       if n==0
	       then uint(0) < z
	       else if n==1 
	       then uint(x.body.0) < z
	       else if n==2
	       then uint(x.body.0) + (uint(x.body.1) << 16) < z
	       else false
	       ));
export (x:int) < (y:Integer) : bool := y > x;
export (x:int) > (y:Integer) : bool := y < x;
export (x:Integer) >= (y:int) : bool := !(x < y);
export (x:Integer) <= (y:int) : bool := !(x > y);
export (x:int) <= (y:Integer) : bool := !(y < x);
export (x:int) >= (y:Integer) : bool := !(y > x);
export (x:int) + (y:Integer) : Integer := toInteger(x) + y;
export (x:Integer) + (y:int) : Integer := (
     if y == 1 && !x.negative 
     then bumpone(x)
     else x + toInteger(y)
     );
export (x:ushort) + (y:Integer) : Integer := toInteger(x) + y;
export (x:Integer) + (y:ushort) : Integer := (
     if y == ushort(1) && !x.negative 
     then bumpone(x)
     else x + toInteger(y)
     );
export (x:int) - (y:Integer) : Integer := toInteger(x) - y;
export (x:Integer) - (y:int) : Integer := x - toInteger(y);
export (x:ushort) - (y:Integer) : Integer := toInteger(x) - y;
export (x:Integer) - (y:ushort) : Integer := x - toInteger(y);
export (x:int) * (y:Integer) : Integer := toInteger(x) * y;
export (x:Integer) * (y:int) : Integer := x * toInteger(y);
export (x:double) + (y:Integer) : double := x + toDouble(y);
export (x:Integer) + (y:double) : double := toDouble(x) + y;
export (x:double) - (y:Integer) : double := x - toDouble(y);
export (x:Integer) - (y:double) : double := toDouble(x) - y;
export (x:double) * (y:Integer) : double := x * toDouble(y);
export (x:Integer) * (y:double) : double := toDouble(x) * y;
export (x:double) / (y:Integer) : double := x / toDouble(y);
export (x:Integer) / (y:double) : double := toDouble(x) / y;
export abs(x:Integer) : Integer := if x<0 then -x else x;
export (x:Integer) // (y:Integer) : Integer := (
     if length(y.body) == 0 then fatal("division by zero");
     if length(x.body) == 0 
     then zeroInteger
     else if length(y.body) == 1
     then if y.negative then (-x + (y.body.0 - 1)) // y.body.0 else x // y.body.0
     else (
	  nx := numbits(x);
	  ny := numbits(y);
	  if ny > nx+1 then return(
	       if x.negative then (
		    if y.negative then toInteger(1) else toInteger(-1)
		    )
	       else zeroInteger
	       );
	  n := nx + 8;			  -- a few extra bits of precision
	  p := twopower(n);		  -- p = 2^n;
	  z := Floor(twopower64/mantissa(y) + 0.5) << (n - 64 - 16*length(y.body));
					  -- warning: that 16 is the number of bits in a short!
					  -- z is now an approximation of 2^n/y
          -- stderr << "z = "; dump(stderr,z); stderr << endl;
	  while true do (
	       oz := z;
	       z = z + roundshift(z * (p - z * y), n);
					  -- this is Newton's method for improving z
	       if z === oz then break;
	       -- stderr << "z = "; dump(stderr,z); stderr << endl;
	       );
     	  if length(z.body) == 0 then fatal("internal error in '//' - zero reciprocal");
	  q := roundshift(x * z, n);	  -- q is now an approximation of x/y
	  -- stderr << "q = "; dump(stderr,q); stderr << endl;
	  if compare(q * y, x) == 1 
	  then if y.negative then q + toInteger(1) else q - toInteger(1) -- speed this up
	  else q
	  ));
export (x:Integer) % (y:Integer) : Integer := (
     if length(y.body) == 0 then fatal("division by zero");
     if length(y.body) == 1
     then toInteger(x % y.body.0)
     else x - (x // y) * y
     );
export (x:Integer) ^ (n:Integer) : Integer := (
     if length(n.body) == 0 then return(toInteger(1));
     if n.negative then fatal("negative exponent for integer power");
     if length(x.body) == 0 then return(x);
     y := x;
     z := toInteger(1);
     for i from 0 to length(n.body) - 2 do (
	  nb := int(n.body.i);
	  k := 16;
	  while true do (
	       if (nb & 1) != 0 then z = z * y;
	       nb = nb >> 1;
	       y = y * y;
	       k = k - 1;
	       if k == 0 then break;
	       );
	  );
     nb := int(n.body.(length(n.body) - 1));
     while true do (
	  if (nb & 1) != 0 then z = z * y;
	  nb = nb >> 1;
	  if nb == 0 then break;
	  y = y * y;
	  );
     z
     );
export (x:double) ^ (n:Integer) : double := (
     if length(n.body) == 0 then return(1.);
     if n.negative then x = 1./x;
     if x == 0. then return(x);
     y := x;
     z := 1.;
     for i from 0 to length(n.body) - 2 do (
	  nb := int(n.body.i);
	  while true do (
	       if (nb & 1) != 0 then z = z * y;
	       nb = nb >> 1;
	       y = y * y;
	       );
	  );
     nb := int(n.body.(length(n.body) - 1));
     while true do (
	  if (nb & 1) != 0 then z = z * y;
	  nb = nb >> 1;
	  if nb == 0 then break;
	  y = y * y;
	  );
     z
     );
export (x:Integer) & (y:Integer) : Integer := (
     -- logical and
     if length(x.body) < length(y.body) 
     then (z := x; x = y; y = z); -- now x is longer
     r := Integer(
	  x.negative & y.negative,
	  new array(ushort) len length(y.body) 
	  do foreach u at i in y.body do provide u & x.body.i);
     normalize(r);
     r);
export (x:Integer) | (y:Integer) : Integer := (
     -- logical or
     if length(x.body) < length(y.body) 
     then (z := x; x = y; y = z); -- now x is longer
     r := Integer(
	  x.negative | y.negative,
	  new array(ushort) len length(x.body) do (
	       foreach u at i in y.body do provide u | x.body.i;
	       for i from length(y.body) to length(x.body)-1 do (
		    provide x.body.i)));
     normalize(r);
     r);
export (x:Integer) ^^ (y:Integer) : Integer := (
     -- logical exclusive or
     if length(x.body) < length(y.body) 
     then (z := x; x = y; y = z); -- now x is longer
     r := Integer(
	  x.negative ^^ y.negative,
	  new array(ushort) len length(x.body) do (
	       foreach u at i in y.body do provide u ^^ x.body.i;
	       for i from length(y.body) to length(x.body)-1 do (
		    provide x.body.i)));
     normalize(r);
     r);
export (x:Integer) > (y:double) : bool := toDouble(x) > y;
export (x:Integer) < (y:double) : bool := toDouble(x) < y;
export (x:double) < (y:Integer) : bool := x < toDouble(y);
export (x:double) > (y:Integer) : bool := x > toDouble(y);
export (x:Integer) >= (y:double) : bool := toDouble(x) >= y;
export (x:Integer) <= (y:double) : bool := toDouble(x) <= y;
export (x:double) <= (y:Integer) : bool := x <= toDouble(y);
export (x:double) >= (y:Integer) : bool := x >= toDouble(y);
export valn2(x:Integer):int := (
     if length(x.body) == 0 then fatal("zero passed to valn2");
     n := 0;
     foreach xb in x.body do (
	  if xb == ushort(0) 
	  then n = n + 16
	  else return(n + valn2(int(xb))));
     fatal("internal error in valn2");
     0);
export tostring(x:Integer):string := (
     if x === 0 then "0"
     else (
     	  s := newvarstring(1 + 5 * length(x.body));
	  neg := x < 0;
	  if neg then x = -x;
     	  while x >= 10000 do (
	       xb := x % ushort(10000);
       	       s << "0123456789".(int(xb) % 10);
	       xb = xb / ushort(10);
       	       s << "0123456789".(int(xb) % 10);
	       xb = xb / ushort(10);
       	       s << "0123456789".(int(xb) % 10);
	       xb = xb / ushort(10);
       	       s << "0123456789".(int(xb) % 10);
	       x  = x // ushort(10000);
	       );
	  while true do (
	       xb := x.body.0;
       	       s << "0123456789".(int(xb) % 10);
	       xb = xb / ushort(10);
	       if xb == ushort(0) then break;
       	       s << "0123456789".(int(xb) % 10);
	       xb = xb / ushort(10);
	       if xb == ushort(0) then break;
       	       s << "0123456789".(int(xb) % 10);
	       xb = xb / ushort(10);
	       if xb == ushort(0) then break;
       	       s << "0123456789".(int(xb) % 10);
	       break;
	       );
	  if neg then s << '-';
	  takereversestring(s)));
export (o:file) << (x:Integer) : file := o << tostring(x);

export gcd(x:Integer,y:Integer):Integer := (
     if isInt(x) && isInt(y) then return(toInteger(gcd(toInt(x),toInt(y))));
     if x<0 then x=-x;
     if y<0 then y=-y;
     if x === 0 then return(y);
     if y === 0 then return(x);
     if length(x.body) == 1 then return(toInteger(gcd(x.body.0,y % x.body.0)));
     if length(y.body) == 1 then return(toInteger(gcd(y.body.0,x % y.body.0)));
     a := valn2(x);
     b := valn2(y);
     c := min(a,b);
     x = x >> a;
     y = y >> b;
     if length(x.body) == 1 then return(toInteger(gcd(x.body.0,y % x.body.0)) << c);
     if length(y.body) == 1 then return(toInteger(gcd(y.body.0,x % y.body.0)) << c);
     if x>y then (z:=x;x=y;y=z);
     while true do (			  -- 0 < x <= y, both odd
	  y = y-x;
	  if y === 0 then return(x << c);
     	  if length(y.body) == 1 
	  then return(toInteger(gcd(y.body.0,x % y.body.0)) << c);
     	  y = y >> valn2(y);
     	  if length(y.body) == 1
	  then return(toInteger(gcd(y.body.0,x % y.body.0)) << c);
	  if x>y then (z:=x;x=y;y=z);
	  ));	  

export Rational := {
     numerator:Integer,
     denominator:Integer
     };
export toDouble(x:Rational):double := (
     toDouble(x.numerator)/toDouble(x.denominator));
export (x:double) + (y:Rational) : double := x + toDouble(y);
export (x:double) - (y:Rational) : double := x - toDouble(y);
export (x:double) * (y:Rational) : double := x * toDouble(y);
export (x:double) / (y:Rational) : double := x / toDouble(y);
export (x:Rational) + (y:double) : double := toDouble(x) + y;
export (x:Rational) - (y:double) : double := toDouble(x) - y;
export (x:Rational) * (y:double) : double := toDouble(x) * y;
export (x:Rational) / (y:double) : double := toDouble(x) / y;
     
export (x:Integer) / (y:Integer) : Rational := (
     if y < 0 then (x = -x; y = -y);
     d := gcd(x,y);
     Rational(x//d,y//d));
export (x:int) / (y:Integer) : Rational := toInteger(x) / y;
export (x:Integer) / (y:int) : Rational := x / toInteger(y);
export (x:Rational) + (y:Integer) : Rational := (
     Rational(x.numerator + y * x.denominator, x.denominator));
export (x:Rational) + (y:int) : Rational := x + toInteger(y);
export (x:Integer) + (y:Rational) : Rational := (
     Rational(y.numerator + x * y.denominator, y.denominator));
export (x:int) + (y:Rational) : Rational := toInteger(x) + y;
export (x:Rational) - (y:Integer) : Rational := (
     Rational(x.numerator - y * x.denominator, x.denominator));
export (x:Rational) - (y:int) : Rational := x - toInteger(y);
export (x:Integer) - (y:Rational) : Rational := (
     Rational(x * y.denominator - y.numerator, y.denominator));
export (x:int) - (y:Rational) : Rational := toInteger(x) - y;
export (x:Rational) * (y:Integer) : Rational := (
     g := gcd(x.denominator,y);
     Rational(x.numerator * (y//g), x.denominator//g));
export (x:Rational) * (y:int) : Rational := x * toInteger(y);
export (x:Integer) * (y:Rational) : Rational := (
     g := gcd(y.denominator,x);
     Rational(y.numerator * (x//g), y.denominator//g));
export (x:int) * (y:Rational) : Rational := toInteger(x) * y;
export (x:Rational) / (y:Integer) : Rational := (
     g := gcd(y,x.numerator);
     if y > 0
     then Rational(x.numerator//g,x.denominator*(y//g))
     else Rational(-x.numerator//g,x.denominator*(-y//g)));
export (x:Rational) / (y:int) : Rational := x / toInteger(y);
export (x:Integer) / (y:Rational) : Rational := (
     g := gcd(x,y.numerator);
     Rational(y.denominator * (x//g), y.numerator//g));
export (x:int) / (y:Rational) : Rational := toInteger(x) / y;
export (x:Rational) + (y:Rational) : Rational := (
     g := gcd(x.denominator,y.denominator);
     num := (
	  x.numerator * (y.denominator // g) 
	  + 
	  y.numerator * (x.denominator // g)
	  );
     h := gcd(num,g);
     Rational(num//h,(x.denominator//g)*(y.denominator//h)));
export - (y:Rational) : Rational := Rational(-y.numerator,y.denominator);
export (x:Rational) - (y:Rational) : Rational := x + - y;
export (x:Rational) * (y:Rational) : Rational := (
     g := gcd(x.numerator,y.denominator);
     h := gcd(x.denominator,y.numerator);
     Rational(
	  (x.numerator//g) * (y.numerator//h),
	  (x.denominator//h) * (y.denominator//g)));
export (x:Rational) / (y:Rational) : Rational := (
     g := gcd(x.numerator,y.numerator);
     h := gcd(x.denominator,y.denominator);
     num := (x.numerator//g) * (y.denominator//h);
     den := (x.denominator//h) * (y.numerator//g);
     if den > 0 
     then Rational(num,den)
     else Rational(-num,-den));
export (x:Rational) ^ (nn:Integer) : Rational := (
     n := toInt(nn);			  -- not quite right!
     if n == 0 then return(Rational(toInteger(1),toInteger(1)));
     if n < 0 then (
	  x = 1/x;
	  n = -n);
     y := x;
     z := Rational(toInteger(1),toInteger(1));
     while true do (
	  if (n & 1) != 0 then z = z * y;
	  n = n >> 1;
	  if n == 0 then break;
	  y = y * y;
	  );
     z);
export (o:file) << (x:Rational) : file := (
     o << x.numerator << '/' << x.denominator);
export (x:Rational) === (y:Rational) : bool := (
     x.denominator === y.denominator && x.numerator === y.numerator);

export (x:Rational) === (y:Integer) : bool := (
     x.denominator === 1 && x.numerator === y);
export (x:Rational) === (y:int) : bool := (
     x.denominator === 1 && x.numerator === y);

export (y:Integer) === (x:Rational) : bool := (
     x.denominator === 1 && x.numerator === y);
export (y:int) === (x:Rational) : bool := (
     x.denominator === 1 && x.numerator === y);

export (x:Rational) < (y:Rational) : bool := (
     (x.numerator * y.denominator) < (y.numerator * x.denominator)
     );
export (x:Rational) <= (y:Rational) : bool := (
     (x.numerator * y.denominator) <= (y.numerator * x.denominator)
     );
export (x:Rational) > (y:Rational) : bool := (
     (x.numerator * y.denominator) > (y.numerator * x.denominator)
     );
export (x:Rational) >= (y:Rational) : bool := (
     (x.numerator * y.denominator) >= (y.numerator * x.denominator)
     );
export (x:Integer) < (y:Rational) : bool := (
     x * y.denominator < y.numerator);
export (x:Integer) <= (y:Rational) : bool := (
     x * y.denominator <= y.numerator);
export (x:Integer) > (y:Rational) : bool := (
     x * y.denominator > y.numerator);
export (x:Integer) >= (y:Rational) : bool := (
     x * y.denominator >= y.numerator);
export (x:Rational) < (y:Integer) : bool := (
     x.numerator < y * x.denominator);
export (x:Rational) <= (y:Integer) : bool := (
     x.numerator <= y * x.denominator);
export (x:Rational) > (y:Integer) : bool := (
     x.numerator > y * x.denominator);
export (x:Rational) >= (y:Integer) : bool := (
     x.numerator >= y * x.denominator);
export (x:int) < (y:Rational) : bool := (
     x * y.denominator < y.numerator);
export (x:int) <= (y:Rational) : bool := (
     x * y.denominator <= y.numerator);
export (x:int) > (y:Rational) : bool := (
     x * y.denominator > y.numerator);
export (x:int) >= (y:Rational) : bool := (
     x * y.denominator >= y.numerator);
export (x:Rational) < (y:int) : bool := (
     x.numerator < y * x.denominator);
export (x:Rational) <= (y:int) : bool := (
     x.numerator <= y * x.denominator);
export (x:Rational) > (y:int) : bool := (
     x.numerator > y * x.denominator);
export (x:Rational) >= (y:int) : bool := (
     x.numerator >= y * x.denominator);
export (x:double) < (y:Rational) : bool := (
     x * y.denominator < y.numerator);
export (x:double) <= (y:Rational) : bool := (
     x * y.denominator <= y.numerator);
export (x:double) > (y:Rational) : bool := (
     x * y.denominator > y.numerator);
export (x:double) >= (y:Rational) : bool := (
     x * y.denominator >= y.numerator);
export (x:Rational) < (y:double) : bool := (
     x.numerator < y * x.denominator);
export (x:Rational) <= (y:double) : bool := (
     x.numerator <= y * x.denominator);
export (x:Rational) > (y:double) : bool := (
     x.numerator > y * x.denominator);
export (x:Rational) >= (y:double) : bool := (
     x.numerator >= y * x.denominator);
export abs(x:Rational) : Rational := if x<0 then -x else x;

sm := new array(string) len 64 do (
     b := 0;
     while b < 64 do (
	  provide new string len 1 do provide char(b);
	  b = b+1;
	  );
     );
nsm := new array(string) len 64 do (
     b := 0;
     while b < 64 do (
	  provide new string len 1 do provide char(0x40 | b);
	  b = b+1;
	  );
     );

takebyte(b:array(ushort),bitindex:int):int := (
     i := bitindex / 16;
     k := bitindex - 16*i;
     l := 16 - k;
     if l >= 8 || i+1 == length(b)
     then (int(b.i) >> k)
     else (int(b.i) >> k) | (int(b.(i+1)) << l)
     );
export converttonet(i:Integer):string := (
     body := i.body;
     n := length(body);
     if n == 0 then "\0"
     else if n == 1 then (
	  b := int(body.0);
	  if b < 64 then if i.negative then nsm.b else sm.b
	  else if b < 8192 then new string len 2 do (
	       provide char(0x80 | if i.negative then 0x40 | (b>>7) else b>>7);
	       provide char(b & 0x7f);
	       )
	  else new string len 3 do (
	       provide char(0x80 | if i.negative then 0x40 | (b>>14) else b>>14);
	       provide char(0x80 | (b >> 7));
	       provide char(b & 0x7f);
	       )
	  )
     else (
	  nbits := numbits(i);
	  if nbits <= 20 then new string len 3 do (
	       b := int(body.1);
	       c := int(body.0);
	       provide char(
		    0x80 
		    | (if i.negative then 0x40 else 0) 
		    | (c>>14) 
		    | (b<<2)
		    );
	       provide char(0x80 | (c >> 7));
	       provide char(c & 0x7f);
	       )
	  else if nbits <= 27 then new string len 4 do (
	       b := int(body.1);
	       c := int(body.0);
	       provide char(
		    0x80 
		    | (if i.negative then 0x40 else 0) 
		    | (b>>5)
		    );
	       provide char(
		    0x80 
		    | (c>>14) 
		    | (b<<2)
		    );
	       provide char(0x80 | (c >> 7));
	       provide char(c & 0x7f);
	       )
	  else (
	       n2 := (nbits - 20) / 8;	  -- number of trailing bytes
	       n2b := 8 * n2;		  -- number of trailing bits
	       n1 := (numbits(n2)+6) / 7; -- number of bytes needed to represent n2
	       new string len 4 + n1 + n2 do (
		    provide char(0x80 | takebyte(body,n2b + 21)
		    	 | (if i.negative then 0x40 else 0) 
			 );
		    provide char(0x80 | takebyte(body,n2b + 14));
		    provide char(0x80 | takebyte(body,n2b +  7));
		    provide char(0x80 | takebyte(body,n2b     ));
		    if n2 < 0x80 then (
			 provide char(n2);
			 )
		    else if n2 < 0x4000 then (
			 provide char(0x80 | (n2 >> 7));
			 provide char(0x7f & n2);
			 )
		    else if n2 < 0x200000 then (
			 provide char(0x80 | (n2 >> 14));
			 provide char(0x80 | (n2 >> 7));
			 provide char(0x7f & n2);
			 )
		    else (
			 provide char(0x80 | (n2 >> 21));
			 provide char(0x80 | (n2 >> 14));
			 provide char(0x80 | (n2 >> 7));
			 provide char(0x7f & n2);
			 );
		    while n2 > 0 do (
			 n2 = n2 - 1;
			 provide char(takebyte(body,8*n2));
			 );
		    )
	       )
	  )
     );
