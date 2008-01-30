-- this file is a substitute for arith.d
-- use one or the other but not both

use C;
use err;
use system;
use stdio;
use strings;
use varstrin;

export min(x:int,y:int):int := if x<y then x else y;
export max(x:int,y:int):int := if x<y then y else x;
export min(x:uint,y:uint):uint := if x<y then x else y;
export max(x:uint,y:uint):uint := if x<y then y else x;
export min(x:long,y:long):long := if x<y then x else y;
export max(x:long,y:long):long := if x<y then y else x;
export min(x:ulong,y:ulong):ulong := if x<y then x else y;
export max(x:ulong,y:ulong):ulong := if x<y then y else x;

export (o:file) << (s:Cstring) : file := o << if s == null() then "(null)" else tostring(s);

export limbPointer := {limbPointer:void} or null;
export ZZ := { alloc:int, size:int, limbs:limbPointer};

isPositive0(x:ZZ):bool ::=  1 == Ccode(int, "mpz_sgn((__mpz_struct *)", x, ")");
isZero0    (x:ZZ):bool ::=  0 == Ccode(int, "mpz_sgn((__mpz_struct *)", x, ")");
isNegative0(x:ZZ):bool ::= -1 == Ccode(int, "mpz_sgn((__mpz_struct *)", x, ")");

export isPositive(x:ZZ):bool := isPositive0(x);
export isZero    (x:ZZ):bool := isZero0(x);
export isNegative(x:ZZ):bool := isNegative0(x);
export isEven    (x:ZZ):bool := Ccode(bool, "mpz_even_p((__mpz_struct *)", x, ")");
export isOdd     (x:ZZ):bool := Ccode(bool, "mpz_odd_p((__mpz_struct *)", x, ")");

export isInt(x:ZZ):bool := 0 != Ccode(int, "mpz_fits_sint_p((__mpz_struct *)", x, ")");
export toInt(x:ZZ):int  := int(Ccode(long, "mpz_get_si((__mpz_struct *)", x, ")"));
export isLong(x:ZZ):bool := 0 != Ccode(int, "mpz_fits_slong_p((__mpz_struct *)", x, ")");
export toLong(x:ZZ):long  := Ccode(long, "mpz_get_si((__mpz_struct *)", x, ")");
export isULong(x:ZZ):bool := 0 != Ccode(int, "mpz_fits_ulong_p((__mpz_struct *)", x, ")");
export toULong(x:ZZ):ulong  := Ccode(ulong, "mpz_get_ui((__mpz_struct *)", x, ")");

export hash(x:ZZ):int := (
     if isInt(x) then 0x7fffffff & toInt(x)
     else Ccode(int, 
     	  "mpz_hash(",					    -- see gmp_aux.c for this function
          "(__mpz_struct *)", x, 
     	  ")"));

getstr(str:Cstring, base:int, x:ZZ):Cstring ::= Ccode(Cstring,
     "(Cstring) mpz_get_str(",
	 "(char *)", str, ",",
	 base, ",",
	 "(__mpz_struct *)", x,
     ")"
     );

init(x:ZZ):void ::= Ccode( void, "mpz_init(", "(__mpz_struct *)", x, ")" );

export newZZ():ZZ := (
     x := ZZ(0,0,null());
     init(x);
     x);

set(x:ZZ, y:ZZ):void ::= Ccode( void,
     "mpz_set(",
	 "(__mpz_struct *)", x, ",",
	 "(__mpz_struct *)", y,
     ")" 
     );

export copy(i:ZZ):ZZ := (
     x := ZZ(0,0,null());
     init(x);
     set(x,i);
     x);

set(x:ZZ, n:int):void ::= Ccode( void, "mpz_set_si(", "(__mpz_struct *)", x, ",", "(long)", n, ")" );
set(x:ZZ, n:ulong):void ::= Ccode( void, "mpz_set_ui(", "(__mpz_struct *)", x, ",", n, ")" );
set(x:ZZ, n:long):void ::= Ccode( void, "mpz_set_si(", "(__mpz_struct *)", x, ",", n, ")" );

negsmall := -100;
possmall := 300;
smallints := new array(ZZ) len possmall - negsmall + 1 do for i from negsmall to possmall do (
     x := ZZ(0,0,null());
     init(x);
     set(x,i);
     provide x
     );

isSmall(x:ZZ):bool := isInt(x) && (
     i := toInt(x);
     negsmall <= i && i <= possmall);

export toInteger(i:int):ZZ := (
     if i >= negsmall && i <= possmall then smallints.(i-negsmall)
     else (
	  x := ZZ(0,0,null());
	  init(x);
	  set(x,i);
	  x));

export toInteger(i:ulong):ZZ := (
     if i <= ulong(possmall)
     then smallints.(int(i)-negsmall)
     else (
	  x := ZZ(0,0,null());
	  init(x);
	  set(x,i);
	  x));

export toInteger(i:long):ZZ := (
     if i >= long(negsmall) && i <= long(possmall)
     then smallints.(int(i)-negsmall)
     else (
	  x := ZZ(0,0,null());
	  init(x);
	  set(x,i);
	  x));

neg(x:ZZ, y:ZZ):void ::= Ccode( void,
     "mpz_neg(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y,
     ")" 
     );

export - (x:ZZ) : ZZ := (
     y := ZZ(0,0,null());
     init(y);
     neg(y,x);
     y);

abs(x:ZZ, y:ZZ):void ::= Ccode( void,
     "mpz_abs(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y,
     ")" 
     );

export abs(x:ZZ) : ZZ := (
     if isNegative0(x) then (
	  y := ZZ(0,0,null());
	  init(y);
	  abs(y,x);
	  y)
     else x);

add(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_add(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:ZZ) + (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     add(z,x,y);
     z);

add(x:ZZ, y:ZZ, z:ulong):void ::= Ccode( void,
     "mpz_add_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

sub(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_sub(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:ZZ) - (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     sub(z,x,y);
     z);

compare(x:ZZ, y:ZZ):int ::= Ccode( int,
     "mpz_cmp(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y,
     ")" 
     );
export (x:ZZ) === (y:ZZ) : bool := compare(x,y) == 0;
export (x:ZZ)  >  (y:ZZ) : bool := compare(x,y) >  0;
export (x:ZZ)  <  (y:ZZ) : bool := compare(x,y) <  0;
export (x:ZZ)  >= (y:ZZ) : bool := compare(x,y) >= 0;
export (x:ZZ)  <= (y:ZZ) : bool := compare(x,y) <= 0;

compare(x:ZZ, y:long):int ::= Ccode( int, "mpz_cmp_si(", "(__mpz_struct *)", x, ",", y, ")" );

export (x:ZZ)  >  (y:int) : bool :=  compare(x,long(y)) >  0;
export (x:ZZ)  >= (y:int) : bool :=  compare(x,long(y)) >= 0;
export (x:ZZ) === (y:int) : bool :=  compare(x,long(y)) == 0;
export (x:ZZ)  <  (y:int) : bool :=  compare(x,long(y)) <  0;
export (x:ZZ)  <= (y:int) : bool :=  compare(x,long(y)) <= 0;

export (x:int) < (y:ZZ) : bool := y > x;
export (x:int) > (y:ZZ) : bool := y < x;
export (x:int) <= (y:ZZ) : bool := y >= x;
export (x:int) >= (y:ZZ) : bool := y <= x;
export (x:int) === (y:ZZ) : bool := y === x;

sub(x:ZZ, y:ZZ, z:ulong):void ::= Ccode( void,
     "mpz_sub_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

mul(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_mul(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:ZZ) * (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     mul(z,x,y);
     z);

mul(x:ZZ, y:ZZ, z:int):void ::= Ccode( void,
     "mpz_mul_si(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

mul(x:ZZ, y:ZZ, z:ulong):void ::= Ccode( void,
     "mpz_mul_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

pow(x:ZZ, y:ZZ, n:ulong):void ::= Ccode( void,
     "mpz_pow_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 n,
     ")" 
     );

export (x:ZZ) ^ (n:ulong) : ZZ := (
     if isZero0(x) then return x;
     y := newZZ();
     pow(y,x,n);
     y);

export (x:ZZ) ^ (n:ZZ) : ZZ := (
     if isNegative0(n) then fatal("internal error: negative exponent for integer power"); -- what else can we do???
     if !isULong(n) then fatal("integer exponent too large");
     x^toULong(n));

export powermod(x:ZZ, y:ZZ, n:ZZ) : ZZ := (
     -- z = x^y mod n
     z := newZZ();
     Ccode( void, "mpz_powm(", "(__mpz_struct *)", z, ",", "(__mpz_struct *)", x, ",", "(__mpz_struct *)", y, ",", "(__mpz_struct *)", n, ")" );
     z);

cdiv(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_cdiv_q(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

fdiv(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_fdiv_q(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:ZZ) // (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     if isPositive0(y) then fdiv(z,x,y) else cdiv(z,x,y);
     z);

divexact(x:ZZ, y:ZZ):ZZ := (
     if y === 1 then return x;
     z := ZZ(0,0,null());
     init(z);
     Ccode( void,
	  "mpz_divexact(",
	     "(__mpz_struct *)", z, ",", 
	     "(__mpz_struct *)", x, ",", 
	     "(__mpz_struct *)", y,
	     ")" 
     	  );
     z);

fmod(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_fdiv_r(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

cmod(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_cdiv_r(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:ZZ) % (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     if isPositive0(y) then fmod(z,x,y) else cmod(z,x,y);
     z);

fdiv(x:ZZ, y:ZZ, z:ulong):void ::= Ccode( void,
     "mpz_fdiv_q_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

export (x:ZZ) // (y:ulong) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     fdiv(z,x,y);
     z);

export (x:ZZ) // (y:ushort) : ZZ := x // ulong(y);

fmod(y:ZZ, z:ulong):ulong ::= Ccode( ulong,
     "mpz_fdiv_ui(",
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

export (x:ZZ) % (y:ulong) : ulong := fmod(x,y);
export (x:ZZ) % (y:ushort) : ushort := ushort(x % ulong(y));


gcd(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_gcd(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export gcd(x:ZZ,y:ZZ):ZZ := (
     z := ZZ(0,0,null());
     init(z);
     gcd(z,x,y);
     z);

mul_2exp(x:ZZ, y:ZZ, z:ulong):void ::= Ccode( void,
     "mpz_mul_2exp(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

leftshift(x:ZZ,n:ulong):ZZ := (
     z := ZZ(0,0,null());
     init(z);
     mul_2exp(z,x,n);
     z);

tdiv_q_2exp(x:ZZ, y:ZZ, z:ulong):void ::= Ccode( void,
     "mpz_tdiv_q_2exp(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

rightshift(x:ZZ,n:ulong):ZZ := (
     z := ZZ(0,0,null());
     init(z);
     tdiv_q_2exp(z,x,n);
     z);

export (x:ZZ) << (n:int) : ZZ := (
     if n == 0 then x else if n > 0 then leftshift(x,ulong(n)) else rightshift(x,ulong(-n))
     );
export (x:ZZ) >> (n:int) : ZZ := (
     if n == 0 then x else if n > 0 then rightshift(x,ulong(n)) else leftshift(x,ulong(-n))
     );     

and(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_and(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );
export (x:ZZ) & (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     and(z,x,y);
     z);

ior(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_ior(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );
export (x:ZZ) | (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     ior(z,x,y);
     z);

xor(x:ZZ, y:ZZ, z:ZZ):void ::= Ccode( void,
     "mpz_xor(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );
export (x:ZZ) ^^ (y:ZZ) : ZZ := (
     z := ZZ(0,0,null());
     init(z);
     xor(z,x,y);
     z);

base := 10;
toCstring(x:ZZ):Cstring ::= getstr(Cstring(null()), base, x);
export tostring(x:ZZ):string := tostring(toCstring(x));

export (x:int) + (y:ZZ) : ZZ := toInteger(x) + y;
export (x:ZZ) + (y:int) : ZZ := x + toInteger(y);

export (x:ulong) + (y:ZZ) : ZZ := toInteger(x) + y;
export (x:ZZ) + (y:ulong) : ZZ := x + toInteger(y);

export (x:int) - (y:ZZ) : ZZ := toInteger(x) - y;
export (x:ZZ) - (y:int) : ZZ := x - toInteger(y);

export (x:int) * (y:ZZ) : ZZ := toInteger(x) * y;
export (x:ZZ) * (y:int) : ZZ := x * toInteger(y);

export (x:ulong) * (y:ZZ) : ZZ := toInteger(x) * y;
export (x:ZZ) * (y:ulong) : ZZ := x * toInteger(y);

export (x:int) ^ (y:ulong) : ZZ := toInteger(x) ^ y;

export (o:file) << (x:ZZ) : file := o << tostring(x);


-- Integers and doubles

get_d(x:ZZ):double ::= Ccode( double, "mpz_get_d(", "(__mpz_struct *)", x, ")" );
export toDouble(x:ZZ):double := get_d(x);

export (x:double) + (y:ZZ) : double := x + toDouble(y);
export (x:ZZ) + (y:double) : double := toDouble(x) + y;
export (x:double) - (y:ZZ) : double := x - toDouble(y);
export (x:ZZ) - (y:double) : double := toDouble(x) - y;
export (x:double) * (y:ZZ) : double := x * toDouble(y);
export (x:ZZ) * (y:double) : double := toDouble(x) * y;
export (x:double) / (y:ZZ) : double := x / toDouble(y);
export (x:ZZ) / (y:double) : double := toDouble(x) / y;
export (x:double) ^ (n:ZZ) : double := pow(x,toDouble(n));

export (x:ZZ) > (y:double) : bool := toDouble(x) > y;
export (x:ZZ) < (y:double) : bool := toDouble(x) < y;
export (x:ZZ) >= (y:double) : bool := toDouble(x) >= y;
export (x:ZZ) <= (y:double) : bool := toDouble(x) <= y;

export (x:double) < (y:ZZ) : bool := x < toDouble(y);
export (x:double) > (y:ZZ) : bool := x > toDouble(y);
export (x:double) <= (y:ZZ) : bool := x <= toDouble(y);
export (x:double) >= (y:ZZ) : bool := x >= toDouble(y);

logtwo := log(2.);
bigint := 2147483647.; -- 2^31-1

(x:double) << (n:int) : double ::= ldexp(x, n);
(x:double) >> (n:int) : double ::= ldexp(x,-n);

-- export Floor(x:double):ZZ := (
--      x = floor(x);
--      if x < bigint && x > -bigint
--      then toInteger(int(x))
--      else (
-- 	  wasneg := x < 0.;
-- 	  if wasneg then x = -x;
-- 	  n := 0;
-- 	  x = Ccode(double, "frexp(", x, ", &", n, ")");
-- 	  r := toInteger(0);
-- 	  while (
-- 	       i := int(floor(x));
-- 	       x = x - i;
-- 	       r = r + i;
-- 	       n > 0
-- 	       )
-- 	  do if n > 16 then (
-- 	       n = n - 16;
-- 	       x = x << 16;
-- 	       r = r << 16;
-- 	       )
-- 	  else (
-- 	       x = x << n;
-- 	       r = r << n;
-- 	       n = 0;
-- 	       );
-- 	  if wasneg then (
-- 	       r = -r;
-- 	       if x > 0. then r = r-1;
-- 	       );
-- 	  r));
-- export Round(x:double):ZZ := Floor(x + 0.5);

-----------------------------------------------------------------------------
-- rationals
-----------------------------------------------------------------------------

export QQ := {
     Nalloc:int, Nsize:int, Nlimbs:limbPointer,
     Dalloc:int, Dsize:int, Dlimbs:limbPointer
     };

export numerator(x:QQ):ZZ := (
     z := newZZ();
     Ccode( void,
	  "mpq_get_num(",
	      "(__mpz_struct *)", z, ",", 
	      "(__mpq_struct *)", x,
	  ")" 
     );
     z);

export denominator(x:QQ):ZZ := (
     z := newZZ();
     Ccode( void,
	  "mpq_get_den(",
	      "(__mpz_struct *)", z, ",", 
	      "(__mpq_struct *)", x,
	  ")" 
     );
     z);

numeratorRef  (x:QQ):ZZ ::= Ccode( ZZ,
     "(gmp_ZZ) mpq_numref(", "(__mpq_struct *)", x, ")"
     );
denominatorRef(x:QQ):ZZ ::= Ccode( ZZ, 
     "(gmp_ZZ) mpq_denref(", "(__mpq_struct *)", x, ")"
     );

export hash(x:QQ):int := hash(numeratorRef(x))+1299841*hash(denominatorRef(x));

isNegative0(x:QQ):bool := -1 == Ccode(int, "mpq_sgn((__mpq_struct*)",x,")");
export isNegative(x:QQ):bool := isNegative0(x);

init(x:QQ):void ::= Ccode( void, "mpq_init(", "(__mpq_struct *)", x, ")" );

newRational():QQ := (
     x := QQ(0,0,null(),0,0,null());
     init(x);
     x);

export newRational(i:ZZ,j:ZZ):QQ := (
     x := QQ(0,0,null(),0,0,null());
     init(x);
     set(  numeratorRef(x),i);
     set(denominatorRef(x),j);
     Ccode(void, "mpq_canonicalize((__mpq_struct *)",x,")");
     x);

export newRationalCanonical(i:ZZ,j:ZZ):QQ := ( -- assume gcd(i,j)=1, j>0, and j==1 if i==0
     x := QQ(0,0,null(),0,0,null());
     init(x);
     set(  numeratorRef(x),i);
     set(denominatorRef(x),j);
     x);

export toRational(n:int):QQ := (
     x := newRational();
     Ccode( void, "mpq_set_si(", "(__mpq_struct *)", x, ",(long)", n, ",(long)1)" );
     x);

export toRational(n:ulong):QQ := (
     x := newRational();
     Ccode( void, "mpq_set_ui(", "(__mpq_struct *)", x, ",(unsigned long)", n, ",(unsigned long)1)" );
     x);

-- integers and rationals
     
export toRational(x:ZZ):QQ := (
     z := newRational();
     Ccode(void, "mpq_set_z((__mpq_struct *)", z, ",(__mpz_struct *)", x, ")");
     z);

export floor(x:QQ):ZZ := numeratorRef(x)//denominatorRef(x);

export (x:QQ) + (y:QQ) : QQ := (
     z := newRational();
     Ccode( void,
          "mpq_add(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export - (y:QQ) : QQ := (
     z := newRational();
     Ccode( void,
	  "mpq_neg(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export abs(x:QQ) : QQ := if isNegative0(x) then -x else x;

export inv(y:QQ) : QQ := (			    -- reciprocal
     z := newRational();
     Ccode( void,
	  "mpq_inv(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:QQ) - (y:QQ) : QQ := (
     z := newRational();
     Ccode( void,
          "mpq_sub(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:QQ) * (y:QQ) : QQ := (
     z := newRational();
     Ccode( void,
          "mpq_mul(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:QQ) / (y:QQ) : QQ := (
     z := newRational();
     Ccode( void,
          "mpq_div(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:QQ) === (y:QQ) : bool := (
     Ccode( bool,
          "mpq_equal(",
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	      ")"));

export (x:QQ) + (y:ZZ ) : QQ := x + toRational(y);
export (x:QQ) + (y:int     ) : QQ := x + toRational(y);
export (x:ZZ ) + (y:QQ) : QQ := toRational(x) + y;
export (x:int     ) + (y:QQ) : QQ := toRational(x) + y;

export (x:QQ) - (y:ZZ ) : QQ := x - toRational(y);
export (x:QQ) - (y:int     ) : QQ := x - toRational(y);
export (x:ZZ ) - (y:QQ) : QQ := toRational(x) - y;
export (x:int     ) - (y:QQ) : QQ := toRational(x) - y;

export (x:QQ) * (y:ZZ ) : QQ := x * toRational(y);
export (x:QQ) * (y:int     ) : QQ := x * toRational(y);
export (x:ZZ ) * (y:QQ) : QQ := toRational(x) * y;
export (x:int     ) * (y:QQ) : QQ := toRational(x) * y;

export (x:ZZ ) / (y:ZZ ) : QQ := toRational(x)/toRational(y);
export (x:QQ) / (y:ZZ ) : QQ := x / toRational(y);
export (x:QQ) / (y:int     ) : QQ := x / toRational(y);
export (x:ZZ ) / (y:QQ) : QQ := toRational(x) / y;
export (x:int     ) / (y:QQ) : QQ := toRational(x) / y;

export (x:QQ) ^ (nn:ZZ) : QQ := (
     if !isLong(nn) then fatal("integer exponent too large");
     n := toLong(nn);
     if n == long(0) then return toRational(1);
     if n < 0 then (
	  x = inv(x);
	  n = -n);
     newRationalCanonical(numeratorRef(x)^ulong(n), denominatorRef(x)^ulong(n))
     );

export tostring(x:QQ):string := tostring(numeratorRef(x)) + '/' + tostring(denominatorRef(x));
export (o:file) << (x:QQ) : file := o << numeratorRef(x) << '/' << denominatorRef(x);

export (x:QQ) === (y:ZZ) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;
export (x:QQ) === (y:int) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;
export (y:ZZ) === (x:QQ) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;
export (y:int) === (x:QQ) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;

compare(x:QQ, y:QQ):int ::= Ccode( int, 
     "mpq_cmp((__mpq_struct *)", x, ",(__mpq_struct *)", y, ")" );
compare(x:QQ, y:ulong):int ::= Ccode( int, "mpq_cmp_ui((__mpq_struct *)", x, ",", y, ",1)");
compare(x:QQ, y: long):int ::= Ccode( int, "mpq_cmp_si((__mpq_struct *)", x, ",", y, ",1)");
compare(x:QQ, y: int):int ::= Ccode( int, "mpq_cmp_si((__mpq_struct *)", x, ",(long)", y, ",1)");

export (x:QQ) <  (y:QQ) : bool := compare(x,y) <  0;
export (x:QQ) >= (y:QQ) : bool := compare(x,y) >= 0;
export (x:QQ) >  (y:QQ) : bool := compare(x,y) >  0;
export (x:QQ) <= (y:QQ) : bool := compare(x,y) <= 0;

export (x:ZZ) <  (y:QQ) : bool := toRational(x) <  y;
export (x:ZZ) <= (y:QQ) : bool := toRational(x) <= y;
export (x:ZZ) >  (y:QQ) : bool := toRational(x) >  y;
export (x:ZZ) >= (y:QQ) : bool := toRational(x) >= y;
export (x:QQ) <  (y:ZZ) : bool := x <  toRational(y);
export (x:QQ) <= (y:ZZ) : bool := x <= toRational(y);
export (x:QQ) >  (y:ZZ) : bool := x >  toRational(y);
export (x:QQ) >= (y:ZZ) : bool := x >= toRational(y);

export (x:QQ) <  (y:int) : bool := compare(x,y) <  0;
export (x:QQ) >= (y:int) : bool := compare(x,y) >= 0;
export (x:QQ) >  (y:int) : bool := compare(x,y) >  0;
export (x:QQ) <= (y:int) : bool := compare(x,y) <= 0;

export (x:int) <  (y:QQ) : bool := y >  x;
export (x:int) <= (y:QQ) : bool := y >= x;
export (x:int) >  (y:QQ) : bool := y <  x;
export (x:int) >= (y:QQ) : bool := y <= x;

-- double and rationals

export toDouble(x:QQ):double := Ccode( double, "mpq_get_d(", "(__mpq_struct *)", x, ")" );

export (x:double) + (y:QQ) : double := x + toDouble(y);
export (x:double) - (y:QQ) : double := x - toDouble(y);
export (x:double) * (y:QQ) : double := x * toDouble(y);
export (x:double) / (y:QQ) : double := x / toDouble(y);
export (x:QQ) + (y:double) : double := toDouble(x) + y;
export (x:QQ) - (y:double) : double := toDouble(x) - y;
export (x:QQ) * (y:double) : double := toDouble(x) * y;
export (x:QQ) / (y:double) : double := toDouble(x) / y;

export (x:double) <  (y:QQ) : bool := x * denominatorRef(y) < numeratorRef(y);
export (x:double) <= (y:QQ) : bool := x * denominatorRef(y) <= numeratorRef(y);
export (x:double) >  (y:QQ) : bool := x * denominatorRef(y) > numeratorRef(y);
export (x:double) >= (y:QQ) : bool := x * denominatorRef(y) >= numeratorRef(y);
export (x:QQ) <  (y:double) : bool := numeratorRef(x) < y * denominatorRef(x);
export (x:QQ) <= (y:double) : bool := numeratorRef(x) <= y * denominatorRef(x);
export (x:QQ) >  (y:double) : bool := numeratorRef(x) > y * denominatorRef(x);
export (x:QQ) >= (y:double) : bool := numeratorRef(x) >= y * denominatorRef(x);

-----------------------------------------------------------------------------
-- big reals
-----------------------------------------------------------------------------

export RR := { RR:void };
export CC := { re:RR, im:RR };			    -- must agree with M2_CC in M2types.h

export realPart(z:CC):RR := z.re;
export imaginaryPart(z:CC):RR := z.im;

-- warning: these routines just check the sign bit, and don't verify finiteness!
isPositive0(x:RR):bool ::=  1 == Ccode(int, "mpfr_sgn((__mpfr_struct *)", x, ")");
isNegative0(x:RR):bool ::= -1 == Ccode(int, "mpfr_sgn((__mpfr_struct *)", x, ")");
isZero0    (x:RR):bool ::=  0 == Ccode(int, "mpfr_sgn((__mpfr_struct *)", x, ")");

flagged0():bool ::= 0 != Ccode( int, "mpfr_erangeflag_p()" );
isfinite0(x:RR):bool ::=Ccode(bool,"mpfr_number_p((__mpfr_struct *)",x,")");
isinf0 (x:RR):bool ::= Ccode(bool,"mpfr_inf_p((__mpfr_struct *)",x,")");
isnan0 (x:RR):bool ::= Ccode(bool,"mpfr_nan_p((__mpfr_struct *)",x,")");
sign0(x:RR):bool ::= 0 != Ccode(int,"mpfr_signbit((__mpfr_struct *)",x,")");
exponent0(x:RR):long ::= Ccode(long,"(long)mpfr_get_exp((__mpfr_struct *)",x,")"); -- sometimes int, sometimes long, see gmp.h for type mp_exp_t
sizeinbase0(x:ZZ,b:int):int ::= Ccode( int, "mpz_sizeinbase(", "(__mpz_struct *)", x, ",", b, ")" );

-- warning: these routines just check the sign bit, and don't verify finiteness!
export isPositive(x:RR):bool := isPositive0(x);
export isNegative(x:RR):bool := isNegative0(x);

export isZero    (x:RR):bool := isZero0(x) && isfinite0(x);
export isZero    (x:CC):bool := isZero0(x.re) && isfinite0(x.re) && isZero0(x.im) && isfinite0(x.im);

export defaultPrecision := ulong(53); -- should 53 be computed?

export minprec := Ccode(ulong,"MPFR_PREC_MIN");
export maxprec := Ccode(ulong,"MPFR_PREC_MAX");

export minExponent := Ccode(long,"(long)mpfr_get_emin()-1");
export maxExponent := Ccode(long,"(long)mpfr_get_emax()");

export exponent(x:ZZ):long := if isZero0(x) then minExponent else long(sizeinbase0(x,2));
export exponent(x:RR):long := if isZero0(x) && isfinite0(x) then minExponent else if isfinite0(x) then exponent0(x) else maxExponent;
export exponent(x:CC):long := max(exponent(x.re),exponent(x.im));

export newRR(prec:ulong):RR := (
     if prec < minprec then prec = minprec else if prec > maxprec then prec = maxprec;
     x := Ccode(RR, "(gmp_RR)getmem(sizeof(__mpfr_struct))");
     Ccode( void, "mpfr_init2((__mpfr_struct *)", x, ",(mpfr_prec_t)",prec,")" );
     x);
export newCC(prec:ulong):CC := CC(newRR(prec),newRR(prec));

precision0(x:RR):ulong ::= Ccode(ulong,"(unsigned long)mpfr_get_prec((__mpfr_struct *)", x, ")");
export precision(x:RR):ulong := precision0(x);
export precision(x:CC):ulong := precision0(x.re);

export toRR(x:RR,prec:ulong):RR := (
     if precision0(x) == prec then return x;
     z := newRR(prec);
     Ccode( void, "mpfr_set(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

export toRR(x:QQ,prec:ulong):RR := (
     z := newRR(prec);
     Ccode( void, "mpfr_set_q(", "(__mpfr_struct *)", z, ",", "(__mpq_struct *)", x, ", GMP_RNDN)" );
     z);

export toRR(x:QQ):RR := toRR(x,defaultPrecision);

export toRR(x:ZZ,prec:ulong):RR := (
     z := newRR(prec);
     Ccode( void, "mpfr_set_z(", "(__mpfr_struct *)", z, ",", "(__mpz_struct *)", x, ", GMP_RNDN)" );
     z);

export toRR(x:ZZ):RR := toRR(x,defaultPrecision);

export toRR(n:int,prec:ulong):RR := (
     x := newRR(prec);
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)", x, ",(long)", n, ", GMP_RNDN)" );
     x);

export toRR(n:ulong,prec:ulong):RR := (
     x := newRR(prec);
     Ccode( void, "mpfr_set_ui(", "(__mpfr_struct *)", x, ",(unsigned long)", n, ", GMP_RNDN)" );
     x);

export toRR(n:double,prec:ulong):RR := (
     x := newRR(prec);
     Ccode( void, "mpfr_set_d(", "(__mpfr_struct *)", x, ",", n, ", GMP_RNDN)" );
     x);

export toRR(n:double):RR := toRR(n,defaultPrecision);	   

export infinityRR(prec:ulong,sign:int):RR := (
     x := newRR(prec);
     Ccode(void, "mpfr_set_inf((__mpfr_struct *)",x,",",sign,")");
     x);
export infinityRR(prec:ulong):RR := infinityRR(prec,1);
export nanRR(prec:ulong):RR := (
     x := newRR(prec);
     Ccode(void, "mpfr_set_nan((__mpfr_struct *)",x,")");
     x);

export toCC(x:RR,y:RR):CC := (
     if precision0(x) == precision0(y) then CC(x,y)
     else if precision0(x) < precision0(y) then CC(x,toRR(y,precision0(x)))
     else CC(toRR(x,precision0(y)),y)
     );

export infinityCC(prec:ulong):CC := (x := infinityRR(prec,1); toCC(x,x));
export nanCC(prec:ulong):CC := (x := nanRR(prec); toCC(x,x));

export toCC(x:RR):CC := CC(x,toRR(0,precision0(x)));
export toCC(x:int,y:RR):CC := CC(toRR(x,precision0(y)),y);
export toCC(x:RR,prec:ulong):CC := CC(toRR(x,prec),toRR(0,prec));
export toCC(x:CC,prec:ulong):CC := (
     if precision0(x.re) == prec then x
     else CC(toRR(x.re,prec),toRR(x.im,prec)));
export toCC(x:RR,y:RR,prec:ulong):CC := CC(toRR(x,prec),toRR(y,prec));
export toCC(x:QQ,prec:ulong):CC := CC(toRR(x,prec),toRR(0,prec));
export toCC(x:ZZ,prec:ulong):CC := CC(toRR(x,prec),toRR(0,prec));
export toCC(x:QQ):CC := toCC(x,defaultPrecision);
export toCC(x:ZZ):CC := toCC(x,defaultPrecision);
export toCC(x:int,prec:ulong):CC := CC(toRR(x,prec),toRR(0,prec));
export toCC(x:int,y:int,prec:ulong):CC := CC(toRR(x,prec),toRR(y,prec));
export toCC(x:ulong,prec:ulong):CC := CC(toRR(x,prec),toRR(0,prec));
export toCC(x:double,prec:ulong):CC := CC(toRR(x,prec),toRR(0,prec));
export toCC(x:double,y:double,prec:ulong):CC := CC(toRR(x,prec),toRR(y,prec));

export toDouble(x:RR):double := Ccode( double, "mpfr_get_d(", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
export flagged():bool := flagged0();
export isfinite(x:RR):bool := isfinite0(x);
export isinf(x:RR):bool := isinf0(x);
export isnan(x:RR):bool := isnan0(x);
export isfinite(x:CC):bool := isfinite0(x.re) && isfinite0(x.im);
export isinf(x:CC):bool := isinf0(x.re) && !isnan0(x.im) || isinf0(x.im) && !isnan0(x.re);
export isnan(x:CC):bool := isnan0(x.re) || isnan0(x.im);

export (x:RR) === (y:RR):bool := (			    -- weak equality
     Ccode( void, "mpfr_clear_flags()" );		    -- do we need this?
     0 != Ccode( int, "mpfr_equal_p(", "(__mpfr_struct *)", x, ",", "(__mpfr_struct *)", y, ")" )
     && !flagged0()
     );

export strictequality(x:RR,y:RR):bool := (
     Ccode( void, "mpfr_clear_flags()" );		    -- do we need this?
     0 != Ccode( int, "mpfr_equal_p(", "(__mpfr_struct *)", x, ",", "(__mpfr_struct *)", y, ")" )
     && !flagged0()
     && sign0(x) == sign0(y)
     && precision0(x) == precision0(y)
     );

compare0(x:RR, y:RR):int ::= (
     Ccode( void, "mpfr_clear_flags()" );		    -- do we need this?
     Ccode( int, "mpfr_cmp(", "(__mpfr_struct *)", x, ",", "(__mpfr_struct *)", y, ")" ));
export compare(x:RR, y:RR):int := compare0(x,y);	    -- use flagged(), too!
export (x:RR)  >  (y:RR) : bool := compare0(x,y) >  0 && !flagged0();
export (x:RR)  <  (y:RR) : bool := compare0(x,y) <  0 && !flagged0();
export (x:RR)  >= (y:RR) : bool := compare0(x,y) >= 0 && !flagged0();
export (x:RR)  <= (y:RR) : bool := compare0(x,y) <= 0 && !flagged0();

compare0(x:RR, y:long):int ::= Ccode( int, "mpfr_cmp_si(", "(__mpfr_struct *)", x, ",", y, ")" );
compare0(x:RR, y:int):int ::= Ccode( int, "mpfr_cmp_si(", "(__mpfr_struct *)", x, ",(long)", y, ")" );
export compare(x:RR, y:long):int := Ccode( int, "mpfr_cmp_si(", "(__mpfr_struct *)", x, ",", y, ")" );
export compare(y:long, x:RR):int := Ccode( int, "-mpfr_cmp_si(", "(__mpfr_struct *)", x, ",", y, ")" );
export (x:RR)  >  (y:int) : bool :=  compare0(x,long(y)) >  0 && !flagged0();
export (x:RR)  >= (y:int) : bool :=  compare0(x,long(y)) >= 0 && !flagged0();
export (x:RR) === (y:int) : bool :=  compare0(x,long(y)) == 0 && !flagged0();
export (x:RR)  <  (y:int) : bool :=  compare0(x,long(y)) <  0 && !flagged0();
export (x:RR)  <= (y:int) : bool :=  compare0(x,long(y)) <= 0 && !flagged0();

export (x:CC) === (y:int) : bool :=  x.re === y && x.im === 0;

compare0(x:RR, y:double):int ::= Ccode( int, "mpfr_cmp_d(", "(__mpfr_struct *)", x, ",", y, ")" );
export compare(x:RR, y:double):int := Ccode( int, "mpfr_cmp_d(", "(__mpfr_struct *)", x, ",", y, ")" );
export compare(y:double, x:RR):int := Ccode( int, "-mpfr_cmp_d(", "(__mpfr_struct *)", x, ",", y, ")" );
export (x:RR)  >  (y:double) : bool :=  compare0(x,y) >  0 && !flagged0();
export (x:RR)  >= (y:double) : bool :=  compare0(x,y) >= 0 && !flagged0();
export (x:RR) === (y:double) : bool :=  compare0(x,y) == 0 && !flagged0();
export (x:RR)  <  (y:double) : bool :=  compare0(x,y) <  0 && !flagged0();
export (x:RR)  <= (y:double) : bool :=  compare0(x,y) <= 0 && !flagged0();

compare0(x:RR, y:ZZ):int ::= Ccode( int, "mpfr_cmp_z(", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ")" );
export compare(x:RR, y:ZZ):int := Ccode( int, "mpfr_cmp_z(", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ")" );
export compare(y:ZZ, x:RR):int := Ccode( int, "-mpfr_cmp_z(", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ")" );
export (x:RR)  >  (y:ZZ) : bool :=  compare0(x,y) >  0 && !flagged0();
export (x:RR)  >= (y:ZZ) : bool :=  compare0(x,y) >= 0 && !flagged0();
export (x:RR) === (y:ZZ) : bool :=  compare0(x,y) == 0 && !flagged0();
export (y:ZZ) === (x:RR) : bool :=  compare0(x,y) == 0 && !flagged0();
export (x:RR)  <  (y:ZZ) : bool :=  compare0(x,y) <  0 && !flagged0();
export (x:RR)  <= (y:ZZ) : bool :=  compare0(x,y) <= 0 && !flagged0();

compare0(x:RR, y:QQ):int ::= Ccode( int, "mpfr_cmp_q(", "(__mpfr_struct *)", x, ",(__mpq_struct *)", y, ")" );
export compare(x:RR, y:QQ):int := Ccode( int, "mpfr_cmp_q(", "(__mpfr_struct *)", x, ",(__mpq_struct *)", y, ")" );
export compare(y:QQ, x:RR):int := Ccode( int, "-mpfr_cmp_q(", "(__mpfr_struct *)", x, ",(__mpq_struct *)", y, ")" );
export (x:RR)  >  (y:QQ) : bool :=  compare0(x,y) >  0 && !flagged0();
export (x:RR)  >= (y:QQ) : bool :=  compare0(x,y) >= 0 && !flagged0();
export (x:RR) === (y:QQ) : bool :=  compare0(x,y) == 0 && !flagged0();
export (y:QQ) === (x:RR) : bool :=  compare0(x,y) == 0 && !flagged0();
export (x:RR)  <  (y:QQ) : bool :=  compare0(x,y) <  0 && !flagged0();
export (x:RR)  <= (y:QQ) : bool :=  compare0(x,y) <= 0 && !flagged0();

export hash(x:RR):int := int(precision0(x)) + Ccode(int, 
     "mpfr_hash(",					    -- see gmp_aux.c for this function
         "(__mpfr_struct *)", x, 
     ")"
     );
export hash(x:CC):int := 123 + hash(x.re) + 111 * hash(x.im);
     
export (x:RR) + (y:RR) : RR := (
     z := newRR(min(precision0(x),precision0(y)));
     Ccode( void,
          "mpfr_add(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	      ", GMP_RNDN)" 
     );
     z);

export (x:RR) + (y:int) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_add_si(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RR) + (y:ZZ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_add_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	      ", GMP_RNDN)" 
     );
     z);
     
export (x:RR) + (y:QQ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_add_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export - (y:RR) : RR := (
     z := newRR(precision0(y));
     Ccode( void,
	  "mpfr_neg(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RR) - (y:RR) : RR := (
     z := newRR(min(precision0(x),precision0(y)));
     Ccode( void,
          "mpfr_sub(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RR) - (y:int) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_sub_si(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      y,
	  ", GMP_RNDN)" 
     );
     z);

export (y:int) - (x:RR) : RR := -(x-y);
     
export (x:RR) - (y:ZZ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_sub_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RR) - (y:QQ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_sub_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export abs(x:RR) : RR := if isNegative0(x) then -x else x;
export (x:RR) * (y:RR) : RR := (
     z := newRR(min(precision0(x),precision0(y)));
     Ccode( void,
          "mpfr_mul(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RR) * (y:ZZ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_mul_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (y:ZZ) * (x:RR) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_mul_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RR) * (y:int) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_mul_si(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      y,
	  ", GMP_RNDN)" 
     );
     z);

export (y:int) * (x:RR) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_mul_si(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RR) * (y:QQ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_mul_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RR) / (y:RR) : RR := (
     z := newRR(min(precision0(x),precision0(y)));
     Ccode( void,
          "mpfr_div(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RR) / (y:long) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_div_si(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RR) / (y:int) : RR := x / long(y);
     
export (x:RR) / (y:ZZ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_div_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RR) / (y:QQ) : RR := (
     z := newRR(precision0(x));
     Ccode( void,
          "mpfr_div_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export sqrt(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_sqrt(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

export (x:RR) ^ (n:long) : RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_pow_si(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ",", n, ", GMP_RNDN)" );
     z);

export (x:RR) ^ (n:ulong) : RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_pow_ui(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ",", n, ", GMP_RNDN)" );
     z);

export pow10(n:ulong,prec:ulong):RR := (
     z := newRR(prec);
     Ccode( void, "mpfr_ui_pow_ui(", "(__mpfr_struct *)", z, ",", ulong(10), ",", n, ", GMP_RNDN)" );
     z);
export pow10(n:long,prec:ulong):RR := (
     ng := false;
     if n < long(0)
     then (pow10(ulong(-n),prec))^long(-1)
     else pow10(ulong(n),prec));
export pow10(n:int,prec:ulong):RR := pow10(long(n),prec);

export (n:ulong) ^ (x:RR) : RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_ui_pow(", "(__mpfr_struct *)", z, ",", n, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

export (x:RR) ^ (y:ZZ) : RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_pow_z(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ", GMP_RNDN)" );
     z);

export (x:RR) ^ (y:RR) : RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_pow(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ",(__mpfr_struct *)", y, ", GMP_RNDN)" );
     z);

export floor(x:RR) : ZZ := (
     if !isfinite0(x) then return toInteger(0);			    -- nothing else to do!
     y := newZZ();
     Ccode( void, "mpfr_get_z((__mpz_struct *)", y, ",(__mpfr_struct *)", x, ", GMP_RNDD)" );
     y);

export ceil(x:RR) : ZZ := (
     if !isfinite0(x) then return toInteger(0);			    -- nothing else to do!
     y := newZZ();
     Ccode( void, "mpfr_get_z((__mpz_struct *)", y, ",(__mpfr_struct *)", x, ", GMP_RNDU)" );
     y);

export round(x:RR) : ZZ := (
     if !isfinite0(x) then return toInteger(0);			    -- nothing else to do!
     y := newZZ();
     Ccode( void, "mpfr_get_z((__mpz_struct *)", y, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     y);

export (x:RR) << (n:long) : RR := (
     if n == long(0) then return x;
     z := newRR(precision0(x));
     Ccode( void, "mpfr_mul_2si((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ",", n, ",GMP_RNDN)" );
     z);
export (x:RR) >> (n:long) : RR := x << -n;
export (x:RR) << (n:int) : RR := x << long(n);
export (x:RR) >> (n:int) : RR := x << long(-n);

-- complex arithmetic

export (x:CC) + (y:CC) : CC := CC(x.re+y.re, x.im+y.im);
export (x:CC) - (y:CC) : CC := CC(x.re-y.re, x.im-y.im);
export (x:RR) - (y:CC) : CC := CC(x-y.re,-y.im);
export (x:int) - (y:CC) : CC := CC(x-y.re,-y.im);
export (x:CC) - (y:RR) : CC := CC(x.re-y,x.im);
export (x:CC) + (y:RR) : CC := CC(x.re+y,x.im);
export (x:RR) + (y:CC) : CC := CC(x+y.re,y.im);
export -(y:CC) : CC := CC(-y.re,-y.im);

export (x:CC) * (y:RR) : CC := (
     if isfinite0(x.re) && isfinite0(x.im) && isfinite0(y)
     then CC(x.re*y, x.im*y)
     else if isnan(x) || isnan(y) then nanCC(min(precision(x),precision(y)))
     else infinityCC(min(precision(x),precision(y))));
export (y:RR) * (x:CC) : CC := (
     if isfinite0(x.re) && isfinite0(x.im) && isfinite(y)
     then CC(x.re*y, x.im*y)
     else if isnan(x) || isnan(y) then nanCC(min(precision(x),precision(y)))
     else infinityCC(min(precision(x),precision(y))));
export (y:int) * (x:CC) : CC := (
     if isinf(x) && y != 0
     then infinityCC(precision(x))
     else CC(x.re*y, x.im*y));
export (x:CC) * (y:ZZ) : CC := (
     if isinf(x) && !isZero(y)
     then infinityCC(precision(x))
     else CC(x.re*y, x.im*y));
export (y:ZZ) * (x:CC) : CC := (
     if isinf(x) && !isZero(y)
     then infinityCC(precision(x))
     else CC(x.re*y, x.im*y));
export (x:CC) * (y:CC) : CC := (
     if isinf(x) && !isZero(y) && !isnan(y) || isinf(y) && !isZero(x) && !isnan(x)
     then infinityCC(min(precision(x),precision(y)))
     else CC(x.re*y.re-x.im*y.im, x.im*y.re+x.re*y.im));
export (x:CC) / (y:RR) : CC := (
     if isZero(y) && !isnan(x) && !isZero(x)
     then infinityCC(min(precision(x),precision(y)))
     else CC(x.re/y, x.im/y));
export (x:CC) / (y:int) : CC := (
     if y == 0 && !isnan(x) && !isZero(x)
     then infinityCC(precision(x))
     else CC(x.re/y, x.im/y));
export conj(x:CC):CC := CC(x.re,-x.im);
export norm2(x:CC):RR := x.re*x.re + x.im*x.im;

export (x:CC) << (n:long) : CC := if n == long(0) then x else CC(x.re<<n,x.im<<n);
export (x:CC) >> (n:long) : CC := if n == long(0) then x else CC(x.re>>n,x.im>>n);
export (x:CC) << (n:int) : CC := if n == 0 then x else CC(x.re<<n,x.im<<n);
export (x:CC) >> (n:int) : CC := if n == 0 then x else CC(x.re>>n,x.im>>n);

export inverse(z:CC):CC := (
     if isfinite(z) then 
     if isZero0(z.re) && isZero0(z.im) then infinityCC(precision0(z.re)) 
     else (
     	  expon := exponent(z);
     	  if expon > 10000 || expon < -10000 then z = z >> expon else expon = long(0);
     	  n2 := norm2(z);
     	  toCC((z.re/n2) >> expon, -(z.im/n2) >> expon))
     else if isinf(z) then toCC(0,0,precision(z))
     else nanCC(precision(z)));

export (x:CC) / (y:CC) : CC := x * inverse(y);
export (x:RR) / (y:CC) : CC := x * inverse(y);
export (x:ZZ) / (y:CC) : CC := x * inverse(y);
export (x:int) / (y:CC) : CC := x * inverse(y);

export strictequality(x:CC,y:CC):bool := strictequality(x.re,y.re) && strictequality(x.im,y.im);
     
export (x:CC) === (y:CC) : bool := x.re === y.re && x.im === y.im;
export (x:CC) === (y:RR) : bool := x.re === y && x.im === 0;
export (x:RR) === (y:CC) : bool := x === y.re && y.im === 0;
export (x:CC) === (y:ZZ) : bool := x.re === y && x.im === 0;
export (x:ZZ) === (y:CC) : bool := x === y.re && y.im === 0;
export (x:CC) === (y:QQ) : bool := x.re === y && x.im === 0;
export (x:QQ) === (y:CC) : bool := x === y.re && y.im === 0;

export compare(x:CC,y:CC):int := (
     r := compare(x.re,y.re);
     if flagged() || r != 0 then r
     else compare(x.im,y.im));
export compare(x:CC,y:RR):int := (
     r := compare(x.re,y);
     if flagged() || r != 0 then r
     else compare0(x.im,0));
export compare(x:RR,y:CC):int := (
     r := compare(x,y.re);
     if flagged() || r != 0 then r
     else -compare0(y.im,0));
export compare(x:CC,y:ZZ):int := (
     r := compare(x.re,y);
     if flagged() || r != 0 then r
     else compare0(x.im,0));
export compare(x:ZZ,y:CC):int := (
     r := compare(x,y.re);
     if flagged() || r != 0 then r
     else -compare0(y.im,0));
export compare(x:CC,y:QQ):int := (
     r := compare(x.re,y);
     if flagged() || r != 0 then r
     else compare0(x.im,0));
export compare(x:QQ,y:CC):int := (
     r := compare(x,y.re);
     if flagged() || r != 0 then r
     else -compare0(y.im,0));

export abs(x:CC):RR := (
     z := newRR(precision(x));
     Ccode( void, "mpfr_hypot((__mpfr_struct *)", z, ",(__mpfr_struct *)", x.re, ",(__mpfr_struct *)", x.im, ",GMP_RNDN)" );
     z);
export sqrt(x:CC):CC := (
     z := newCC(precision(x));
     Ccode( void, "mpfc_sqrt((M2_CCC)", z, ",(M2_CCC)", x, ")" );
     z);

-- real transcendental functions

export pi(prec:ulong):RR := (
     z := newRR(prec);
     Ccode( void, "mpfr_const_pi(", "(__mpfr_struct *)", z, ", GMP_RNDN)" );
     z);
export exp(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_exp((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export log(x:RR):RR := (				    -- works only if x>0
     z := newRR(precision0(x));
     Ccode( void, "mpfr_log((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export log(b:RR,x:RR):RR := (				    -- works only if x>0 and b>0
     if precision0(b) < precision0(x) then x = toRR(x,precision0(b))
     else if precision0(b) > precision0(x) then b = toRR(b,precision0(x));
     log(x)/log(b));
export sin(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_sin((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export cos(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_cos((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export tan(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_tan((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export asin(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_asin((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export acos(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_acos((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export atan(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_atan((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export atan2(y:RR,x:RR):RR := (
     -- if isZero0(x) && isZero0(y) && isfinite0(x) && isfinite0(y) then return nanRR(min(precision0(x),precision0(y)));
     z := newRR(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_atan2((__mpfr_struct *)", z, ",(__mpfr_struct *)", y, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export agm(x:RR,y:RR):RR := (
     z := newRR(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_agm((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ",(__mpfr_struct *)", y, ", GMP_RNDN)" );
     z);
export sinh(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_sinh((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export cosh(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_cosh((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export tanh(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_tanh((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export sec(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_sec((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export csc(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_csc((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export cot(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_cot((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export sech(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_sech((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export csch(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_csch((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export coth(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_coth((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export factorial(x:ulong):ZZ := (
     z := newZZ();
     Ccode( void, "mpz_fac_ui((__mpz_struct *)", z, ",", x, ")" );
     z);
export log1p(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_log1p((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export expm1(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_expm1((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export Gamma(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_gamma((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export factorial(x:RR):RR := Gamma(x+1);
export eint(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_eint((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
--export lngamma(x:RR):RR := (
--     z := newRR(precision0(x));
--     Ccode( void, "mpfr_lngamma((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
--     z);
export zeta(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_zeta((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export zeta(x:ulong,prec:ulong):RR := (
     z := newRR(prec);
     Ccode( void, "mpfr_zeta_ui((__mpfr_struct *)", z, ",", x, ", GMP_RNDN)" );
     z);
export erf(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_erf((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export erfc(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_erfc((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export j0(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_j0((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export j1(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_j1((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export jn(n:long,x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_jn((__mpfr_struct *)", z, ",",n,",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export y0(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_y0((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export y1(x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_y1((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);
export yn(n:long,x:RR):RR := (
     z := newRR(precision0(x));
     Ccode( void, "mpfr_yn((__mpfr_struct *)", z, ",",n,",(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

-- printing

getstr(returnexponent:long, base:int, sigdigs:int, x:RR):string ::= tostring(
     Ccode(Cstring, "(Cstring) mpfr_get_str((char *)0,&", returnexponent, ",",
	  base, ",(size_t)", sigdigs, ",(__mpfr_struct *)", x, ",GMP_RNDN)"));
export sign(x:RR):bool := 0 != Ccode(int,"mpfr_signbit((__mpfr_struct *)",x,")");
export format(
     s:int,			  -- number of significant digits (0 means all)
     ac:int,	    -- accuracy, how far to right of point to go (-1 means all)
     l:int,					   -- max number leading zeroes
     t:int,				    -- max number extra trailing digits
     sep:string,		     -- separator between mantissa and exponent
     x:RR						-- the number to format
     ) : string := (
     ng := sign0(x);
     if isinf0(x) then return if ng then "-infinity" else "infinity";
     if isnan0(x) then return if ng then "-NotANumber" else "NotANumber";
     sgn := "";
     if ng then (
	  sgn = "-";
	  x = -x;
	  );
     if x === 0 then return if ng then "-0" else "0";
     ex := long(0);
     mantissa := getstr(ex, base, s, x);
     nt := 0;
     for i from length(mantissa)-1 to 0 by -1 do (
	  if mantissa.i != '0' then break;
	  nt = nt + 1;
	  );
     s = length(mantissa) - nt;
     mantissa = substr(mantissa,0,s);
     pt := 0;
     if ex < 0 then (
	  t = 0;
	  if -ex <= l then (
	       l = int(-ex);
	       ex = long(0);
	       )
	  else l = 0)
     else (
	  l = 0;
	  if ex <= s then (
	       pt = int(ex);
	       ex = long(0);
	       t = 0;
	       )
	  else if ex <= s+t then (
	       pt = int(ex);
	       t = int(ex)-s;
	       ex = long(0);
	       )
	  else t = 0);
     if pt == 0 && l == 0 && ex != long(0) then (
	  pt = 1;
	  ex = ex - 1;
	  );
     manlen := length(mantissa);
     maxmanlen := ac-l+pt+ex;
     if ac != -1 && long(manlen) > maxmanlen then (
	  if maxmanlen < 0 then return "0";
	  manlen = int(ac-l+pt+ex);
	  if mantissa.manlen >= '5' then (		    -- round, at least in base 10
	       while manlen > 0 do (
		    if mantissa.(manlen-1) == '9'
		    then manlen = manlen - 1 
		    else (
		    	 mantissa.(manlen-1) = mantissa.(manlen-1) + 1;
		    	 break;
			 ));
	       if manlen == 0 then (
		    mantissa = "1";
		    manlen = 1;
		    if l > 0 then l = l-1 else ex = ex + 1 ) ) );
     if manlen == 0 then return "0";
     concatenate(array(string)(
	       sgn,
	       if pt == 0 then "." else "",
	       if l == 0 then "" else new string len l do provide '0',
	       substr(mantissa,0,pt),
	       if pt > 0 && pt < manlen then "." else "",
	       substr(mantissa,pt,manlen-pt),
	       new string len t do provide '0',
	       if ex != long(0) then sep else "",
	       if ex != long(0) then tostring(ex) else ""
	       )));

export printingPrecision := 6;				    -- 0 indicates all
export printingAccuracy := -1;				    -- -1 indicates all
export printingLeadLimit := 5;
export printingTrailLimit := 5;
export printingSeparator := "e";			    -- was "*10^"

log2ten := log(10.) / log(2.);
export tostringRR(x:RR):string := (
     prec := printingPrecision;				    -- interpret printingPrecision==0 as infinity
     meaningful := int(floor(precision0(x) / log2ten));
     if prec == 0 || prec > meaningful then prec = meaningful; -- print at most the "meaningful" digits
     format(prec,printingAccuracy,printingLeadLimit,printingTrailLimit,printingSeparator,x)
     );

export toExternalString(x:RR):string := (
     if isinf0(x) then return if x < 0 then "-infinity" else "infinity";
     if isnan0(x) then return if sign0(x) then "-NotANumber" else "NotANumber";
     ng := sign0(x);
     if ng then x = -x;
     ex := long(0);
     s := getstr(ex, base, 0, x);
     nt := 0;
     for i from length(s)-1 to 0 by -1 do (
	  if s.i != '0' then break;
	  nt = nt + 1;
	  );
     newlen := max(1,length(s) - nt);
     concatenate(array(string)(
	       if ng then "-" else "",
	       ".",
	       substr(s,0,newlen),
	       "p",
	       tostring(precision0(x)),
	       if ex != long(0) then "e" else "",
	       if ex != long(0) then tostring(ex) else ""
	       )));

export tostringCC(z:CC):string := (
     x := tostringRR(realPart(z));
     y := tostringRR(imaginaryPart(z));
     if y === "0" then return x;
     if x === "0" then return y + "*ii";
     if y === "-1" then return x + "-ii";
     if y ===  "1" then return x + "+ii";
     if y.0 == '-' then return concatenate(array(string)(x,y,"*ii"));
     concatenate(array(string)(x,"+",y,"*ii")));
export tonetCC(z:CC):string := (
     x := tostringRR(realPart(z));
     y := tostringRR(imaginaryPart(z));
     if y === "0" then return x;
     if x === "0" then return y + "i";
     if y === "-1" then return x + "-i";
     if y ===  "1" then return x + "+i";
     if y.0 == '-' then return concatenate(array(string)(x,y,"i"));
     concatenate(array(string)(x,"+",y,"i")));
export toExternalString(z:CC):string := concatenate(array(string)(
     	  "toCC(",
	  toExternalString(realPart(z)),
	  ",",
	  toExternalString(imaginaryPart(z)),
	  ")"
	  ));

-- complex transcendental functions

export exp(z:CC):CC := exp(z.re) * toCC(cos(z.im),sin(z.im));
export log(z:CC):CC := toCC(log(abs(z)),atan2(z.im,z.re));
export logc(x:RR):CC := (				    -- works also for x<0
     if x<0 then toCC(log(-x),pi(precision0(x))) else toCC(log(x)));
export logc(b:RR,x:RR):CC := (				    -- works also for x<0 or b<0
     if precision0(b) < precision0(x) then x = toRR(x,precision0(b))
     else if precision0(b) > precision0(x) then b = toRR(b,precision0(x));
     if b<0 then (
     	  if x<0 then logc(x)/logc(b) else log(x)/logc(b)
	  )
     else if x<0 then logc(x)/log(b) else toCC(log(x)/log(b)));
export log(b:CC,x:CC):CC := (
     if precision(b) < precision(x) then x = toCC(x,precision(b))
     else if precision(b) > precision(x) then b = toCC(b,precision(x));
     log(x)/log(b));
export log(b:RR,x:CC):CC := (
     if precision(b) < precision(x) then x = toCC(x,precision0(b))
     else if precision(b) > precision(x) then b = toRR(b,precision(x));
     if b<0 then log(x)/logc(b) else log(x)/log(b));
export log(b:CC,x:RR):CC := (
     if precision(b) < precision(x) then x = toRR(x,precision(b))
     else if precision(b) > precision(x) then b = toCC(b,precision(x));
     if x<0 then logc(x)/log(b) else log(x)/log(b));
export agm(x:CC,y:CC):CC := (
     if precision(y) < precision(x) then x = toCC(x,precision(y))
     else if precision(y) > precision(x) then y = toCC(y,precision(x));
     while true do (
     	  if !isfinite0(x.re) || !isfinite0(x.im) then return x;
     	  if !isfinite0(y.re) || !isfinite0(y.im) then return y;
     	  if x === 0 then return x;
     	  if y === 0 then return y;
	  t := (x+y)/2;
	  diff := x-y;
	  prec := long(precision(x));			    -- in practice, max prec is 2^31 - 1, so fits in an int, too.
	  if exponent(diff) + 3*(prec/4) < exponent(x) then return t;
	  u := sqrt(x*y);
	  x = t;
	  y = u;
	  ));

itimes(z:CC):CC := toCC(-z.im, z.re);
mitimes(z:CC):CC := toCC(z.im, -z.re);
idiv(z:CC):CC := toCC(z.im, -z.re);
eitimes(z:CC):CC := exp(itimes(z));
emitimes(z:CC):CC := exp(mitimes(z));
export cos(z:CC):CC := (eitimes(z) + emitimes(z))/2;
export sin(z:CC):CC := idiv(eitimes(z) - emitimes(z))/2;
export cot(z:CC):CC := cos(z)/sin(z);
export tan(z:CC):CC := sin(z)/cos(z);
export csc(z:CC):CC := 1/sin(z);
export sec(z:CC):CC := 1/cos(z);

export cosh(z:CC):CC := (exp(z) + exp(-z))/2;
export sinh(z:CC):CC := (exp(z) - exp(-z))/2;
export tanh(z:CC):CC := (exp(z) - exp(-z))/(exp(z) + exp(-z));
export coth(z:CC):CC := (exp(z) + exp(-z))/(exp(z) - exp(-z));
export sech(z:CC):CC := 1/cosh(z);
export csch(z:CC):CC := 1/sinh(z);

square(z:CC):CC := (
     if isfinite0(z.re) && isfinite0(z.im) then toCC(z.re^long(2)-z.im^long(2),2*z.re*z.im)
     else if isnan0(z.re) || isnan0(z.im) then nanCC(precision0(z.re))
     else infinityCC(precision0(z.re))
     );
export acos(z:CC):CC := idiv(log(z+itimes(sqrt(1-square(z)))));
export asin(z:CC):CC := idiv(log(sqrt(1-square(z))+itimes(z)));
export abs2(z:CC):RR := z.re^long(2) + z.im^long(2);
export atan(x:CC):CC := (
     if isnan(x) then return x;
     if isinf(x) then return toCC(atan(infinityRR(precision(x))));
     ss := abs2(x);
     y2 := x.im << 1;
     toCC( atan2(x.re<<1,1-ss)>>1, log((ss+1+y2)/(ss+1-y2))>>2 ));
export (x:CC) ^ (y:CC):CC := exp(log(x)*y);
export (x:CC) ^ (y:RR):CC := exp(log(x)*y);
export (x:CC) ^ (y:ZZ):CC := (
     if isZero0(y) then return toCC(1,0,precision0(x.re));
     if isZero0(x.re) && isZero0(x.im) && isfinite0(x.re) && isfinite0(x.im) then return if isNegative0(y) then infinityCC(precision0(x.re)) else x;
     if isinf(x) then return if isNegative0(y) then toCC(0,precision0(x.re)) else x;
     if isLong(y) then (
	  n := toLong(y);
     	  if n == long(0) then return toCC(1,precision(x));
	  if n == long(1) then return x;
	  if n == long(-1) then return inverse(x);
	  if n == long(2) then return square(x);
	  if n == long(-2) then return inverse(square(x));
	  -- we could do a few more of these optimizations here...
	  );
     exp(log(x)*y));
export (x:RR) ^ (y:CC):CC := if isNegative(x) then exp(log(toCC(x))*y) else exp(log(x)*y);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
