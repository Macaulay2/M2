-- this file is a substitute for arith.d
-- use one or the other but not both

use C;
use err;
use system;
use stdio;
use strings;

export min(x:int,y:int):int := if x<y then x else y;
export max(x:int,y:int):int := if x<y then y else x;

export (o:file) << (s:Cstring) : file := o << if s == null() then "(null)" else tostring(s);

export limbPointer := {limbPointer:void} or null;
export Integer := { alloc:int, size:int, limbs:limbPointer};

isPositive(x:Integer):bool ::=  1 == Ccode(int, "mpz_sgn((__mpz_struct *)", x, ")");
isZero    (x:Integer):bool ::=  0 == Ccode(int, "mpz_sgn((__mpz_struct *)", x, ")");
isNegative(x:Integer):bool ::= -1 == Ccode(int, "mpz_sgn((__mpz_struct *)", x, ")");

export isInt(x:Integer):bool := 0 != Ccode(int, "mpz_fits_sint_p((__mpz_struct *)", x, ")");
export toInt(x:Integer):int  := Ccode(int, "mpz_get_si((__mpz_struct *)", x, ")");

export hash(x:Integer):int := (
     if isInt(x) then 0x7fffffff & toInt(x)
     else Ccode(int, 
     	  "mpz_hash(",					    -- see gmp_aux.c for this function
          "(__mpz_struct *)", x, 
     	  ")"));

getstr(str:Cstring, base:int, x:Integer):Cstring ::= Ccode(Cstring,
     "(Cstring) mpz_get_str(",
	 "(char *)", str, ",",
	 base, ",",
	 "(__mpz_struct *)", x,
     ")"
     );

init(x:Integer):void ::= Ccode( void, "mpz_init(", "(__mpz_struct *)", x, ")" );

newInteger():Integer := (
     x := Integer(0,0,null());
     init(x);
     x);

sizeinbase(x:Integer,b:int):int ::= Ccode( int, "mpz_sizeinbase(", "(__mpz_struct *)", x, ",", b, ")" );

set(x:Integer, y:Integer):void ::= Ccode( void,
     "mpz_set(",
	 "(__mpz_struct *)", x, ",",
	 "(__mpz_struct *)", y,
     ")" 
     );

export copy(i:Integer):Integer := (
     x := Integer(0,0,null());
     init(x);
     set(x,i);
     x);

set(x:Integer, n:int):void ::= Ccode( void,
     "mpz_set_si(",
	 "(__mpz_struct *)", x, ",",
	 "(unsigned long)", n,
     ")" 
     );

negsmall := -100;
possmall := 300;
smallints := new array(Integer) len possmall - negsmall + 1 do for i from negsmall to possmall do (
     x := Integer(0,0,null());
     init(x);
     set(x,i);
     provide x
     );

isSmall(x:Integer):bool := isInt(x) && (
     i := toInt(x);
     negsmall <= i && i <= possmall);

export toInteger(i:int):Integer := (
     if i >= negsmall && i <= possmall then smallints.(i-negsmall)
     else (
	  x := Integer(0,0,null());
	  init(x);
	  set(x,i);
	  x));

set(x:Integer, n:ulong):void ::= Ccode( void,
     "mpz_set_ui(",
	 "(__mpz_struct *)", x, ",",
	 n,
     ")" 
     );

export toInteger(i:ulong):Integer := (
     if i <= ulong(possmall)
     then smallints.(int(i)-negsmall)
     else (
	  x := Integer(0,0,null());
	  init(x);
	  set(x,i);
	  x));

neg(x:Integer, y:Integer):void ::= Ccode( void,
     "mpz_neg(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y,
     ")" 
     );

export - (x:Integer) : Integer := (
     y := Integer(0,0,null());
     init(y);
     neg(y,x);
     y);

abs(x:Integer, y:Integer):void ::= Ccode( void,
     "mpz_abs(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y,
     ")" 
     );

export abs(x:Integer) : Integer := (
     if isNegative(x) then (
	  y := Integer(0,0,null());
	  init(y);
	  abs(y,x);
	  y)
     else x);

add(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_add(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:Integer) + (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     add(z,x,y);
     z);

add(x:Integer, y:Integer, z:ulong):void ::= Ccode( void,
     "mpz_add_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

sub(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_sub(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:Integer) - (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     sub(z,x,y);
     z);

compare(x:Integer, y:Integer):int ::= Ccode( int,
     "mpz_cmp(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y,
     ")" 
     );
export (x:Integer) === (y:Integer) : bool := compare(x,y) == 0;
export (x:Integer)  >  (y:Integer) : bool := compare(x,y) >  0;
export (x:Integer)  <  (y:Integer) : bool := compare(x,y) <  0;
export (x:Integer)  >= (y:Integer) : bool := compare(x,y) >= 0;
export (x:Integer)  <= (y:Integer) : bool := compare(x,y) <= 0;

compare(x:Integer, y:long):int ::= Ccode( int, "mpz_cmp_si(", "(__mpz_struct *)", x, ",", y, ")" );

export (x:Integer)  >  (y:int) : bool :=  compare(x,long(y)) >  0;
export (x:Integer)  >= (y:int) : bool :=  compare(x,long(y)) >= 0;
export (x:Integer) === (y:int) : bool :=  compare(x,long(y)) == 0;
export (x:Integer)  <  (y:int) : bool :=  compare(x,long(y)) <  0;
export (x:Integer)  <= (y:int) : bool :=  compare(x,long(y)) <= 0;

export (x:int) < (y:Integer) : bool := y > x;
export (x:int) > (y:Integer) : bool := y < x;
export (x:int) <= (y:Integer) : bool := y >= x;
export (x:int) >= (y:Integer) : bool := y <= x;
export (x:int) === (y:Integer) : bool := y === x;

sub(x:Integer, y:Integer, z:ulong):void ::= Ccode( void,
     "mpz_sub_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

mul(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_mul(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:Integer) * (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     mul(z,x,y);
     z);

mul(x:Integer, y:Integer, z:int):void ::= Ccode( void,
     "mpz_mul_si(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

mul(x:Integer, y:Integer, z:ulong):void ::= Ccode( void,
     "mpz_mul_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

pow(x:Integer, y:Integer, n:int):void ::= Ccode( void,
     "mpz_pow_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 n,
     ")" 
     );

export (x:Integer) ^ (n:int) : Integer := (
     if n < 0 then return toInteger(0);
     y := newInteger();
     pow(y,x,n);
     y);

export (x:Integer) ^ (n:Integer) : Integer := (
     if isNegative(n) then fatal("negative exponent for integer power"); -- what else can we do???
     if isZero(x) then return x;
     if !isInt(n) then fatal("integer exponent too large");
     x^toInt(n));

export powermod(x:Integer, y:Integer, n:Integer) : Integer := (
     -- z = x^y mod n
     z := newInteger();
     Ccode( void, "mpz_powm(", "(__mpz_struct *)", z, ",", "(__mpz_struct *)", x, ",", "(__mpz_struct *)", y, ",", "(__mpz_struct *)", n, ")" );
     z);

cdiv(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_cdiv_q(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

fdiv(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_fdiv_q(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:Integer) // (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     if isPositive(y) then fdiv(z,x,y) else cdiv(z,x,y);
     z);

divexact(x:Integer, y:Integer):Integer := (
     if y === 1 then return x;
     z := Integer(0,0,null());
     init(z);
     Ccode( void,
	  "mpz_divexact(",
	     "(__mpz_struct *)", z, ",", 
	     "(__mpz_struct *)", x, ",", 
	     "(__mpz_struct *)", y,
	     ")" 
     	  );
     z);

fmod(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_fdiv_r(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

cmod(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_cdiv_r(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export (x:Integer) % (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     if isPositive(y) then fmod(z,x,y) else cmod(z,x,y);
     z);

fdiv(x:Integer, y:Integer, z:ulong):void ::= Ccode( void,
     "mpz_fdiv_q_ui(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

export (x:Integer) // (y:ulong) : Integer := (
     z := Integer(0,0,null());
     init(z);
     fdiv(z,x,y);
     z);

export (x:Integer) // (y:ushort) : Integer := x // ulong(y);

fmod(y:Integer, z:ulong):ulong ::= Ccode( ulong,
     "mpz_fdiv_ui(",
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

export (x:Integer) % (y:ulong) : ulong := fmod(x,y);
export (x:Integer) % (y:ushort) : ushort := ushort(x % ulong(y));


gcd(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_gcd(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );

export gcd(x:Integer,y:Integer):Integer := (
     z := Integer(0,0,null());
     init(z);
     gcd(z,x,y);
     z);

mul_2exp(x:Integer, y:Integer, z:ulong):void ::= Ccode( void,
     "mpz_mul_2exp(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

leftshift(x:Integer,n:ulong):Integer := (
     z := Integer(0,0,null());
     init(z);
     mul_2exp(z,x,n);
     z);

tdiv_q_2exp(x:Integer, y:Integer, z:ulong):void ::= Ccode( void,
     "mpz_tdiv_q_2exp(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 z,
     ")" 
     );

rightshift(x:Integer,n:ulong):Integer := (
     z := Integer(0,0,null());
     init(z);
     tdiv_q_2exp(z,x,n);
     z);

export (x:Integer) << (n:int) : Integer := (
     if n >= 0 then leftshift(x,ulong(n)) else rightshift(x,ulong(-n))
     );
export (x:Integer) >> (n:int) : Integer := (
     if n >= 0 then rightshift(x,ulong(n)) else leftshift(x,ulong(-n))
     );     

and(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_and(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );
export (x:Integer) & (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     and(z,x,y);
     z);

ior(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_ior(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );
export (x:Integer) | (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     ior(z,x,y);
     z);

xor(x:Integer, y:Integer, z:Integer):void ::= Ccode( void,
     "mpz_xor(",
	 "(__mpz_struct *)", x, ",", 
	 "(__mpz_struct *)", y, ",", 
	 "(__mpz_struct *)", z,
     ")" 
     );
export (x:Integer) ^^ (y:Integer) : Integer := (
     z := Integer(0,0,null());
     init(z);
     xor(z,x,y);
     z);

base := 10;
toCstring(x:Integer):Cstring ::= getstr(Cstring(null()), base, x);
export tostring(x:Integer):string := tostring(toCstring(x));

export (x:int) + (y:Integer) : Integer := toInteger(x) + y;
export (x:Integer) + (y:int) : Integer := x + toInteger(y);

export (x:ulong) + (y:Integer) : Integer := toInteger(x) + y;
export (x:Integer) + (y:ulong) : Integer := x + toInteger(y);

export (x:int) - (y:Integer) : Integer := toInteger(x) - y;
export (x:Integer) - (y:int) : Integer := x - toInteger(y);

export (x:int) * (y:Integer) : Integer := toInteger(x) * y;
export (x:Integer) * (y:int) : Integer := x * toInteger(y);

export (x:ulong) * (y:Integer) : Integer := toInteger(x) * y;
export (x:Integer) * (y:ulong) : Integer := x * toInteger(y);

export (x:int) ^ (y:int) : Integer := toInteger(x) ^ y;

export (o:file) << (x:Integer) : file := o << tostring(x);


-- Integers and doubles

get_d(x:Integer):double ::= Ccode( double, "mpz_get_d(", "(__mpz_struct *)", x, ")" );
export toDouble(x:Integer):double := get_d(x);

export (x:double) + (y:Integer) : double := x + toDouble(y);
export (x:Integer) + (y:double) : double := toDouble(x) + y;
export (x:double) - (y:Integer) : double := x - toDouble(y);
export (x:Integer) - (y:double) : double := toDouble(x) - y;
export (x:double) * (y:Integer) : double := x * toDouble(y);
export (x:Integer) * (y:double) : double := toDouble(x) * y;
export (x:double) / (y:Integer) : double := x / toDouble(y);
export (x:Integer) / (y:double) : double := toDouble(x) / y;
export (x:double) ^ (n:Integer) : double := pow(x,toDouble(n));

export (x:Integer) > (y:double) : bool := toDouble(x) > y;
export (x:Integer) < (y:double) : bool := toDouble(x) < y;
export (x:Integer) >= (y:double) : bool := toDouble(x) >= y;
export (x:Integer) <= (y:double) : bool := toDouble(x) <= y;

export (x:double) < (y:Integer) : bool := x < toDouble(y);
export (x:double) > (y:Integer) : bool := x > toDouble(y);
export (x:double) <= (y:Integer) : bool := x <= toDouble(y);
export (x:double) >= (y:Integer) : bool := x >= toDouble(y);

logtwo := log(2.);
bigint := 2147483647.; -- 2^31-1

(x:double) << (n:int) : double ::= ldexp(x, n);
(x:double) >> (n:int) : double ::= ldexp(x,-n);

export Floor(x:double):Integer := (
     x = floor(x);
     if x < bigint && x > -bigint
     then toInteger(int(x))
     else (
	  wasneg := x < 0.;
	  if wasneg then x = -x;
	  n := 0;
	  x = Ccode(double, "frexp(", x, ", &", n, ")");
	  r := toInteger(0);
	  while (
	       i := int(floor(x));
	       x = x - i;
	       r = r + i;
	       n > 0
	       )
	  do if n > 16 then (
	       n = n - 16;
	       x = x << 16;
	       r = r << 16;
	       )
	  else (
	       x = x << n;
	       r = r << n;
	       n = 0;
	       );
	  if wasneg then (
	       r = -r;
	       if x > 0. then r = r-1;
	       );
	  r));
export Round(x:double):Integer := Floor(x + 0.5);

-----------------------------------------------------------------------------
-- rationals
-----------------------------------------------------------------------------

export Rational := {
     Nalloc:int, Nsize:int, Nlimbs:limbPointer,
     Dalloc:int, Dsize:int, Dlimbs:limbPointer
     };

export numerator(x:Rational):Integer := (
     z := newInteger();
     Ccode( void,
	  "mpq_get_num(",
	      "(__mpz_struct *)", z, ",", 
	      "(__mpq_struct *)", x,
	  ")" 
     );
     z);

export denominator(x:Rational):Integer := (
     z := newInteger();
     Ccode( void,
	  "mpq_get_den(",
	      "(__mpz_struct *)", z, ",", 
	      "(__mpq_struct *)", x,
	  ")" 
     );
     z);

numeratorRef  (x:Rational):Integer ::= Ccode( Integer,
     "(gmp_Integer) mpq_numref(", "(__mpq_struct *)", x, ")"
     );
denominatorRef(x:Rational):Integer ::= Ccode( Integer, 
     "(gmp_Integer) mpq_denref(", "(__mpq_struct *)", x, ")"
     );

export hash(x:Rational):int := hash(numeratorRef(x))+1299841*hash(denominatorRef(x));

isNegative(x:Rational):bool := -1 == Ccode(int, "mpq_sgn((__mpq_struct*)",x,")");

init(x:Rational):void ::= Ccode( void, "mpq_init(", "(__mpq_struct *)", x, ")" );

newRational():Rational := (
     x := Rational(0,0,null(),0,0,null());
     init(x);
     x);

export newRational(i:Integer,j:Integer):Rational := (
     x := Rational(0,0,null(),0,0,null());
     init(x);
     set(  numeratorRef(x),i);
     set(denominatorRef(x),j);
     Ccode(void, "mpq_canonicalize((__mpq_struct *)",x,")");
     x);

export newRationalCanonical(i:Integer,j:Integer):Rational := ( -- assume gcd(i,j)=1, j>0, and j==1 if i==0
     x := Rational(0,0,null(),0,0,null());
     init(x);
     set(  numeratorRef(x),i);
     set(denominatorRef(x),j);
     x);

export toRational(n:int):Rational := (
     x := newRational();
     Ccode( void, "mpq_set_si(", "(__mpq_struct *)", x, ",(long)", n, ",(long)1)" );
     x);

export toRational(n:ulong):Rational := (
     x := newRational();
     Ccode( void, "mpq_set_ui(", "(__mpq_struct *)", x, ",(unsigned long)", n, ",(unsigned long)1)" );
     x);

-- integers and rationals
     
export toRational(x:Integer):Rational := (
     z := newRational();
     Ccode(void, "mpq_set_z((__mpq_struct *)", z, ",(__mpz_struct *)", x, ")");
     z);

export floor(x:Rational):Integer := numeratorRef(x)//denominatorRef(x);

export (x:Rational) + (y:Rational) : Rational := (
     z := newRational();
     Ccode( void,
          "mpq_add(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export - (y:Rational) : Rational := (
     z := newRational();
     Ccode( void,
	  "mpq_neg(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export abs(x:Rational) : Rational := if isNegative(x) then -x else x;

export inv(y:Rational) : Rational := (			    -- reciprocal
     z := newRational();
     Ccode( void,
	  "mpq_inv(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:Rational) - (y:Rational) : Rational := (
     z := newRational();
     Ccode( void,
          "mpq_sub(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:Rational) * (y:Rational) : Rational := (
     z := newRational();
     Ccode( void,
          "mpq_mul(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:Rational) / (y:Rational) : Rational := (
     z := newRational();
     Ccode( void,
          "mpq_div(",
	      "(__mpq_struct *)", z, ",", 
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ")" 
     );
     z);

export (x:Rational) === (y:Rational) : bool := (
     Ccode( bool,
          "mpq_equal(",
	      "(__mpq_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	      ")"));

export (x:Rational) + (y:Integer ) : Rational := x + toRational(y);
export (x:Rational) + (y:int     ) : Rational := x + toRational(y);
export (x:Integer ) + (y:Rational) : Rational := toRational(x) + y;
export (x:int     ) + (y:Rational) : Rational := toRational(x) + y;

export (x:Rational) - (y:Integer ) : Rational := x - toRational(y);
export (x:Rational) - (y:int     ) : Rational := x - toRational(y);
export (x:Integer ) - (y:Rational) : Rational := toRational(x) - y;
export (x:int     ) - (y:Rational) : Rational := toRational(x) - y;

export (x:Rational) * (y:Integer ) : Rational := x * toRational(y);
export (x:Rational) * (y:int     ) : Rational := x * toRational(y);
export (x:Integer ) * (y:Rational) : Rational := toRational(x) * y;
export (x:int     ) * (y:Rational) : Rational := toRational(x) * y;

export (x:Integer ) / (y:Integer ) : Rational := toRational(x)/toRational(y);
export (x:Rational) / (y:Integer ) : Rational := x / toRational(y);
export (x:Rational) / (y:int     ) : Rational := x / toRational(y);
export (x:Integer ) / (y:Rational) : Rational := toRational(x) / y;
export (x:int     ) / (y:Rational) : Rational := toRational(x) / y;

export (x:Rational) ^ (nn:Integer) : Rational := (
     if !isInt(nn) then fatal("integer exponent too large");
     n := toInt(nn);
     if n == 0 then return toRational(1);
     if n < 0 then (
	  x = inv(x);
	  n = -n);
     newRationalCanonical(numeratorRef(x)^n, denominatorRef(x)^n)
     );

export tostring(x:Rational):string := tostring(numeratorRef(x)) + '/' + tostring(denominatorRef(x));
export (o:file) << (x:Rational) : file := o << numeratorRef(x) << '/' << denominatorRef(x);

export (x:Rational) === (y:Integer) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;
export (x:Rational) === (y:int) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;
export (y:Integer) === (x:Rational) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;
export (y:int) === (x:Rational) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;

compare(x:Rational, y:Rational):int ::= Ccode( int, 
     "mpq_cmp((__mpq_struct *)", x, ",(__mpq_struct *)", y, ")" );
compare(x:Rational, y:ulong):int ::= Ccode( int, "mpq_cmp_ui((__mpq_struct *)", x, ",", y, ",1)");
compare(x:Rational, y: long):int ::= Ccode( int, "mpq_cmp_si((__mpq_struct *)", x, ",", y, ",1)");
compare(x:Rational, y: int):int ::= Ccode( int, "mpq_cmp_si((__mpq_struct *)", x, ",(long)", y, ",1)");

export (x:Rational) <  (y:Rational) : bool := compare(x,y) <  0;
export (x:Rational) >= (y:Rational) : bool := compare(x,y) >= 0;
export (x:Rational) >  (y:Rational) : bool := compare(x,y) >  0;
export (x:Rational) <= (y:Rational) : bool := compare(x,y) <= 0;

export (x:Integer) <  (y:Rational) : bool := toRational(x) <  y;
export (x:Integer) <= (y:Rational) : bool := toRational(x) <= y;
export (x:Integer) >  (y:Rational) : bool := toRational(x) >  y;
export (x:Integer) >= (y:Rational) : bool := toRational(x) >= y;
export (x:Rational) <  (y:Integer) : bool := x <  toRational(y);
export (x:Rational) <= (y:Integer) : bool := x <= toRational(y);
export (x:Rational) >  (y:Integer) : bool := x >  toRational(y);
export (x:Rational) >= (y:Integer) : bool := x >= toRational(y);

export (x:Rational) <  (y:int) : bool := compare(x,y) <  0;
export (x:Rational) >= (y:int) : bool := compare(x,y) >= 0;
export (x:Rational) >  (y:int) : bool := compare(x,y) >  0;
export (x:Rational) <= (y:int) : bool := compare(x,y) <= 0;

export (x:int) <  (y:Rational) : bool := y >  x;
export (x:int) <= (y:Rational) : bool := y >= x;
export (x:int) >  (y:Rational) : bool := y <  x;
export (x:int) >= (y:Rational) : bool := y <= x;

-- double and rationals

export toDouble(x:Rational):double := Ccode( double, "mpq_get_d(", "(__mpq_struct *)", x, ")" );

export (x:double) + (y:Rational) : double := x + toDouble(y);
export (x:double) - (y:Rational) : double := x - toDouble(y);
export (x:double) * (y:Rational) : double := x * toDouble(y);
export (x:double) / (y:Rational) : double := x / toDouble(y);
export (x:Rational) + (y:double) : double := toDouble(x) + y;
export (x:Rational) - (y:double) : double := toDouble(x) - y;
export (x:Rational) * (y:double) : double := toDouble(x) * y;
export (x:Rational) / (y:double) : double := toDouble(x) / y;

export (x:double) <  (y:Rational) : bool := x * denominatorRef(y) < numeratorRef(y);
export (x:double) <= (y:Rational) : bool := x * denominatorRef(y) <= numeratorRef(y);
export (x:double) >  (y:Rational) : bool := x * denominatorRef(y) > numeratorRef(y);
export (x:double) >= (y:Rational) : bool := x * denominatorRef(y) >= numeratorRef(y);
export (x:Rational) <  (y:double) : bool := numeratorRef(x) < y * denominatorRef(x);
export (x:Rational) <= (y:double) : bool := numeratorRef(x) <= y * denominatorRef(x);
export (x:Rational) >  (y:double) : bool := numeratorRef(x) > y * denominatorRef(x);
export (x:Rational) >= (y:double) : bool := numeratorRef(x) >= y * denominatorRef(x);

-----------------------------------------------------------------------------
-- big reals
-----------------------------------------------------------------------------

export RRR := { prec:int, sign:int, exp:int, limbs:limbPointer }; -- must agree with __mpfr_struct in mpfr.h, and with M2_RRR in M2types.h
export CCC := { 					    -- must agree with M2_CCC in M2types.h
     REprec:int, REsign:int, REexp:int, RElimbs:limbPointer,
     IMprec:int, IMsign:int, IMexp:int, IMlimbs:limbPointer
     };

export realPart(z:CCC):RRR := RRR(z.REprec, z.REsign, z.REexp, z.RElimbs);
export imaginaryPart(z:CCC):RRR := RRR(z.IMprec, z.IMsign, z.IMexp, z.IMlimbs);
export bigComplex(x:RRR,y:RRR):CCC := CCC(x.prec, x.sign, x.exp, x.limbs, y.prec, y.sign, y.exp, y.limbs);

isPositive(x:RRR):bool ::=  1 == Ccode(int, "mpfr_sgn((__mpfr_struct *)", x, ")");
isZero    (x:RRR):bool ::=  0 == Ccode(int, "mpfr_sgn((__mpfr_struct *)", x, ")");
isNegative(x:RRR):bool ::= -1 == Ccode(int, "mpfr_sgn((__mpfr_struct *)", x, ")");

export precision(x:RRR):int := int(Ccode(ulong, "mpfr_get_prec((__mpfr_struct*)",x,")"));

BitsPerLimb := int(Ccode(int,"__GMP_BITS_PER_MP_LIMB"));
accuracy(x:RRR):int := BitsPerLimb * (x.prec - x.exp);
size(x:RRR):int := BitsPerLimb * x.exp;

newBigReal(prec:int):RRR := (
     x := RRR(0,0,0,null());
     Ccode( void, "mpfr_init2(", "(__mpfr_struct *)", x, ",",prec,")" );
     x);

newBigComplex(prec:int):CCC := (
     x := CCC(0,0,0,null(),0,0,0,null());
     Ccode( void, "mpfr_init2(", "(__mpfr_struct *)", x, ",",prec,")" );
     Ccode( void, "mpfr_init2(", "(__mpfr_struct *)&", x, "->IMprec,",prec,")" );
     x);

export toBigReal(x:RRR,prec:int):RRR := (
     z := newBigReal(prec);
     Ccode( void, "mpfr_set(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

export toBigReal(x:Rational,prec:int):RRR := (
     z := newBigReal(prec);
     Ccode( void, "mpfr_set_q(", "(__mpfr_struct *)", z, ",", "(__mpq_struct *)", x, ", GMP_RNDN)" );
     z);

export toBigReal(x:Integer,prec:int):RRR := (
     z := newBigReal(prec);
     Ccode( void, "mpfr_set_z(", "(__mpfr_struct *)", z, ",", "(__mpz_struct *)", x, ", GMP_RNDN)" );
     z);

export toBigReal(n:int,prec:int):RRR := (
     x := newBigReal(prec);
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)", x, ",(long)", n, ", GMP_RNDN)" );
     x);

export toBigReal(n:ulong,prec:int):RRR := (
     x := newBigReal(prec);
     Ccode( void, "mpfr_set_ui(", "(__mpfr_struct *)", x, ",(unsigned long)", n, ", GMP_RNDN)" );
     x);

export toBigReal(n:double,prec:int):RRR := (
     x := newBigReal(prec);
     Ccode( void, "mpfr_set_d(", "(__mpfr_struct *)", x, ",", n, ", GMP_RNDN)" );
     x);

export toBigComplex(x:RRR,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)&", z, "->IMprec,", 0, ", GMP_RNDN)" );
     z);

export toBigComplex(x:CCC,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set(", "(__mpfr_struct *)&", z, "->IMprec,", "(__mpfr_struct *)&", x, "->IMprec, GMP_RNDN)" );
     z);

export toBigComplex(x:RRR,y:RRR,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set(", "(__mpfr_struct *)&", z, "->IMprec,", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

export toBigComplex(x:Rational,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set_q(", "(__mpfr_struct *)", z, ",", "(__mpq_struct *)", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)&", z, "->IMprec,", 0, ", GMP_RNDN)" );
     z);

export toBigComplex(x:Integer,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set_z(", "(__mpfr_struct *)", z, ",", "(__mpz_struct *)", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)&", z, "->IMprec,", 0, ", GMP_RNDN)" );
     z);

export toBigComplex(x:int,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)", z, ",(long)", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)&", z, "->IMprec,", 0, ", GMP_RNDN)" );
     z);

export toBigComplex(x:ulong,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set_ui(", "(__mpfr_struct *)", z, ",(unsigned long)", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)&", z, "->IMprec,", 0, ", GMP_RNDN)" );
     z);

export toBigComplex(x:double,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set_d(", "(__mpfr_struct *)", z, ",", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set_si(", "(__mpfr_struct *)&", z, "->IMprec,", 0, ", GMP_RNDN)" );
     z);

export toBigComplex(x:double,y:double,prec:int):CCC := (
     z := newBigComplex(prec);
     Ccode( void, "mpfr_set_d(", "(__mpfr_struct *)", z, ",", x, ", GMP_RNDN)" );
     Ccode( void, "mpfr_set_d(", "(__mpfr_struct *)&", z, "->IMprec,", y, ", GMP_RNDN)" );
     z);

export toDouble(x:RRR):double := Ccode( double, "mpfr_get_d(", "(__mpfr_struct *)", x, ", GMP_RNDN)" );

getstr(str:Cstring, exp:long, base:int, digits:int, x:RRR):Cstring ::= Ccode(Cstring,
     "(Cstring) mpfr_get_str(",
	 "(char *)", str, ",",				    -- target string
	 "&", exp, ",",					   -- target exponent
	 base, ",",					    -- base
	 "(size_t)", digits, ",",	      -- number of digits to generate
	 "(__mpfr_struct *)", x,
     ", GMP_RNDN)"
     );

export tostring(x:RRR):string := (
     e := long(0);
     s := getstr(Cstring(null()), e, base, 0, x);
     "."+tostring(s) + "*10^" + tostring(int(e)));

export tostring(z:CCC):string := tostring(realPart(z)) + " + " + tostring(imaginaryPart(z)) + " * ii";

export tostring(x:RRR,digits:int):string := (
     e := long(0);
     s := getstr(Cstring(null()), e, base, digits, x);
     "."+tostring(s) + "*10^" + tostring(int(e)));

compare0(x:RRR, y:RRR):int ::= Ccode( int, "mpfr_cmp(", "(__mpfr_struct *)", x, ",", "(__mpfr_struct *)", y, ")" );
export compare(x:RRR, y:RRR):int := Ccode( int, "mpfr_cmp(", "(__mpfr_struct *)", x, ",", "(__mpfr_struct *)", y, ")" );
export (x:RRR) === (y:RRR) : bool := compare0(x,y) == 0;
export (x:RRR)  >  (y:RRR) : bool := compare0(x,y) >  0;
export (x:RRR)  <  (y:RRR) : bool := compare0(x,y) <  0;
export (x:RRR)  >= (y:RRR) : bool := compare0(x,y) >= 0;
export (x:RRR)  <= (y:RRR) : bool := compare0(x,y) <= 0;

compare0(x:RRR, y:long):int ::= Ccode( int, "mpfr_cmp_si(", "(__mpfr_struct *)", x, ",", y, ")" );
export compare(x:RRR, y:long):int := Ccode( int, "mpfr_cmp_si(", "(__mpfr_struct *)", x, ",", y, ")" );
export compare(y:long, x:RRR):int := Ccode( int, "-mpfr_cmp_si(", "(__mpfr_struct *)", x, ",", y, ")" );
export (x:RRR)  >  (y:int) : bool :=  compare0(x,long(y)) >  0;
export (x:RRR)  >= (y:int) : bool :=  compare0(x,long(y)) >= 0;
export (x:RRR) === (y:int) : bool :=  compare0(x,long(y)) == 0;
export (x:RRR)  <  (y:int) : bool :=  compare0(x,long(y)) <  0;
export (x:RRR)  <= (y:int) : bool :=  compare0(x,long(y)) <= 0;

compare0(x:RRR, y:double):int ::= Ccode( int, "mpfr_cmp_d(", "(__mpfr_struct *)", x, ",", y, ")" );
export compare(x:RRR, y:double):int := Ccode( int, "mpfr_cmp_d(", "(__mpfr_struct *)", x, ",", y, ")" );
export compare(y:double, x:RRR):int := Ccode( int, "-mpfr_cmp_d(", "(__mpfr_struct *)", x, ",", y, ")" );
export (x:RRR)  >  (y:double) : bool :=  compare0(x,y) >  0;
export (x:RRR)  >= (y:double) : bool :=  compare0(x,y) >= 0;
export (x:RRR) === (y:double) : bool :=  compare0(x,y) == 0;
export (x:RRR)  <  (y:double) : bool :=  compare0(x,y) <  0;
export (x:RRR)  <= (y:double) : bool :=  compare0(x,y) <= 0;

compare0(x:RRR, y:Integer):int ::= Ccode( int, "mpfr_cmp_z(", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ")" );
export compare(x:RRR, y:Integer):int := Ccode( int, "mpfr_cmp_z(", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ")" );
export compare(y:Integer, x:RRR):int := Ccode( int, "-mpfr_cmp_z(", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ")" );
export (x:RRR)  >  (y:Integer) : bool :=  compare0(x,y) >  0;
export (x:RRR)  >= (y:Integer) : bool :=  compare0(x,y) >= 0;
export (x:RRR) === (y:Integer) : bool :=  compare0(x,y) == 0;
export (x:RRR)  <  (y:Integer) : bool :=  compare0(x,y) <  0;
export (x:RRR)  <= (y:Integer) : bool :=  compare0(x,y) <= 0;

compare0(x:RRR, y:Rational):int ::= Ccode( int, "mpfr_cmp_q(", "(__mpfr_struct *)", x, ",(__mpq_struct *)", y, ")" );
export compare(x:RRR, y:Rational):int := Ccode( int, "mpfr_cmp_q(", "(__mpfr_struct *)", x, ",(__mpq_struct *)", y, ")" );
export compare(y:Rational, x:RRR):int := Ccode( int, "-mpfr_cmp_q(", "(__mpfr_struct *)", x, ",(__mpq_struct *)", y, ")" );
export (x:RRR)  >  (y:Rational) : bool :=  compare0(x,y) >  0;
export (x:RRR)  >= (y:Rational) : bool :=  compare0(x,y) >= 0;
export (x:RRR) === (y:Rational) : bool :=  compare0(x,y) == 0;
export (x:RRR)  <  (y:Rational) : bool :=  compare0(x,y) <  0;
export (x:RRR)  <= (y:Rational) : bool :=  compare0(x,y) <= 0;

export (x:CCC) === (y:CCC) : bool := (
     0 == Ccode( int, "mpfr_cmp((__mpfr_struct *)", x, ",(__mpfr_struct *)", y, ")" )
     &&
     0 == Ccode( int, "mpfr_cmp((__mpfr_struct *)(&", x, "->IMprec),(__mpfr_struct *)(&", y, "->IMprec))" )
     );

export hash(x:RRR):int := Ccode(int, 
     "mpfr_hash(",					    -- see gmp_aux.c for this function
         "(__mpfr_struct *)", x, 
     ")"
     );
export hash(x:CCC):int := (
     Ccode(int, "mpfr_hash(", "(__mpfr_struct *)", x, ")" )
     + 111 * 
     Ccode(int, "mpfr_hash(", "(__mpfr_struct *)(&", x, "->IMprec))" )
     );
     
export (x:RRR) + (y:RRR) : RRR := (
     z := newBigReal(min(x.prec,y.prec));
     Ccode( void,
          "mpfr_add(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) + (y:Integer) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_add_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) + (y:Rational) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_add_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RRR) - (y:RRR) : RRR := (
     z := newBigReal(min(x.prec,y.prec));
     Ccode( void,
          "mpfr_sub(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) - (y:Integer) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_sub_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) - (y:Rational) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_sub_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export - (y:RRR) : RRR := (
     z := newBigReal(y.prec);
     Ccode( void,
	  "mpfr_neg(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export abs(x:RRR) : RRR := if isNegative(x) then -x else x;

export (x:RRR) * (y:RRR) : RRR := (
     z := newBigReal(min(x.prec,y.prec));
     Ccode( void,
          "mpfr_mul(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) * (y:Integer) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_mul_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) * (y:Rational) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_mul_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export (x:RRR) / (y:RRR) : RRR := (
     z := newBigReal(min(x.prec,y.prec));
     Ccode( void,
          "mpfr_div(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpfr_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) / (y:Integer) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_div_z(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpz_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);
     
export (x:RRR) / (y:Rational) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void,
          "mpfr_div_q(",
	      "(__mpfr_struct *)", z, ",", 
	      "(__mpfr_struct *)", x, ",", 
	      "(__mpq_struct *)", y,
	  ", GMP_RNDN)" 
     );
     z);

export sqrt(x:RRR):RRR := (
     z := newBigReal(x.prec);
     Ccode( void, "mpfr_sqrt(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

export (x:RRR) ^ (n:int) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void, "mpfr_pow_si(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ",", n, ", GMP_RNDN)" );
     z);

export (n:uint) ^ (x:RRR) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void, "mpfr_ui_pow(", "(__mpfr_struct *)", z, ",", n, ",", "(__mpfr_struct *)", x, ", GMP_RNDN)" );
     z);

export (x:RRR) ^ (y:Integer) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void, "mpfr_pow_z(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ",(__mpz_struct *)", y, ", GMP_RNDN)" );
     z);

export (x:RRR) ^ (y:RRR) : RRR := (
     z := newBigReal(x.prec);
     Ccode( void, "mpfr_pow(", "(__mpfr_struct *)", z, ",", "(__mpfr_struct *)", x, ",(__mpfr_struct *)", y, ", GMP_RNDN)" );
     z);

export floor(x:RRR) : Integer := (
-- - Function: void mpz_set_f (mpz_t ROP, mpfr_t OP)
--     Set the value of ROP from OP.

--  - Function: void mpfr_ceil (mpfr_t ROP, mpfr_t OP)
--  - Function: void mpfr_floor (mpfr_t ROP, mpfr_t OP)
--  - Function: void mpfr_trunc (mpfr_t ROP, mpfr_t OP)
--      Set ROP to OP rounded to an integer.  `mpfr_ceil' rounds to the
--      next higher integer, `mpfr_floor' to the next lower, and `mpfr_trunc'
--      to the integer towards zero.

     z := newBigReal(x.prec);
     Ccode( void, "mpfr_floor((__mpfr_struct *)", z, ",(__mpfr_struct *)", x, ")" );
     y := newInteger();
     Ccode( void, "mpfr_get_z((__mpz_struct *)", y, ",(__mpfr_struct *)", z, ", GMP_RNDN)" );
     y);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
