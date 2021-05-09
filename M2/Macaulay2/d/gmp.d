--This file contains gmp declarations and elementary functions.
--Functions in this file should not need to make calls to stdio.

use arithmetic;
use stdiop0;

declarations "
  #include <M2/math-include.h>
";

header "
  #include \"gmp_aux.h\"
  #include <M2mem.h>
";

-- We introduce two types of big gmp-type integers here.  One type is mutable, and the vector of limbs gets
-- allocated with the standard memory allocator used by libgmp (or by its replacement, libmpir), when we use
-- gmp routines to create the integers.  The other type is immutable, and the limbs are allocated with libgc
-- by us in final step after the computation.  The types ZZmutable and ZZ are distinct in the D language, so
-- neither one can be used as the other, but the underlying pointer types are the same, except that mpz_srcptr
-- is const, so an mpz_ptr can be used as an mpz_srcptr, perhaps mistakenly.
export ZZmutable := Pointer "mpz_ptr";

export ZZ := Pointer "mpz_srcptr";

export ZZorNull := ZZ or null;

export ZZcell := {+v:ZZ};

export ZZpair := {a:ZZ,b:ZZ};

export ZZpairOrNull := ZZpair or null;

export QQmutable := Pointer "mpq_ptr";

export QQ := Pointer "mpq_srcptr";

export QQorNull := QQ or null;

export QQcell := {+v:QQ};

export RRmutable := Pointer "mpfr_ptr";

export RR := Pointer "mpfr_srcptr";

export RRorNull := RR or null;

export RRcell := {+v:RR};

export CCmutable := { re:RRmutable, im:RRmutable };

export CC := { re:RR, im:RR };

export CCorNull := CC or null;

export CCcell := {+v:CC};
dummy(x:RR):string := "";

export tostringRRpointer := dummy;
dummy(x:CC):string := "";

export tonetCCpointer := dummy;

export tonetCCparenpointer := dummy;


export min(x:int,y:int):int := if x<y then x else y;

export max(x:int,y:int):int := if x<y then y else x;

export min(x:uint,y:uint):uint := if x<y then x else y;

export max(x:uint,y:uint):uint := if x<y then y else x;

export min(x:long,y:long):long := if x<y then x else y;

export max(x:long,y:long):long := if x<y then y else x;

export min(x:ulong,y:ulong):ulong := if x<y then x else y;

export max(x:ulong,y:ulong):ulong := if x<y then y else x;



isPositive0(x:ZZ) ::=  1 == Ccode(int, "mpz_sgn(", x, ")");
isZero0    (x:ZZ) ::=  0 == Ccode(int, "mpz_sgn(", x, ")");
isNegative0(x:ZZ) ::= -1 == Ccode(int, "mpz_sgn(", x, ")");

export isPositive(x:ZZ):bool := isPositive0(x);

export isZero    (x:ZZ):bool := isZero0(x);

export isNegative(x:ZZ):bool := isNegative0(x);

export isEven    (x:ZZ):bool := Ccode(bool, "mpz_even_p(", x, ")");

export isOdd     (x:ZZ):bool := Ccode(bool, "mpz_odd_p(", x, ")");

export isUShort(x:ZZ):bool := 0 != Ccode(int, "mpz_fits_ushort_p(", x, ")");

export toUShort(x:ZZ):ushort := ushort(Ccode(ulong, "mpz_get_ui(", x, ")"));

export isInt(x:ZZ):bool := 0 != Ccode(int, "mpz_fits_sint_p(", x, ")");

export isInt(x:ZZcell):bool := 0 != Ccode(int, "mpz_fits_sint_p(", x.v, ")");

export toInt(x:ZZ):int  := int(Ccode(long, "mpz_get_si(", x, ")"));

export toInt(x:ZZcell):int  := int(Ccode(long, "mpz_get_si(", x.v, ")"));

export isLong(x:ZZ):bool := 0 != Ccode(int, "mpz_fits_slong_p(", x, ")");

export isLong(x:ZZcell):bool := 0 != Ccode(int, "mpz_fits_slong_p(", x.v, ")");

export toLong(x:ZZ):long  := Ccode(long, "mpz_get_si(", x, ")");

export toLong(x:ZZcell):long  := Ccode(long, "mpz_get_si(", x.v, ")");

export isULong(x:ZZ):bool := 0 != Ccode(int, "mpz_fits_ulong_p(", x, ")");

export isULong(x:ZZcell):bool := 0 != Ccode(int, "mpz_fits_ulong_p(", x.v, ")");

export toULong(x:ZZ):ulong  := Ccode(ulong, "mpz_get_ui(", x, ")");

export toULong(x:ZZcell):ulong  := Ccode(ulong, "mpz_get_ui(", x.v, ")");

export minprec := Ccode(ulong,"MPFR_PREC_MIN");

export maxprec := Ccode(ulong,"MPFR_PREC_MAX");

export hash(x:ZZ):int := (
     if isInt(x) then 0x7fffffff & toInt(x)
     else Ccode(int, "mpz_hash(",					    -- see gmp_aux.c for this function
           x, ")"));

getstr(str:charstarOrNull, base:int, x:ZZ) ::= Ccode(charstarOrNull, "mpz_get_str(", str, ",", base, ",", x, ")" );

init(x:ZZmutable) ::= Ccode( ZZmutable, "(mpz_init(",  x, "),",x,")" );

export newZZmutable():ZZmutable := init(GCmalloc(ZZmutable));

clear(x:ZZmutable) ::= Ccode( void, "mpz_clear(",  x, ")" );

init(x:QQmutable) ::= Ccode( QQmutable, "(mpq_init(",  x, "),",x,")" );

export newQQmutable():QQmutable :=  init(GCmalloc(QQmutable));

clear(x:QQmutable) ::= Ccode( void, "mpq_clear(",  x, ")" );

init(x:RRmutable,prec:ulong):RRmutable := (
    if prec < minprec then prec = minprec else if prec > maxprec then prec = maxprec;
    Ccode( RRmutable, "(mpfr_init2(", x, ",(mpfr_prec_t)",prec,"),",x,")" )
   );

export newRRmutable(prec:ulong):RRmutable := init(GCmalloc(RRmutable),prec);

clear(x:RRmutable) ::= Ccode( void, "mpfr_clear(",  x, ")" );

clear(z:CCmutable):void := ( clear(z.re); clear(z.im); );

export moveToZZ(z:ZZmutable):ZZ := (
     y := GCmalloc(ZZmutable);
     Ccode(void, "
	  int s = z->_mp_size, ss = s>=0 ? s : -s;
          mp_limb_t *p = (mp_limb_t *)getmem_atomic(ss * sizeof(mp_limb_t));
	  memcpy(p,z->_mp_d,ss*sizeof(mp_limb_t));
	  ",y,"->_mp_alloc = ss, ",y,"->_mp_size = s, ",y,"->_mp_d = p;
	  ");
     Ccode(ZZ,y));

export moveToZZandclear(z:ZZmutable):ZZ := (
     w := moveToZZ(z);
     clear(z);
     w);

export moveToRR(z:RRmutable):RR := (
     y := GCmalloc(RRmutable);
     Ccode(void, "
  	  int limb_size = (",z,"->_mpfr_prec - 1) / GMP_NUMB_BITS + 1;
  	  mp_limb_t *p = (mp_limb_t*) getmem_atomic(limb_size * sizeof(mp_limb_t));
  	  memcpy(p, ",z,"->_mpfr_d, limb_size * sizeof(mp_limb_t));
  	  ",y,"->_mpfr_prec = ",z,"->_mpfr_prec;
  	  ",y,"->_mpfr_sign = ",z,"->_mpfr_sign;
  	  ",y,"->_mpfr_exp  = ",z,"->_mpfr_exp;
  	  ",y,"->_mpfr_d    = p;
	  ");
    Ccode(RR,y)
   );

export moveToRRandclear(z:RRmutable):RR := (
     w := moveToRR(z);
     clear(z);
     w);
     
set(x:ZZmutable, y:ZZ   ) ::= Ccode( void, "mpz_set   (", x, ",", y, ")" );
set(x:ZZmutable, n:int  ) ::= Ccode( void, "mpz_set_si(", x, ",", n, ")" );
set(x:ZZmutable, n:long ) ::= Ccode( void, "mpz_set_si(", x, ",", n, ")" );
set(x:ZZmutable, n:ulong) ::= Ccode( void, "mpz_set_ui(", x, ",", n, ")" );

negsmall := -100;
possmall := 300;
smallints := (
     new array(ZZ) len possmall - negsmall + 1 do for i from negsmall to possmall do (
     	  x := newZZmutable();
     	  set(x,i);
     	  provide moveToZZandclear(x)));

export toInteger(i:int):ZZ := (
     if i >= negsmall && i <= possmall then smallints.(i-negsmall)
     else (
	  x := newZZmutable();
	  set(x,i);
	  moveToZZandclear(x)));

export zeroZZ := toInteger(0);

export  oneZZ := toInteger(1);

export  minusoneZZ := toInteger(-1);

export zeroZZcell := ZZcell(zeroZZ);

export  oneZZcell := ZZcell( oneZZ);

export  minusoneZZcell := ZZcell( minusoneZZ);

export toInteger(i:ushort):ZZ := toInteger(int(i));

export toInteger(i:ulong):ZZ := (
     if i >= ulong(negsmall) && i <= ulong(possmall) then smallints.(int(i)-negsmall)
     else (
	  x := newZZmutable();
	  set(x,i);
	  moveToZZandclear(x)));

export toInteger(i:long):ZZ := (
     if i >= long(negsmall) && i <= long(possmall) then smallints.(int(i)-negsmall)
     else (
	  x := newZZmutable();
	  set(x,i);
	  moveToZZandclear(x)));

neg(x:ZZmutable, y:ZZ) ::= Ccode( void, "mpz_neg(", x, ",", y, ")" );

export - (x:ZZ) : ZZ := (
     w := newZZmutable();
     neg(w,x);
     moveToZZandclear(w));

abs(x:ZZmutable, y:ZZ) ::= Ccode( void, "mpz_abs(", x, ",", y, ")" );

export abs(x:ZZ) : ZZ := (
    if isNegative0(x) then (
	 w := newZZmutable();
	 abs(w,x);
	 moveToZZandclear(w))
    else x
   );
add(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_add(", x, ",", y, ",", z, ")" );

export (x:ZZ) + (y:ZZ) : ZZ := (
     w := newZZmutable();
     add(w,x,y);
     moveToZZandclear(w));
add(x:ZZmutable, y:ZZ, z:ulong) ::= Ccode( void, "mpz_add_ui(", x, ",", y, ",", z, ")" );
sub(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_sub(", x, ",", y, ",", z, ")" );

export (x:ZZ) - (y:ZZ) : ZZ := (
     w := newZZmutable();
     sub(w,x,y);
     moveToZZandclear(w));
compare(x:ZZ, y:ZZ) ::= Ccode( int, "mpz_cmp(", x, ",", y, ")" );

export (x:ZZ) === (y:ZZ) : bool := compare(x,y) == 0;

export (x:ZZ)  >  (y:ZZ) : bool := compare(x,y) >  0;

export (x:ZZ)  <  (y:ZZ) : bool := compare(x,y) <  0;

export (x:ZZ)  >= (y:ZZ) : bool := compare(x,y) >= 0;

export (x:ZZ)  <= (y:ZZ) : bool := compare(x,y) <= 0;
compare(x:ZZ, y:long) ::= Ccode( int, "mpz_cmp_si(",  x, ",", y, ")" );

export (x:ZZ)  >  (y:int) : bool :=  compare(x,long(y)) >  0;

export (x:ZZ)  >= (y:int) : bool :=  compare(x,long(y)) >= 0;

export (x:ZZ) === (y:int) : bool :=  compare(x,long(y)) == 0;

export (x:ZZcell) === (y:int) : bool :=  compare(x.v,long(y)) == 0;

export (x:ZZ)  <  (y:int) : bool :=  compare(x,long(y)) <  0;

export (x:ZZ)  <= (y:int) : bool :=  compare(x,long(y)) <= 0;

export (x:int) < (y:ZZ) : bool := y > x;

export (x:int) > (y:ZZ) : bool := y < x;

export (x:int) <= (y:ZZ) : bool := y >= x;

export (x:int) >= (y:ZZ) : bool := y <= x;

export (x:int) === (y:ZZ) : bool := y === x;
sub(x:ZZmutable, y:ZZ, z:ulong) ::= Ccode( void, "mpz_sub_ui(", x, ",", y, ",", z, ")" );
mul(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_mul(", x, ",", y, ",", z, ")" );

export (x:ZZ) * (y:ZZ) : ZZ := (
     w := newZZmutable();
     mul(w,x,y);
     moveToZZandclear(w));
mul(x:ZZmutable, y:ZZ, z:int) ::= Ccode( void, "mpz_mul_si(", x, ",", y, ",", z, ")" );
mul(x:ZZmutable, y:ZZ, z:ulong) ::= Ccode( void, "mpz_mul_ui(", x, ",", y, ",", z, ")" );
pow(x:ZZmutable, y:ZZ, n:ulong) ::= Ccode( void, "mpz_pow_ui(", x, ",", y, ",", n, ")" );

export (x:ZZ) ^ (n:ulong) : ZZ := (
     w := newZZmutable();
     pow(w,x,n);
     moveToZZandclear(w));

cdiv(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_cdiv_q(", x, ",", y, ",", z, ")" );
fdiv(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_fdiv_q(", x, ",", y, ",", z, ")" );

export (x:ZZ) // (y:ZZ) : ZZ := (
     w := newZZmutable();
     if isPositive0(y) then fdiv(w,x,y) else cdiv(w,x,y);
     moveToZZandclear(w));

fmod(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_fdiv_r(", x, ",", y, ",", z, ")" );
cmod(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_cdiv_r(", x, ",", y, ",", z, ")" );

export (x:ZZ) % (y:ZZ) : ZZ := (
     w := newZZmutable();
     if isPositive0(y) then fmod(w,x,y) else cmod(w,x,y);
     moveToZZandclear(w));

fdiv(x:ZZmutable, y:ZZ, z:ulong) ::= Ccode( void, "mpz_fdiv_q_ui(", x, ",", y, ",", z, ")" );

export (x:ZZ) // (y:ulong) : ZZ := (
     w := newZZmutable();
     fdiv(w,x,y);
     moveToZZandclear(w));

export (x:ZZ) // (y:ushort) : ZZ := x // ulong(y);

fmod(y:ZZ, z:ulong) ::= Ccode( ulong, "mpz_fdiv_ui(", y, ",", z, ")" );

export (x:ZZ) % (y:ulong) : ulong := fmod(x,y);

export (x:ZZ) % (y:ushort) : ushort := ushort(x % ulong(y));
gcd(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_gcd(", x, ",", y, ",", z, ")" );

export gcd(x:ZZ,y:ZZ):ZZ := (
     w := newZZmutable();
     gcd(w,x,y);
     moveToZZandclear(w));

mul_2exp(x:ZZmutable, y:ZZ, z:ulong) ::= Ccode( void, "mpz_mul_2exp(", x, ",", y, ",", z, ")" );

leftshift(x:ZZ,n:ulong):ZZ := (
     w := newZZmutable();
     mul_2exp(w,x,n);
     moveToZZandclear(w));

tdiv_q_2exp(x:ZZmutable, y:ZZ, z:ulong) ::= Ccode( void, "mpz_tdiv_q_2exp(", x, ",", y, ",", z, ")" );

rightshift(x:ZZ,n:ulong):ZZ := (
     w := newZZmutable();
     tdiv_q_2exp(w,x,n);
     moveToZZandclear(w));

export (x:ZZ) << (n:int) : ZZ := (
     if n == 0 then x else if n > 0 then leftshift(x,ulong(n)) else rightshift(x,ulong(-n))
    );

export (x:ZZ) >> (n:int) : ZZ := (
     if n == 0 then x else if n > 0 then rightshift(x,ulong(n)) else leftshift(x,ulong(-n))
    );     

and(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_and(", x, ",", y, ",", z, ")" );

export (x:ZZ) & (y:ZZ) : ZZ := (
     w := newZZmutable();
     and(w,x,y);
     moveToZZandclear(w));

ior(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_ior(", x, ",", y, ",", z, ")" );

export (x:ZZ) | (y:ZZ) : ZZ := (
     w := newZZmutable();
     ior(w,x,y);
     moveToZZandclear(w));

xor(x:ZZmutable, y:ZZ, z:ZZ) ::= Ccode( void, "mpz_xor(", x, ",", y, ",", z, ")" );

export (x:ZZ) ^^ (y:ZZ) : ZZ := (
     w := newZZmutable();
     xor(w,x,y);
     moveToZZandclear(w));

base := 10;
toCstring(x:ZZ) ::= getstr(charstarOrNull(null()), base, x);

export tostring(x:ZZ):string := (
     cstr := toCstring(x);
     ret := tostring(cstr);
     Ccode(void,"mp_free_str(", cstr, ")");
     ret);

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



-- Integers and doubles

get_d(x:ZZ) ::= Ccode( double, "mpz_get_d(",  x, ")" );

export toDouble(x:ZZ):double := get_d(x);

export toDouble(x:ZZcell):double := get_d(x.v);

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

log(x:double) ::= Ccode(double, "log(", x, ")" );
logtwo := log(2.);
bigint := 2147483647.; -- 2^31-1

(x:double) << (n:int) ::= ldexp(x, n);
(x:double) >> (n:int) ::= ldexp(x,-n);

-----------------------------------------------------------------------------
-- rationals
-----------------------------------------------------------------------------

-- TO DO : this is inefficient -- we could just grab the numerator without copying its limbs
export numerator(x:QQ):ZZ := (
     w := newZZmutable();
     Ccode( void, "mpq_get_num(", w, ",", x, ")" );
     moveToZZandclear(w));

-- TO DO : this is inefficient -- we could just grab the denominator without copying its limbs
export denominator(x:QQ):ZZ := (
     w := newZZmutable();
     Ccode( void, "mpq_get_den(", w, ",", x, ")" );
     moveToZZandclear(w));

export numeratorRef  (x:QQ) ::= Ccode( ZZ, "mpq_numref(",  x, ")");

export denominatorRef(x:QQ) ::= Ccode( ZZ, "mpq_denref(",  x, ")");

export numeratorRef  (x:QQmutable) ::= Ccode( ZZmutable, "mpq_numref(",  x, ")");

export denominatorRef(x:QQmutable) ::= Ccode( ZZmutable, "mpq_denref(",  x, ")");

export hash(x:QQ):int := hash(numeratorRef(x))+1299841*hash(denominatorRef(x));

isNegative0(x:QQ):bool := -1 == Ccode(int, "mpq_sgn(",x,")");

export isNegative(x:QQ):bool := isNegative0(x);

export newQQCanonical(i:ZZ,j:ZZ):QQ := (
     -- assume canonical: gcd(i,j)=1, j>0, and j==1 if i==0
     Ccode(void, "
	  mpq_ptr z = (mpq_ptr) getmem(sizeof(__mpq_struct));
	  memcpy(&z->_mp_num,",i,",sizeof(__mpz_struct));
	  memcpy(&z->_mp_den,",j,",sizeof(__mpz_struct))"
	 );
     Ccode(QQ,"z"));

moveToQQ(y:QQmutable):QQ := newQQCanonical(moveToZZ(numeratorRef(y)),moveToZZ(denominatorRef(y)));

export moveToQQandclear(z:QQmutable):QQ := (
     w := moveToQQ(z);
     clear(z);
     w);

export newQQ(i:ZZ,j:ZZ):QQ := (
     x := newQQmutable();
     set(  numeratorRef(x),i);
     set(denominatorRef(x),j);
     Ccode(void, "mpq_canonicalize(",x,")");
     moveToQQandclear(x)
    );

-- integers and rationals
     
export toRational(x:ZZ):QQ := newQQCanonical(x,oneZZ);

export toRational(n:int):QQ := toRational(toInteger(n));

export toRational(n:ulong):QQ := toRational(toInteger(n));

export floor(x:QQ):ZZ := numeratorRef(x)//denominatorRef(x);

export (x:QQ) + (y:QQ) : QQ := (
     z := newQQmutable();
     Ccode( void, "mpq_add(", z, ",", x, ",", y, ")" );
     moveToQQandclear(z));

export - (y:QQ) : QQ := (
     z := newQQmutable();
     Ccode( void, "mpq_neg(", z, ",", y, ")" );
     moveToQQandclear(z));

export abs(x:QQ) : QQ := if isNegative0(x) then -x else x;

export inv(y:QQ) : QQ := (			    -- reciprocal
     z := newQQmutable();
     Ccode( void, "mpq_inv(", z, ",", y, ")" );
     moveToQQandclear(z));

export (x:QQ) - (y:QQ) : QQ := (
     z := newQQmutable();
     Ccode( void, "mpq_sub(", z, ",", x, ",", y, ")" );
     moveToQQandclear(z));

export (x:QQ) * (y:QQ) : QQ := (
     z := newQQmutable();
     Ccode( void, "mpq_mul(", z, ",", x, ",", y, ")" );
     moveToQQandclear(z));

export (x:QQ) / (y:QQ) : QQ := (
     z := newQQmutable();
     Ccode( void, "mpq_div(", z, ",", x, ",", y, ")" );
     moveToQQandclear(z));

export (x:QQ) === (y:QQ) : bool := Ccode( bool, "mpq_equal(", x, ",", y, ")");

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


export tostring(x:QQ):string := tostring(numeratorRef(x)) + '/' + tostring(denominatorRef(x));


export (x:QQ) === (y:ZZ) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;

export (x:QQ) === (y:int) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;

export (y:ZZ) === (x:QQ) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;

export (y:int) === (x:QQ) : bool := denominatorRef(x) === 1 && numeratorRef(x) === y;

compare(x:QQ, y:QQ) ::= Ccode( int, 
     "mpq_cmp(", x, ",", y, ")" );
compare(x:QQ, y:ulong) ::= Ccode( int, "mpq_cmp_ui(", x, ",", y, ",1)");
compare(x:QQ, y: long) ::= Ccode( int, "mpq_cmp_si(", x, ",", y, ",1)");
compare(x:QQ, y: int) ::= Ccode( int, "mpq_cmp_si(", x, ",(long)", y, ",1)");

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

export toDouble(x:QQ):double := Ccode( double, "mpq_get_d(",  x, ")" );

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

export realPart(z:CC):RR := z.re;

export imaginaryPart(z:CC):RR := z.im;

-- warning: these routines just check the sign bit, and don't verify finiteness!
isPositive0(x:RR) ::=  1 == Ccode(int, "mpfr_sgn(", x, ")");
isNegative0(x:RR) ::= -1 == Ccode(int, "mpfr_sgn(", x, ")");
isZero0    (x:RR) ::=  0 == Ccode(int, "mpfr_sgn(", x, ")");

flagged0() ::= 0 != Ccode( int, "mpfr_erangeflag_p()" );
setflag0() ::= Ccode( void, "mpfr_set_erangeflag()" );
isfinite0(x:RR) ::=Ccode(bool,"mpfr_number_p(",x,")");
isinf0 (x:RR) ::= Ccode(bool,"mpfr_inf_p(",x,")");
isnan0 (x:RR) ::= Ccode(bool,"mpfr_nan_p(",x,")");
sign0(x:RR) ::= 0 != Ccode(int,"mpfr_signbit(",x,")");
exponent0(x:RR) ::= Ccode(long,"(long)mpfr_get_exp(",x,")"); -- sometimes int, sometimes long, see gmp.h for type mp_exp_t
sizeinbase0(x:ZZ,b:int) ::= Ccode( int, "mpz_sizeinbase(",  x, ",", b, ")" );

-- warning: these routines just check the sign bit, and don't verify finiteness!
export isPositive(x:RR):bool := isPositive0(x);

export isNegative(x:RR):bool := isNegative0(x);

export isZero    (x:RR):bool := isZero0(x) && isfinite0(x);

export isZero    (x:CC):bool := isZero0(x.re) && isfinite0(x.re) && isZero0(x.im) && isfinite0(x.im);

export defaultPrecision := ulong(53); -- should 53 be computed?

export minExponent := Ccode(long,"(long)mpfr_get_emin()-1");

export maxExponent := Ccode(long,"(long)mpfr_get_emax()");

export exponent(x:ZZ):long := if isZero0(x) then minExponent else long(sizeinbase0(x,2));

export exponent(x:RR):long := if isZero0(x) && isfinite0(x) then minExponent else if isfinite0(x) then exponent0(x) else maxExponent;

export exponent(x:CC):long := max(exponent(x.re),exponent(x.im));

export newCCmutable(prec:ulong):CCmutable := CCmutable(newRRmutable(prec),newRRmutable(prec));

export moveToCC(y:CCmutable):CC := CC(moveToRR(y.re), moveToRR(y.im));

export moveToCCandclear(z:CCmutable):CC := (
     w := moveToCC(z);
     clear(z);
     w);

precision0(x:RR) ::= Ccode(ulong,"(unsigned long)mpfr_get_prec(", x, ")");

export precision(x:RR):ulong := precision0(x);

export precision(x:CC):ulong := precision0(x.re);

export toRR(x:RR,prec:ulong):RR := (
     if precision0(x) == prec then return x;
     z := newRRmutable(prec);
     Ccode( void, "mpfr_set(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export toRR(s:string,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void,  "mpfr_set_str(",  z,", (char *)",  s, "->array,", "0,", "GMP_RNDN", ")" ); 
     moveToRRandclear(z));

export toRR(x:QQ,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_set_q(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export toRR(x:QQ):RR := toRR(x,defaultPrecision);

export toRR(x:ZZ,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_set_z(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export toRR(x:ZZ):RR := toRR(x,defaultPrecision);

export toRR(n:int,prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode( void, "mpfr_set_si(",  x, ",(long)", n, ", GMP_RNDN)" );
     moveToRRandclear(x));

export toRR(n:ulong,prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode( void, "mpfr_set_ui(",  x, ",(unsigned long)", n, ", GMP_RNDN)" );
     moveToRRandclear(x));

export toRR(n:double,prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode( void, "mpfr_set_d(",  x, ",", n, ", GMP_RNDN)" );
     moveToRRandclear(x));

export toRR(n:double):RR := toRR(n,defaultPrecision);	   

export infinityRR(prec:ulong,sign:int):RR := (
     x := newRRmutable(prec);
     Ccode(void, "mpfr_set_inf(",x,",",sign,")");
     moveToRRandclear(x));

export infinityRR(prec:ulong):RR := infinityRR(prec,1);

export nanRR(prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode(void, "mpfr_set_nan(",x,")");
     moveToRRandclear(x));

export toCC(x:RR,y:RR):CC := (
     if ( isnan0(x) || isnan0(y) ) then (prec := precision0(x); z := nanRR(prec); CC(z,z))
     else if ( isinf0(x) || isinf0(y) ) then (prec := precision0(x); z := infinityRR(prec,1); CC(z,z))
     else if precision0(x) == precision0(y) then CC(x,y)
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

export toDouble(x:RR):double := Ccode( double, "mpfr_get_d(",  x, ", GMP_RNDN)" );

export toDouble(x:RRcell):double := Ccode( double, "mpfr_get_d(",  x.v, ", GMP_RNDN)" );

export flagged():bool := flagged0();

export isfinite(x:RR):bool := isfinite0(x);

export isinf(x:RR):bool := isinf0(x);

export isnan(x:RR):bool := isnan0(x);

export isfinite(x:CC):bool := isfinite0(x.re) && isfinite0(x.im);

export isinf(x:CC):bool := isinf0(x.re) && !isnan0(x.im) || isinf0(x.im) && !isnan0(x.re);

export isnan(x:CC):bool := isnan0(x.re) || isnan0(x.im);

export (x:RR) === (y:RR):bool := (			    -- weak equality
     Ccode( void, "mpfr_clear_flags()" );
     0 != Ccode( int, "mpfr_equal_p(",  x, ",",  y, ")" )
     && !flagged0()
    );

export strictequality(x:RR,y:RR):bool := (
     Ccode( void, "mpfr_clear_flags()" );
     0 != Ccode( int, "mpfr_equal_p(",  x, ",",  y, ")" )
     && !flagged0()
     && sign0(x) == sign0(y)
     && precision0(x) == precision0(y)
    );

compare0(x:RR, y:RR) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp(",  x, ",",  y, "))" );

export compare(x:RR, y:RR):int := compare0(x,y);	    -- use flagged(), too!
export (x:RR)  >  (y:RR) : bool := compare0(x,y) >  0 && !flagged0();

export (x:RR)  <  (y:RR) : bool := compare0(x,y) <  0 && !flagged0();

export (x:RR)  >= (y:RR) : bool := compare0(x,y) >= 0 && !flagged0();

export (x:RR)  <= (y:RR) : bool := compare0(x,y) <= 0 && !flagged0();

compare0(x:RR, y:long) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_si(",  x, ",", y, "))" );
compare0(x:RR, y:int ) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_si(",  x, ",(long)", y, "))" );

export compare(x:RR, y:long):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_si(",  x, ",", y, "))" );

export compare(y:long, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_si(",  x, ",", y, "))" );

export (x:RR)  >  (y:int) : bool :=  compare0(x,long(y)) >  0 && !flagged0();

export (x:RR)  >= (y:int) : bool :=  compare0(x,long(y)) >= 0 && !flagged0();

export (x:RR) === (y:int) : bool :=  compare0(x,long(y)) == 0 && !flagged0();

export (x:RR)  <  (y:int) : bool :=  compare0(x,long(y)) <  0 && !flagged0();

export (x:RR)  <= (y:int) : bool :=  compare0(x,long(y)) <= 0 && !flagged0();

export (x:CC) === (y:int) : bool :=  x.re === y && x.im === 0;

compare0(x:RR, y:double) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_d(",  x, ",", y, "))" );

export compare(x:RR, y:double):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_d(",  x, ",", y, "))" );

export compare(y:double, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_d(",  x, ",", y, "))" );

export (x:RR)  >  (y:double) : bool :=  compare0(x,y) >  0 && !flagged0();

export (x:RR)  >= (y:double) : bool :=  compare0(x,y) >= 0 && !flagged0();

export (x:RR) === (y:double) : bool :=  compare0(x,y) == 0 && !flagged0();

export (x:RR)  <  (y:double) : bool :=  compare0(x,y) <  0 && !flagged0();

export (x:RR)  <= (y:double) : bool :=  compare0(x,y) <= 0 && !flagged0();

compare0(x:RR, y:ZZ) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_z(",  x, ",", y, "))" );

export compare(x:RR, y:ZZ):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_z(",  x, ",", y, "))" );

export compare(y:ZZ, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_z(",  x, ",", y, "))" );

export (x:RR)  >  (y:ZZ) : bool :=  compare0(x,y) >  0 && !flagged0();

export (x:RR)  >= (y:ZZ) : bool :=  compare0(x,y) >= 0 && !flagged0();

export (x:RR) === (y:ZZ) : bool :=  compare0(x,y) == 0 && !flagged0();

export (y:ZZ) === (x:RR) : bool :=  compare0(x,y) == 0 && !flagged0();

export (x:RR)  <  (y:ZZ) : bool :=  compare0(x,y) <  0 && !flagged0();

export (x:RR)  <= (y:ZZ) : bool :=  compare0(x,y) <= 0 && !flagged0();

compare0(x:RR, y:QQ) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_q(",  x, ",", y, "))" );

export compare(x:RR, y:QQ):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_q(",  x, ",", y, "))" );

export compare(y:QQ, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_q(",  x, ",", y, "))" );

export (x:RR)  >  (y:QQ) : bool :=  compare0(x,y) >  0 && !flagged0();

export (x:RR)  >= (y:QQ) : bool :=  compare0(x,y) >= 0 && !flagged0();

export (x:RR) === (y:QQ) : bool :=  compare0(x,y) == 0 && !flagged0();

export (y:QQ) === (x:RR) : bool :=  compare0(x,y) == 0 && !flagged0();

export (x:RR)  <  (y:QQ) : bool :=  compare0(x,y) <  0 && !flagged0();

export (x:RR)  <= (y:QQ) : bool :=  compare0(x,y) <= 0 && !flagged0();

export hash(x:RR):int := int(precision0(x)) + Ccode(int, 
     "mpfr_hash(",					    -- see gmp_aux.c for this function
          x, 
     ")"
    );

export hash(x:CC):int := 123 + hash(x.re) + 111 * hash(x.im);
     
export (x:RR) + (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_add(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) + (y:int) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_add_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
     
export (x:RR) + (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_add_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
     
export (x:RR) + (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_add_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export - (y:RR) : RR := (
     z := newRRmutable(precision0(y));
     Ccode( void, "mpfr_neg(", z, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) - (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_sub(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) - (y:int) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sub_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (y:int) - (x:RR) : RR := -(x-y);
     
export (x:RR) - (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sub_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
     
export (x:RR) - (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sub_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export abs(x:RR) : RR := if isNegative0(x) then -x else x;

export (x:RR) * (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_mul(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) * (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (y:ZZ) * (x:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) * (y:int) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (y:int) * (x:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
     
export (x:RR) * (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) / (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_div(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) / (y:long) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_div_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) / (y:int) : RR := x / long(y);
     
export (x:RR) / (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_div_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
     
export (x:RR) / (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_div_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export sqrt(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sqrt(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) ^ (n:long) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow_si(",  z, ",",  x, ",", n, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) ^ (n:ulong) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow_ui(",  z, ",",  x, ",", n, ", GMP_RNDN)" );
     moveToRRandclear(z));

export pow10(n:ulong,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_ui_pow_ui(",  z, ",", ulong(10), ",", n, ", GMP_RNDN)" );
     moveToRRandclear(z));

export pow10(n:long,prec:ulong):RR := (
     if n < long(0)
     then (pow10(ulong(-n),prec))^long(-1)
     else pow10(ulong(n),prec));

export pow10(n:int,prec:ulong):RR := pow10(long(n),prec);

export (n:ulong) ^ (x:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_ui_pow(",  z, ",", n, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) ^ (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow_z(",  z, ",",  x, ",", y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) ^ (y:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow(",  z, ",",  x, ",", y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export floor(x:RR) : ZZ := (
     if !isfinite0(x) then return zeroZZ;			    -- nothing else to do!
     w := newZZmutable();
     Ccode( void, "mpfr_get_z(", w, ",", x, ", GMP_RNDD)" );
     moveToZZandclear(w));

export ceil(x:RR) : ZZ := (
     if !isfinite0(x) then return zeroZZ;			    -- nothing else to do!
     w := newZZmutable();
     Ccode( void, "mpfr_get_z(", w, ",", x, ", GMP_RNDU)" );
     moveToZZandclear(w));

export round(x:RR) : ZZ := (
     if !isfinite0(x) then return zeroZZ;			    -- nothing else to do!
     w := newZZmutable();
     Ccode( void, "mpfr_get_z(", w, ",", x, ", GMP_RNDN)" );
     moveToZZandclear(w));

export (x:RR) << (n:long) : RR := (
     if n == long(0) then return x;
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_2si(", z, ",", x, ",", n, ",GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RR) >> (n:long) : RR := x << -n;

export (x:RR) << (n:int) : RR := x << long(n);

export (x:RR) >> (n:int) : RR := x << long(-n);

-- complex arithmetic

export (x:CC) + (y:CC) : CC := toCC(x.re+y.re, x.im+y.im);

export (x:CC) - (y:CC) : CC := toCC(x.re-y.re, x.im-y.im);

export (x:RR) - (y:CC) : CC := toCC(x-y.re,-y.im);

export (x:int) - (y:CC) : CC := toCC(x-y.re,-y.im);

export (x:CC) - (y:RR) : CC := toCC(x.re-y,x.im);

export (x:CC) + (y:RR) : CC := toCC(x.re+y,x.im);

export (x:RR) + (y:CC) : CC := toCC(x+y.re,y.im);

export -(y:CC) : CC := toCC(-y.re,-y.im);

export (x:CC) * (y:RR) : CC := (
     if isfinite0(x.re) && isfinite0(x.im) && isfinite0(y)
     then toCC(x.re*y, x.im*y)
     else if isnan(x) || isnan(y) then nanCC(min(precision(x),precision(y)))
     else infinityCC(min(precision(x),precision(y))));

export (y:RR) * (x:CC) : CC := (
     if isfinite0(x.re) && isfinite0(x.im) && isfinite(y)
     then toCC(x.re*y, x.im*y)
     else if isnan(x) || isnan(y) then nanCC(min(precision(x),precision(y)))
     else infinityCC(min(precision(x),precision(y))));

export (y:int) * (x:CC) : CC := (
     if isinf(x) && y != 0
     then infinityCC(precision(x))
     else toCC(x.re*y, x.im*y));

export (x:CC) * (y:ZZ) : CC := (
     if isinf(x) && !isZero(y)
     then infinityCC(precision(x))
     else toCC(x.re*y, x.im*y));

export (y:ZZ) * (x:CC) : CC := (
     if isinf(x) && !isZero(y)
     then infinityCC(precision(x))
     else toCC(x.re*y, x.im*y));

export (x:CC) * (y:CC) : CC := (
     if isinf(x) && !isZero(y) && !isnan(y) || isinf(y) && !isZero(x) && !isnan(x)
     then infinityCC(min(precision(x),precision(y)))
     else toCC(x.re*y.re-x.im*y.im, x.im*y.re+x.re*y.im));

export (x:CC) / (y:RR) : CC := (
     if isZero(y) && !isnan(x) && !isZero(x)
     then infinityCC(min(precision(x),precision(y)))
     else toCC(x.re/y, x.im/y));

export (x:CC) / (y:int) : CC := (
     if y == 0 && !isnan(x) && !isZero(x)
     then infinityCC(precision(x))
     else toCC(x.re/y, x.im/y));

export conj(x:CC):CC := toCC(x.re,-x.im);

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
     if ( isinf(x.re) || isinf(y.re) || isinf(x.im) || isinf(y.im) ) then (
       setflag0();
       return 0;
    );
     r := compare(x.re,y.re);
     if flagged() || r != 0 then r
     else compare(x.im,y.im));

export compare(x:CC,y:RR):int := (
     if ( isinf(x.re) || isinf(x.im) || isinf(y) ) then (
       setflag0();
       return 0;
    );
     r := compare(x.re,y);
     if flagged() || r != 0 then r
     else compare0(x.im,0));

export compare(x:RR,y:CC):int := (
     if ( isinf(x) || isinf(y.re) || isinf(y.im) ) then (
       setflag0();
       return 0;
    );
     r := compare(x,y.re);
     if flagged() || r != 0 then r
     else -compare0(y.im,0));

export compare(x:CC,y:ZZ):int := (
     if ( isinf(x.re) || isinf(x.im) ) then (
       setflag0();
       return 0;
    );
     r := compare(x.re,y);
     if flagged() || r != 0 then r
     else compare0(x.im,0));

export compare(x:ZZ,y:CC):int := (
     if ( isinf(y.re) || isinf(y.im) ) then (
       setflag0();
       return 0;
    );
     r := compare(x,y.re);
     if flagged() || r != 0 then r
     else -compare0(y.im,0));

export compare(x:CC,y:QQ):int := (
     if ( isinf(x.re) || isinf(x.im) ) then (
       setflag0();
       return 0;
    );
     r := compare(x.re,y);
     if flagged() || r != 0 then r
     else compare0(x.im,0));

export compare(x:QQ,y:CC):int := (
     if ( isinf(y.re) || isinf(y.im) ) then (
       setflag0();
       return 0;
    );
     r := compare(x,y.re);
     if flagged() || r != 0 then r
     else -compare0(y.im,0));

export abs(x:CC):RR := (
     z := newRRmutable(precision(x));
     Ccode( void, "mpfr_hypot(", z, ",", x.re, ",", x.im, ",GMP_RNDN)" );
     moveToRRandclear(z));

header "#include <complex.h> ";

export sqrt(x:CC):CC := (
     z := newCCmutable(precision(x));
     Ccode( void, "mpfc_sqrt(", z, ",", x, ")" );	    -- see ../e/complex.c
     moveToCCandclear(z));

-- real transcendental functions

export pi(prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_const_pi(",  z, ", GMP_RNDN)" );
     moveToRRandclear(z));

export exp(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_exp(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export log(x:RR):RR := (				    -- works only if x>0
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_log(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export log(b:RR,x:RR):RR := (				    -- works only if x>0 and b>0
     if precision0(b) < precision0(x) then x = toRR(x,precision0(b))
     else if precision0(b) > precision0(x) then b = toRR(b,precision0(x));
     log(x)/log(b));

export sin(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sin(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export cos(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_cos(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export tan(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_tan(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export asin(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_asin(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export acos(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_acos(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export atan(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_atan(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export atan2(y:RR,x:RR):RR := (
     -- if isZero0(x) && isZero0(y) && isfinite0(x) && isfinite0(y) then return nanRR(min(precision0(x),precision0(y)));
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_atan2(", z, ",", y, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export agm(x:RR,y:RR):RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_agm(", z, ",", x, ",", y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export sinh(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sinh(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export cosh(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_cosh(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export tanh(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_tanh(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export sec(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sec(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export csc(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_csc(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export cot(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_cot(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export sech(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sech(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export csch(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_csch(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export coth(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_coth(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export factorial(x:ulong):ZZ := (
     w := newZZmutable();
     Ccode( void, "mpz_fac_ui(", w, ",", x, ")" );
     moveToZZandclear(w));

export log1p(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_log1p(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export expm1(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_expm1(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export Gamma(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_gamma(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export factorial(x:RR):RR := Gamma(x+1);

export eint(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_eint(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
--export lngamma(x:RR):RR := (
--     z := newRRmutable(precision0(x));
--     Ccode( void, "mpfr_lngamma(", z, ",", x, ", GMP_RNDN)" );
--     moveToRRandclear(z));

export zeta(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_zeta(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export zeta(x:ulong,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_zeta_ui(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export erf(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_erf(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export erfc(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_erfc(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export j0(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_j0(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export j1(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_j1(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export jn(n:long,x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_jn(", z, ",",n,",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export y0(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_y0(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export y1(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_y1(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export yn(n:long,x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_yn(", z, ",",n,",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));



export sign(x:RR):bool := 0 != Ccode(int,"mpfr_signbit(",x,")");




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

export arrayZZ := array(ZZ);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d gmp.o "
-- End:
