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

export RR := Pointer "mpfr_srcptr";

export RRorNull := RR or null;

export RRcell := {+v:RR};

export RRimutable := Pointer "mpfi_ptr";

export RRi := Pointer "mpfi_srcptr";
export leftRR(x:RRi):RR := Ccode( RR, "&", x, "->left" );
export rightRR(x:RRi):RR := Ccode( RR, "&", x, "->right" );

export RRiorNull := RRi or null;

export RRicell := {+v:RRi};

export RRmutable := Pointer "mpfr_ptr";

export CCmutable := { re:RRmutable, im:RRmutable };

export CC := { re:RR, im:RR };

export CCorNull := CC or null;

export CCcell := {+v:CC};
dummy(x:RR):string := "";
dummyi(x:RRi):string := "";  -- Added for MPFI

export tostringRRpointer := dummy;
export tostringRRipointer := dummyi;  -- Added for MPFI
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

init(x:RRimutable,prec:ulong):RRimutable := (
    if prec < minprec then prec = minprec else if prec > maxprec then prec = maxprec;
    Ccode( RRimutable, "(mpfi_init2(", x, ",(mpfr_prec_t)",prec,"),",x,")" )
    );

export newRRmutable(prec:ulong):RRmutable := init(GCmalloc(RRmutable),prec);

export newRRimutable(prec:ulong):RRimutable := init(GCmalloc(RRimutable),prec);

clear(x:RRmutable) ::= Ccode( void, "mpfr_clear(",  x, ")" );

clear(x:RRimutable) ::= Ccode( void, "mpfi_clear(",  x, ")" );

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

export moveToRRi(z:RRimutable):RRi := (
    y := GCmalloc(RRimutable);

    -- left
    Ccode(void, "
    int limb_size = (",z,"->left._mpfr_prec - 1) / GMP_NUMB_BITS + 1;
    mp_limb_t *p = (mp_limb_t*) getmem_atomic(limb_size * sizeof(mp_limb_t));
    memcpy(p, ",z,"->left._mpfr_d, limb_size * sizeof(mp_limb_t));
    ",y,"->left._mpfr_prec = ",z,"->left._mpfr_prec;
    ",y,"->left._mpfr_sign = ",z,"->left._mpfr_sign;
    ",y,"->left._mpfr_exp  = ",z,"->left._mpfr_exp;
    ",y,"->left._mpfr_d    = p;
    ");

    -- right
    Ccode(void, "
    limb_size = (",z,"->right._mpfr_prec - 1) / GMP_NUMB_BITS + 1;
    p = (mp_limb_t*) getmem_atomic(limb_size * sizeof(mp_limb_t));
    memcpy(p, ",z,"->right._mpfr_d, limb_size * sizeof(mp_limb_t));
    ",y,"->right._mpfr_prec = ",z,"->right._mpfr_prec;
    ",y,"->right._mpfr_sign = ",z,"->right._mpfr_sign;
    ",y,"->right._mpfr_exp  = ",z,"->right._mpfr_exp;
    ",y,"->right._mpfr_d    = p;
    ");

    Ccode(RRi,y)
    );

export moveToRRandclear(z:RRmutable):RR := (
     w := moveToRR(z);
     clear(z);
     w);

export moveToRRiandclear(z:RRimutable):RRi := (
    w := moveToRRi(z);
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
                                    
isPositive0(x:RRi) ::=  0 < Ccode(int, "mpfi_is_strictly_pos(", x, ")");
isNegative0(x:RRi) ::=  0 < Ccode(int, "mpfi_is_strictly_neg(", x, ")");
isZero0    (x:RRi) ::=  0 < Ccode(int, "mpfi_is_zero(", x, ")");
contains0  (x:RRi) ::= 0 < Ccode(int, "mpfi_has_zero(", x, ")");

flagged0() ::= 0 != Ccode( int, "mpfr_erangeflag_p()" );
setflag0() ::= Ccode( void, "mpfr_set_erangeflag()" );
isfinite0(x:RR) ::=Ccode(bool,"mpfr_number_p(",x,")");
isfinite0(x:RRmutable) ::=Ccode(bool,"mpfr_number_p(",x,")");
isfinite0(x:RRi) ::=Ccode(bool,"mpfi_bounded_p(",x,")");
isfinite0(x:RRimutable) ::=Ccode(bool,"mpfi_bounded_p(",x,")");
isinf0 (x:RR) ::= Ccode(bool,"mpfr_inf_p(",x,")");
isinf0 (x:RRi) ::= Ccode(bool,"mpfi_inf_p(",x,")");
isnan0 (x:RR) ::= Ccode(bool,"mpfr_nan_p(",x,")");
isnan0 (x:RRi) ::= Ccode(bool,"mpfi_nan_p(",x,")");
sign0(x:RR) ::= 0 != Ccode(int,"mpfr_signbit(",x,")");
sign0(x:RRi) ::= 0 != Ccode(int,"mpfi_is_strictly_neg(",x,")");
export isEmpty(x:RRi):bool := Ccode(bool,"mpfi_is_empty(",x,")");
                                    
exponent0(x:RR) ::= Ccode(long,"(long)mpfr_get_exp(",x,")"); -- sometimes int, sometimes long, see gmp.h for type mp_exp_t
exponent0(x:RRi) ::= max(exponent0(rightRR(x)),exponent0(leftRR(x)));
sizeinbase0(x:ZZ,b:int) ::= Ccode( int, "mpz_sizeinbase(",  x, ",", b, ")" );

-- warning: these routines just check the sign bit, and don't verify finiteness!
export isPositive(x:RR):bool := isPositive0(x);
export isPositive(x:RRi):bool := isPositive0(x);

export isNegative(x:RR):bool := isNegative0(x);
export isNegative(x:RRi):bool := isNegative0(x);

export isZero    (x:RR):bool := isZero0(x) && isfinite0(x);
export isZero    (x:RRi):bool := isZero0(x) && isfinite0(x);

export isZero    (x:CC):bool := isZero0(x.re) && isfinite0(x.re) && isZero0(x.im) && isfinite0(x.im);

export defaultPrecision := ulong(53); -- should 53 be computed?

export minExponent := Ccode(long,"(long)mpfr_get_emin()-1");

export maxExponent := Ccode(long,"(long)mpfr_get_emax()");

export exponent(x:ZZ):long := if isZero0(x) then minExponent else long(sizeinbase0(x,2));

export exponent(x:RR):long := if isZero0(x) && isfinite0(x) then minExponent else if isfinite0(x) then exponent0(x) else maxExponent;
export exponent(x:RRi):long := if isZero0(x) && isfinite0(x) then minExponent else if isfinite0(x) then exponent0(x) else maxExponent;
                                    
export exponent(x:CC):long := max(exponent(x.re),exponent(x.im));

export newCCmutable(prec:ulong):CCmutable := CCmutable(newRRmutable(prec),newRRmutable(prec));

export moveToCC(y:CCmutable):CC := CC(moveToRR(y.re), moveToRR(y.im));

export moveToCCandclear(z:CCmutable):CC := (
     w := moveToCC(z);
     clear(z);
     w);

precision0(x:RR) ::= Ccode(ulong,"(unsigned long)mpfr_get_prec(", x, ")");

precision0(x:RRi) ::= Ccode(ulong,"(unsigned  long)mpfi_get_prec(", x, ")");

export precision(x:RR):ulong := precision0(x);

export precision(x:RRi):ulong := precision0(x);

export precision(x:CC):ulong := precision0(x.re);

export toRR(x:RR,prec:ulong):RR := (
     if precision0(x) == prec then return x;
     z := newRRmutable(prec);
     Ccode( void, "mpfr_set(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export toRRi(x:RRi,prec:ulong):RRi := (
    if precision0(x) == prec then return x;
    z := newRRimutable(prec);
    Ccode( void, "mpfi_set(",  z, ",",  x, ")" );
    moveToRRiandclear(z));

export toRR(s:string,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void,  "mpfr_set_str(",  z,", (char *)",  s, "->array,", "10,", "GMP_RNDN", ")" );
     moveToRRandclear(z));

export toRRi(s:string,prec:ulong):RRi := (
    z := newRRimutable(prec);
    Ccode( void,  "mpfi_set_str(",  z,", (char *)",  s, "->array,", "10", ")" );
    moveToRRiandclear(z));

export toRR(x:QQ,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_set_q(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export toRRi(x:QQ,prec:ulong):RRi := (
    z := newRRimutable(prec);
    Ccode( void, "mpfi_set_q(",  z, ",",  x, ")" );
    moveToRRiandclear(z));

export toRR(x:QQ):RR := toRR(x,defaultPrecision);

export toRRi(x:QQ):RRi := toRRi(x,defaultPrecision);

export toRR(x:ZZ,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_set_z(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export toRRi(x:ZZ,prec:ulong):RRi := (
    z := newRRimutable(prec);
    Ccode( void, "mpfi_set_z(",  z, ",",  x, ")" );
    moveToRRiandclear(z));

export toRR(x:ZZ):RR := toRR(x,defaultPrecision);

export toRRi(x:ZZ):RRi := toRRi(x,defaultPrecision);

export toRR(n:int,prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode( void, "mpfr_set_si(",  x, ",(long)", n, ", GMP_RNDN)" );
     moveToRRandclear(x));

export toRRi(n:int,prec:ulong):RRi := (
    x := newRRimutable(prec);
    Ccode( void, "mpfi_set_si(",  x, ",(long)", n, ")" );
    moveToRRiandclear(x));

export toRR(n:ulong,prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode( void, "mpfr_set_ui(",  x, ",(unsigned long)", n, ", GMP_RNDN)" );
     moveToRRandclear(x));

export toRRi(n:ulong,prec:ulong):RRi := (
    x := newRRimutable(prec);
    Ccode( void, "mpfi_set_ui(",  x, ",(unsigned long)", n, ")" );
    moveToRRiandclear(x));

export toRR(n:double,prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode( void, "mpfr_set_d(",  x, ",", n, ", GMP_RNDN)" );
     moveToRRandclear(x));

export toRRi(n:double,prec:ulong):RRi := (
    x := newRRimutable(prec);
    Ccode( void, "mpfi_set_d(",  x, ",", n, ")" );
    moveToRRiandclear(x));

export toRR(n:double):RR := toRR(n,defaultPrecision);

export toRRi(n:double):RRi := toRRi(n,defaultPrecision);

export toRRi(n:RR,prec:ulong):RRi := (
       x := newRRimutable(prec);
       Ccode(void, "mpfi_set_fr(", x, ",", n, ")" );
       moveToRRiandclear(x));

export toRRi(n:RR):RRi := toRRi(n,precision(n));
                                    
export toRRi(a:ZZ,b:ZZ,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set_z( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set_z( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));
                                  
export toRRi(a:ZZ,b:ZZ):RRi := toRRi(a,b,defaultPrecision);

export toRRi(a:ZZ,b:QQ,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set_z( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set_q( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));

export toRRi(a:ZZ, b:QQ):RRi := toRRi(a,b,defaultPrecision);

export toRRi(a:ZZ,b:RR,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set_z( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));

export toRRi(a:ZZ, b:RR):RRi := toRRi(a,b,precision(b));

export toRRi(a:QQ,b:ZZ,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set_q( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set_z( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));
                                  
export toRRi(a:QQ,b:ZZ):RRi := toRRi(a,b,defaultPrecision);

export toRRi(a:QQ,b:QQ,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set_q( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set_q( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));

export toRRi(a:QQ, b:QQ):RRi := toRRi(a,b,defaultPrecision);

export toRRi(a:QQ,b:RR,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set_q( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));

export toRRi(a:QQ, b:RR):RRi := toRRi(a,b,precision(b));

export toRRi(a:RR,b:ZZ,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set_z( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));
                                  
export toRRi(a:RR,b:ZZ):RRi := toRRi(a,b,precision(a));

export toRRi(a:RR,b:QQ,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set_q( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));

export toRRi(a:RR, b:QQ):RRi := toRRi(a,b,precision(a));

export toRRi(a:RR,b:RR,prec:ulong):RRi := (
     x := newRRimutable(prec);
     Ccode( void, "mpfr_set( &", x, "->left," , a, ",GMP_RNDD)");
     Ccode( void, "mpfr_set( &", x, "->right," , b, ",GMP_RNDU)");
     moveToRRiandclear(x));

export toRRi(a:RR, b:RR):RRi := toRRi(a,b,min(precision(a),precision(b)));
                                    
export midpointRR(x:RRi):RR := (
     z := newRRmutable(precision0(x));
     Ccode( RR, "mpfi_mid(",z, ",", x, ")");
     moveToRRandclear(z));
                                    
export midpointRR(x:RRi,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( RR, "mpfi_mid(",z, ",", x, ")");
     moveToRRandclear(z));
                                
export widthRR(x:RRi):RR := (
     z := newRRmutable(precision0(x));
     Ccode( RR, "mpfi_diam_abs(",z, ",", x, ")");
     moveToRRandclear(z));

export infinityRR(prec:ulong,sign:int):RR := (
     x := newRRmutable(prec);
     Ccode(void, "mpfr_set_inf(",x,",",sign,")");
     moveToRRandclear(x));

export infinityRR(prec:ulong):RR := infinityRR(prec,1);
                                    
export infinityRRi(prec:ulong,sign:int):RRi := toRRi(infinityRR(prec,sign));
export infinityRRi(prec:ulong):RRi := toRRi(infinityRR(prec));

export nanRR(prec:ulong):RR := (
     x := newRRmutable(prec);
     Ccode(void, "mpfr_set_nan(",x,")");
     moveToRRandclear(x));
                                    
export nanRRi(prec:ulong):RRi := toRRi(nanRR(prec));

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
                                    
export toDouble(x:RRi):double := toDouble(midpointRR(x));

export toDouble(x:RRcell):double := Ccode( double, "mpfr_get_d(",  x.v, ", GMP_RNDN)" );
                                    
export toDouble(x:RRicell):double := toDouble(midpointRR(x.v));

export flagged():bool := flagged0();

export isfinite(x:RR):bool := isfinite0(x);
                                    
export isfinite(x:RRi):bool := isfinite0(x);

export isinf(x:RR):bool := isinf0(x);

export isinf(x:RRi):bool := isinf0(x);

export isnan(x:RR):bool := isnan0(x);

export isnan(x:RRi):bool := isnan0(x);

export isfinite(x:CC):bool := isfinite0(x.re) && isfinite0(x.im);

export isinf(x:CC):bool := isinf0(x.re) && !isnan0(x.im) || isinf0(x.im) && !isnan0(x.re);

export isnan(x:CC):bool := isnan0(x.re) || isnan0(x.im);

export (x:RR) === (y:RR):bool := (			    -- weak equality
     Ccode( void, "mpfr_clear_flags()" );
     0 != Ccode( int, "mpfr_equal_p(",  x, ",",  y, ")" )
     && !flagged0()
    );

export (x:RRi) === (y:RRi):bool := (                -- weak equality
    Ccode( void, "mpfr_clear_flags()" ); -- No equivalent in mpfi
    leftRR(x) === leftRR(y) && rightRR(x) === rightRR(y) && !flagged0() -- equality is not defined in mpfi
    );

export strictequality(x:RR,y:RR):bool := (
     Ccode( void, "mpfr_clear_flags()" );
     0 != Ccode( int, "mpfr_equal_p(",  x, ",",  y, ")" )
     && !flagged0()
     && sign0(x) == sign0(y)
     && precision0(x) == precision0(y)
    );

export strictequality(x:RRi,y:RRi):bool := (
     Ccode( void, "mpfr_clear_flags()" ); -- No equivalent in mpfi
     leftRR(x) === leftRR(y)
     && rightRR(x) === rightRR(y)
     && !flagged0()
     && sign0(x) == sign0(y)
     && precision0(x) == precision0(y)
    );

compare0(x:RR, y:RR) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp(",  x, ",",  y, "))" );

compare0(x:RRi, y:RRi) ::= Ccode( int,  "(mpfr_clear_flags(),mpfi_cmp(",  x, ",",  y, "))" ); -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(x:RR, y:RR):int := compare0(x,y);	    -- use flagged(), too!
export (x:RR)  >  (y:RR) : bool := compare0(x,y) >  0 && !flagged0();

export (x:RR)  <  (y:RR) : bool := compare0(x,y) <  0 && !flagged0();

export (x:RR)  >= (y:RR) : bool := compare0(x,y) >= 0 && !flagged0();

export (x:RR)  <= (y:RR) : bool := compare0(x,y) <= 0 && !flagged0();
                                    
export compare(x:RRi, y:RRi):int := compare0(x,y);	    -- use flagged(), too!
export (x:RRi)  >  (y:RRi) : bool := compare0(x,y) >  0 && !flagged0();

export (x:RRi)  <  (y:RRi) : bool := compare0(x,y) <  0 && !flagged0();

export (x:RRi)  >= (y:RRi) : bool := (compare0(x,y)>0 || (leftRR(x) == rightRR(y))) && !flagged0();

export (x:RRi)  <= (y:RRi) : bool := (compare0(x,y)<0 || (rightRR(x) == leftRR(y))) && !flagged0();

compare0(x:RR, y:long) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_si(",  x, ",", y, "))" );

compare0(x:RRi, y:long) ::= Ccode( int,  "(mpfr_clear_flags(),mpfi_cmp_si(",  x, ",", y, "))" ); -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

compare0(x:RR, y:int ) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_si(",  x, ",(long)", y, "))" );

compare0(x:RRi, y:int ) ::= Ccode( int, "(mpfr_clear_flags(),mpfi_cmp_si(",  x, ",(long)", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(x:RR, y:long):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_si(",  x, ",", y, "))" );

export compare(y:long, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_si(",  x, ",", y, "))" );
                                    
export compare(x:RRi, y:long):int := Ccode( int, "(mpfr_clear_flags(), mpfi_cmp_si(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(y:long, x:RRi):int := Ccode( int, "(mpfr_clear_flags(),-mpfi_cmp_si(",  x, ",", y, "))" ); -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export (x:RR)  >  (y:int) : bool :=  compare0(x,long(y)) >  0 && !flagged0();
                                    
export (x:RRi)  >  (y:int) : bool :=  compare0(x,long(y)) >  0 && !flagged0();

export (x:RR)  >= (y:int) : bool :=  compare0(x,long(y)) >= 0 && !flagged0();

export (x:RR) === (y:int) : bool :=  compare0(x,long(y)) == 0 && !flagged0();
                                    
export (x:RRi) === (y:int) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();
                                    
export (x:RRi)  >= (y:int) : bool :=  ((compare0(x,long(y)) > 0) || (leftRR(x) === y)) && !flagged0();

export (x:RR)  <  (y:int) : bool :=  compare0(x,long(y)) <  0 && !flagged0();
                                    
export (x:RRi)  <  (y:int) : bool :=  compare0(x,long(y)) <  0 && !flagged0();

export (x:RR)  <= (y:int) : bool :=  compare0(x,long(y)) <= 0 && !flagged0();
                                    
export (x:RRi)  <= (y:int) : bool :=  (compare0(x,long(y)) < 0 || rightRR(x) === y) && !flagged0();

export (x:CC) === (y:int) : bool :=  x.re === y && x.im === 0;
                                    
compare0(x:RR, y:double) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_d(",  x, ",", y, "))" );  

export compare(x:RR, y:double):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_d(",  x, ",", y, "))" );

export compare(y:double, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_d(",  x, ",", y, "))" );
                                    
compare0(x:RRi, y:double) ::= Ccode( int, "(mpfr_clear_flags(),mpfi_cmp_d(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(x:RRi, y:double):int := Ccode( int, "(mpfr_clear_flags(), mpfi_cmp_d(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(y:double, x:RRi):int := Ccode( int, "(mpfr_clear_flags(),-mpfi_cmp_d(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export (x:RR)  >  (y:double) : bool :=  compare0(x,y) >  0 && !flagged0();
                                    
export (x:RRi)  >  (y:double) : bool :=  compare0(x,y) >  0 && !flagged0();

export (x:RR)  >= (y:double) : bool :=  compare0(x,y) >= 0 && !flagged0();

export (x:RR) === (y:double) : bool :=  compare0(x,y) == 0 && !flagged0();
                                    
export (x:RRi) === (y:double) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();
                                    
export (x:RRi)  >= (y:double) : bool :=  ((compare0(x,y) > 0) || (leftRR(x) === y)) && !flagged0();

export (x:RR)  <  (y:double) : bool :=  compare0(x,y) <  0 && !flagged0();
                                    
export (x:RRi)  <  (y:double) : bool :=  compare0(x,y) <  0 && !flagged0();

export (x:RR)  <= (y:double) : bool :=  compare0(x,y) <= 0 && !flagged0();
                                    
export (x:RRi)  <= (y:double) : bool :=  ((compare0(x,y) < 0) || (rightRR(x) === y)) && !flagged0();

compare0(x:RR, y:ZZ) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_z(",  x, ",", y, "))" );

export compare(x:RR, y:ZZ):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_z(",  x, ",", y, "))" );

export compare(y:ZZ, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_z(",  x, ",", y, "))" );
                                    
compare0(x:RRi, y:ZZ) ::= Ccode( int, "(mpfr_clear_flags(),mpfi_cmp_z(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(x:RRi, y:ZZ):int := Ccode( int, "(mpfr_clear_flags(), mpfi_cmp_z(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(y:ZZ, x:RRi):int := Ccode( int, "(mpfr_clear_flags(),-mpfi_cmp_z(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export (x:RR)  >  (y:ZZ) : bool :=  compare0(x,y) >  0 && !flagged0();
                                    
export (x:RRi)  >  (y:ZZ) : bool :=  compare0(x,y) >  0 && !flagged0();

export (x:RR)  >= (y:ZZ) : bool :=  compare0(x,y) >= 0 && !flagged0();

export (x:RR) === (y:ZZ) : bool :=  compare0(x,y) == 0 && !flagged0();
                                    
export (x:RRi) === (y:ZZ) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();

export (y:ZZ) === (x:RR) : bool :=  compare0(x,y) == 0 && !flagged0();
                                    
export (y:ZZ) === (x:RRi) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();
                                    
export (x:RRi)  >= (y:ZZ) : bool :=  ((compare0(x,y) > 0) || (leftRR(x) === y)) && !flagged0();

export (x:RR)  <  (y:ZZ) : bool :=  compare0(x,y) <  0 && !flagged0();
                                    
export (x:RRi)  <  (y:ZZ) : bool :=  compare0(x,y) <  0 && !flagged0();

export (x:RR)  <= (y:ZZ) : bool :=  compare0(x,y) <= 0 && !flagged0();
                                    
export (x:RRi)  <= (y:ZZ) : bool :=  ((compare0(x,y) < 0) || (rightRR(x) === y)) && !flagged0();

compare0(x:RR, y:QQ) ::= Ccode( int, "(mpfr_clear_flags(),mpfr_cmp_q(",  x, ",", y, "))" );

export compare(x:RR, y:QQ):int := Ccode( int, "(mpfr_clear_flags(), mpfr_cmp_q(",  x, ",", y, "))" );

export compare(y:QQ, x:RR):int := Ccode( int, "(mpfr_clear_flags(),-mpfr_cmp_q(",  x, ",", y, "))" );
                                    
compare0(x:RRi, y:QQ) ::= Ccode( int, "(mpfr_clear_flags(),mpfi_cmp_q(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(x:RRi, y:QQ):int := Ccode( int, "(mpfr_clear_flags(), mpfi_cmp_q(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export compare(y:QQ, x:RRi):int := Ccode( int, "(mpfr_clear_flags(),-mpfi_cmp_q(",  x, ",", y, "))" );  -- No equivalent for clear in mpfi.  -- returns 0 when x and y overlap, different behavior than mpfr

export (x:RR)  >  (y:QQ) : bool :=  compare0(x,y) >  0 && !flagged0();
                                    
export (x:RRi)  >  (y:QQ) : bool :=  compare0(x,y) >  0 && !flagged0();

export (x:RR)  >= (y:QQ) : bool :=  compare0(x,y) >= 0 && !flagged0();

export (x:RR) === (y:QQ) : bool :=  compare0(x,y) == 0 && !flagged0();
                                    
export (x:RRi) === (y:QQ) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();

export (y:QQ) === (x:RR) : bool :=  compare0(x,y) == 0 && !flagged0();
                                    
export (y:QQ) === (x:RRi) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();
                                    
export (x:RRi)  >= (y:QQ) : bool :=  ((compare0(x,y) > 0) || (leftRR(x) === y)) && !flagged0();

export (x:RR)  <  (y:QQ) : bool :=  compare0(x,y) <  0 && !flagged0();
                                    
export (x:RRi)  <  (y:QQ) : bool :=  compare0(x,y) <  0 && !flagged0();

export (x:RR)  <= (y:QQ) : bool :=  compare0(x,y) <= 0 && !flagged0();
                                    
export (x:RRi)  <= (y:QQ) : bool :=  ((compare0(x,y) < 0) || (rightRR(x) === y)) && !flagged0();
                                    
export (x:RRi) === (y:RR) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();
                                    
export (y:RR) === (x:RRi) : bool := rightRR(x) === y && leftRR(x) === y && !flagged0();
                                    
export contains (y:ZZ, x:RRi):bool := Ccode(int,"mpfi_is_inside_z(",y,",",x,")") > 0;
                                    
export contains (y:QQ, x:RRi):bool := Ccode(int,"mpfi_is_inside_q(",y,",",x,")") > 0;
                                    
export contains (y:RR, x:RRi):bool := Ccode(int,"mpfi_is_inside_fr(",y,",",x,")") > 0;
                                    
export contains (y:RRi, x:RRi):bool := Ccode(int,"mpfi_is_inside(",y,",",x,")") > 0;
                                    
export intersectRRi (x:RRi, y:RRi):RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_intersect(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export intersectRRi (x:RRi, y:RRi, prec:ulong):RRi := (
     z := newRRimutable(prec);
     Ccode( void, "mpfi_intersect(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export hash(x:RR):int := int(precision0(x)) + Ccode(int, 
     "mpfr_hash(",					    -- see gmp_aux.c for this function
          x, 
     ")"
    );

export hash(x:RRi):int := int(precision0(x)) + Ccode(int,
    "mpfi_hash(",     -- Added for MPFI
    x,
    ")"
    ); -- End added for MPFI

export hash(x:CC):int := 123 + hash(x.re) + 111 * hash(x.im);
     
export (x:RR) + (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_add(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) + (y:RRi) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_add(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (x:RR) + (y:int) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_add_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RRi) + (y:int) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_add_si(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
     
export (x:RR) + (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_add_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RRi) + (y:ZZ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_add_z(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
     
export (x:RR) + (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_add_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (x:RRi) + (y:QQ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_add_q(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (x:RRi) + (y:RR) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_add_fr(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export - (y:RR) : RR := (
     z := newRRmutable(precision0(y));
     Ccode( void, "mpfr_neg(", z, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export - (y:RRi) : RRi := (
    z := newRRimutable(precision0(y));
    Ccode( void, "mpfi_neg(", z, ",",  y, ")" );
    moveToRRiandclear(z));

export (x:RR) - (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_sub(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) - (y:RRi) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_sub(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (x:RR) - (y:int) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sub_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export (y:int) - (x:RR) : RR := -(x-y);
  
export (x:RRi) - (y:int) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sub_si(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export (y:int) - (x:RRi) : RRi := -(x-y);
                                    
export (x:RR) - (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sub_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) - (y:ZZ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sub_z(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
     
export (x:RR) - (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sub_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) - (y:QQ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sub_q(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export (x:RRi) - (y:RR) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_sub_fr(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export abs(x:RR) : RR := if isNegative0(x) then -x else x;
                                    
export abs(x:RRi) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_abs(", z, ",",  x, ")" );
     moveToRRiandclear(z));

export (x:RR) * (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_mul(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
 
export (x:RRi) * (y:RRi) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_mul(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (x:RR) * (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) * (y:ZZ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_mul_z(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (y:ZZ) * (x:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (y:ZZ) * (x:RRi) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_mul_z(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (x:RR) * (y:int) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) * (y:int) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_mul_si(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (y:int) * (x:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (y:int) * (x:RRi) : RRi := x*y;
     
export (x:RR) * (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) * (y:QQ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_mul_q(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export (x:RRi) * (y:RR) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_mul_fr(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (x:RR) / (y:RR) : RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_div(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) / (y:RRi) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_div(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));

export (x:RR) / (y:long) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_div_si(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RR) / (y:int) : RR := x / long(y);
                                    
export (x:RRi) / (y:long) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_div_si(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export (y:long) / (x:RRi) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_si_div(", z, ",",  y, ",",  x, ")" );
     moveToRRiandclear(z));
                                    
export (x:RRi) / (y:int) : RRi := x / long(y);
                                    
export (y:int) / (x:RRi) : RRi := long(y) / x;                                     
     
export (x:RR) / (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_div_z(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) / (y:ZZ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_div_z(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export (y:ZZ) / (x:RRi) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_z_div(", z, ",",  y, ",",  x, ")" );
     moveToRRiandclear(z));
     
export (x:RR) / (y:QQ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_div_q(", z, ",",  x, ",",  y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) / (y:QQ) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_div_q(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export (y:QQ) / (x:RRi) : RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_q_div(", z, ",",  y, ",",  x, ")" );
     moveToRRiandclear(z));
                                    
export (x:RRi) / (y:RR) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_div_fr(", z, ",",  x, ",",  y, ")" );
     moveToRRiandclear(z));
                                    
export (y:RR) / (x:RRi) : RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_fr_div(", z, ",",  y, ",",  x, ")" );
     moveToRRiandclear(z));

export sqrt(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sqrt(",  z, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export sqrt(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sqrt(",  z, ",",  x, ")" );
     moveToRRiandclear(z));

export (x:RR) ^ (n:long) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow_si(",  z, ",",  x, ",", n, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) ^ (n:long) : RRi := (
    if (n == long(0)) then return toRRi(1,precision0(x));
                                    
    left := newRRmutable(precision0(x));
    right := newRRmutable(precision0(x));
    extra := newRRmutable(precision0(x));
          
    upper := newRRmutable(precision0(x));
    lower := newRRmutable(precision0(x));
          
    if contains0(x) then Ccode(void, "mpfr_set_si(", extra, ",0, GMP_RNDN)")
    else Ccode( void, "mpfr_set_inf(", extra, ",-1)" );
                                    
    Ccode( void, "mpfr_pow_si(",  left, ",",  leftRR(x), ",", n, ", GMP_RNDU)" );
    Ccode( void, "mpfr_pow_si(",  right, ",",  rightRR(x), ",", n, ", GMP_RNDU)" );
          
    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") > 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", left, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", right, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"));
          
    if !contains0(x) then Ccode( void, "mpfr_set_inf(", extra, ",1)" );
                                    
    Ccode( void, "mpfr_pow_si(",  left, ",",  leftRR(x), ",", n, ", GMP_RNDD)" );
    Ccode( void, "mpfr_pow_si(",  right, ",",  rightRR(x), ",", n, ", GMP_RNDD)" );
          
    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") < 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", left, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", right, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"));
                 
    clear(left);
    clear(right);
    clear(extra);
                                    
    toRRi(moveToRRandclear(lower),moveToRRandclear(upper)));
                                    
export (x:RR) ^ (n:ulong) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow_ui(",  z, ",",  x, ",", n, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) ^ (n:ulong) : RRi := (
    if (n == ulong(0)) then return toRRi(1,precision0(x));
                                    
    left := newRRmutable(precision0(x));
    right := newRRmutable(precision0(x));
    extra := newRRmutable(precision0(x));
          
    upper := newRRmutable(precision0(x));
    lower := newRRmutable(precision0(x));
          
    if contains0(x) then Ccode(void, "mpfr_set_ui(", extra, ",0, GMP_RNDN)")
    else Ccode( void, "mpfr_set_inf(", extra, ",-1)" );
                                    
    Ccode( void, "mpfr_pow_ui(",  left, ",",  leftRR(x), ",", n, ", GMP_RNDU)" );
    Ccode( void, "mpfr_pow_ui(",  right, ",",  rightRR(x), ",", n, ", GMP_RNDU)" );
          
    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") > 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", left, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", right, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"));
          
    if !contains0(x) then Ccode( void, "mpfr_set_inf(", extra, ",1)" );
                                    
    Ccode( void, "mpfr_pow_ui(",  left, ",",  leftRR(x), ",", n, ", GMP_RNDD)" );
    Ccode( void, "mpfr_pow_ui(",  right, ",",  rightRR(x), ",", n, ", GMP_RNDD)" );
          
    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") < 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", left, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", right, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"));
                 
    clear(left);
    clear(right);
    clear(extra);
                                    
    toRRi(moveToRRandclear(lower),moveToRRandclear(upper)));

export pow10(n:ulong,prec:ulong):RR := (
     z := newRRmutable(prec);
     Ccode( void, "mpfr_ui_pow_ui(",  z, ",", ulong(10), ",", n, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export pow10RRi(n:ulong,prec:ulong):RRi := (
     left := newRRmutable(prec);
     right := newRRmutable(prec);
     Ccode( void, "mpfr_ui_pow_ui(",  left, ",", ulong(10), ",", n, ", GMP_RNDD)" );
     Ccode( void, "mpfr_ui_pow_ui(",  right, ",", ulong(10), ",", n, ", GMP_RNDU)" );
     toRRi(moveToRRandclear(left),moveToRRandclear(right)));

export pow10(n:long,prec:ulong):RR := (
     if n < long(0)
     then (pow10(ulong(-n),prec))^long(-1)
     else pow10(ulong(n),prec));
                                           
export pow10RRi(n:long,prec:ulong):RRi := (
     if n < long(0)
     then (pow10RRi(ulong(-n),prec))^long(-1)
     else pow10RRi(ulong(n),prec));

export pow10(n:int,prec:ulong):RR := pow10(long(n),prec);
                                           
export pow10RRi(n:int,prec:ulong):RRi := pow10RRi(long(n),prec);

export (n:ulong) ^ (x:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_ui_pow(",  z, ",", n, ",",  x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (n:ulong) ^ (x:RRi) : RRi := (
     if (n == ulong(1)) then return toRRi(1,precision0(x));
                                     
     left := newRRmutable(precision0(x));
     right := newRRmutable(precision0(x));
                                     
     if (n > ulong(1)) then (
        Ccode( void, "mpfr_ui_pow(",  left, ",", n, ",", leftRR(x), ", GMP_RNDD)" );
        Ccode( void, "mpfr_ui_pow(",  right, ",", n, ",", rightRR(x), ", GMP_RNDU)" ))
     else (
        Ccode( void, "mpfr_ui_pow(",  left, ",", n, ",", rightRR(x), ", GMP_RNDD)" );
        Ccode( void, "mpfr_ui_pow(",  right, ",", n, ",", leftRR(x), ", GMP_RNDU)" ));
                                     
     toRRi(moveToRRandclear(left),moveToRRandclear(right)));

export (x:RR) ^ (y:ZZ) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow_z(",  z, ",",  x, ",", y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (x:RRi) ^ (y:ZZ) : RRi := (
    if (isZero0(y)) then return toRRi(1,precision0(x));

    left := newRRmutable(precision0(x));
    right := newRRmutable(precision0(x));
    extra := newRRmutable(precision0(x));

    upper := newRRmutable(precision0(x));
    lower := newRRmutable(precision0(x));

    if contains0(x) then Ccode(void, "mpfr_set_ui(", extra, ",0, GMP_RNDN)")
    else Ccode( void, "mpfr_set_inf(", extra, ",-1)" );

    Ccode( void, "mpfr_pow_z(",  left, ",",  leftRR(x), ",", y, ", GMP_RNDU)" );
    Ccode( void, "mpfr_pow_z(",  right, ",",  rightRR(x), ",", y, ", GMP_RNDU)" );

    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") > 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", left, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", right, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"));

    if !contains0(x) then Ccode( void, "mpfr_set_inf(", extra, ",1)" );

    Ccode( void, "mpfr_pow_z(",  left, ",",  leftRR(x), ",", y, ", GMP_RNDD)" );
    Ccode( void, "mpfr_pow_z(",  right, ",",  rightRR(x), ",", y, ", GMP_RNDD)" );

    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") < 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", left, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", right, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"));

    clear(left);
    clear(right);
    clear(extra);

    toRRi(moveToRRandclear(lower),moveToRRandclear(upper)));

export (x:RR) ^ (y:RR) : RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_pow(",  z, ",",  x, ",", y, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                    
export (y:RR) ^ (x:RRi) : RRi := (
     -- Needs y >= 0
     if (y == toRR(1,precision0(y))) then return toRRi(1,min(precision0(x),precision0(y)));
                                     
     left := newRRmutable(min(precision0(x),precision0(y)));
     right := newRRmutable(min(precision0(x),precision0(y)));
                                     
     if (y > toRR(1,precision0(y))) then (
        Ccode( void, "mpfr_pow(",  left, ",", y, ",", leftRR(x), ", GMP_RNDD)" );
        Ccode( void, "mpfr_pow(",  right, ",", y, ",", rightRR(x), ", GMP_RNDU)" ))
     else (
        Ccode( void, "mpfr_pow(",  left, ",", y, ",", rightRR(x), ", GMP_RNDD)" );
        Ccode( void, "mpfr_pow(",  right, ",", y, ",", leftRR(x), ", GMP_RNDU)" ));
                                     
     toRRi(moveToRRandclear(left),moveToRRandclear(right)));
                                    
export (x:RRi) ^ (y:RR) : RRi := (
    if (y == toRR(0,precision0(y))) then return toRRi(1,min(precision0(x),precision0(y)));

    left := newRRmutable(min(precision0(x),precision0(y)));
    right := newRRmutable(min(precision0(x),precision0(y)));
    extra := newRRmutable(min(precision0(x),precision0(y)));

    upper := newRRmutable(min(precision0(x),precision0(y)));
    lower := newRRmutable(min(precision0(x),precision0(y)));

    if contains0(x) then Ccode(void, "mpfr_set_ui(", extra, ",0, GMP_RNDN)")
    else Ccode( void, "mpfr_set_inf(", extra, ",-1)" );

    Ccode( void, "mpfr_pow(",  left, ",",  leftRR(x), ",", y, ", GMP_RNDU)" );
    Ccode( void, "mpfr_pow(",  right, ",",  rightRR(x), ",", y, ", GMP_RNDU)" );

    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") > 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", left, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") > 0)
        then Ccode(void, "mpfr_set(", upper, ",", right, ", GMP_RNDU)")
        else Ccode(void, "mpfr_set(", upper, ",", extra, ", GMP_RNDU)"));

    if !contains0(x) then Ccode( void, "mpfr_set_inf(", extra, ",1)" );

    Ccode( void, "mpfr_pow(",  left, ",",  leftRR(x), ",", y, ", GMP_RNDD)" );
    Ccode( void, "mpfr_pow(",  right, ",",  rightRR(x), ",", y, ", GMP_RNDD)" );

    if (Ccode(int, "mpfr_cmp(", left, ",", right, ")") < 0)
    then (if (Ccode(int, "mpfr_cmp(", left, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", left, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"))
    else (if (Ccode(int, "mpfr_cmp(", right, ",", extra, ")") < 0)
        then Ccode(void, "mpfr_set(", lower, ",", right, ", GMP_RNDD)")
        else Ccode(void, "mpfr_set(", lower, ",", extra, ", GMP_RNDD)"));

    clear(left);
    clear(right);
    clear(extra);

    toRRi(moveToRRandclear(lower),moveToRRandclear(upper)));
                                    
export (y:RRi) ^ (x:RRi) : RRi := (
     -- Assumes that y >= 0
--     if (y == toRRi(1,precision0(y))) then return toRRi(1,min(precision0(x),precision0(y)));

     left := newRRmutable(min(precision0(x),precision0(y)));
     right := newRRmutable(min(precision0(x),precision0(y)));
                                     
     upperright := newRRmutable(min(precision0(x),precision0(y)));
     upperleft := newRRmutable(min(precision0(x),precision0(y)));
     lowerright := newRRmutable(min(precision0(x),precision0(y)));
     lowerleft := newRRmutable(min(precision0(x),precision0(y)));

     Ccode( void, "mpfr_pow(",  upperleft, ",",  leftRR(y), ",", rightRR(x), ", GMP_RNDU)" );
     Ccode( void, "mpfr_pow(",  upperright, ",",  rightRR(y), ",", rightRR(x), ", GMP_RNDU)" );
     Ccode( void, "mpfr_pow(",  lowerleft, ",",  leftRR(y), ",", leftRR(x), ", GMP_RNDU)" );
     Ccode( void, "mpfr_pow(",  lowerright, ",",  rightRR(y), ",", leftRR(x), ", GMP_RNDU)" );
                                   
     if (Ccode(int, "mpfr_cmp(", upperleft, ",", upperright, ")") > 0) then (
         if (Ccode(int, "mpfr_cmp(", lowerleft, ",", lowerright, ")") > 0) then (
             if (Ccode(int, "mpfr_cmp(", upperleft, ",", lowerleft, ")") > 0)
             then Ccode(void, "mpfr_set(", right, ",", upperleft, ", GMP_RNDU)")
             else Ccode(void, "mpfr_set(", right, ",", lowerleft, ", GMP_RNDU)"))
         else (
             if (Ccode(int, "mpfr_cmp(", upperleft, ",", lowerright, ")") > 0)
             then Ccode(void, "mpfr_set(", right, ",", upperleft, ", GMP_RNDU)")
             else Ccode(void, "mpfr_set(", right, ",", lowerright, ", GMP_RNDU)")))
      else (
         if (Ccode(int, "mpfr_cmp(", lowerleft, ",", lowerright, ")") > 0) then (
             if (Ccode(int, "mpfr_cmp(", upperright, ",", lowerleft, ")") > 0)
             then Ccode(void, "mpfr_set(", right, ",", upperright, ", GMP_RNDU)")
             else Ccode(void, "mpfr_set(", right, ",", lowerleft, ", GMP_RNDU)"))
         else (
             if (Ccode(int, "mpfr_cmp(", upperright, ",", lowerright, ")") > 0)
             then Ccode(void, "mpfr_set(", right, ",", upperright, ", GMP_RNDU)")
             else Ccode(void, "mpfr_set(", right, ",", lowerright, ", GMP_RNDU)")));

     Ccode( void, "mpfr_pow(",  upperleft, ",",  leftRR(y), ",", rightRR(x), ", GMP_RNDD)" );
     Ccode( void, "mpfr_pow(",  upperright, ",",  rightRR(y), ",", rightRR(x), ", GMP_RNDD)" );
     Ccode( void, "mpfr_pow(",  lowerleft, ",",  leftRR(y), ",", leftRR(x), ", GMP_RNDD)" );
     Ccode( void, "mpfr_pow(",  lowerright, ",",  rightRR(y), ",", leftRR(x), ", GMP_RNDD)" );
                                   
     if (Ccode(int, "mpfr_cmp(", upperleft, ",", upperright, ")") < 0) then (
         if (Ccode(int, "mpfr_cmp(", lowerleft, ",", lowerright, ")") < 0) then (
             if (Ccode(int, "mpfr_cmp(", upperleft, ",", lowerleft, ")") < 0)
             then Ccode(void, "mpfr_set(", left, ",", upperleft, ", GMP_RNDD)")
             else Ccode(void, "mpfr_set(", left, ",", lowerleft, ", GMP_RNDD)"))
         else (
             if (Ccode(int, "mpfr_cmp(", upperleft, ",", lowerright, ")") < 0)
             then Ccode(void, "mpfr_set(", left, ",", upperleft, ", GMP_RNDD)")
             else Ccode(void, "mpfr_set(", left, ",", lowerright, ", GMP_RNDD)")))
      else (
         if (Ccode(int, "mpfr_cmp(", lowerleft, ",", lowerright, ")") < 0) then (
             if (Ccode(int, "mpfr_cmp(", upperright, ",", lowerleft, ")") < 0)
             then Ccode(void, "mpfr_set(", left, ",", upperright, ", GMP_RNDD)")
             else Ccode(void, "mpfr_set(", left, ",", lowerleft, ", GMP_RNDD)"))
         else (
             if (Ccode(int, "mpfr_cmp(", upperright, ",", lowerright, ")") < 0)
             then Ccode(void, "mpfr_set(", left, ",", upperright, ", GMP_RNDD)")
             else Ccode(void, "mpfr_set(", left, ",", lowerright, ", GMP_RNDD)")));

     clear(upperleft);
     clear(lowerleft);
     clear(upperright);
     clear(lowerright);

     toRRi(moveToRRandclear(left),moveToRRandclear(right)));
                                    
export floor(x:RR) : ZZ := (
     if !isfinite0(x) then return zeroZZ;			    -- nothing else to do!
     w := newZZmutable();
     Ccode( void, "mpfr_get_z(", w, ",", x, ", GMP_RNDD)" );
     moveToZZandclear(w));
                                     
export floor(x:RRi) : ZZ := floor(leftRR(x));

export ceil(x:RR) : ZZ := (
     if !isfinite0(x) then return zeroZZ;			    -- nothing else to do!
     w := newZZmutable();
     Ccode( void, "mpfr_get_z(", w, ",", x, ", GMP_RNDU)" );
     moveToZZandclear(w));
                                     
export ceil(x:RRi) : ZZ := ceil(rightRR(x));

export round(x:RR) : ZZ := (
     if !isfinite0(x) then return zeroZZ;			    -- nothing else to do!
     w := newZZmutable();
     Ccode( void, "mpfr_get_z(", w, ",", x, ", GMP_RNDN)" );
     moveToZZandclear(w));
                                    
export round(x:RRi) : ZZ := (
     if !isfinite0(x) then return zeroZZ;			    -- nothing else to do!
     w := newRRmutable(precision0(x));
     Ccode( void, "mpfi_get_fr(", w, ",", x, ")" );
     if !isfinite0(w) then return zeroZZ;			    -- nothing else to do!
     y := newZZmutable();
     Ccode( void, "mpfr_get_z(", y, ",", w, ", GMP_RNDN)" );
     clear(w);
     moveToZZandclear(y));

export (x:RR) << (n:long) : RR := (
     if n == long(0) then return x;
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_mul_2si(", z, ",", x, ",", n, ",GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export (x:RRi) << (n:long) : RRi := (
     if n == long(0) then return x;
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_mul_2si(", z, ",", x, ",", n, ")" );
     moveToRRiandclear(z));

export (x:RR) >> (n:long) : RR := x << -n;
                                     
export (x:RRi) >> (n:long) : RRi := x << -n;

export (x:RR) << (n:int) : RR := x << long(n);
                                     
export (x:RRi) << (n:int) : RRi := x << long(n);

export (x:RR) >> (n:int) : RR := x << long(-n);
                                     
export (x:RRi) >> (n:int) : RRi := x << long(-n);

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
                                     
export piRRi(prec:ulong):RRi := (
     z := newRRimutable(prec);
     Ccode( void, "mpfi_const_pi(",  z, ")" );
     moveToRRiandclear(z));
                                    
export eRRi(prec:ulong):RRi := (
     z := newRRimutable(prec);
     Ccode( void, "mpfi_const_euler(",  z, ")" );
     moveToRRiandclear(z));

export cRRi(prec:ulong):RRi := (
     z := newRRimutable(prec);
     Ccode( void, "mpfi_const_catalan(",  z, ")" );
     moveToRRiandclear(z));

export exp(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_exp(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export exp(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_exp(", z, ",", x, ")" );
     moveToRRiandclear(z));

export log(x:RR):RR := (				    -- works only if x>0
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_log(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export log(x:RRi):RRi := (				    -- works only if x>0
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_log(", z, ",", x, ")" );
     moveToRRiandclear(z));

export log(b:RR,x:RR):RR := (				    -- works only if x>0 and b>0
     if precision0(b) < precision0(x) then x = toRR(x,precision0(b))
     else if precision0(b) > precision0(x) then b = toRR(b,precision0(x));
     log(x)/log(b));
                                     
export log(b:RRi,x:RRi):RRi := (				    -- works only if x>0 and b>0
     if precision0(b) < precision0(x) then x = toRRi(x,precision0(b))
     else if precision0(b) > precision0(x) then b = toRRi(b,precision0(x));
     log(x)/log(b));

export sin(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sin(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export sin(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sin(", z, ",", x, ")" );
     moveToRRiandclear(z));

export cos(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_cos(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export cos(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_cos(", z, ",", x, ")" );
     moveToRRiandclear(z));

export tan(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_tan(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export tan(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_tan(", z, ",", x, ")" );
     moveToRRiandclear(z));

export asin(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_asin(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export asin(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_asin(", z, ",", x, ")" );
     moveToRRiandclear(z));

export acos(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_acos(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export acos(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_acos(", z, ",", x, ")" );
     moveToRRiandclear(z));

export atan(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_atan(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export atan(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_atan(", z, ",", x, ")" );
     moveToRRiandclear(z));

export atan2(y:RR,x:RR):RR := (
     -- if isZero0(x) && isZero0(y) && isfinite0(x) && isfinite0(y) then return nanRR(min(precision0(x),precision0(y)));
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_atan2(", z, ",", y, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export atan2(y:RRi,x:RRi):RRi := (
     z := newRRimutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfi_atan2(", z, ",", y, ",", x, ")" );
     moveToRRiandclear(z));

export Beta(x:RR,y:RR):RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_beta(", z, ",", y, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export agm(x:RR,y:RR):RR := (
     z := newRRmutable(min(precision0(x),precision0(y)));
     Ccode( void, "mpfr_agm(", z, ",", x, ",", y, ", GMP_RNDN)" );
     moveToRRandclear(z));

export sinh(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sinh(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export sinh(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sinh(", z, ",", x, ")" );
     moveToRRiandclear(z));

export cosh(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_cosh(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export cosh(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_cosh(", z, ",", x, ")" );
     moveToRRiandclear(z));

export tanh(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_tanh(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export tanh(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_tanh(", z, ",", x, ")" );
     moveToRRiandclear(z));

export sec(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sec(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export sec(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sec(", z, ",", x, ")" );
     moveToRRiandclear(z));

export csc(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_csc(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export csc(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_csc(", z, ",", x, ")" );
     moveToRRiandclear(z));

export cot(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_cot(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export cot(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_cot(", z, ",", x, ")" );
     moveToRRiandclear(z));

export sech(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_sech(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export sech(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_sech(", z, ",", x, ")" );
     moveToRRiandclear(z));

export csch(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_csch(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export csch(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_csch(", z, ",", x, ")" );
     moveToRRiandclear(z));

export coth(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_coth(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export coth(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_coth(", z, ",", x, ")" );
     moveToRRiandclear(z));

export factorial(x:ulong):ZZ := (
     w := newZZmutable();
     Ccode( void, "mpz_fac_ui(", w, ",", x, ")" );
     moveToZZandclear(w));

export log1p(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_log1p(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export log1p(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_log1p(", z, ",", x, ")" );
     moveToRRiandclear(z));

export expm1(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_expm1(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));
                                     
export expm1(x:RRi):RRi := (
     z := newRRimutable(precision0(x));
     Ccode( void, "mpfi_expm1(", z, ",", x, ")" );
     moveToRRiandclear(z));

export Gamma(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_gamma(", z, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export incompleteGamma(s:RR,x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_gamma_inc(", z, ",", s, ",", x, ", GMP_RNDN)" );
     moveToRRandclear(z));

export Digamma(x:RR):RR := (
     z := newRRmutable(precision0(x));
     Ccode( void, "mpfr_digamma(", z, ",", x, ", GMP_RNDN)" );
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

export sign(x:RRi):bool := 0 != Ccode(int,"mpfi_is_neg(",x,")");

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
