-- this file is small, so the exported definitions can be imported into the engine

header "";

--This file contains gmp related functions.
--Functions in this file may make calls to stdio.

use gmp;
use stdio;
use err;

pow(x:ZZmutable, y:ZZ, n:ulong) ::= Ccode( void, "mpz_pow_ui(", x, ",", y, ",", n, ")" );

export (x:ZZ) ^ (n:int) : ZZ := (
     if n < 0 then fatal("internal error: negative exponent for integer power"); -- what else can we do???
     if isZero(x) then return x;
     y := newZZmutable();
     pow(y,x,ulong(n));
     moveToZZandclear(y));

export (x:ZZ) ^ (n:ZZ) : ZZ := (
     if isNegative(n) then fatal("internal error: negative exponent for integer power"); -- what else can we do???
     if !isULong(n) then fatal("integer exponent too large");
     x^toULong(n));

export powermod(x:ZZ, y:ZZ, n:ZZ) : ZZ := (
     -- z = x^y mod n
     z := newZZmutable();
     Ccode( void, "mpz_powm(",  z, ",",  x, ",",  y, ",",  n, ")" );
     moveToZZandclear(z)
     );

export (x:QQ) ^ (nn:ZZ) : QQ := (
     if !isLong(nn) then fatal("integer exponent too large");
     n := toLong(nn);
     if n == long(0) then return toRational(1);
     if n < 0 then (
	  x = inv(x);
	  n = -n);
     newQQCanonical(numeratorRef(x)^ulong(n), denominatorRef(x)^ulong(n))
     );

-- printing

log(x:double) ::= Ccode(double, "log(", x, ")" );
floor(x:double) ::= Ccode(double, "floor(", x, ")" );

log2ten := log(10.) / log(2.);
base := 10;

getstr(returnexponent:long, base:int, sigdigs:int, x:RR) ::= (
     strptr := Ccode(charstarOrNull, "(M2_charstarOrNull) mpfr_get_str((char *)0,&", returnexponent, ",",
	  base, ",(size_t)", sigdigs, ",", x, ",GMP_RNDN)");
     ret := tostring(strptr);
     Ccode(void, "mpfr_free_str(", strptr, ")");
     ret);

getstr(returnexponent:long, base:int, sigdigs:int, x:RRi) ::= (
     strptr := Ccode(charstarOrNull, "(M2_charstarOrNull) mpfr_get_str((char *)0,&", returnexponent, ",",
	  base, ",(size_t)", sigdigs, ",", x, ",GMP_RNDN)");
     ret := tostring(strptr);
     Ccode(void, "mpfr_free_str(", strptr, ")");
     ret);

export format(
     s:int,			  -- number of significant digits (0 means all)
     ac:int,	    -- accuracy, how far to right of point to go (-1 means all)
     l:int,					   -- max number leading zeroes
     t:int,				    -- max number extra trailing digits
     sep:string,		     -- separator between mantissa and exponent
     x:RR						-- the number to format
     ) : array(string) := (	   -- return: ("-","132.456") or ("","123.456")
     ng := sign(x);
     if isinf(x) then return array(string)(if ng then "-" else "","infinity");
     if isnan(x) then return array(string)(if ng then "-" else "","NotANumber");
     meaningful := int(floor(precision(x) / log2ten));
     if s == 0 || s > meaningful then s = meaningful; -- print at most the "meaningful" digits
     sgn := "";
     if ng then (
	  sgn = "-";
	  x = -x;
	  );
     if x === 0 then return array(string)(if ng then "-" else "","0");
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
	  if maxmanlen < 0 then return array(string)("","0");
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
     if manlen == 0 then return array(string)("","0");
     array(string)(
	  sgn,
	  concatenate(
	       array(string)(
		    if pt == 0 then "." else "",
		    if l == 0 then "" else new string len l do provide '0',
		    substr(mantissa,0,pt),
		    if pt > 0 && pt < manlen then "." else "",
		    substr(mantissa,pt,manlen-pt),
		    new string len t do provide '0',
		    if ex != long(0) then sep else "",
		    if ex != long(0) then tostring(ex) else ""
		    ))));

export printingPrecision := 6;				    -- 0 indicates all
export printingAccuracy := -1;				    -- -1 indicates all
export printingLeadLimit := 5;
export printingTrailLimit := 5;
export printingSeparator := "e";			    -- was "*10^"

-- this can be used by the engine for printing matrices to a uniform precision

export tostringRR(x:RR):string := concatenate(format(printingPrecision,printingAccuracy,printingLeadLimit,printingTrailLimit,printingSeparator,x));
tostringRRpointer = tostringRR;

export tostringRRi(x:RRi):string := concatenate( 
    array(string)(
       	"[",
       	tostringRR(leftRR(x)),
       	",",
       	tostringRR(rightRR(x)),
       	"]",
        if isEmpty(x) then " (an empty interval)" else ""
       	));  
tostringRRipointer = tostringRRi;  


export toExternalString(x:RR):string := (
     if isinf(x) then return if x < 0 then "-infinity" else "infinity";
     if isnan(x) then return if sign(x) then "-NotANumber" else "NotANumber";
     ng := sign(x);
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
	       tostring(precision(x)),
	       if ex != long(0) then "e" else "",
	       if ex != long(0) then tostring(ex) else ""
	       )));

export toExternalString(x:RRi):string := (
     concatenate(array(string)("[", toExternalString(leftRR(x)),",", toExternalString(rightRR(x)),"]")));

export format(
     s:int,			  -- number of significant digits (0 means all)
     ac:int,	    -- accuracy, how far to right of point to go (-1 means all)
     l:int,					   -- max number leading zeroes
     t:int,				    -- max number extra trailing digits
     sep:string,		     -- separator between mantissa and exponent
     abb:bool,				  -- whether to abbreviate "*ii" to "i"
     paren:bool,    -- whether to parenthesize a sum and prepend a possible '-'
     z:CC						-- the number to format
     ) : string := (
     if isnan(z.re) || isnan(z.im) then return "NotANumber";
     if isinf(z.re) || isinf(z.im) then return "infinity";
     if s != 0 && ac == -1 && !isZero(z.re) && !isZero(z.im) then ac = s - int(floor(double(exponent(z))/log2ten));
     star := if abb then ""  else "*" ;
     i  := if abb then "i" else "ii";
     x := format(s,ac,l,t,sep,z.re);
     y := format(s,ac,l,t,sep,z.im);
     if y.1 === "0" then return if x.1 === "0" then "0" else concatenate(x);
     if y.1 === "1" then (y.1 = ""; star = "");
     if x.1 === "0" then return concatenate(array(string)(y.0,y.1,star,i));
     if y.0 === "" then y.0 = "+";
     lp := "";
     rp := "";
     if paren then (
	  if x.0 === "-" 
	  then (
	       x.0 = "";
	       lp = "-("; 
	       if y.0 .0 == '-' then y.0 = "+" else y.0 = "-")
	  else lp = "(";
	  rp = ")"; 
	  );
     concatenate(array(string)(lp,x.0,x.1,y.0,y.1,star,i,rp)));

export tostringCC(z:CC):string := (
     format(printingPrecision,printingAccuracy,printingLeadLimit,printingTrailLimit,printingSeparator,false,false,z)
     );
export tonetCC(z:CC):string := (
     format(printingPrecision,printingAccuracy,printingLeadLimit,printingTrailLimit,printingSeparator,true,false,z)
     );
tonetCCpointer = tonetCC;
export tonetCCparen(z:CC):string := (
     format(printingPrecision,printingAccuracy,printingLeadLimit,printingTrailLimit,printingSeparator,true,true,z)
     );
tonetCCparenpointer = tonetCCparen;

export toExternalString(z:CC):string := concatenate(array(string)(
     	  "toCC(",
	  toExternalString(realPart(z)),
	  ",",
	  toExternalString(imaginaryPart(z)),
	  ")"
	  ));


export (o:file) << (s:charstarOrNull) : file := o << if s == null() then "(null)" else tostring(s);
export (o:file) << (x:ZZ) : file := o << tostring(x);
export (o:file) << (x:QQ) : file := o << numeratorRef(x) << '/' << denominatorRef(x);

