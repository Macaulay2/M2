use stdio;
use strings;

export gmpStruct := { alloc:int, size:int, limbs:null };

getstr(str:Cstring, base:int, x:gmpStruct):Cstring ::= Ccode(Cstring,
     "mpz_get_str(",
     "(char *)", str, ",",
     base, ",",
     "(__mpz_struct *)", x,
     ")"
     );
init(x:gmpStruct):void ::= Ccode( void,
     "mpz_init(",
     "(__mpz_struct *)", x, 
     ")"
     );
sizeinbase(x:gmpStruct,b:int):int ::= Ccode( int,
     "mpz_sizeinbase(",
     "(__mpz_struct *)", x, ",",
     b,
     ")"
     );
set(x:gmpStruct, n:int):void ::= Ccode( void,
     "mpz_set_si(",
     "(__mpz_struct *)", x, ",",
     n, ")" 
     );

export toInteger(i:int):gmpStruct := (
     x := gmpStruct(0,0,null());
     init(x);
     set(x,i);
     x);

mul(x:gmpStruct, y:gmpStruct, z:gmpStruct):void ::= Ccode( void,
     "mpz_mul(",
     "(__mpz_struct *)", x, ",", 
     "(__mpz_struct *)", y, ",", 
     "(__mpz_struct *)", z,
     ")" 
     );

export (x:gmpStruct) * (y:gmpStruct) : gmpStruct := (
     z := gmpStruct(0,0,null());
     init(z);
     mul(z,x,y);
     z);

pow(x:gmpStruct, y:gmpStruct, n:int):void ::= Ccode( void,
     "mpz_pow_ui(",
     "(__mpz_struct *)", x, ",", 
     "(__mpz_struct *)", y, ",", 
     n, ")" 
     );

export (x:gmpStruct) ^ (n:int) : gmpStruct := (
     y := gmpStruct(0,0,null());
     init(y);
     pow(y,x,n);
     y);

toCstring(x:gmpStruct):Cstring ::= getstr(null(), 10, x);

export (o:file) << (x:gmpStruct) : file := o << toCstring(x);
