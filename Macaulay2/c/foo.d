header "int main () { return 0; }";

A := {+ x:int };
B := {+ y:int };
C := A or B;
f(x:C):int := when x is A do 1 is B do 2 else 3;	    -- oops!  This needs to be fixed.

threadLocal t := A(123);

-- poly0 := tarray(int);
-- polyN := tarray(poly);
-- poly := polyN or poly0;
-- x := poly(polyN(polyN(poly0(2,3,4),poly0(2,3)),polyN(poly0(2,3,4,5),poly0(2,3,7))));

-- fun (P : A -> B -> C) (Q : A -> B) (R : A) => P R (Q R)
--      : (A -> B -> C) -> (A -> B) -> A -> C

-- A := Type "struct A";
-- B := Type "struct B";
-- C := Type "struct C";
-- D := function(function(A):B):function(A):C;



-- bug3():void := void();
-- bug2():int := 3;
-- bug1(x:int):int := (
--      if x == 2222
--      then bug2()
--      else return 1;
--      if x == 1111
--      then bug2()
--      else Ccode(exits,"abort()");
--      if x == 3333
--      then return 1
--      else bug2();
--      if x == 4444
--      then Ccode(exits,"abort()")
--      else bug2();
--      3);

-- INT := atomicType "int";
-- -- zero:int;

-- num ::= 4;
-- AT := array(int,num);
-- AT0 := AT(1,2,3,4);
-- AT0len := length(AT0);
-- tu := 0;

-- (x:int) + (y:int) ::= (
--      Ccode(void, "tu++");
--      Ccode(int,"((",x,")+(",y,"))"));

-- export mv ::= 3333;						    -- macro variable
-- mov := mv + 4444;

-- re(a:array(char)):void := void();
-- re(x:int):int := 44;
-- re():void := (
--      re("re");
--      re;
--      re();
--      re(4+5);
--      re();
--      re();
--      re();
--      );

-- (expo(x:char):int)('z');
-- expo('a');
-- export expo(x:int):int := 5;
-- expo('b');
-- expo(4);
-- export expo(x:char):int := 4;
-- expo('c');

-- taw := f():bool := true;
-- web := a := 0;
-- export threadLocal b := if f() then 4 else 5;
-- c := 2;
-- (threadLocal d := 3) = 33;

-- threadLocal tree := (expo(444););

-- threadLocal E := int;
-- threadLocal EE := {x:int}(111);
-- threadLocal EEE := 111111;

-- ry():int := if false then t := 0 else 4;

-- ggg():int := 2;
-- ab := if 4+5 == 3+1 then 5+4+4 else 4+3+1+1;

-- leftOperator 7 "??";
-- rightOperator 7 "%%";
-- prefixOperator 7 "++";

-- (x:int) ?? (y:int) ::= 14;
-- ff := 4 ?? 5;

-- (x:int) %% (y:int) ::= 15;
-- gg := 4 %% 5;

-- ii := 1 ?? 2 %% 3;

-- ++ (y:int) ::= 16;
-- hh := ++ 45;


-- l := 0;
-- iar := array(int)(1,2,3,4);

-- declarations "struct AMEM {int x;};";
-- declarations "struct MEM {int *x;};";
-- APtr := atomicPointer "struct AMEM *";
-- Ptr := Pointer "struct MEM *";
-- amem := malloc(APtr);				 -- should use GC_MALLOC_ATOMIC
-- mem := malloc(Ptr);				 -- should use GC_MALLOC

-- threadLocal third := 45;

-- dd(x:int) ::= 14;
-- dd(x:char) ::= 1000;
-- dd1 := dd(1);
-- dd2 := dd('a');

-- K1 := {+a:int};
-- K2 := {+b:int};
-- K3 := {+c:int};
-- KK := K1 or K2 or K3;
-- f1(t:KK):int := when t is t:K1 do t.a else 14;
-- f2(t:KK):int := when t is t:K1 do t.a is t:K2 do t.b else 14;
-- f3(t:KK):int := when t is t:K1 do t.a is t:K2 do t.b is t:K3 do t.c;

-- LL := K1 or K2 or K3 or null;
-- g1(t:LL):int := when t is t:K1 do t.a else 14;
-- g2(t:LL):int := when t is t:K1 do t.a is t:K2 do t.b else 14;
-- gg1(t:LL):int := when t is null do 12 is t:K1 do t.a else 14;
-- gg2(t:LL):int := when t is null do 12 is t:K1 do t.a is t:K2 do t.b else 14;
-- gg3(t:LL):int := when t is null do 12 is t:K1 do t.a is t:K2 do t.b is t:K3 do t.c;

-- MM := K1 or null;
-- h1(t:MM):int := when t is t:K1 do t.a else 14;
-- h2(t:MM):int := when t is t:K1 do t.a is null do 1000;
-- h3(t:MM):int := when t is null do 1000 is t:K1 do t.a;
-- h4(t:MM):int := when t is null do 1000 else 14;

-- fff():int := 1;

-- iii :=  1;

-- g():void := u := 1;

-- protected := 1;
-- xp := protected ;
-- foo1 := { protected:bool };

-- declarations "typedef int int32;";
-- declarations "extern int32 fooflag;";  
-- header "int32 fooflag;";  
-- export fooflag() ::= Ccode(int,"fooflag");
-- import fooflag2() ::= Ccode(int,"fooflag");
-- setfooflag(n:int) ::= Ccode(void,"fooflag=",n,"");
-- y := fooflag();
-- setfooflag(3);



-- xx := "hi";


-- F := {y:int};
-- f(x:F or null):int := (
--      when x is x:F do 1 else 2
--      );

-- header "struct foo {int x,y,*p;};";
-- header "struct bar {int x,y,z;};";
-- R := Pointer "struct foo *";
-- t := malloc(R);
-- K := atomicPointer "struct foo *";
-- u := malloc(K);

-- Seq := seq or tarray(char);
-- export seq := tarray(int);

-- defun := 4;


-- g(i:int) ::= i;

-- dr() ::= 1234;
-- rr := dr();

-- A := {x:null or A};
-- a2 := (null or A)(null());
-- A(a2);
-- f(x:null or A):int := 1;
-- f(a2);
-- x := 1;
-- zzz := {+ j:int };
-- uuu := zzz or tarray(char);
-- WW := tarray(char);
-- bbb := WW(char(1));
-- ddd := zzz(1);
-- ccc := uuu(bbb);
-- fff(k:uuu):int := (
--      when k
--      is y:WW do int(y.0)
--      is z:zzz do z.j
--      );
-- fff(uuu({+ j:int }(333)));

-- T := tarray(int,3);
-- S := tarray(int);
-- c3 := T(2,3,4);
-- d4 := S(2);
-- e : S or null := d4;

-- S1 := tarray(int);
-- S2 := tarray(int);				   -- this gets identified to S
-- T1 := tarray(U);                                   -- this does not get identified to S, because U is known too late
-- T2 := tarray(U);                                   -- this gets identified to T1 but not to S, because U is known too late
-- U := int;
-- T3 := tarray(V);				   -- this does not get identified to T1, because V is not known yet
-- export V := int;
-- export T4 := tarray(V);				   -- this gets identified to S

-- header "#include <stdio.h>";

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/c run-foo foo.sig "
-- End:
