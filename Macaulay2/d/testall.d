--		Copyright 1994 by Daniel R. Grayson
use varstrin;
use system;
use strings;
use stdio;

  -- TEST here we check that reference counts are handled correctly by "if"
dog := {a:int, next: (null or dog)};
  checkif():void := (
	x := dog( 44444, NULL);
	f():dog := dog(4444,NULL);
	r1 := dog( 11111, NULL);
	r2 := dog (22222, r1);
	r3 := dog (33333, r2);
	if 44444 == x.a then  (stdout << '1' << endl);
	if 33333 == f().a then (stdout << '1' << endl);
	if 44444 == x.a then (stdout << '1' << endl;) else (stdout << '2' << endl;);
	if 33333 == f().a then (stdout << '1' << endl;) else (stdout << '2' << endl;););
  checkif();

--   TEST the "return" statement, to see that no references get dropped

fort(i:bool, x:dog):dog := (
	z := dog(33333,NULL);
	(
		y:=dog(22222,NULL);
		if i then return(y);
		stdout << x.a << endl
	);
	z);

fort(false, dog(44444,NULL));
fort(true,  dog(55555,NULL));

-- TEST here we test destructive assignment

id(x:dog):dog := x;

chkset():void := (
	i:=4;
	x:=dog(444,NULL);
	i=8;
	x=dog(555,x);
	x.next=dog(777,NULL);
	id(x).next=dog(777,NULL);
	x.next=x;        --it's safe to make a circular reference only
	x.next=NULL;	-- if you remember to clear it later
	);
chkset();

-- TEST the return statement in unusual locations
testret():void := (
	a := {a:int}((return();4));
	b := array(int)((return();4));
	);

testret();

-- TEST passing functions as arguments
helper(x:int):int := x+30000;
user(h:function(int):int):int := h(77);
holder := {helperfun : function(int):int};
holdit := holder(helper);
getholdit():holder := holder(helper);
stdout << user(helper) << endl; -- will print 30077
stdout << user(holdit.helperfun) << endl; -- will print 30077
stdout << holdit.helperfun(77) << endl; -- will print 30077
stdout << getholdit().helperfun(77) << endl; -- will print 30077

-- TEST lists of chars used as strings

ch := {a:char, next:(null or ch)};
(a:char) >> (n:null) : ch := ch(a,n);
(a:char) >> (n:ch) : ch := ch(a,n);
(stdout:file) << (x:ch) : file := (
	stdout << x.a;
	when x.next
	is z:ch do (stdout << z;)
	else nothing;
	stdout
	);

stdout <<  'h' >> 'o' >> ' ' >> 't' >> 'h' >> 'e' >> 'r' >> 'e' >> '\n' >> NULL;

stdout << "hi there" << endl;

-- TEST typecase used to compute length of a list

number := {a:int};
cons := {item:thing, next:list};
thing := (null or cons or number);
list := (null or cons);

(x:null) >> (y:null):list := cons(x,y);
(x:null) >> (y:cons):list := cons(x,y);
(x:null) >> (y:list):list := cons(x,y);
(x:cons) >> (y:null):list := cons(x,y);
(x:cons) >> (y:cons):list := cons(x,y);
(x:cons) >> (y:list):list := cons(x,y);
(x:list) >> (y:null):list := when x is xx:null do cons(xx,y) is xx:cons do cons(xx,y);
(x:list) >> (y:cons):list := when x is xx:null do cons(xx,y) is xx:cons do cons(xx,y);
(x:list) >> (y:list):list := when x is xx:null do cons(xx,y) is xx:cons do cons(xx,y);
(x:number) >> (y:null):list := cons(x,y);
(x:number) >> (y:cons):list := cons(x,y);
(x:number) >> (y:list):list := cons(x,y);


bigone := (
	x := cons(number(567),NULL);
	x = cons(x,x); x = cons(x,x); x = cons(x,x);
	x = cons(x,x); x = cons(x,x); x = cons(x,x);
	x = cons(x,x); x = cons(x,x); x = cons(x,x);
	x = cons(x,x);
	x);

lng(x:list):int := (
     when x 
     is null do 0
     is y:cons do 1 + lng(y.next)
     );

lng(x:cons):int := 1 + lng(x.next);

stdout << (lng(bigone)) << endl;

-- TEST put on ints

stdout << 0 << " " << -12345 << " " << 67890 << endl;

-- ;TEST put on doubles

stdout << 12.3456 << endl;

-- TEST forward definitions

tram(x:int):int;
tram(x:bool):int := if x then tram(5) else tram(6);
tram(x:int):int := x;
stdout << tram(3) << endl;
stdout << tram(true) << endl;
stdout << tram(false) << endl;

-- TEST a recursive routine for Fibonacci numbers

fib(n:int):int := if n<=1 then 1 else fib(n-1)+fib(n-2);
stdout << (fib(10)) << endl;

-- this next section doesn't work now
-- 
-- trump := {a:int, next:trump };
-- frump := {a:int, next:grump };
-- grump := {a:int, next:frump };
-- 
-- --  the translator detects that these three types are actually the same
-- export next0(x:trump):trump := x.next;
-- export next1(x:frump):frump := x.next;
-- export next2(x:grump):grump := x.next;
-- 
-- export foo0(x:trump):void := next0(x);
-- export foo1(x:trump):void := next1(x);
-- export foo2(x:trump):void := next2(x);
-- export foo3(x:frump):void := next0(x);
-- export foo4(x:frump):void := next1(x);
-- export foo5(x:frump):void := next2(x);
-- export foo6(x:grump):void := next0(x);
-- export foo7(x:grump):void := next1(x);
-- export foo8(x:grump):void := next2(x);

-- TEST x=x.next
fooo := {next:fooo};
bar():void := nothing;
export tttt(x:fooo):void := (
	bar();
	x = x.next;
	bar();
	);

-- TEST typecase

export test0():void := nothing;

gooo := {next:(null or gooo)};

export uuuu(x:(null or gooo)):void := (
	bar();
	when x
	is y:gooo do x=y.next
	else nothing;
	bar();
	);
-- ;;; (memstats)

(stdout:file) << (x:thing):file;
(stdout:file) << (x:list):file;
(stdout:file) << (x:null):file := stdout << '(' << ')';
(stdout:file) << (x:cons):file := ( 
	stdout << '('; 
	stdout << x.item;
	z := x.next;
	while true do (
	     when z
	     is null do ( 
		  stdout << ')'; 
		  return(stdout); 
		  )
	     is y:cons do (
		  stdout << ' ' << y.item;
		  z = y.next;
		  )
	     )
	);
(stdout:file) << (x:thing):file := (
     when x 
     is y:null do stdout << y
     is y:cons do stdout << y
     is y:number do stdout << y.a
     );
(stdout:file) << (x:list):file := (
     when x 
     is y:null do stdout << y
     is y:cons do stdout << y
     );

testlist := (
	a := number(111);
	b := number(222);
	c := number(333);
	a >> b >> a >> (b >> NULL) >> c >> NULL
	);

arr := array(int);
arr2:= array(array(int));
(stdout:file) << (x:array(string)):file := (
	first := true;
	stdout << '[';
	foreach i in x do (
		if first then first = false else (stdout << ' ';);
		stdout << i);
	stdout << ']');
(stdout:file) << (x:arr):file := (
	first := true;
	stdout << '[';
	foreach i in x do (
		if first then first = false else (stdout << ' ';);
		stdout << i);
	stdout << ']');
(stdout:file) << (x:array(list)):file := (
	first := true;
	stdout << '[';
	foreach i in x do (
		if first then first = false else (stdout << ' ';);
		stdout << i);
	stdout << ']');
(stdout:file) << (x:arr2):file := (
	first := true;
	stdout << '[';
	foreach i in x do (
		if first then first = false else (stdout << ' ';);
		stdout << i);
	stdout << ']');
(stdout:file) << (x:(null or string)):file := (
     when x
     is null do stdout << ("NULL")
     is y:string do stdout << y
     );
(stdout:file) << (x:array((null or string))):file := (
	first := true;
	stdout << '[';
	foreach i in x do (
		if first then first = false else (stdout << ' ';);
		stdout << i);
	stdout << ']');
--  here is where we need some polymorphism!

stdout << array(list)() << endl;

larray := array(list)(NULL,testlist,testlist,cons(number(55),NULL));
stdout << larray << endl;

larray. 2 = cons(number(88888888),larray. 2);
stdout << larray << endl;

stdout << (arr2(arr(3,4,5),arr(3,4,5,6,7,8),arr(3,4,5,6,7,8,9,10))) << endl;

test1():list := testlist;
test1();

test2():void := if true then test1() else test1();
test2();

test3():void := ( t := if true then test1() else test1());
test3();

test4():void := (
	t:list := testlist;
	t = if true then test1() else test1()
	);
test4();

test5():list := (
	return (if true then test1() else test1());
	NULL);
test5();

test6():list := return(testlist);
test6();

test8(x:list):void := testlist;
test8(list(testlist));

test9(x:list):void := (
	test6();
	x = testlist);
test9(list(testlist));

export compare():void := (
	a:thing := NULL;
	b:thing := NULL;
	c:cons := cons(a,NULL);
	d:cons := cons(a,NULL);
	i:int := 3;
	j := a==b;
	k := b != c;
	l := c == d;
	m := i == 3;);

size(x:cons):int ;
size(x:thing):int := (
     when x
     is null do 1
     is number do 1
     is y:cons do size(y)
     );

size(x:list):int :=(
      when x
      is null do 1
      is y:cons do size(y)
      );

size(x:cons):int := size(x.item) + size(x.next);
stdout << (size(bigone)) << endl;
stdout << testlist << endl;

reverse(x:cons):list := (
     y:list := NULL;
     while true do (
	  y = cons(x.item,y);
	  when x.next
	  is null do return(y)
	  is z:cons do x=z
	  )
     );
reverse(x:list):list := (
     when x
     is null do list(NULL)
     is y:cons do list(reverse(y))
     );
stdout << (reverse(testlist)) << endl;

export wwww(x:list):void := x = reverse(x);

-- file io

ABS(x:double):double := if x<0. then -x else x;
ABS(x:int):int := if x<0 then -x else x;
f(x:double):void := nothing;
g(x:int):void := nothing;
f(ABS(-3.5));
g(ABS(-3));

stdout << (join("abc","def")) << endl;
stdout << (substr("abcdef",1,4)) << endl;
stdout << (new string len 78 do ( provide 'a'; provide 'b' )) << endl;
stdout << (new string len 26 do (
    c:='a'; 
    while true do ( provide c; c = c+1 )
    )) << endl;
stdout << (new array(string) len 4 do (
	xx := "aaaa";
	yy := "bbbb";
	provide join(xx,yy))) << endl;

stdout << ( 
	xx := join("aaa","bbb");
	new array(string) len 5 do (
		provide xx;
		provide join("aaaa","bbbb"))) << endl;

stdout << (new array((null or string)) len 4 do
	(provide NULL; provide("aaaa"))) << endl;

stdout << (new array(int) len 21 do (
	j:=1;
	k:=1;
	provide(j);
	provide(k);
	while true do (
		tmp := k;
		k = j+k;
		j = tmp;
		provide(k)))) << endl;


-- objects with no nonvoid members

(
	r := {};
	s := {special:void};
	x := r();
	y := s();
);

-- TEST selection from arrays with return value / error packets

a := arr(3,6,7,11,45,677);
packet := {val:int};
ret := (null or packet);
(stdout:file) << (b:ret):file := (
     when b
     is null do stdout << "none"
     is x:packet do stdout << x.val
     );
crit1(x:int):bool := x>8;
crit2(x:int):bool := x>22;
crit3(x:int):bool := x>1000;
select(b:arr,crit:function(int):bool):ret := (
	foreach i in b do if crit(i) then return(packet(i));
	NULL);
nonnull(x:ret):bool := (
     when x
     is null do false
     is packet do true 
     );
exists(b:arr, crit:function(int):bool):bool := nonnull(select(b,crit));
exists2(b:arr, crit:function(int):bool):bool := (
	return (nonnull(select(b,crit)));
	false
	);

stdout << (select(a,crit1)) << endl;
stdout << (select(a,crit2)) << endl;
stdout << (select(a,crit3)) << endl;
stdout << (exists(a,crit1)) << endl;
stdout << (exists(a,crit2)) << endl;
stdout << (exists(a,crit3)) << endl;
stdout << (exists2(a,crit1)) << endl;
stdout << (exists2(a,crit2)) << endl;
stdout << (exists2(a,crit3)) << endl;

-- TEST the "break" statement, to see that no references get dropped
fort2(i:bool,x:dog):dog := (
	j := true;
	w := dog(33333,NULL);
	while j do (
		j = false;
		z := dog(33333,NULL);
		(
			yyyy := dog(22222,NULL);
			if i then break;
			stdout << x.a << endl;
		);
	);
	w
	);
fort2( false, dog( 44444, NULL));
fort2( true,  dog( 55555, NULL));

-- TEST tostring converts int to string using lists of chars

stdout << (tostring(-55555)); stdout << ' ';
stdout << (tostring(77777)); stdout << ' ';
stdout << (tostring(0)) << endl;

-- TEST tostring, from int to char, with arrays, and reverse for arrays
numdigits(x:int):int := (
	n:=0;
	while x>0 do (n = n+1; x=x/10);
	n);
digit(x:int):char := if x<10 then '0' + x else 'a' + (x - 10);
tostring2(x:int):string := (
	if x==0 then return ("0");
	neg := x<0;
	xx := if neg then -x else x;
	s := new string len numdigits(x) do (provide(digit(xx%10));xx=xx/10);
	reverse(s));

-- TEST double dereferencing of a hash table

foooo := {x:int};
barr := {p:foooo};
t := barr(foooo(22));
stdout << t.p.x << endl;

-- TEST foreach with step size -1
-- 
s := "hi there";
foreach c in s do stdout << c;
stdout << " ---reverse---> ";
foreach c in s by -1 do stdout << c;
stdout << endl;

-- TEST the loop identifier for 'foreach' is an lvalue
v := "this shouldn't print out";
foreach c in v do c = 'b';
stdout << v << endl;
(use system; stdout << "args = "; stdout << argv << endl);
stdout << (new string len 20 at i do provide(i+'a')) << endl;
stdout << (new array(string) len 20 at i do 
    provide(join("*",tostring2(i))))
    << endl;
stdout << (new array(int) len 20 at i do provide(i)) << endl;
n := 11;
f():string := "hi ";
for n do ( gg:="hoho"; stdout << f()); stdout << endl;
for i to n do ( stdout << i << " " ); stdout << endl;
for i from -3 to n do ( stdout << i << " " ); stdout << endl;
for i from n to 1 by -1 do ( stdout << i << " " ); stdout << endl;
for i from n to 0 by -1 do ( stdout << i << " " ); stdout << endl;
for i from n to -1 by -1 do ( stdout << i << " " ); stdout << endl;
for i from n to -2 by -1 do ( stdout << i << " " ); stdout << endl;
for i from n to -3 by -1 do ( stdout << i << " " ); stdout << endl;
stdout << 0xff << endl;
export infinite():int := while true do nothing;

-- TEST self

number2 := {a:int};
cons2 := {item:thing2,next:cons2};
thing2 := (cons2 or number2);
(item:number2) >> (next:cons2):cons2 := cons2(item,next);
op<<(stdout:file,x:cons2):file;
op<<(stdout:file,x:thing2):file := (
     when x
     is y:cons2 do stdout << y
     is z:number2 do stdout << z.a
     );
op<<(stdout:file,x:cons2):file := (
	stdout << '[';
	if x != x.next 
	then while true do (
		stdout << x.item;
		x = x.next;
		if x == x.next then break;
		stdout << ',';
		);
	stdout << ']');
nil2 := cons2(number2(0),self);
use system; 
accountfor(sizeof(nil2));
accountfor(sizeof(nil2.item));

stdout <<  number2 (11111) >>  number2 (22222) >>  number2 (33333) >>
 	   number2 (44444) >> nil2  << endl;

stdout << 3 + 5 + 6 + 8 + 9 + 10 << endl;

cons3 := {a:int,next:cons3};
(a:int) >> (next:cons3):cons3 := cons3(a,next);
nil3 := cons3( 0, self);  
accountfor (sizeof (nil3));
op<<(stdout:file,x:cons3):file := (
	stdout << '(';
	if x != nil3 
	then while true do (
		stdout << x.a;
		x = x.next;
		if x == nil3 then break;
		stdout << ' ';
		);
	stdout << ')');
stdout <<  44 >> 55 >> 66 >> 77 >> 88 >> 99 >> 100 >> nil3  << endl;

-- TEST model for closures
closure := {fn:function(closure,int):int, a:int, b:int};
apply(f:closure,x:int):int := f.fn(f,x);
g(env:closure,x:int):int := x + env.a + env.b ;
h := closure(g,200,30);
k := closure(g,400,50);
stdout << apply(h,3) << endl;
stdout << apply(k,3) << endl;

-- ; TEST escape characters in strings
stdout << "hi there\n";

-- stdout << (reverse("abcdefghij"));newline();

-- stdout << ( reverse ( string('a','b','c','d','e')));newline();

(
bbb := {x:double,y:double}(888.,999.);
);

use strings;
use stdio;

(
bbb := {x:double,y:double}(888.,999.);
);

package math (
	export complex := {re:double, im:double};
	www := complex(3.,4.);
	export foo():complex := www;
	export op*(z:complex,w:complex):complex := complex(
		z.re * w.re - z.im * w.im,
		z.re * w.im + z.im * w.re);
	export op<<(o:file,z:complex):file := (
		o << z.re << " + " << z.im << " * i";
		o);
	);

(
use math;
bbb := complex(888.,999.);
use stdio;
stdout << "bbb = " << bbb << endl;
);

use stdio;

package foo (
	export f(x:int):void := stdout << "foo " << x << endl;
);

(
use math;
bbb := complex(888.,999.);
stdout << "bbb = " << bbb << endl;
);

package bar (
	export f(x:int):void := stdout << "bar " << x << endl;
);

(
use math;
bbb := complex(888.,999.);
stdout << "bbb = " << bbb << endl;
);

package main (
	(
		use foo; 
		f(3)
	);
	(	
		use math;
		use stdio;
		aa := complex(4.,5.);
		stdout << aa*complex(6.,7.) << endl;
		stdout << aa*complex(7.,6.) << endl;
	);
		use bar; 
		f(3);

);

use math;
cccc := complex(555.,666.);
stdout << "cccc = " << cccc << endl;


qwert := {next:qwert};
qnil := qwert(self);
accountfor(sizeof(qnil));
x := qwert(qwert(qnil));
x = x.next;

qf():qwert := x.next;
x = qwert(qwert(qnil));
x = qf();

qg():qwert := x;
x = qwert(qwert(qnil));
x = qg().next;

u := (null or {});
tar():u := return (u(null()));

common1 := {a:int, c:int}; common2 := {a:int, b:int}; 
common := (common1 or common2);
cx := common(common1(3,4)); cy := common(common2(5,6));
stdout << cx.a << " --- " << cy.a << endl;


-- this used to fail because kkk was identified with aaa prematurely
aaa := null or ppp;
kkk := null or qqq;
ppp := { pppp:int };
qqq := { qqqq:int };
get(o:kkk):void := when o is null do 1 is qqq do 2;

--  some problems with null pointers:

well := {x:int};
form := null or well;
grab():form := (
     while true
     do return(form(null()));	-- this caused a problem once
     );
grab();

-- test breaks in foreach loops
count := 0;
foreach c in "abcde" do (
     count = count + 1;
     if c == 'c' then break;
     );
foreach s in array(array(char))("a","b","c","d","e") do (
     count = count + 1;
     if s.0 == 'c' then break;
     );
stdout << count << endl;


