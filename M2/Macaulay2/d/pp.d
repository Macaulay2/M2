--		Copyright 1994 by Daniel R. Grayson
use strings;
use stdio;

-- pretty printer from page 312 in Paulson's book on ML

cons := {item:expr, next:list};
list := null or cons;
Break := { lng:int };
Block := {contents:list, indent:int, lng:int};
expr := Break or Block or string;

lng(x:expr):int :=
  when x 
  is x:Block do x.lng
  is x:string do length(x)
  is x:Break do x.lng;

breakdist(e:list, after:int):int :=
  when e 
  is null do after
  is e:cons do (
       when e.item
       is b:Block do b.lng + breakdist(e.next,after)
       is b:Break do 0
       is s:string do length(s) + breakdist(e.next,after));

sum(e:list):int := (
  k := 0;
  while true do
    when e 
    is null do return(k)
    is f:cons do (k=k+lng(f.item); e=f.next));
  
str(s:string):expr := s;
brk(lng:int):expr := Break(lng);
blo(indent:int, e:list):expr := Block(e,indent,sum(e));
space := 0;
width := 0;
newline():void := (stdout << endl; space=width);
printblanks(n:int):void := (
	space = space-n;
	while n>0 do ( stdout << ' '; n = n-1));
printing(l:list, blockspace:int, after:int):void := (
     when l
     is l:cons do (
       	  when l.item 
       	  is b:Block do printing(b.contents,
	       space-b.indent,breakdist(l.next,after))
       	  is s:string do (stdout << s; space = space - length(s))
       	  is b:Break do (
	       if b.lng + breakdist(l.next,after) <= space
	       then printblanks(b.lng)
	       else (newline(); printblanks(width-blockspace))
	       );
	  printing(l.next,blockspace,after)
	  )
     else nothing
     );
pr(e:expr,width0:int):void := (
    space = width0;
    width = width0;
    printing(list(cons(e,null())),width,0);
    newline(););

con := {item:thing, next:lis};
thing := null or string or con;
lis := null or con;
  

-- convert(x:lis):expr;
-- convert(x:thing):expr;

convert(x:lis):expr;
convert(x:thing):expr;

converttail(x:lis):list :=
  when x
  is null do cons(str(")"),null())
  is x:con do cons(brk(1), cons(convert(x.item),converttail(x.next)));
convert(x:con):expr :=
  blo(3,list(cons(str("("),
		  cons(convert(x.item),converttail(x.next)))));
convert(x:thing):expr :=
  when x
  is null do blo(3,list(cons(str("()"),null())))
  is x:string do str(x)
  is x:con do convert(x);
convert(x:lis):expr :=
  when x
  is null do blo(3,list(cons(str("()"),null())))
  is x:con do convert(x);
dashes(i:int):void := while i>0 do (stdout << '-'; i=i-1 );
pp(x:lis,width:int):void := (
    dashes(width);
    stdout << endl;
    pr(convert(x),width););

arr:=array(string);
export atol(x:arr):lis := (
  i := length(x)-1;
  z := lis(null());
  while i >= 0 do ( z = con(x.i,z); i = i-1 );
  z);

	a := con("hi",con("there",con("paul",lis(null()))));
	c := con("This",con("is",con("a",con("rather",con("long",
		con("sentence",con("with",con("several",con("words.",
		lis(null()))))))))));
	b := lis(con(a,con(c,con(a,con(c,con(a,null()))))));

	pp(b,72);
	pp(b,60);
	pp(b,47);

