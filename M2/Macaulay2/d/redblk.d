--		Copyright 1994 by Daniel R. Grayson
-- TEST an implementation of red-black trees
use stdio;
use strings;

-- pretty printer from page 312 in Paulson's book on ML

cons := {item:expr, next:list};
list := null or cons;
Break := { lng:int };
Block := {contents:list, indent:int, lng:int};
expr := Break or Block or string;

lng(x:expr):int := (
     when x
     is x:Block do x.lng
     is x:string do length(x)
     is x:Break do x.lng
     );

breakdist(e:list, after:int):int := (
     when e 
     is null do after
     is e:cons do (
       	  when e.item
       	  is b:Block do b.lng + breakdist(e.next,after)
       	  is b:Break do 0
       	  is s:string do length(s) + breakdist(e.next,after)));

sum(e:list):int := (
     k := 0;
     while true do (
    	  when e
    	  is null do return(k)
    	  is f:cons do (k=k+lng(f.item); e=f.next)));

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
       printing(l.next,blockspace,after))
  else nothing);
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

-- convert(x:lis):expr;
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
-- convert(x:lis):expr :=
--   when x {
--    null do blo(3,list(cons(str("()"),null()))),
--    x:con do convert(x)};
export dashes(i:int):void := while i>0 do ( stdout << '-'; i=i-1 );

-- now the red black trees

red := 1;
blk := 2;
node := { color:int, key:int, left:ptr, right:ptr, parent:ptr};
tree := {root:ptr, destroy : function(tree):void  };
ptr := null or node;

recycle(x:ptr):void := 
  when x
  is y:node do (
       y.parent = null();
       recycle (y.left);
       recycle (y.right);)
  else nothing;
recycle(t:tree):void := ( recycle(t.root); t.root = null(); );

newnode(key:int):node := node(red,key,null(),null(),null());
newtree():tree := tree(null(),recycle);

(o:file) << (x:ptr):file :=
  when x
  is null do o
  is x:node do o << "(" << x.left << " " << x.key << " " << x.right << ")";

export (o:file) << (t:tree):void := o << "[" << t.root << "]";

export leftrotate(t:tree,x:node):void :=
    when x.right
    is y:node do (
	 when y.left
	 is null do x.right = null()
	 is beta:node do (x.right=beta; beta.parent=x);
	 when x.parent
	 is null do (y.parent = null(); t.root = y;)
	 is p:node do (
	      y.parent = p;
	      if x==p.left then p.left=y else p.right=y;);
	 y.left = x;
	 x.parent = y;
	 )
    else nothing;

export rightrotate(t:tree,x:node):void :=
    when x.left
    is y:node do (
	 when y.right
	 is null do x.left = null()
	 is beta:node do (x.left=beta; beta.parent=x);
	 when x.parent
	 is null do (y.parent = null(); t.root = y;)
	 is p:node do (
	      y.parent = p;
	      if x==p.right then p.right=y else p.left=y;);
	 y.right = x;
	 x.parent = y;
	 )
    else nothing
    ;

treeinsert(t:tree, z:node):void := (
    y:ptr := null();
    x:ptr := t.root;
    while true do (
	 when x 
	 is null do break
	 is xx:node do (
	      y=xx;
	      if z.key < xx.key then x=xx.left else x=xx.right;));
    z.parent = y;
    when y
    is null do t.root=z
    is y:node do if z.key < y.key then y.left = z else y.right = z;
    );

export rbinsert(t:tree, x:node):void := (	-- page 268
    x.color = red;
    while true do (
	 when x.parent
	 is null do return()		-- so x is the root in this case
	 is p:node do if red==p.color then return()
	 ));

insertkey(t:tree,key:int):void := treeinsert(t,newnode(key));

--  conversion of a tree to an expression for the pretty printer

convert(x:ptr):expr :=
    when x
    is null do blo(0,list(null()))
    is x:node do blo(4,list(cons(
			str("("),
			cons(convert(x.left),
			cons(brk(1),
     	       	    	cons(tostring(x.key),
			cons(brk(1),
			cons(convert(x.right),
			cons(str(")"), 
			null())))))))));

convert(t:tree):expr := convert(t.root);
pp(t:tree):void := pr(convert(t),72);

	t:=newtree();
	   insertkey(t,34);
	   insertkey(t,11);
	   insertkey(t,67);
	   insertkey(t,14);
	   insertkey(t,21);
	   insertkey(t,55);
	   insertkey(t,33);
	   insertkey(t,34);
	   insertkey(t,53);
	   insertkey(t,110);
	   insertkey(t,670);
	   insertkey(t,140);
	   insertkey(t,210);
	   insertkey(t,551);
	   insertkey(t,331);
	   insertkey(t,341);
	   insertkey(t,531);
	   insertkey(t,1101);
	   insertkey(t,6701);
	   insertkey(t,1401);
	   insertkey(t,2101);
	   insertkey(t,550);
	   insertkey(t,330);
	   insertkey(t,340);
	   insertkey(t,530);
	   insertkey(t,44);
	   insertkey(t,12);
	   insertkey(t,77);
	   insertkey(t,32);
	pp(t);
	t.destroy(t);

