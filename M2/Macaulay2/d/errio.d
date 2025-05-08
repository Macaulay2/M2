--		Copyright 2010 by Daniel R. Grayson
-- io routines for stderr that do not depend on global variables having been initialized,
-- so they can be used early in the execution of a thread, e.g., by finalizers
-- we queue up the strings so the entire message can be written with one write() system call
use varstrin;
export BasicFile := varstring or null;	-- there can be only one of these, but that's enough for now
export threadLocal stderr := BasicFile(null()); -- this is safe for other threads, because it is initially 0
i():varstring := when stderr is v:varstring do v else (t := newvarstring(200); stderr = t; t);
export (stderr:BasicFile) << (x:char) : BasicFile := i()<<x;
export (stderr:BasicFile) << (x:string) : BasicFile := i()<<x;

export BasicManipulator := {fun:function(BasicFile):int};
export (bf:BasicFile) << (m:BasicManipulator) : int := m.fun(bf);
flushfun(stderr:BasicFile):int := write(2,takestring(i()));
export basicFlush := BasicManipulator(flushfun);
endlfun(stderr:BasicFile):int := stderr << newline << basicFlush;
export basicEndl := BasicManipulator(endlfun);

putdigit(o:BasicFile,x:int):void := o << (x + if x<10 then '0' else 'a'-10) ;
putneg(o:BasicFile,x:int):void := (
     if x<0 then (
	  q := x/10;
	  r := x%10;
	  if r>0 then (r=r-10;q=q+1);
     	  putneg(o,q);
     	  putdigit(o,-r)));
export (o:BasicFile) << (x:int) : BasicFile :=  (
   if x==0
   then putdigit(o,0)
   else (
	if x<0 
	then ( 
	     o << '-';
	     putneg(o,x);
	     )
	else putneg(o,-x);
	);
   o);
export (o:BasicFile) << (x:short) : BasicFile := o << int(x);
export (o:BasicFile) << (x:ushort) : BasicFile := o << int(x);
export (o:BasicFile) << (x:uchar) : BasicFile := o << int(x);
export (o:BasicFile) << (b:bool) : BasicFile := o << if b then "true" else "false";

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d errio.o "
-- End:

