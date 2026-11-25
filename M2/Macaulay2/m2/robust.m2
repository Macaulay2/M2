-- all this code should go!!

needs "lists.m2"
needs "max.m2"
needs "nets.m2"

timelimit := (t,f) -> (alarm t; r := f(); alarm 0; r)

printingTimeLimit = 20
errorPrintingTimeLimit := 3
symbol debugError <- identity				    -- use '<-' to bypass global assignment method
robustNet := y -> (
     fun := () -> net y;
     try timelimit(printingTimeLimit, fun) else (
	  alarm 0;
	  global debugError <- fun;
	  stderr << endl << "--error or time limit reached in conversion of output to net: type 'debugError()' to run it again; will try conversion to string" << endl ;
	  try timelimit(errorPrintingTimeLimit, () -> toString y) else (
	       alarm 0;
	       stderr << endl << "--error in conversion of output to string" << endl;
	       simpleToString y)))
truncNet := (wid,ht,s) -> (
     if wid > 0 and width s > wid then (
	  s = stack apply( unstack s, l -> if width l > wid then substring(l,0,wid-1) else l);
	  s = s | (stack ( height s + depth s : "." ))^(height s - 1));
     if ht > 0 and height s + depth s  > ht then (
	  s = stack take(unstack s,ht-1);
	  s = s || concatenate(width s : "."));
     s)
truncString := (wid,s) -> if wid > 0 and width s > wid then concatenate(substring(s,0,wid-1),"$") else s
checkNet    := n -> if instance(n, Net   ) then n else error "didn't format correctly"
checkString := n -> if instance(n, String) then n else error "didn't format correctly"
Nothing.Format = toString
String.Format = format
silentRobustNet = (wid,ht,sec,y) -> (
     truncNet(wid,ht,
	  try timelimit (sec, () -> checkNet if lookup(symbol Format,class y) =!= null then (lookup(symbol Format,class y)) y else net y)
	  else (
	       alarm 0;
	       simpleToString y)
	  ))
silentRobustNetWithClass = (wid,ht,sec,y) -> (			    -- we know wid is at least 80
     part2 := horizontalJoin(" (of class ", silentRobustNet(wid//2,           ht,sec,class y), ")");
     part1 :=                               silentRobustNet(wid - width part2,ht,sec,      y);
     horizontalJoin(part1, part2));
silentRobustString = (wid,sec,y) -> (
     truncString(wid,
	  try timelimit (sec, () -> checkString toExternalString y)
	  else (
	       alarm 0;
	       simpleToString y)))
silentRobustStringWithClass = (wid,sec,y) -> (
     part2 := concatenate(" (of class ", silentRobustString(wid//2,           sec,class y), ")");
     part1 :=                            silentRobustString(wid - width part2,sec,      y);
     concatenate(part1, part2));


-- mainly called by processErrorArgs0 in debugging.dd to print errors
Thing.RobustPrintStringMethod = obj -> try silentRobustString(40, 3, obj)

-- mainly called by KeyNotFound in hashtables.dd to print missing key errors
Thing.RobustPrintNetMethod = obj -> try silentRobustNetWithClass(60, 5, 3, obj)

Thing#{Standard,Print} = x -> (
     oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
     save := printWidth;
     if printWidth != 0 then printWidth = printWidth - #oprompt;
     z := robustNet x;
     wrapper := lookup(global Wrap,class x);
     if wrapper =!= null then (
	  fun := () -> z = wrapper z;
	  try timelimit(printingTimeLimit, fun)
	  else (
	       alarm 0;
	       global debugError <- fun;
	       stderr << "--error or time limit reached in applying Wrap method to output; type 'debugError()' to see it" << endl << endl);
	  );
     << endl << oprompt << z << endl;
     printWidth = save;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
