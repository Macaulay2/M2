--		Copyright 2000 by Daniel R. Grayson

use getline;
use util;
use evaluate;

TeXmacsEvaluate := makeProtectedSymbolClosure("TeXmacsEvaluate");

export topLevelTeXmacs():int := (
     unsetprompt(stdIO);
     while true do (
	  stdIO.bol = false;				    -- sigh, prevent a possible prompt
     	  stdIO.echo = false;
	  r := getLine(stdIO);
	  when r is e:errmsg do (
	       stdIO << flush;
     	       endLine(stdError);
	       stderr << "can't get line : " << e.message << endl;
	       exit(1);
	       )
	  is item:stringCell do (
	       if stdIO.eof then return 0;
	       method := lookup(stringClass,TeXmacsEvaluate);
	       if method == nullE 
	       then (
		    stdIO << flush;
     		    endLine(stdError);
		    stderr << "no method for TeXmacsEvaluate" << endl;
		    )
	       else (
		    applyEE(method,Expr(item));
		    )
	       )));
topLevelTeXmacs(e:Expr):Expr := toExpr(topLevelTeXmacs());
setupfun("topLevelTeXmacs",topLevelTeXmacs);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d texmacs.o "
-- End:
