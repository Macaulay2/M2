--		Copyright 2000 by Daniel R. Grayson

use C;
use system;
use strings;
use stdio;
use getline;
use tokens;
use objects;
use binding;
use convertr;
use struct;

TeXmacsEvaluate := makeProtectedSymbolClosure("TeXmacsEvaluate");

export topLevelTexmacs():bool := (
     unsetprompt(stdin);
     while true do (
	  stdin.bol = false;				    -- sigh, prevent a possible prompt
	  r := getline(stdin);
	  when r is e:errmsg do (
	       stderr << "can't get line : " << e.message << endl << Flush;
	       exit(1);
	       )
	  is item:string do (
	       if stdin.eof then return(true);;
	       method := lookup(stringClass,TeXmacsEvaluate);
	       if method == nullE 
	       then (
		    flush(stdout);
		    stderr << "no method for TeXmacsEvaluate" << endl;
		    )
	       else (
		    apply(method,Expr(item));
		    )
	       )));
topLevelTexmacs(e:Expr):Expr := toExpr(topLevelTexmacs());
setupfun("topLevelTexmacs",topLevelTexmacs);
