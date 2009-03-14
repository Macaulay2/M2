newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString", "sysGetObject", "objectType"
     }

export { "pythonHelp", "context", "rs", "val", "eval", "valuestring", "process", "expr", "Preprocessor" }
pythonHelp = Command (() -> runString ///help()///)

PythonObject#{Standard,AfterPrint} = x -> (
     << endl;
     << concatenate(interpreterDepth:"o") << lineNumber << " : PythonObject of type " << replace("<type '(.*)'>","\\1",toString objectType x) << endl;
     )

rs = s -> ( 
     s = concatenate s;
     if debugLevel > 0 then stderr << "--python command: " << s << endl; 
     runString s);

numContexts := 0
nextContext := () -> (
     numContexts = numContexts + 1;
     "context" | toString numContexts)
Context = new Type of HashTable
context = method(Options => {
	  Preprocessor => ""
	  })
context String := opts -> init -> (
     d := nextContext();
     rs("eval(compile( '",d," = {}','','single' ),__builtins__) ");
     access := s -> concatenate(d,"[", format s, "]");
     val := s -> rs access s;
     eval := s -> rs concatenate("eval(compile(",s,",'','single' ),",d,")");
     evalstring := s -> eval format concatenate s;
     evalstring init;
     valuestring := s -> (
	  evalstring("tmp = ",s);
	  val "tmp");
     process := s -> (
	  evalstring("tmp = ",opts.Preprocessor,"(",format s,")");
	  if debugLevel > 0 then stderr << "--intermediate value: tmp = " << format toString runString access "tmp" << endl;
	  eval access "tmp";);
     expr := s -> (
	  s = "temp = " | s;
	  process s;
	  val "temp");
     new HashTable from {
	  global val => val,
	  global eval => evalstring,
	  global valuestring => valuestring,
	  global process => process,
	  global expr => expr	  
	  }
     )

end

debugLevel = 1
loadPackage "Python"
sage = context("from sage.all import *", Preprocessor => "preparse")
sage.process "x = var('x')"
sage.process "plot(sin(x))"
sage.expr "320"

rs = s -> ( if debugLevel > 0 then stderr << "python command: " << s << endl; runString s);
rs "eval(compile( 'd = {}','','single' ),__builtins__) "
access = (d,s) -> concatenate(d,"[", format s, "]");
val = s -> rs access("d",s);
eval = s -> rs concatenate("eval(compile(",s,",'','single' ),d)");
evalstring = s -> eval format s;
valuestring = s -> ( evalstring("tmp = "|s); val "tmp");
sage = s -> (evalstring("tmp = preparse("|format s|")"); eval access("d","tmp"));
dir = s -> valuestring concatenate("dir(", s, ")");

runSimpleString "x=2"
runSimpleString "print(x)"
rs "dir()"
rs "dict"
rs "__builtins__.keys()"
rs "help()"
quit
rs "d.keys()"
rs "__builtins__['d']"
evalstring "x=2"
rs "d.keys()"
val "x"
valuestring "range(2,100)"
rs "range(2,100)"
-- math
evalstring "from math import *"
valuestring "sin(4.5)"
rs "d.keys()"
-- module sys
-- http://docs.python.org/library/sys.html#module-sys
sysGetObject "subversion"
sysGetObject "builtin_module_names"
sysGetObject "copyright"
evalstring "import sys"
valuestring "sys.version"
evalstring "from sys import *"
valuestring "version"
valuestring "modules.keys()"
valuestring "copyright"
valuestring "prefix"
valuestring "executable"
-- sage
evalstring "from sage.all import *"
valuestring "sage"
valuestring "dir(sage)"
valuestring "sage.version"
valuestring "version()"
valuestring "dir(sage.version)"
valuestring "sage.version.version"
valuestring "plot"
valuestring "preparse"
valuestring "preparse('x=1')"
sage "x=2^100"
val "x"
objectType oo
sage "R.<x,y,z> = QQ[]"
valuestring "R"
valuestring "var('x')"
valuestring "plot(sin(x))"
sage "plot(sin(x))"
valuestring "show(plot(sin(x)))"
sage "I = ideal(x^2,y*z)"
valuestring "I"
val "I"
sage "G = I.groebner_basis()"
valuestring "G"
dir "I"
