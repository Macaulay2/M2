newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString", "sysGetObject", "objectType", "initspam"
     }

export { "pythonHelp", "context", "rs", "val", "eval", "valuestring", "stmt", "expr", "Preprocessor" }
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
     stmt := s -> (
	  evalstring("tmp = ",opts.Preprocessor,"(",format s,")");
	  if debugLevel > 0 then stderr << "--intermediate value: tmp = " << format toString runString access "tmp" << endl;
	  eval access "tmp";);
     expr := s -> (
	  s = "temp = " | s;
	  stmt s;
	  val "temp");
     kys := () -> runString concatenate("__builtins__[",format d,"].keys()");
     new HashTable from {
	  global dictionary => d,
	  global val => val,
	  global eval => evalstring,
	  global valuestring => valuestring,
	  global stmt => stmt,
	  global expr => expr,
	  global keys => kys
	  }
     )

end

pythonHelp
quit

runSimpleString "x=2"
runSimpleString "print(x)"
rs "dir()"
rs "dict"
rs "__builtins__.keys()"
rs "range(2,100)"

-- module sys
-- http://docs.python.org/library/sys.html#module-sys
sysGetObject "subversion"
sysGetObject "builtin_module_names"
sysGetObject "copyright"

sys = context "import sys"
sys.expr "sys.version"

sys2 = context "from sys import *"
sys2.expr "version"
sys2.expr "modules.keys()"
sys2.expr "copyright"
sys2.expr "prefix"
sys2.expr "executable"

debugLevel = 1
loadPackage "Python"

math = context "from math import *"
math.keys()
math.stmt "x = sin(3.4)"
math.expr "sin(3.4)"
math.expr "x"
math.expr "e"

sage = context("from sage.all import *", Preprocessor => "preparse")
sage.stmt "x = var('x')"
sage.stmt "plot(sin(x))"
sage.expr "320"
sage.expr "sage"
sage.expr "dir(sage)"
sage.expr "sage.version"
sage.expr "version()"
sage.expr "dir(sage.version)"
sage.expr "sage.version.version"
sage.expr "plot"
sage.expr "preparse"
sage.expr "preparse('x=1')"
sage.expr "x=2^100"
sage.expr "x"
sage.stmt "R.<x,y,z> = QQ[];"
sage.expr "R"
sage.stmt "x = var('x')"
sage.expr "plot(sin(x))"
sage.stmt "plot(sin(x))"
sage.expr "show(plot(sin(x)))"
sage.stmt "I = ideal(x^2,y*z)"
sage.expr "I"
sage.expr "dir(I)"

initspam()
sys2.expr "modules['spam']"

spam = context "from spam import *"
spam.keys()
spam.expr "system"
spam.expr "system('echo hi there')"
