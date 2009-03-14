newPackage "Python"

export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "runSimpleString", "PythonObject", "runString", "sysGetObject", "objectType", "initspam"
     }

export { "pythonHelp", "context", "rs", "Preprocessor" }

exportMutable { "val", "eval", "valuestring", "stmt", "expr", "dict", "symbols" }

pythonHelp = Command (() -> runString ///help()///)

PythonObject#{Standard,AfterPrint} = x -> (
     << endl;
     t := toString objectType x;
     t = replace("<([a-z]+) '(.*)'>","of \\1 \\2",t);
     << concatenate(interpreterDepth:"o") << lineNumber << " : PythonObject " << t << endl;
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
globalAssignment Context
use Context := c -> scanPairs(c,(k,v) -> k <- v)
context = method(Options => {
	  Preprocessor => ""
	  })
context String := opts -> init -> (
     dict := nextContext();
     rs("eval(compile( '",dict," = {}','','single' ),__builtins__) ");
     access := s -> concatenate(dict,"[", format s, "]");
     val := s -> rs access s;
     eval := s -> rs concatenate("eval(compile(",s,",'','single' ),",dict,")");
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
     symbols := () -> runString concatenate("__builtins__[",format dict,"].keys()");
     c := new Context from {
	  global dict => dict,
	  global val => val,
	  global eval => evalstring,
	  global valuestring => valuestring,
	  global stmt => stmt,
	  global expr => expr,
	  global symbols => symbols
	  };
     use c;
     c)

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

sys = context "import sys";
expr "sys.version"

sys2 = context "from sys import *";
expr "version"
expr "modules.keys()"
expr "copyright"
expr "prefix"
expr "executable"

debugLevel = 1
loadPackage "Python"

math = context "from math import *";
symbols()
stmt "x = sin(3.4)"
expr "sin(3.4)"
expr "x"
expr "e"

sage = context("from sage.all import *", Preprocessor => "preparse");
stmt "x = var('x')"
stmt "plot(sin(x))"
expr "320"
expr "sage"
expr "dir(sage)"
expr "sage.version"
expr "version()"
expr "dir(sage.version)"
expr "sage.version.version"
expr "dir(sage.categories.morphism)"
expr "sage.categories.morphism.__file__"
expr "sage.categories.morphism.__doc__"
expr "sage.categories.morphism.homset.Hom"
expr "dir(sage.categories.morphism.homset.Hom)"
expr "sage.categories.morphism.homset.Hom.__doc__"
hash expr "SymmetricGroup(3)"
hash expr "SymmetricGroup(3)" == hash expr "SymmetricGroup(3)"
hash expr "SymmetricGroup(2)"
hash expr "SymmetricGroup(2)" == hash expr "SymmetricGroup(3)"
stmt "G = SymmetricGroup(3)"
expr "G"
expr "dir(G)"
expr "G.set()"
expr "G.sylow_subgroup(3)"
expr "G.sylow_subgroup(2)"
expr "G.dump.__doc__"
expr "G.multiplication_table()"
expr "plot"
expr "preparse"
expr "preparse('x=1')"
expr "x=2^100"
expr "x"
stmt "R.<x,y,z> = QQ[];"
expr "R"
stmt "x = var('x')"
expr "plot(sin(x))"
stmt "plot(sin(x))"
expr "show(plot(sin(x)))"
stmt "I = ideal(x^2,y*z)"
expr "I"
expr "dir(I)"
stmt "R.<t> = PowerSeriesRing(QQ)"
expr "R"
expr "exp(t)"

stmt "p = plot(sin(x))"
p = expr "p"
hash p			  -- this displays the plot and gives a hash code of 0!


initspam()
spam = context "from spam import *";
symbols()
expr "system"
expr "system('echo hi there')"

