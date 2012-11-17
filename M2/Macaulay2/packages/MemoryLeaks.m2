debug Core

scan( {
	  BasicList => (x,p) -> apply(#x, i -> (x#i, (p,i))),
	  Boolean => a -> null,
	  CompiledFunction => a -> null,
	  CompiledFunctionBody => a -> null,
	  CompiledFunctionClosure => (x,p) -> (frame x, (p,frame)),
	  Database => a -> null,
	  Dictionary => (x,p) -> apply(pairs x, (nam,sym) -> (sym,(p,nam))),
	  File => a -> null,
	  FunctionBody => a -> null,
	  FunctionClosure => (x,p) -> (frame x, (p,frame)),
	  HashTable => (x,p) -> apply(pairs x, (key,val) -> (val,(p,key))),
	  LibxmlAttribute => a -> null,
	  LibxmlNode => a -> null,
	  MysqlConnection => a -> null,
	  MysqlField => a -> null,
	  MysqlResult => a -> null,
	  Net => a -> null,
	  NetFile => a -> null,
	  Nothing => a -> null,
	  Number => a -> null,
	  Pseudocode => a -> null,
	  PythonObject => a -> null,
	  RawObject => a -> null,
	  String => a -> null,
	  Symbol => (x,p) -> (value x,(p,value)),
	  SymbolBody => a -> null,
	  Thing => (x,p) -> error ("findAll: internal error: unhandled type ", toString class x),
	  Task => a -> null
	  },
     t -> t#0 # global findAll = t#1)

findAll = X -> (
     seen := new MutableHashTable;
     descend := new MutableHashTable;
     see := (x,p) -> if not seen#?x then descend#x = seen#x = p;
     scan(dictionaryPath, d -> scan(values d, sym -> see(sym,sym)));
     while #descend > 0 do scan(pairs descend, (x,p) -> (
	       remove(descend,x);
	       f := lookup(global findAll, class x);
	       y := f(x,p);
	       if instance(y,Sequence) then see y
	       else if instance(y,List) then see \ y
	       else assert(y === null);
	       ));
     new MutableHashTable from apply(select(pairs seen, (k,v) -> instance(k,X)), (k,v) -> (k,toList deepSplice v)))
