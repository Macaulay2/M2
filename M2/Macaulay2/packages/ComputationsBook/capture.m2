saveClearAll := clearAll
erase symbol clearAll
F := openOut "test.oldvalues"
F << "-- version " << version#"VERSION" << endl;
F << "{" << endl
g := (a,b) -> F << "  " <<  a << " => " << b << "," << endl
f := (e,x) -> g(format toString e,format toString e x)
dump := () -> scan(
     userSymbols(), osym -> if osym =!= symbol clearAll then (
	  nam := toString osym;
	  val := value osym;
	  F << format nam << " => {" << endl;
	  scan({toString,net,toExternalString,class},p -> f(p,val)); 
	  F << "}," << endl;
	  ))
clearAll = new Command from { () -> (
	  dump();
	  saveClearAll();
	  g("clearAll","");
	  )}
addEndFunction(
     () -> (
	  dump();
	  F << "}" << endl;
	  close F;
	  ))
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/ComputationsBook capture"
-- End:
