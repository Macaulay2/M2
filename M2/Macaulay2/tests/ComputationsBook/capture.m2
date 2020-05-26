saveClearAll := clearAll
erase symbol clearAll
stash := new MutableHashTable
unstash := R -> if stash#?R then stash#R else R
F := openOut "test.oldvalues"
F << "-- version " << version#"VERSION" << endl;
F << "{" << endl
g := (a,b) -> F << "  " <<  a << " => " << b << "," << endl
f := (e,x) -> (
     if class e =!= Sequence then e = 1:e;
     scan(reverse e, f -> x = f x);
     g(format concatenate between(" ",apply(e,toString)),format toString unstash x))
m := s -> x -> scan(s,p -> f(p,x))
h := method(SingleArgumentDispatch=>true)
h Thing := m(class,toString)
h Function := m()
h GroebnerBasis := m(class,ring,(degrees,target,generators),(degrees,source,generators),(toString,generators))
h Ideal := m(class,ring,toString,(degrees,source,generators))
h Ring := R -> (
     (m(class,gens,degrees)) R;
     if R.?name then f(toString,R);
     if (try (coefficientRing R; true) else false) then f(coefficientRing,R);
     if instance(R,QuotientRing) then (m(ambient,ideal)) R;
     )
h RingElement := m(class,toString,degree)
h Matrix := t -> (
     (m(class,toString,ring)) t;
     R := ring t;
     (m((degrees,ambient,target),(degrees,ambient,source),degree)) t;
     if (target t).?generators then (m((degrees,source,generators,target),(generators,target))) t;
     if (target t).?relations then (m((degrees,source,relations,target),(relations,target))) t;
     if (source t).?generators then (m((degrees,source,generators,source),(generators,source))) t;
     if (source t).?relations then (m((degrees,source,relations,source),(relations,source))) t;
     )
h Module := M -> (
     (m(class,ring,(degrees,ambient))) M;
     if M.?generators then (m((degrees,source,generators),generators)) M;
     if M.?relations then (m((degrees,source,relations),relations)) M;
     )     
h ChainComplex := C -> (
     complete C;
     (m(class,ring)) C;
     scan(keys C, i -> if instance(i,ZZ) then (
	       F << i << " => {" << endl;
	       h C#i;
	       F << "}," << endl;
	       ));
     F << "dd" << " => {" << endl;
     complete C.dd;
     scan(keys C.dd, i -> if instance(i,ZZ) then (
	       F << i << " => {" << endl;
	       f(toString,C.dd#i);
	       F << "}," << endl;
	       ));
     F << "}," << endl;
     )
h ChainComplexMap := s -> (
     (m(class,ring)) s;
     complete s;
     scan(keys s, i -> if instance(i,ZZ) then (
	       F << i << " => {" << endl;
	       f(toString,s#i);
	       F << "}," << endl;
	       ));
     )
dump := () -> scan(
     userSymbols(), osym -> if osym =!= symbol clearAll then (
	  F << format toString osym << " => {" << endl;
	  val := value osym;
	  h val;
	  stash#val = osym;
	  F << "}," << endl;
	  ))
clearAll = new Command from { () -> (
	  dump();
	  saveClearAll();
	  g("clearAll","null");
	  )}
protect symbol clearAll
addEndFunction(
     () -> (
	  dump();
	  F << "}" << endl;
	  close F;
	  ))
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/ComputationsBook capture"
-- End:
