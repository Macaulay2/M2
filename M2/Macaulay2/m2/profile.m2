--		Copyright 1997 by Daniel R. Grayson

needs "methods.m2"

head := () -> ("#run", "%time", "position")
form := (ttime, t, n, loc) -> (n, format(4,2,2,2,"e", 100 * t / ttime), loc)
tail := (ttime, tticks) -> (tticks, format(4,4,4,4,"e",ttime) | "s", "elapsed total")

profileSummary = method(Dispatch => Thing)
profileSummary Thing := x -> profileSummary if x === () then "" else first locate x
profileSummary String := filename -> (
    dataset := select(pairs ProfileTable,
	(k, v) -> match_filename toString k);
    if #dataset == 0 then return TABLE {head(), tail(0,0)};
    (ttime, tticks) := ProfileTable#"total";
    data := sort pairs hashTable(join, apply(dataset,
	    (k, v) -> if k =!= "total" then (v, {k})));
    rows := min(20, #data);
    high := reverse take(data, {#data - rows - 1, #data - 1});
    body := apply(rows, i -> form_ttime splice high#i);
    TABLE join({head()}, body, {tail(ttime, tticks)}))
profileSummary = new Command from profileSummary

coverageSummary = method(Dispatch => Thing)
coverageSummary Thing := x -> coverageSummary if x === () then "" else first locate x
coverageSummary String := filename -> (
    body := sort select(toString \ keys ProfileTable, match_filename);
    stack join({"covered lines:"}, body))
coverageSummary = new Command from coverageSummary

end--

profile = method()

record := new MutableHashTable

profile Function := Function => f -> profile (toString f, f)

profile(String,Function) := (n,f) -> (
     record#n = m := new MutableList from {0,0.};
     args -> (
	  ret := timing f args;
     	  m#0 = m#0 + 1;
	  m#1 = m#1 + ret#0;
	  ret#1
	  )
     )

profileSummary = Command (() -> 
     scan(sort pairs record, (n,v) -> 
	  if v#0 != 0 then
	  << n << ": "
	  << v#0 << " times, used " 
	  << v#1 << " seconds"
	  << endl
	  ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
