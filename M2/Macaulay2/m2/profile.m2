--		Copyright 1997 by Daniel R. Grayson

needs "methods.m2"

leaderboard = dataset -> (
    (ttime, tticks) := toSequence sum(toList \ values dataset);
    data := sort pairs hashTable(join,
	apply(pairs dataset, (k, v) -> (v, {k})));
    rows := min(20, #data);
    high := reverse take(data, {#data - rows - 1, #data - 1});
    form := (loc, num, t) -> (loc, num, format(4,2,2,2,"e", 100 * t / ttime));
    body := apply(rows,
	i -> splice { pad(floor log_10(rows) + 1, i | "."),
	    form reverse splice high#i});
    TABLE join({{"#", "Address", "#run", "%time"}}, body, {{"-", "Totals:", tticks, ttime}}))

profileSummary = Command(() -> leaderboard ProfileTable)

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
