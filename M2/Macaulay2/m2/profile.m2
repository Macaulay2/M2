-- Copyright 2024 by Mahrud Sayrafi

-- 'profile' is an interpreter keyword, defined in d/profiler.dd,
-- which logs statistics of executed M2 code in 'ProfileTable'.
-- TODO: log the relationship between function calls and return
-- in an external format like Graphviz, pprof, etc.

needs "methods.m2"

head := () -> ("#run", "%time", "position")
form := (ttime, t, n, loc) -> (n, format(4,2,2,2,"e", 100 * t / ttime), loc)
tail := (ttime, tticks) -> (tticks, format(4,4,4,4,"e",ttime) | "s", "elapsed total")

-- prints the statistics logged by the profiler in a readable table
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

-- prints a list of lines which have been seen by the profiler so far
-- TODO: also highlight missing lines or sections within a line
-- TODO: compute the percentage of covered code
coverageSummary = method(Dispatch => Thing)
coverageSummary Thing := x -> coverageSummary if x === () then "" else first locate x
coverageSummary String := filename -> (
    body := sort select(toString \ keys ProfileTable, match_filename);
    stack join({"covered lines:"}, body))
coverageSummary = new Command from coverageSummary
