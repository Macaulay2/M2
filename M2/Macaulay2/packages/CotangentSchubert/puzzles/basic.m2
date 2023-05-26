makeStates = d -> try myget ("states-"|toString d|".m2") else error "this value of d not implemented"
-- actually the d=4 states are never used anywhere...
-- memoize?
basicTriangles = d -> ( -- the procedure below only works for d<=3
    if d>=4 then error "no non generic puzzles for this value of d";
    states := makeStates d;
        l := new MutableList;
        -- single digit triangles
        scan(0..d, i -> l#(#l) = { toString i, toString i, toString i });
        -- combos
        table(states,states, (a,b) -> (
                c := (if #b == 1 then b else "(" | b | ")") |  (if #a == 1 then a else "(" | a | ")");
                if member(c,states) then (
                    l#(#l) = {a,b,c};
                    l#(#l) = {c,a,b};
                    l#(#l) = {b,c,a};
                )));
        new List from l
        )
