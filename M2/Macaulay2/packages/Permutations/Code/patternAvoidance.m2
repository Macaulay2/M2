------------------------------------
-- Pattern avoidance
------------------------------------
avoidsPattern = method(TypicalValue => Boolean)
avoidsPattern (Permutation,List) := (w, pattern) -> (
    --assume permutation is pattern-avoiding, break if not true
    for idx in subsets(0 .. #w-1, #pattern) do {
        vals := w_(idx);
        sortedVals := sort(vals);
        relPositions := hashTable toList apply(0..#vals-1, i -> {sortedVals#i, i});
        p := toList apply(vals, i -> (relPositions#i) + 1); 
        if p == pattern then return false;
    };
    true
)


avoidsPatterns = method(TypicalValue => Boolean)
avoidsPatterns (Permutation, List) := (w, patterns) -> (
    all(patterns, pattern -> avoidsPattern(w, pattern))
)


isVexillary = method(TypicalValue => Boolean)
isVexillary Permutation := (w) -> (
    avoidsPattern(w, {2,1,4,3})
)


isCartwrightSturmfels = method(TypicalValue => Boolean)
isCartwrightSturmfels Permutation := w -> (
    patterns := {{1,2,5,4,3}, 
                 {1,3,2,5,4}, 
                 {1,3,5,2,4}, 
                 {1,3,5,4,2}, 
                 {2,1,5,4,3}, 
                 {1,2,5,3,6,4}, 
                 {1,2,5,6,3,4}, 
                 {2,1,5,3,6,4},
                 {2,1,5,6,3,4},
                 {3,1,5,2,6,4},
                 {3,1,5,6,2,4},
                 {3,1,5,6,4,2}};
    avoidsPatterns(w, patterns)
)


isCDG = method(TypicalValue => Boolean)
isCDG Permutation := w -> (
    patterns := {{1,3,2,5,4},
                 {2,1,5,4,3},
                 {2,1,4,6,3,5},
                 {2,1,5,3,6,4},
                 {2,1,5,6,3,4},
                 {2,4,1,6,3,5},
                 {3,1,5,2,6,4},
                 {4,2,6,1,7,3,5}};
    avoidsPatterns(w, patterns)
)

isSeparable = method(TypicalValue => Boolean)
isSeparable Permutation := w -> (
    avoidsPatterns(w, {{2,4,1,3}, {3,1,4,2}})
)