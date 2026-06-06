------------------------------------
-- Local utilities
------------------------------------
to1Index := w -> (w / (i -> i+1))
to0Index := w -> (w / (i -> i-1))


------------------------------------
-- Indexing permutations as lists
------------------------------------
Permutation _ ZZ := ZZ => (w,n) -> ((toList w)_(n-1))
Permutation _ List := List => (w,l) -> ((toList w)_l)
Permutation _ Sequence := List => (w,s) -> ((toList w)_(toList s))


------------------------------------
-- Basic permutation operations
------------------------------------
Permutation == Permutation := Boolean => (w, v) -> (
    (w, v) = extend(w,v);
    toList(w) == toList(v)
)


Permutation * Permutation := Permutation => (w, v) -> (
    (w,v) = extend(w,v);
    trim permutation w_(to0Index toList v)
)


-- power implementation modified from Mahrud's in https://github.com/Macaulay2/M2/issues/2581
Permutation ^ ZZ := Permutation => (w, n) -> fold(if n < 0 then (-n):(permutation to1Index inversePermutation to0Index toList w) else n:w,
                                                  permutation toList (1 .. #w),
                                                  (w, v) -> w*v)


------------------------------------
-- Group actions
------------------------------------
Permutation * VisibleList := VisibleList => (w, l) -> (
    if #(trim w) > #l then error(toString w | " permutes more than " | toString #l | " elements.") 
    else l_(to0Index toList extend(w, #l))
)
VisibleList _ Permutation := VisibleList => (l, w) -> (w*l)


-- group action on a matrix permutes the rows/columns of the matrix
Permutation * Matrix := Matrix => (w, M) -> (
    m := numRows M;
    if #(trim w) > m then error(toString w | " permutes more than " | toString m | " elements.") 
    else (matrix extend(w, m)) * M
)


Matrix _ Permutation := Matrix => (M, w) -> (w*M)
Matrix * Permutation := Matrix => (M, w) -> (transpose(w*(transpose M)))
Matrix ^ Permutation := Matrix => (M, w) -> (M*w)