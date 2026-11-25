--		Copyright 1993-1999 by Daniel R. Grayson

needs "expressions.m2"
needs "methods.m2"

varIndices := new MutableHashTable

varName := i -> (
     if 0 <= i and i < 26 then ascii(97 + i)
     else if 26 <= i and i < 52 then ascii(65 + i - 26)
     else if i < 0
     then "X" | toString(-i)
     else "x" | toString(i-52))

vars ZZ := i -> (
    x := getSymbol varName i;
    varIndices#x = i;
    x)

vars List := vars Sequence := args -> apply(flatten splice args, j -> vars j)

isUserSymbol := (s,a) -> User#"private dictionary"#?s and User#"private dictionary"#s === a

reverseVars := a -> (
     if varIndices#?a then varIndices#a
     else (
	  s := toString a;
	  if isUserSymbol(s,a) and match("^[a-zA-Z]$",s) then (
	       first ascii s + if match("^[A-Z]$",s) then 26 - first ascii "A" else - first ascii "a"
	       )
     	  else error(a, " is not one of the symbols known to 'vars'")
     	  )
     )

Symbol .. Symbol := (a,z) -> if a === z then 1:a else vars( reverseVars a .. reverseVars z )
Symbol ..< Symbol := (a,z) -> if a === z then () else vars( reverseVars a ..< reverseVars z )

succS = new MutableHashTable;
for i from 0 to 50 do succS#(varName i) = varName(i+1)
succ = method()
succ(ZZ,ZZ) := (x,y) -> x+1 === y
succ(Sequence,Sequence) := (x,y) -> ( -- for multiple indices
    #x === #y and all(#x-1,i-> x#i === y#i) and succ(last x,last y)
)
succ(BinaryOperation,BinaryOperation) := (x,y) -> x#0 === symbol .. and y#0 === symbol .. and (
    succ (x#2,y#1) or (
	instance(x#1,Subscript) and instance(x#2,Subscript)
	and instance(y#1,Subscript) and instance(y#2,Subscript) and (
	    a := sequence x#1#1; b := sequence x#2#1; c := sequence y#1#1; d := sequence y#2#1;
	    -- find what endpoints have in common, remove
	    while #a>0 and #c>0 and last a === last c do ( a=drop(a,-1); c=drop(c,-1); );
	    if #a === 0 then a=x#1#0 else a=new Subscript from {x#1#0,unsequence a};
	    if #c === 0 then c=y#1#0 else c=new Subscript from {y#1#0,unsequence c};
	    while #b>0 and #d>0 and last b === last d do ( b=drop(b,-1); d=drop(d,-1); );
	    if #b === 0 then b=x#2#0 else b=new Subscript from {x#2#0,unsequence b};
	    if #d === 0 then d=y#2#0 else d=new Subscript from {y#2#0,unsequence d};
	    a===b and c===d and succ(a,c)
	)))
succ(Holder,Thing) := (x,y) -> succ(x#0,y)
succ(Thing,Holder) := (x,y) -> succ(x,y#0)
succ(Symbol,Symbol) := (x,y) -> (
     (s,t) := (toString x, toString y);
     isUserSymbol(s,x) and isUserSymbol(t,y) and succS#?s and succS#s === t)
succ(Subscript,Subscript) := (x,y) -> x#0 === y#0 and succ(x#1,y#1)
succ(Thing,Thing) := x -> false

-- Blocks contain an encoded item, its number of original entries, and the
-- original items if it is a provisional two-entry range.
runLengthEncodeDuplicates = blocks -> (
    i := 0;
    while i < #blocks list (
	j := i + 1;
	weight := blocks#i#1;
	while j < #blocks and blocks#j#0 === blocks#i#0 do (
	    weight += blocks#j#1;
	    j += 1);
	count := j - i;
	block := if count === 1 then blocks#i else
	    (hold count : expression blocks#i#0, weight, null);
	i = j;
	block))

runLengthRangeStart = x -> if instance(x, BinaryOperation) then x#1 else expression x
runLengthRangeEnd   = x -> if instance(x, BinaryOperation) then x#2 else expression x

runLengthEncodeSuccessors = blocks -> (
    i := 0;
    while i < #blocks list (
	j := i + 1;
	weight := blocks#i#1;
	while j < #blocks and succ(blocks#(j-1)#0, blocks#j#0) do (
	    weight += blocks#j#1;
	    j += 1);
	block := if j - i > 1 then (
		runLengthRangeStart(blocks#i#0) .. runLengthRangeEnd(blocks#(j-1)#0),
		weight,
		if weight === 2 then apply(i..j-1, k -> hold blocks#k#0))
	    else blocks#i;
	i = j;
	block))

runLengthEncodeOutput = blocks -> splice apply(blocks,
    block -> if block#2 === null then hold block#0 else block#2)

runLengthEncode1 = x -> (
    blocks := apply(x, item -> (item, 1, null));
    done := false;
    while not done do (
	next := runLengthEncodeSuccessors runLengthEncodeDuplicates blocks;
	done = #next === #blocks;
	blocks = next);
    new class x from runLengthEncodeOutput blocks)

runLengthEncode = method(Dispatch => Thing)
runLengthEncode VisibleList := x -> apply(runLengthEncode1 x, runLengthEncode)
runLengthEncode Holder := x -> hold unsequence runLengthEncode x#0
runLengthEncode Option := x -> x#0 => runLengthEncode x#1
runLengthEncode VerticalList :=
runLengthEncode Thing := identity

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
