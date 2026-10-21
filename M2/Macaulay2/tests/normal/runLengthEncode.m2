-- runLengthEncode prefers repeated entries and avoids two-entry ranges.
assert(toString runLengthEncode {1,2} == "{1, 2}")
assert(toString runLengthEncode {1,2,3} == "{1..3}")
assert(toString runLengthEncode {7,2,5,6} == "{7, 2, 5, 6}")
assert(toString runLengthEncode {1,2,2,2,3,3,3} == "{1, 3:2, 3:3}")
assert(toString runLengthEncode {1,1,2,2,3,3} == "{2:1, 2:2, 2:3}")
assert(toString runLengthEncode {1,2,3,1,2,3} == "{2:1..3}")

assert(toString runLengthEncode {a,b} == "{a, b}")
assert(toString runLengthEncode {a,b,c} == "{a..c}")

R22 = QQ[m_(1,1)..m_(2,2)]
assert(toString runLengthEncode(expression \ gens R22) == "{m_(1,1)..m_(2,2)}")

-- Recursive encoding subsumes the former private rle helper.
assert(toString runLengthEncode {{1,2,3},{4,4,4}} == "{{1..3}, {3:4}}")
assert(toString runLengthEncode (1,(2,2),(3,3)) == "(1,2:2,2:3)")
assert(instance(first runLengthEncode {{1,2,3}}, Holder))
assert(toString runLengthEncode {Degrees => {1,1,1}, Heft => {1,2}} ==
    "{Degrees => {3:1}, Heft => {1, 2}}")
assert(class(runLengthEncode (1,2,3)) === Sequence)
assert(class(runLengthEncode [1,2,3]) === Array)
assert(class(runLengthEncode {1,2,3}) === List)

-- Singleton holders are needed for the documented value round trip.
x = {1,2,3,a,b,c,a,b,c,4,4,4,"asdf"}
y = runLengthEncode x
assert(x === deepSplice(value \ y))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/tests/normal runLengthEncode.out"
-- End:
