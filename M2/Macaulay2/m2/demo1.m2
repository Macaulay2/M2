-- when running examples, we usually specify the --stop option
-- here we reverse that so we can demonstrate the debugger in an example
stopIfError = false
debuggingMode = true

f := x -> (
     a := "hi there";
     b := 1/x;
     b+1)

g = y -> (
     c := f(y-1);
     d := f(y-2);
     c+d)
