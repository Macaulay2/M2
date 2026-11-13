-- when running examples, we usually specify the --stop option
-- here we reverse that so we can demonstrate the debugger in an example
stopIfError = false
debuggingMode = true

f = t -> (
     x := 1;
     error "debug me";
     y := t+1;
     z := 1/t;
     w := x+t;
     )

F = t -> (
     x := 1;
     breakpoint "debug me";
     y := t+1;
     z := 1/t;
     w := x+t;
     )

G = t -> (
     x := 1;
     F(t);
     Z := "Zedd"
)

h = t -> (
     x := 1;
     breakpoint y := t+1;
     z := 1/t;
     w := x+t;
)
