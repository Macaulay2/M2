--		Copyright 1994 by Daniel R. Grayson

Function @@ Function := (f,g) -> x -> f g x

document { "functions",
     "There are two types of functions, those functions built in
     to the system, and those created by the user, but in practice
     the user has no way of distinguishing the two types.  The user
     creates new functions with the ", TO "->", " operator.",
     PARA,
     "Operations on functions:",
     MENU {
 	  (TO "@@", " -- composition"),
	  (TO "ultimate", " -- ultimate value for an iteration")
 	  },
     "Particular functions:",
     MENU {
	  TO "identity"
	  }
     }

TEST "
stream = (action,state) -> () -> stream(action, action state)
fib = stream( (i,j) -> (j,i+j), (0,1))
scan(1 .. 22, i -> fib = fib())
"

ultimate = method()
ultimate(Function,Thing) := (f,x) -> (
     while try (ox := x; x = f x; ox =!= x) else false do ();
     x)

document { quote ultimate,
     TT "ultimate(f,x)", " -- yields the value ultimately obtained by
     applying the function ", TT "f", " to ", TT "x", ".",
     PARA,
     "Iteration ceases when an error occurs during application of the
     function, or the result is the same.  Errors are not reported."
     }
