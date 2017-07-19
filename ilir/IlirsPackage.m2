newPackage(
        "IlirsPackage",
        Version => "1.0",
        Date => "July 19, 2017",
        Authors => {{Name => "IlirDema",
                  Email => "ilir.dema@mail.utoronto.ca",
                  HomePage => ""}},
        Headline => "an example Macaulay2 package",
        DebuggingMode => true
        )

--needsPackage"SimpleDoc"

export {"firstFunction", "frobenius"}

firstFunction = method(TypicalValue => String)
firstFunction ZZ := String => n -> if n == 1 then "Hello World!" else "D’oh!"

-- begin frobenius
frobenius = method()
frobenius MutableList := ZZ => (generators) -> (

   D := new MutableList from {}; 
   C := new MutableList from {}; 
   E := new MutableList from {}; 
   F := new MutableList from {};
   n := #generators;
   
   a1 := generators#0;
   
   for j from 1 to n - 1 do (
      a := generators#j;
      aj := a - a // a1 * a1;
	  D#aj = j+1;
	  C#aj = j+1;
	  E#(j+1) = aj;
	  F#aj = a;
   );
   a2 := a1 - 1;
   lenF = #F;
   print lenF;
   for k from 1 to a2 do (
      if k >= lenF then F#k = 0;
      if instance(F#k, Nothing) then F#k = 0;
   );
   
   m = n;
   j = 2;
   while j <= m do (
      i = E#j;
		if instance(C#i, ZZ) then (
			if C#i == j then (
				k = 2;
				while k <= D#i do (
					 val = F#i + generators#(k-1);
					 val1 = val - val // a1 * a1;
					 if not(val1 == 0) and (F#val1 <= 0 or F#val1 > val) then (
						m = m + 1;
						D#val1 = k;
						C#val1 = m;
						E#m = val1;
						F#val1 = val;
					 );
				     k = k + 1;
			    );
		    );
        );
	   j = j + 1;
   );
   result = -1;
   for x in F do (
      if instance(x, ZZ) then (
         if x > result then result = x;
	  );
   );
   result-a1
)
-- end frobenius
beginDocumentation()
doc ///
    Key
        IlirsPackage
    Headline
        an example Macaulay2 package
    Description
        Text
            This package is a basic package to be used as an example.
    Caveat
        Still trying to figure this out.
///

doc ///
    Key
        firstFunction
        (firstFunction,ZZ)
    Headline
	a silly first function
    Usage
	f = firstFunction n
    Inputs
	n:ZZ
    Outputs
	f:String 
            a silly string, depending on the value of n
    Description
	Text
	    Here we show an example.
        Example
	    firstFunction 1
	    firstFunction 0

///

TEST ///
	assert ( firstFunction 2 == "D’oh!" )
///
end
