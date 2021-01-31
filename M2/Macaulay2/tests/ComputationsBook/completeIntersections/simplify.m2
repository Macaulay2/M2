--		Copyright 1993-2000 by Daniel R. Grayson

-- this can replace some code in hilbertSeries in modules2.m2

easyFactor = method()
easyFactor RingElement := f -> (
     -- must return a product of powers
     -- check to see if f = 1 - monomial
     R := ring f;
     if degreeLength R > 0 then (
     	  p := listForm (1-f);
     	  if #p == 1 and last first p == 1 then (
	       d := first first p;
	       c := gcd d;
	       if c > 1 then (
		    d' := d / (i -> i // c);
		    if even c
		    then (
			 q := f//((1-R_d')*(1+R_d'));
			 if q == 1
			 then Product{Power{1-R_d',1},Power{1+R_d',1}}
			 else Product{Power{1-R_d',1},Power{1+R_d',1},Power{q,1}}
			 )
		    else (
			 q = f//(1-R_d');
			 if q == 1
			 then Product{Power{1-R_d',1}}
			 else Product{Power{1-R_d',1},Power{q,1}}
			 )
		    )
	       else Product{Power{f,1}}
	       )
	  else Product{Power{f,1}}
	  )
     else Product{Power{f,1}}
     )

combinePowers := x -> (
     -- x is a Product of Powers
     new Product from apply(pairs sum apply(toList x, e -> new Tally from { e#0 => e#1 }), (f,i) -> Power{f,i})
     )

easyFactor Product := x -> Product (
     -- sort
     toList splice apply(apply(x, easyFactor), 
     	  y -> if class y === Product then toSequence y else y)
     )
easyFactor Power := x -> apply(easyFactor x#0, fac -> Power{fac#0, fac#1 * x#1})
easyFactor Divide := x -> Divide{easyFactor x#0, easyFactor x#1}

simplify = method()
simplify Divide := x -> (
     num := x#0;
     den := easyFactor x#1;				    -- a Product
     den = splice apply(den,
	  pow -> (					    -- a Power
	       f := pow#0;				    -- a ring element
	       e := pow#1;				    -- an integer
	       while true do (
		    if num % f == 0
		    then (
			 num = num // f;
			 e = e-1;
			 if e == 0 then break ();	    -- the () will be spliced out
			 )
		    else break Power{f,e}
		    )
	       )
	  );
     Divide{num,combinePowers den}
     )

end

TEST ///
A = ZZ/103[x,y]
B = A/(x^2,y^3)
k = coker vars B
M = k
E = Ext(M,k)
T = degreesRing ring E
T' = ZZ[t,u,Inverses=>true]
substitute(hilbertSeries E,
        {T_0=>T'_(-E.abjust{1,0}),T_1=>T'_(-E.abjust{0,1})})
substitute(oo, u => 1_T')
simplify oo

N = cokernel random (B^1, B^{-2,-2})
E = Ext(N,k)
substitute(hilbertSeries E,
        {T_0=>T'_(-E.abjust{1,0}),T_1=>T'_(-E.abjust{0,1})})
substitute(oo, u => 1_T')
simplify oo
///
