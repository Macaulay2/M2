-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

-------------------------------------------------------------------------
-- bFunction (M, w, m) -> bf
-- M = finitely generated D-module 
-- w = weight
-- bf = b-function of M with respect to weight w and shift vector m 
-- (polynomial in K[s], where K is the coefficient ring)
------------------------------------------------------------------------

bFunction(Module, List, List) := RingElement => o -> (M, w, m) -> (
     s := symbol s;
     S := QQ[s];
     bf := 1_S; 
     M' := image presentation M; 
     R := ring M';
     F := super M';
     n := numgens F;
    
     --sanity check
     if #m != n then 
     error "wrong shift vector length";
     if any(m, u -> class u =!= ZZ) then
     error "shift vector should consist of integers";
     
     i := 0;
     while i < n do (
	  -- N = i-th component of F
	  N := image map(ambient M', R^1, (toList(i : {0_R})) | {{1}} | (toList((n-i-1):{0_R})) );  
	  NM := intersect(M', N);
	  I := ideal apply(numgens NM, j -> NM_j_i);
	  bf' := bFunction(I, w);
	  bf' = (map(S, ring bf', matrix{{s}})) bf';
	  bf' = substitute(bf', { s => s - m#i });
	  -- bf = lcm (bf, bf')
	  bf = bf * (bf' // gcd(bf, bf'));
	  i = i + 1;
	  );
     bf 
     );-- end bFunction(Module)

///
bFunction2 = method(Options=>{Strategy=>TryGeneric})
bFunction2(Module, List, List) := o -> (M, w, m) -> (
     if not isQuotientModule M 
     then error "expected quotient module";
          
     N := image presentation M;
     R := ring N;
     M := super N;
     n := numgens M;
     pInfo(666, {M,N,R,n});
          
     --sanity check
     if #m != n then 
     error "wrong shift vector length";
     if any(m, u -> class u =!= ZZ) then
     error "shift vector should consist of integers";
     
     createDpairs R;
     dpI := R.dpairInds;
     w' := apply(numgens R, i -> (
	       p := position(dpI#1, u -> u == i); 
	       if p =!= null  then w#p
	       else (
		    p = position(dpI#0, u -> u == i);
		    -w#p
		    )  
	       ));

     --R' := newRing(R, Weights => w');
     --N = substitute(N, R');
     row := symbol row;
     h := symbol h;
     R' := (coefficientRing R)(monoid [h, (row_0..row_(n-1)), (entries vars R)#0, 
	  WeylAlgebra => R.monoid.Options.WeylAlgebra,
	  MonomialOrder => ProductOrder{1,n,numgens R}]);
     RtoR' := map(R', R, (vars R')_{1..numgens R});
     R'toR := map(R, R', matrix{toList(n+1:1_W)} | (vars W));
      
     pInfo(666, vars R');
     M' := substitute(M, R');
     big := max m + first max( 
	  toList ((0,0)..(numgens N-1, n-1)) / (u->degree N_(u#0)_(u#1)) );
     N' = ideal (
	  toList (0..(numgens N - 1)) / 
     	  (i -> ( -- generator number
	       sum( toList (0..n-1) / (j -> ( -- component number
			 sum(listForm N_i_j ,  u->(
			      u#1 * R'_(i+1) * R'_( {
					sum(toList(0..numgens R - 1)/(t->w'#t*u#0#t))+m#j+big
					} | toList (n:0) | u#0 )
			      
			      ))
			 --*M'_j
			 )))
	       ))
     	  );
     G := first entries gens gb N';
     debugG = new MutableList from toList(n:null); 
     pInfo (666, {"N = '", N', endl, "G = ", G});
     
     -- form the list of ideals I_i = N_i/N_(i-1) where N_(-1)=0
     I := new MutableList from toList (n : {});
     i := n-1;
     while i >= 0 do( 
          j := 0;
     	  debugG#i = G; 
     	  while j < #G do(
	       lF := listForm G#j;
	       IthComponent := sum(select(lF, u->u#0#(i+1)==1), v->v#1*R'_(v#0));
	       if IthComponent == 0 then j = j + 1
	       else(
		    I#i = I#i | {R'toR IthComponent};
		    G = drop(G, {j,j});
		    --G = G_(delete(j, toList(0..numgens source G - 1)));
		    );
	       );
	  i = i - 1
	  );
     
     S := QQ[symbol s];
     bf := 1_S; 
     scan(I, print);
     i = 0;
     while i < n do (
	  bf' := bFunction(ideal I#i, w, o);
	  bf' = (map(S, ring bf', matrix{{S_0-m#i}})) bf';
	  bf = bf * (bf' // gcd(bf, bf'));
	  i = i + 1;
	  );
     bf      
     );
///


TEST ///
debug Dmodules
W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
w = {1,1}
m = zeroize transpose matrix{{-Dy^2-Dy, -x*Dx-x*Dy-x+1, y*Dy^2+y*Dy-Dy-7},
     {-Dx+Dy, x-(5/4)*y, -1}, {0, -y, -4}, {-1, 0, -x}}
M = cokernel m
wt = {1,1}
shift = {0,0,0}

assert(listForm bFunction(M, wt, shift) == {({6}, 1), ({5}, 24), ({4}, 163), ({3}, 48), ({2}, - 1676), ({1}, 1440)})
///


