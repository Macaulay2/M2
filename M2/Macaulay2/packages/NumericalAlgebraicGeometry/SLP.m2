-- this is the OLD way of building SLPs used in the old "track"
-- "trackHomotopy" relies on SLPexpressions.m2 

------------ preSLPs ------------------------------------------------------------------------
-- preSLP = (constants, program, output_description)
--   constants = list of elements in CC
--   program = node, node, ...
--     node = {binary_operation, a, b}
--         or {multi_operation, n, a_1, ... , a_m(n)}   -- e.g., m(n) = n^2 for det        
--         or {copy, a}                                 -- copies node n    
--       a,b,ai are 
--          negative integers (relative position) 
--          or const => i (refers to i-th constant)
-- 	    or in => i (refers to i-th input) 
--       binary_operation = {sum, product}
--       multi_operation = {msum, mproduct, det} 
--   output_description = Matrix over ZZ (each entry refers to a node)

libPREFIX = "/tmp/slpFN.";
slpCOMPILED = 100;
slpPREDICTOR = 101;
slpCORRECTOR = 102;
slpEND = 0;
slpCOPY = 1; --"COPY"; -- node positions for slpCOPY commands are absolute
slpMULTIsum = 2; --"MULTIsum";
slpPRODUCT = 3; --"PRODUCT";
slpDET = 4; --"DET";


CONST := symbol CONST;
INPUT := symbol INPUT;
protect CONST
protect INPUT

-- types of predictors
predRUNGEKUTTA = 1;
predTANGENT = 2;
predEULER = 3;
predPROJECTIVENEWTON = 4;

-- Xn are inputs
-- Cn are constants
-- Gn are "gates"
-- output is listed in the end
printSLP = method()
printSLP (List,List,Matrix) := (cc,pp,o) -> (
    scan(#cc,i-><<"C"<<i<<" = "<<cc#i<<endl);
    scan(#pp,i->(
	    p := pp#i;
	    <<"G"<<i<<" = ";
	    (op,ops) := 
	    if first p === slpPRODUCT then (" * ",drop(p,1))
	    else if first p === slpMULTIsum then (" + ",drop(p,2))
	    else error "unknown gate";
	    scan(between(op,apply(ops,
			o->if instance(o,Option) then (
			    if o#0 === CONST then "C" else "X"
			    )|toString o#1 else "G"|toString(i+o)
			)),x-><<x);
	    << endl;
	    ));
    << "output:" << endl;
    scan(flatten entries o, x-> << "G" << x <<endl); 
    )

shiftConstsSLP = method(TypicalValue=>List);
shiftConstsSLP (List,ZZ) := (slp,shift) -> apply(slp, 
     n->apply(n, b->
	  if class b === Option and b#0 === CONST 
     	  then CONST=>shift+b#1 
     	  else b
	  )
     );

poly2preSLP = method(TypicalValue=>Sequence)
poly2preSLP RingElement :=  g -> (
     prog := {}; -- our SLP
     R := ring g;
     const := coefficient(1_R,g);
     finalMULTIsum := {}; -- list of nodes for final multisum
     constants := if const == 0 then {} else ( finalMULTIsum = finalMULTIsum | {CONST=>0}; {const} );
     f := g - const;
     scan(numgens R, i->(
	       fnox := sum(select(listForm f,ec->(first ec)#i==0), (e,c)->c*R_e); -- fnox := f%R_i;
	       if fnox == 0 then fnox = 0_R;
	       fx := f - fnox;
	       if fx != 0 then (
		    fxOverRi := --fx//R_i
		    sum(listForm fx, (e,c)->c*R_(take(e,i)|{e#i-1}|drop(e,i+1)));
		    if fxOverRi == 0 then fxOverRi = 0_R;      
	       	    (constfx, progfx, outfx) := poly2preSLP(
			 fxOverRi
			 );
	       	    -- shift constant nodes positions
	       	    prog = prog | shiftConstsSLP(progfx, #constants); 
	       	    constants = constants | constfx;
	       	    -- multiply by x=R_i
	       	    prog = prog | {{slpPRODUCT, INPUT=>i, -1}};
	       	    finalMULTIsum = finalMULTIsum | {#prog-1};
		    );	       
	       f = fnox;
	       )); 
     curPos := #prog;
     if #finalMULTIsum === 1 then (
       	  if finalMULTIsum#0 === curPos-1  -- if trivial 
       	  then null -- do nothing
       	  else if class finalMULTIsum#0 === Option and finalMULTIsum#0#0 === CONST then 
	  prog = prog | {{slpCOPY, finalMULTIsum#0}}
	  else error "unknown trivial MULTIsum"; 
	  )   
     else prog = prog | {{slpMULTIsum, #finalMULTIsum} | apply(finalMULTIsum, 
	       p->if class p === Option then p 
	       else p - curPos -- add a relative position
	       )};    
      (constants, prog, matrix{{#prog-1}})
     )

matrix2preSLP = method() -- make a preSLP evaluating the matrix with polynomial entries
matrix2preSLP Matrix :=  M -> stackPreSLPs apply(entries M, row -> concatPreSLPs apply(row, a->poly2preSLP a))

detPreSLP = method() -- determinant of a preSLP 
detPreSLP (List,List,Matrix) :=  (c,p,M) -> (
    n := numgens target M;
    assert (n == numgens source M);
    (c,p|{{slpDET, n}|apply(flatten entries M,i->i-#p)}, matrix{{#p}})    
    )

shift'constants = method() -- outputs the program obtained from p by shifting constants by s
shift'constants (List, ZZ) := (p, s) -> apply(p, a->
    apply(a, b->
	if class b === Option and b#0 === CONST 
	then b#0=>b#1+s -- shift the constants
	else b
	) 
    )
concatPreSLPs = method() -- concatenate pre-slps 
-- ( if 2 slp's output matrices are A and B, their concatenation is A|B )
concatPreSLPs List := S -> (
     c := {};
     p := {}; 
     o := null;
     scan(S, s->(
	       if o === null then o = s#2
	       else -- shift output by the current length of the program 
	            o = o | (s#2 + map(ZZ^(numgens target s#2), ZZ^(numgens source s#2), (i,j)->#p));  
	       p = p | shift'constants(s#1, #c);
	       c = c | s#0;
	       ));     
     (c,p,o)
     );

stackPreSLPs = method() -- stacks pre-slps (A||B)
stackPreSLPs List := S -> (
     c := {};
     p := {}; 
     o := null;
     scan(S, s->(
	       if o === null then o = s#2
	       else -- shift output by the current length of the program 
	            o = o || (s#2 + map(ZZ^(numgens target s#2), ZZ^(numgens source s#2), (i,j)->#p));  
	       p = p | shift'constants(s#1, #c);
	       c = c | s#0;
	       ));     
     (c,p,o)
     );

addPreSLPs = method() -- adds pre-slps 
-- ( output matrix dimensions should match)
addPreSLPs List := S -> (
     c := {};
     p := {}; 
     nRows := null;
     nCols := null;
     o'summands := apply(S, s->(
	     (c',p',o') := s;
	     if nRows === null then (nRows = numgens target o'; nCols = numgens source o')
	     else (
		 -- check if the dimensions of the output matrix are the same
		 assert (nRows == numgens target o' and nCols == numgens source o');
		 -- shift output by the current length of the program 
		 o' = o' + map(ZZ^nRows, ZZ^nCols, (i,j)->#p)
		 );  
	     p = p | shift'constants(p', #c);
	     c = c | c';
	     o'
	     ));
     o := map(ZZ^nRows, ZZ^nCols, (i,j)->(
	     p = p | {{slpMULTIsum, #o'summands}|apply(o'summands,a->a_(i,j)-#p)};
	     #p-1 -- position of just added node
	     ));
     (c,p,o)
     );
     
evaluatePreSLP = method() -- evaluates preSLP S at v
evaluatePreSLP (Sequence,List) := (S,v)-> (
     -- sign of a permutation 
     sign := p -> (
	 N := #p;
	 p = new MutableList from p;
	 s := 1;
	 I := 0;
	 while I < N-1 do (
	     J := p#I;
	     if J == I then I = I+1
	     else (s = -s; p#I = p#J; p#J = J)
	     );
	 s
	 );
     det' := if isPolynomialRing ring first v 
             then det 
             else M -> (
		 (p,L,U) := LUdecomposition M;
		 sign p * product(numgens target M, i->L_(i,i)*U_(i,i))
		 ); -- this is a hack!!! lapack det needs to be wrapped
     val := {};
     constants := S#0;
     slp := S#1;
     scan(#slp, i->(
	   n := slp#i;
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 === CONST then val = val | {constants#(n#1#1)}
		else error "unknown node type"; 
		)  
	   else if k === slpMULTIsum then (
		val = val | { sum(2..1+n#1, 
			  j->if class n#j === Option and n#j#0 === CONST then constants#(n#j#1)
    		       	  else if class n#j === Option and n#j#0 === INPUT then v#(n#j#1)
			  else if class n#j === ZZ then val#(i+n#j)
			  else error "unknown node type" 
			  )
		     }
	   	)
	   else if k === slpPRODUCT then (
		val = val | { 
		     product(1..2, j->(
          		       if class n#j === Option and n#j#0 === CONST then constants#(n#j#1)
			       else if class n#j === Option and n#j#0 === INPUT then v#(n#j#1)
			       else if class n#j === ZZ then val#(i+n#j)
			       else error "unknown node type" 
			       ))
		     }
		)
	    else if k === slpDET then (
		N := n#1; -- NxN matrix
		assert(N>0);
		M := matrix apply(N, a->apply(N, b->(
			  ab := 2+a*N+b;
			  if class n#ab === Option and n#ab#0 === CONST then constants#(n#ab#1)
			  else if class n#ab === ZZ then val#(i+n#ab)
			  else error "unknown node type" 
			  )));
		val = val | { det' M } 
	   	)

	   else error "unknown SLP node key";   
	   ));
 matrix apply(entries S#2, r->apply(r, e->val#e))
 )

transposePreSLP = method() 
transposePreSLP(List,List,Matrix) := (C,slp,M) -> (C, slp, transpose M)

jacobianPreSLP = method() -- finds jacobian of S with respect to inputs listed in L
jacobianPreSLP (Sequence, List) := (S,L) -> (  
--     constants := S#0 | {0_CC,1_CC};
     constants := S#0 | {0,1};
     slp := S#1;
     outMatrix := S#2;
     -- create "zero node"
     zeroNode := #slp;
     slp = slp | {{slpCOPY, CONST=>#constants-2}};
     if numgens target outMatrix != 1 then error "preSLP: row vector expected";
     diffNodeVar := (ni,vj)->( 
	  -- augments slp with nodes necessary to differentiate node n#ni w.r.t. input vj 
	  -- output: the (absolute) position of the result in slp (or -1 "zero")
	  n := slp#ni;
	  k := first n;
	  if k === slpCOPY then (
	       if class n#1 === Option and n#1#0 === CONST 
	       then return -1 -- "zero"
	       else error "unknown node type"; 
	       )  
	  else if k === slpMULTIsum then (
	       pos := toList apply(2..1+n#1, j->
		    if class n#j === Option and n#j#0 === CONST then -1 -- "zero"
		    else if class n#j === Option and n#j#0 === INPUT then (
			if n#j#1 == vj then ( 
     	       	    	      	   slp = slp | {{slpCOPY,CONST=> #constants-1}}; -- "one"
				   (#slp-1)
				   )
			else -1 -- "zero"
			)
		    else if class n#j === ZZ then diffNodeVar(ni+n#j,vj)
		    else error "unknown node type" 
		    );
	       summands := select(pos, p->p!=-1);
	       if #summands == 0 then return -1 -- "zero"
	       else if #summands == 1 then return first summands
	       else (
		    slp = slp | {{slpMULTIsum,#summands} | apply(summands, i->i-#slp)};
		    return (#slp-1);
		    )
	       )
	   else if k === slpPRODUCT then (
	       pos = toList apply(1..2, j->(
			 if class n#j === Option and n#j#0 === CONST then -1 -- "zero"
			 else if class n#j === Option and n#j#0 === INPUT then (
			      if n#j#1 == vj then ( 
     	       	    	      	   slp = slp | {{slpPRODUCT}|
					toList apply(1..2, t->if t==j 
					     then CONST=> #constants-1 -- "one"
					     else (
						  if class n#t === ZZ then ni+n#t-#slp
					     	  else n#t
						  )
					     )};
				   (#slp-1)
				   )
			      else -1 -- "zero"
			      )
			 else if class n#j === ZZ then (
			      p:=diffNodeVar(ni+n#j,vj);
			      if p==-1 then -1 -- "zero"
			      else (
			      	   slp = slp | {
				   	{slpPRODUCT}|
				   	toList apply(1..2, t->
					     if t==j 
					     then p-#slp 
					     else (
					     	  if class n#t === ZZ then ni+n#t-#slp
					     	  else n#t
					     	  )
					     )};
			      	   (#slp-1)
				   )
			      )
			 else error "unknown node type" 
			 ));
	       summands = select(pos, p->p!=-1);
	       if #summands == 0 then return -1 -- "zero"
	       else if #summands == 1 then return first summands
	       else (
		    slp = slp | {{slpMULTIsum,#summands} | apply(summands, p->p-#slp)};
		    return (#slp-1);
		    )
	       )
	   else if k === slpDET then (
	       e := apply(drop(n,2), a->ni+a); -- matrix entries (global node references)
	       pos = toList apply(2..(#e+1), j->(
		       if class n#j === ZZ then diffNodeVar(ni+n#j,vj)
		       else error "unknown node type" 
		       ));
	       N := n#1; 
	       slp = slp | apply(N, row->{slpDET,N}|
		       -- references: global->local
		       apply(
			   take(e,N*row)|take(pos,{N*row,N*(row+1)-1})|drop(e,N*(row+1)),
			   a->(if a == -1 then zeroNode else a)-(#slp+row)
			   )
		       ) | {
		       {slpMULTIsum,N} | toList((-N)..(-1))
		       };
	       return (#slp-1)
	       )
	   else error "unknown SLP node key";   
	  ); 
     newOut := transpose matrix apply(first entries outMatrix, ni->apply(L, vj->diffNodeVar(ni,vj)));
     ( constants, slp,  
	 matrix apply(entries newOut, row->apply(row, i->if i==-1 then zeroNode else i)) ) 
     )

prunePreSLP = method() -- finds jacobian of S with respect to inputs listed in L
prunePreSLP (List,List,Matrix) := (C,slp,outMatrix) -> (
     -- look for duplicate constants
     newC := {};
     remap := apply(#C, i->(
	       p := position(newC,c->C#i==c);
	       if p =!= null 
	       then p
	       else ( 
		    newC = newC | {C#i};
		    #newC - 1
		    )
	       ));
     newslp := apply(slp, n->(
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 === CONST then {n#0,CONST=>remap#(n#1#1)}
		else error "unknown node type"
		)  
	   else if k === slpMULTIsum then (
		{n#0,n#1} | toList apply(2..1+n#1, 
		     j -> if class n#j === Option and n#j#0 === CONST 
		     then CONST=>remap#(n#j#1) 
		     else n#j
		     )
	   	)
	   else if k === slpPRODUCT then (
		{n#0} | toList apply(1..2, j->
		     if class n#j === Option and n#j#0 === CONST 
		     then CONST => remap#(n#j#1)
		     else n#j
		     )
		)
	   else if k === slpDET then n
	   else error "unknown SLP node key"   
	   ));
     	  (newC,newslp,outMatrix)
     )

-- create a file <fn>.cpp with C++ code for the function named fn that evaluates a preSLP S
-- format:
--   void fn(const complex* a, complex* b)
-- here: input array a
--       output array b 
preSLPtoCPP = method(TypicalValue=>Nothing, Options=>{System=>MacOsX})
preSLPtoCPP (Sequence,String) := o-> (S,filename)-> (
     constants := S#0;
     slp := S#1;
     fn := "slpFN"; -- function name
     f := openOut(filename);
     f << ///
#include<stdio.h>
#include<math.h>

class complex
{
private:
  double real;  // Real Part
  double imag;      //  Imaginary Part                                                                                                       
public:
  complex();
  complex(double,double);
  complex(const complex&);
  //complex(M2_CCC);
  complex operator +(complex);
  complex operator -(complex);
  complex operator *(complex);
  complex operator /(complex);
  complex getconjugate();
  complex getreciprocal();
  double getreal();
  double getimaginary();
  bool operator ==(complex);
  void operator =(complex);
  void sprint(char*);
};

complex::complex() { }
complex::complex(double r, double im)
{
  real=r;
  imag=im;
}
 
//                                 COPY CONSTRUCTOR
complex::complex(const complex &c)
{
  this->real=c.real;
  this->imag=c.imag;
}
 
void complex::operator =(complex c)
{
  real=c.real;
  imag=c.imag;
}
 
 
complex complex::operator +(complex c)
{
  complex tmp;
  tmp.real=this->real+c.real;
  tmp.imag=this->imag+c.imag;
  return tmp;
}
 
complex complex::operator -(complex c)
{
  complex tmp;
  tmp.real=this->real - c.real;
  tmp.imag=this->imag - c.imag;
  return tmp;
}
 
complex complex::operator *(complex c)
{
  complex tmp;
  tmp.real=(real*c.real)-(imag*c.imag);
  tmp.imag=(real*c.imag)+(imag*c.real);
  return tmp;
}
 
complex complex::operator /(complex c)
{
  double div=(c.real*c.real) + (c.imag*c.imag);
  complex tmp;
  tmp.real=(real*c.real)+(imag*c.imag);
  tmp.real/=div;
  tmp.imag=(imag*c.real)-(real*c.imag);
  tmp.imag/=div;
  return tmp;
}
complex complex::getconjugate()
{
  complex tmp;
  tmp.real=this->real;
  tmp.imag=this->imag * -1;
  return tmp;
}
 
complex complex::getreciprocal()
{
  complex t;
  t.real=real;
  t.imag=imag * -1;
  double div;
  div=(real*real)+(imag*imag);
  t.real/=div;
  t.imag/=div;
  return t;
}
 
double complex::getreal()
{
  return real;
}
 
double complex::getimaginary()
{
  return imag;
}
 
bool complex::operator ==(complex c)
{
  return (real==c.real)&&(imag==c.imag) ? 1 : 0;
}

void complex::sprint(char* s)
{
  sprintf(s, "(%lf) + i*(%lf)", real, imag);
}
/// << endl;
     if o.System === MacOsX then f << ///#define EXPORT __attribute__((visibility("default")))/// <<endl;
     if o.System === MacOsX then f << /// extern "C" EXPORT ///;
     f << "void " << fn << "(complex* a, complex* b)" << endl  
     << "{" << endl 
     << "  complex ii(0,1);" << endl
     << "  complex c[" << #constants << "]; " << endl
     << "  complex node[" << #slp << "];" << endl
     << "  complex* n = node;" << endl; -- current node      
     -- hardcode the constants
     scan(#constants, i-> f << "c[" << i << "] = " << "complex(" <<
	  realPart constants#i << "," << imaginaryPart constants#i << ");" << endl);
     scan(#slp, i->(
	   n := slp#i;
	   k := first n;
	   f << "  *n = ";
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 === CONST 
		then f << "c[" << n#1#1 << "];" 
		else error "unknown node type"; 
		)  
	   else if k === slpMULTIsum then (
		scan(2..1+n#1, j->(
			  if class n#j === Option and n#j#0 === CONST 
			  then f << "c[" << n#j#1 << "]"
			  else if class n#j === ZZ 
			  then f << "node[" << i+n#j << "]"
		     	  else error "unknown node type";
			  if j < 1+n#1 then f << " + ";
		     	  ));
		f << ";";
		)
	   else if k === slpPRODUCT then (
		scan(1..2, j->(
			  if class n#j === Option then (
			       if n#j#0 === INPUT 
			       then f << "a[" << n#j#1 << "]"
			       else if n#j#0 === CONST
			       then f << "c[" << n#j#1 << "]"
			       else error "unknown node type"
			       ) 
			  else if class n#j === ZZ 
			  then f << "node[" << i+n#j << "]"
			  else error "unknown node type";
			  if j < 2 then f << " * "; 
			  ));
		f << ";";
	   	)
	   else error "unknown SLP node key";   
	   f << " n++;" << endl
	   ));
      f << "  // creating output" << endl << "  n = b;" << endl;
      scan(flatten entries S#2, e->(
	   	f << "  *(n++) = node[" << e << "];" << endl 		
		));
      f << "}" << endl << close; 	              
      )

-- create a file <fn>.c with C code for the function named fn that evaluates a preSLP S
-- format:
--   void fn(const complex* a, complex* b)
-- here: input array a
--       output array b 
preSLPtoC = method(TypicalValue=>Nothing, Options=>{System=>MacOsX})
preSLPtoC (Sequence,String) := o-> (S,filename)-> (
     constants := S#0;
     slp := S#1;
     fn := "slpFN"; -- function name
     f := openOut(filename);
     f << ///
#include<stdio.h>
#include<math.h>

typedef struct 
{
  double re;  
  double im;  
} complex;

inline void init_complex(complex* c, double r, double i) __attribute__((always_inline));
void init_complex(complex* c, double r, double i)
{ c->re = r; c->im = i; }

/* #define init_complex(c,r,i) { c->re = r; c->im = i; } */

/* register */ 
static double r_re, r_im; 

inline set_r(complex c) __attribute__((always_inline));
inline set_r(complex c) 
{ r_re = c.re; r_im = c.im; }

/* #define set_r(c) { r_re = c.re; r_im = c.im; } */

inline copy_r_to(complex* c) __attribute__((always_inline));
inline copy_r_to(complex* c) 
{ c->re = r_re; c->im = r_im; }

/* #define copy_r_to(c) { c->re = r_re; c->im = r_im; } */

inline add(complex c) __attribute__((always_inline));
inline add(complex c)
{ r_re += c.re; r_im += c.im; }

/* #define add(c) { r_re += c.re; r_im += c.im; } */

inline mul(complex c) __attribute__((always_inline));
inline mul(complex c)
{ 
  double t_re = r_re*c.re - r_im*c.im;
  r_im = r_re*c.im + r_im*c.re;
  r_re = t_re;
}

/*#define mul(c) { double t_re = r_re*c.re - r_im*c.im; r_im = r_re*c.im + r_im*c.re; r_re = t_re; } */

/// << endl;
     -- if o.System === MacOsX then f << ///#define EXPORT __attribute__((visibility("default")))/// <<endl;
     -- if o.System === MacOsX then f << /// extern "C" EXPORT ///;
     f << "void " << fn << "(complex* a, complex* b)" << endl  
     << "{" << endl 
     << "  complex c[" << #constants << "]; " << endl
     << "  complex node[" << #slp << "];" << endl
     << "  complex* cp = c;" << endl
     << "  complex* n = node;" << endl; -- current node      
     -- hardcode the constants
     scan(#constants, i-> f <<  "init_complex(cp," <<
	  realPart constants#i << "," << imaginaryPart constants#i << "); cp++;" << endl);
     scan(#slp, i->(
	   n := slp#i;
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 === CONST 
		then f << "  *n = c[" << n#1#1 << "];" 
		else error "unknown node type"; 
		)  
	   else if k === slpMULTIsum then (
		scan(2..1+n#1, j->(
			  if class n#j === Option and n#j#0 === CONST 
			  then f << (if j>2 then "add" else "set_r") << "(c[" << n#j#1 << "]); "
			  else if class n#j === ZZ 
			  then f << (if j>2 then "add" else "set_r") << "(node[" << i+n#j << "]); "
		     	  else error "unknown node type";
		     	  ));
		f << "copy_r_to(n);";
		)
	   else if k === slpPRODUCT then (
		scan(1..2, j->(
			  if class n#j === Option then (
			       if n#j#0 === INPUT 
			       then f << (if j>1 then "mul" else "set_r") << "(a[" << n#j#1 << "]); "
			       else if n#j#0 === CONST
			       then f << (if j>1 then "mul" else "set_r") << "(c[" << n#j#1 << "]); "
			       else error "unknown node type"
			       ) 
			  else if class n#j === ZZ 
			  then f << (if j>1 then "mul" else "set_r") << "(node[" << i+n#j << "]); "
			  else error "unknown node type";
			  ));
		f << "copy_r_to(n);";
	   	)
	   else error "unknown SLP node key";   
	   f << " n++;" << endl
	   ));
      f << "  // creating output" << endl << "  n = b;" << endl;
      scan(flatten entries S#2, e->(
	   	f << "  *(n++) = node[" << e << "];" << endl 		
		));
      f << "}" << endl << close; 	              
      )

///
loadPackage "NumericalAlgebraicGeometry"; debug NumericalAlgebraicGeometry;
R = CC[x,y,z]
g = 3*y^2+(2.1+ii)*x
--g = 1 + 2*x^2 + 3*x*y^2 + 4*z^2
--g = random(3,R)
pre = poly2preSLP g
g3 = concatPreSLPs {pre,pre,pre}
g6 = stackPreSLPs {g3,g3}
eg = evaluatePreSLP(g6,gens R)
eg_(1,1) == g
--preSLPtoCPP(g6,"slpFN")
debug Core
(constMAT, prog) = preSLPinterpretedSLP(3,g6)
rSLP = rawSLP(raw constMAT, prog)
K = CC_53
params = matrix{{ii_K,1_K,-1_K}}; 
result = rawEvaluateSLP(rSLP, raw params)
sub(g, params) - (map(K,result))_(0,0)

///
----------------- SLPs -----------------------------------------------------
-- SLP = (constants, array of ints)
-- constants = one-row matrix
-- array of ints = 
--0  #constants
--1  #inputs 
--2  #rows in output
--3  #columns in output
--4  type of program (slpCOMPILED,slpINTERPRETED,slpPREDICTOR) 
--   OR the beginning of slp operations list
--
--   if COMPILED then {
--5     integer -> used to create the dynamic library filename
--   }
--   else if PREDICTOR then {
--5     predictor type  
--6+    list of catalog numbers of SLPs for Hx,Ht,H
--   } else {
--     list of commands
--     output matrix entries (numbers of nodes) 
--   }
  
preSLPinterpretedSLP = method()
preSLPinterpretedSLP (ZZ,Sequence) := (nIns,S) -> (
-- makes input for rawSLP from pre-slp
     consts := S#0;
     slp := S#1;   
     o := S#2;
     SLPcounter = SLPcounter + 1;
     curNode := #consts+nIns;
     p := {};
     scan(slp, n->(
	   k := first n;
	   if k === slpCOPY then (
	   	if class n#1 === Option and n#1#0 === CONST then p = p | {slpCOPY} | {n#1#1} 
		else error "unknown node type" 
		)  
	   else if k === slpMULTIsum then (
		p = p | {slpMULTIsum, n#1} | toList apply(2..1+n#1, 
		     j->if class n#j === Option and n#j#0 === CONST then n#j#1
		     else if class n#j === Option and n#j#0 === INPUT then #consts + n#j#1
		     else if class n#j === ZZ then curNode+n#j
		     else error "unknown node type" 
		     )
	   	)
	   else if k === slpPRODUCT then (
		p = p | {slpPRODUCT} | toList apply(1..2, j->(
          		       if class n#j === Option then (
				    if n#j#0 === INPUT then #consts + n#j#1
				    else if n#j#0 === CONST then n#j#1
				    else error "unknown node type"
				    ) 
			       else if class n#j === ZZ then curNode+n#j
			       else error "unknown node type" 
			       ))
		)
	   else error "unknown SLP node key";   
	   curNode = curNode + 1;
	   ));
     p = {#consts,nIns,numgens target o, numgens source o} | p | {slpEND}
	 | apply(flatten entries o, e->e+#consts+nIns); 
     (map(CC^1,CC^(#consts), {consts}), p)
     )

preSLPcompiledSLP = method(TypicalValue=>Nothing, Options=>{System=>MacOsX, Language=>LanguageC})
preSLPcompiledSLP (ZZ,Sequence) := o -> (nIns,S) -> (
-- makes input for rawSLP from pre-slp
     consts := S#0;
     slp := S#1;   
     out := S#2;
     fname := SLPcounter; SLPcounter = SLPcounter + 1; -- this gives libraries distinct names 
                                                       -- the name of the function stays the same, should it change?
     curNode := #consts+nIns;
     p := {#consts,nIns,numgens target out, numgens source out} | {slpCOMPILED}
          | { fname }; -- "lib_name" 
     cppName := libPREFIX | toString fname | if o.Language === LanguageCPP then ".cpp" else ".c";
     libName := libPREFIX | toString fname | if o.System === Linux then ".so" else  ".dylib";
     (if o.Language === LanguageCPP then preSLPtoCPP else preSLPtoC) (S, cppName, System=>o.System);
     compileCommand := if o.System === Linux then "gcc -shared -Wl,-soname," | libName | " -o " | libName | " " | cppName | " -lc -fPIC"
     else if o.System === MacOsX and version#"pointer size" === 8 then "g++ -m64 -dynamiclib -O2 -o " | libName | " " | cppName
     else if o.System === MacOsX then (
     	  "gcc -dynamiclib -O1 -o " | libName | " " | cppName
	  )
     else error "unknown OS";
     print compileCommand;
     run compileCommand;      
     (map(CC^1,CC^(#consts), {consts}), p)
     )

----------------------------------------------------------------------------------------------------
-- SLPexpressions tests

-- returns (consts,program), modifies pos
appendToProgram = method()
appendToProgram (Gate,List,List,MutableHashTable) := (g,consts,program,pos)->(    
    )
appendToProgram (InputGate,List,List,MutableHashTable) := (g,consts,program,pos) -> (
    if pos#?g then return (consts,program); -- do nothing
    if isConstant g then (
	pos#g = #consts;
	(append(consts,g.Name),program)
	) else (
	if not pos#?g then error "a variable is not specified as input";
	(consts,program)
	)
    )
appendToProgram (SumGate,List,List,MutableHashTable) := (g,consts,program,pos)->( 
    if pos#?g then return (consts,program); -- do nothing
    scan(g.Inputs,f->(consts,program)=appendToProgram(f,consts,program,pos));
    abs'pos := #program;
    pos#g = abs'pos;
    (
	consts,
    	append(program, {slpMULTIsum} | {#g.Inputs} | apply(g.Inputs,f->
	    if instance(f,InputGate) then (
	    	if isConstant f then CONST=>pos#f
		else INPUT=>pos#f
		)
	    else pos#f-abs'pos))
        )
    )
appendToProgram (ProductGate,List,List,MutableHashTable) := (g,consts,program,pos)->(    
    if pos#?g then return (consts,program); -- do nothing
    if #g.Inputs!=2 then error "cannot convert products of more than 2 numbers to preSLP";
    scan(g.Inputs,f->(consts,program)=appendToProgram(f,consts,program,pos));
    abs'pos := #program;
    pos#g = abs'pos;
    (
	consts,
    	append(program, {slpPRODUCT} | apply(g.Inputs,f->
	    if instance(f,InputGate) then (
	    	if isConstant f then CONST=>pos#f
		else INPUT=>pos#f
		)
	    else pos#f-abs'pos))
        )
    )

-- assembles a preSLP (see NumericalAlgebraicGeometry/SLP.m2) 
-- that takes a list of InputGates and a list of Gates that produce the output
toPreSLP = method()
toPreSLP (List,List) := (inputs,outputs) -> (
    consts := {};
    program := {};
    pos := new MutableHashTable from apply(#inputs,i->inputs#i=>i);
    scan(outputs,o->(consts,program)=appendToProgram(o,consts,program,pos));
    (consts, program, matrix{outputs/(o->pos#o)})
    )  

TEST ///
debug needsPackage "NumericalAlgebraicGeometry"
debug needsPackage "SLPexpressions"
-- evaluate toPreSLP == compress 

X = inputGate symbol X
Y = inputGate symbol Y
E = inputGate 2
oneGate = inputGate 1
F = product{E*(X*X+E*Y)+oneGate, oneGate}
G = (sub(sub(matrix{{F}},X=>X+Y),Y=>X*Y))_(0,0) 
R = CC[x,y] 

output = {F, compress diff(X,F), G}
preSLP = toPreSLP({X,Y},output)
out'eval = evaluatePreSLP(preSLP, gens R)
out'comp = matrix applyTable(entries compress sub(matrix{output},{X=>inputGate x,Y=>inputGate y}), g->g.Name)
assert(out'eval == out'comp)
printSLP preSLP
printAsSLP ({X,Y},output)
///

