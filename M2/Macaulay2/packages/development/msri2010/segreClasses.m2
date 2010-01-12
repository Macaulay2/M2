segreClass = method();
segreClass (Ring) := (R) -> (
-- R is a bigraded tower ring R = S[e_0..e_m], with gens e_i in bidegrees (di,1) (di pos or neg ints)
-- S is a bigraded polynomial ring with generators in degrees (1,0)
-- the script computes the Segre classes of the cone represented by Bi-Proj R over Proj S = P^n.
-- If q: C -> Pn is the structure map of the cone, with relative dimension rD,
-- then the Segre classes are
-- s^i(C) = q_*(c_1(OO_C(1))^(i+rD)\cap [C]).

-- The idea is to first twist the cone by a sufficiently positive line bundle so O(1) is generated
-- by global sections, and then to push forward intersections of more and more general sections.
-- The pushforward of a given class is computed by restricting to a general plane section of the
-- image variety, and then computing the dimension of the module of 
-- global sections of the sheaf represented by the 1-dimensional
-- module that is the pushforward of the structure sheaf of the fiber.

--The Segre classses of the cone and of its twist are related by a simple formula
--because OO_{C(L)}(1) = OO_C(1)\otimes L, and thus 
--q_*(c_1(OO_{C(L)}(1))^i+rD) = sum_j(c_1(L)^j q_*(c_1(OO_C(1))^(i+rD-j)).
--This is an invertible transformation that is undone by the matrix "inv" at the end of this script.

-- CAVEATS: We assume that "random" is "general". To fix that, one should put in tests
-- and either choose again if the choices are bad or else return an error.
  dimR := dim R;
  Rflat := first flattenRing R; -- we'll make our computations here
  irrRflat := gens R/(i->sub(i,Rflat)); -- the irrelevant ideal
  degListR := degree \ gens R;
  d := max(0, max(degListR/(dl -> dl#0))); -- tells how much we have to twist

  S := coefficientRing R;
  dimS := dim S;
  dimSbar := dim ker map(R,S);
  rD := dimR - dimSbar -1; -- relative  dimension of the cone and its image

-- Form a list of random global sections of O(1) on the twisted cone
  RflatList := flatten entries random(Rflat^1, Rflat^{(dimR):{-d,-1}}); 
-- and compute the ideals they generate, saturated   
  LRflat := apply(rD-1..dimR-3, i -> if i<0 then ideal(0_Rflat) else
         saturate(ideal RflatList_{0..i}, ideal irrRflat));

-- Form the images in P^n
LS := apply(LRflat, i->
     if i==ideal(1_Rflat) then ideal(1_S) else -- this line should not be necessary!
     ker map(Rflat/i, S)
     );


-- Reduce each ideal in LS to dimension 0, saturating at the dimension 1 step
Slist := flatten entries random(S^1, S^{dimSbar: {-1,0}});
LSbar := apply(#LS, i->(
	  dimi := dim LS#i;
	  if dimi === 1 then I1 = saturate(LS#i, ideal vars S)
	  else if dimi<1 then I1 = ideal(S_1) 
	  else I1 = saturate(LS#i+ideal(Slist_{0..(dimi-2)}), ideal vars S);
	  I1+ideal(Slist_(dim LS#i -1))
	  ));
-- and take the fiber over it of the variety defined by the i-th ideal of LRflat
LRmods := apply(#LS, i -> Rflat^1/(LRflat#i+sub(LSbar#i, Rflat)));

--Calculate the Segre classes of the twisted cone
twistedSegre := apply(#LSbar, i->(
	  Mreg := regularity(LRmods#i, Weights=>{0,1});
	  sum(0..regularity(S^1/LSbar#i), j->if Mreg == -infinity then 0 else numcols basis({j,Mreg}, LRmods#i))));

-- Undo the effect of the twist (if any)
if d===0 then ans1 := twistedSegre else(
inv := inverse map(QQ^dimSbar, QQ^dimSbar, (i,j)->((binomial(rD+i, rD+j))*d^(i-j)));
ans1 = flatten entries(inv*(transpose matrix{twistedSegre})));

-- Pad with leading zeros if S -> R is not an inclusion
toList(dimS-#ans1:0)|ans1
)
end

restart
load "segreClasses.m2"

--example 0: Proj of the symmetric algebra of the trivial vector bundle
S = kk[x_0..x_2, Degrees => {3:{1,0}}];
R = S[e_0, e_1, Degrees => {2:{0,1}}, Join => false]
s0=segreClass R -- inverse of the Chern class of OO_{P2}++OO_{P2}

     
--example 1: Proj of the symmetric alg of the vector bundle OO_{P2}(-2)++OO_{P2}
S = kk[x_0..x_2, Degrees => {3:{1,0}}];
R = S[e_0, e_1, Degrees => {{0,1}, {2,1}}, Join => false]
s1=segreClass R -- inverse of the Chern class of OO_{P2}(2)++OO_{P2}

--example 2: Proj of the symmetric alg of the tangent bundle of P2
S = kk[x_0..x_2, Degrees => {3:{1,0}}];
R1 = S[e_0..e_2, Degrees => {3:{-1,1}}, Join => false]
R=R1/ideal(sum(3,i->x_i*e_i))
s2=segreClass R -- inverse of the Chern class of the cotangent bundle

--example 3: The blowup of a point in P2
S = kk[x_0..x_2, Degrees => {3:{1,0}}];
R1 = S[e_0..e_1, Degrees => {2:{1,1}}, Join => false]
J = minors(2,matrix{
	  {x_0,x_1},
	  {e_0,e_1}})
R=R1/J
s3=segreClass R 

--example 4: The blowup of a point in a P2 regarded as a plane in P3
S = kk[x_0..x_3, Degrees => {4:{1,0}}];
R1 = S[e_0..e_1, Degrees => {2:{1,1}}, Join => false]
J = minors(2,matrix{
	  {x_0,x_1},
	  {e_0,e_1}})
R=R1/(J+ideal(sub(x_3,R1)))
s4=segreClass R 


--Goal: figure out what the Segre classes mean for Rees algebras.


assert(s0=={1,0,0})
assert(s1=={1,-2,4})
assert(s2=={1,3,3})
assert(s3=={1,0,-1})
assert(s4=={0,1,0,-1}

--example 3A: The blowup of a point in P2

--THIS DOESN'T WORK YET BECAUSE OF THE WAY DEGREES ARE MADE IN ReesAlgebra.m2;
--need to add an option to that package so that when S has bidegrees of the right
--kind, R is formed with "the same sort" of bidegrees; something like Join=> false.

viewHelp ReesAlgebra
S = kk[x_0..x_2, Degrees => {3:{1,0}}]
R = reesAlgebra(ideal(x_0,x_1), x_0)
segreClass R 


--bug?
restart
kk = ZZ/101
S = kk[x]
R = S[y]
map(R,S)
map(R,S, sub(vars S, R))
ker map(R,S,{0_R})
--the following two lines each return the zero ideal;
--they should return the unit ideal.
ker map(R/ideal(1_R), S)
ker map(R/ideal(1_R), S, sub(vars S, R))

