coverHomToProduct = (M,N) -> (
    --provides the correct map cover Hom(M,N) --> dual cover M ** cover N
    pM := presentation M;
    kMN :=gens ker map(dual source pM**N, 
	       dual target pM ** cover N,
	      dual pM**map(N,cover N, 1));
    gensMN' := (dual cover M ** gens N)*kMN;
    (kMN, kMN*(gens Hom(M,N)//gensMN'))
	)

coverHomFromProduct = (M,N) -> (
    --provides the correct map dual cover M ** cover N--> cover Hom(M,N)
    pM := presentation M;
    kMN :=gens ker map(dual source pM**N, 
	       dual target pM ** cover N,
	      dual pM**map(N,cover N, 1));
    gensMN' := (dual cover M ** gens N)*kMN;
    gensMN'//gens Hom(M,N))

compose1 = (M,N,P) -> (
    CN := cover N;
    (kMN, toProductMN) := coverHomToProduct(M,N);
    (kNP, toProductNP) := coverHomToProduct(N,P);
    (kMP, toProductMP) := coverHomToProduct(M,P);    
    fromProductMP := coverHomFromProduct(M,P);
    ev := reshape((ring N)^1, CN**(dual CN), id_CN);
    contractor := (dual cover M **ev**cover P)//kMP;
    map(Hom(M,P), Hom(M,N)**Hom(N,P),
    fromProductMP*contractor*(toProductMN**toProductNP)
       )
    )

compose = method()
compose(Module, Module, Module) := (M,N,P) ->(
    --defines the map Hom(M,N)**Hom(N,P) -> Hom(M,P)
    
    --the following just simplify notation:
    MN := Hom(M,N);
    NP := Hom(N,P);
    MP := Hom(M,P);
    CN := cover N;
    pN := presentation N;
    pM := presentation M;
    
    --next define a version of MN whose cover naturally maps to (dual cover M ** cover N), 
    --and similarly for NP, MP
    gensMN' := (dual cover M ** gens N)*gens ker map(dual source pM**N, dual target pM ** cover N,
	      dual pM**map(N,cover N, 1));
    MN' := subquotient(gensMN', relations MN);
    toMN' := map(MN', MN, gens MN'//gens MN);  
    
    gensNP' := (dual cover N ** gens P)*
              gens ker map(dual source pN**P, dual target pN ** cover P, dual pN**map(P,cover P, 1));
    NP' := subquotient(gensNP', relations NP);
    toNP' := map(NP', NP, gens NP'//gens NP);  

    gensMP' := (dual cover M ** gens P)*gens ker map(dual source pM**P, dual target pM ** cover P,
	      dual pM**map(P,cover P, 1));
    MP' := subquotient(gensMP', relations MP);
    toMP := map(MP, MP', gens MP'//gens MP); -- note that this goes the other way from toMN'
    
    --define the map the cover of the new version of MN into the tensor product, and similarly for NP and MP
    toCMStarCN := map (dual cover M**cover N, cover MN', 
	gens ker map(dual source pM**N, dual target pM ** cover N, dual pM**map(N,cover N, 1)));
    toCNStarCP := map (dual cover N**cover P, cover NP', 
	gens ker map(dual source pN**P, dual target pN ** cover P, dual pN**map(P,cover P, 1)));
    toCMStarCP := map (dual cover M**cover P, cover MP', 
	gens ker map(dual source pM**P, dual target pM ** cover P, dual pM**map(P,cover P, 1)));

    --now that we can map cover MN'** cover NP' --> (dual cover M)**cover N **(dual cover N) ** cover P
    --we can contract the middle terms
    ev := reshape((ring CN)^1, CN**(dual CN), id_CN);
    contractor := map(MP',MN'**NP', ((dual cover M **ev**cover P)*(toCMStarCN**toCNStarCP))//toCMStarCP);
    toMP*(contractor)*(toMN'**toNP')
    )

end

TEST///
restart
load "compose.m2"
R=QQ[x,y]
M=image vars R ++ R^2
f = compose(M,M,M);
f1 = compose1(M,M,M);
f == f1 -- false!!
H = Hom(M,M);

scan(numgens H,i->(
	scan(numgens H, j->(
g = H_{i};
g1 = H_{j};
h = homomorphism g;
h1 = homomorphism g1;
if (homomorphism (f * (g ** g1)) != h1 * h) then error();
))))

-- compose1, in the following test fails as of 5/25, 12:30
scan(numgens H,i->(
	scan(numgens H, j->(
g = H_{i};
g1 = H_{j};
h = homomorphism g;
h1 = homomorphism g1;
if (homomorphism (f1 * (g ** g1)) != h1 * h) then error();
))))


///

TEST///
S = ZZ/101[a,b,c]
A = matrix"a,b,c;b,c,a" 
B = matrix"a,b;b,c"
N = subquotient(A,B)
assert( (minimalPresentation compose1(N,N,N)) === 
    map(cokernel map((S)^1,(S)^{{-2}},{{b^2-a*c}}),
		cokernel map((S)^1,(S)^{{-2}},
			{{b^2-a*c}}),{{1}}) );
 ///
restart
load "compose.m2"
R=ZZ/101[x_0..x_4]
F = res coker vars R
M = apply(length F, i-> image F.dd_i);
elapsedTime c = compose(M_1, M_2, M_3);
elapsedTime c1 = compose1(M_1, M_2, M_3);
c != c1
///


