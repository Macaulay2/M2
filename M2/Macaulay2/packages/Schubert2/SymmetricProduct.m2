needsPackage "Schubert2"
load "./BrillNoether.m2"


CxCd = (g,d) -> (
     ACxCd := QQ[x', theta', gamma, eta]/(
	  x'^(d+1),
	  theta'^(g+1), 
	  theta'^(d+1),
	  gamma^3,gamma*eta,
	  eta^2,
	  gamma^2+2*eta*theta',
	  theta'^g*gamma,
	  theta'^(g+1)
	  );
     --d-th symmetric power of a curve of genus g 
     ACd := QQ[x,theta]/(theta^(g+1), theta^(d+1), x^(d+1)); 
     Cd := abstractVariety(d,ACd);
     --Cd.TangentBundle = defined later
     integral ACd := f -> (
	  i := sum for a from 0 to min(d,g) list 
	            (g!/(g-a)!)*
		         coefficient(x^(d-a)*theta^a,f);
	  try lift(i,ZZ) else lift(i,QQ));
     use ACxCd;
     C := Curve g;

     CxCd := abstractVariety(d+1, ACxCd);
     CxCd.UniversalDivisor = d*eta+gamma+x';
     integral ACxCd := f -> (
	  i := sum for a from 0 to min(d,g) list 
	            (g!/(g-a)!)*
		         coefficient(x'^(d-a)*theta'^a*eta,f);
	  try lift(i,ZZ) else lift(i,QQ));
--      CxCd.TangentBundle = abstractSheaf(CxCd,
-- 	  Rank => dim CxCd, 
-- 	  ChernClass => 1 + (2-2*g) * eta);
     CxCd.PoincareBundle = abstractSheaf(CxCd,
	  Rank => 1,
	  ChernClass => 1 + d*eta + gamma+x');
     pf2 := method();
     pi1 := new AbstractVarietyMap from {
	  symbol source => CxCd,
	  symbol target => C
	  -- PullBack => 
	  -- PushForward => 
	  -- SectionClass => 
	  };
     pi2 := new AbstractVarietyMap from {
	  symbol source => CxCd,
	  symbol target => Cd,
	  TangentBundle => abstractSheaf(CxCd, 
	       Rank => 1, 
	       ChernClass => 1 + (2-2*g) * eta),
	  symbol PushForward => pf2
	  -- PullBack => 
	  -- SectionClass => 
	  };

     rm := map(ACd, ACxCd, {x, theta, 0,0});
     pf2 ACxCd := b -> (
	  rm first first entries last coefficients(b, Variables => {eta}, Monomials => {eta})
	  );
     pf2 AbstractSheaf := E -> abstractSheaf(Cd,
	  ChernCharacter => pf2 (ch E * todd pi2)
	  );
     Cd.TangentBundle = pi2_* (OO(CxCd.UniversalDivisor)-1);
     CxCd.projections = {pi1,pi2};
     CxCd);
