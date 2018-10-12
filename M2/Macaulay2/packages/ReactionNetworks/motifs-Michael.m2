export{"nSiteProcessiveModification","nSiteDistributiveModification","nSiteImmuneReaction","nSiteSequestration","nSiteDiffusion","nSitePoreForming","nSiteAutocatalytic"}

TriDiag=method(TypicalValue=>Matrix);
TriDiag (ZZ,List) := (K,d) ->(
  Diag := diagonalMatrix(K,K,d_0*(for i from 1 to K list 1));
  SuperDiag := ((transpose matrix{for i from 1 to K-1 list 0})|diagonalMatrix(K-1,K-1,d_1*(for i from 1 to K-1 list 1)))||(matrix{for i from 1 to K list 0});
  SubDiag := matrix{for i from 1 to K list 0}||diagonalMatrix(K-1,K,d_2*(for i from 1 to K-1 list 1));
  M := Diag+SubDiag+SuperDiag;
  return M;
  );

DistGamma=method(TypicalValue=>Matrix);
DistGamma ZZ:=m->(
  Zeros := matrix{(for i from 1 to m list 0)};
  ZeroN := (transpose Zeros)**Zeros;
  Ones := matrix{(for i from 1 to m list 1)};
  Identity := id_(QQ^m);
  G1 := -Ones||Zeros||-Identity||Zeros||Identity||ZeroN;
  G2 := Ones||Zeros||Zeros||Identity||-Identity||ZeroN;
  G3 := Zeros||-Ones||Zeros||-Identity||ZeroN||Identity;
  G4 := Zeros||Ones||Identity||Zeros||ZeroN||-Identity;
  G := G1|(-G1)|G2|G3|(-G3)|G4;
  return G;
  );

ProcGamma=method(TypicalValue=>Matrix);
ProcGamma ZZ:=m->(
  Zeros := matrix{(for i from 1 to m list 0)};
  ZeroN := (transpose Zeros)**Zeros;
  Ones := matrix{(for i from 1 to m list 1)};
  Identity := id_(ZZ^m);
  G1 := ((matrix({{-1},{0},{-1},{0}})|(matrix({{0},{0},{0},{0}})**matrix{for i from 2 to m list 0})|matrix({{1},{0},{0},{1}})))||(matrix ZZ^m_0|TriDiag(m,{-1,0,1}))||(transpose(Zeros)**matrix{for i from 0 to m list 0});
  G2 := (matrix({{1},{0},{1},{0}})|matrix({{0},{0},{0},{0}})**matrix{for i from 2 to m list 0})||TriDiag(m,{-1,1,0})||ZeroN;
  G3 := ((matrix({{0},{1},{1},{0}})|matrix({{0},{0},{0},{0}})**matrix{for i from 2 to m list 0}|matrix({{0},{-1},{0},{-1}})))||(transpose Zeros)**matrix{for i from 0 to m list 0}||(TriDiag(m,{-1,1,0})|matrix ZZ^m_(m-1));
  G4 := ((matrix({{0},{0},{0},{0}})**matrix{for i from 2 to m list 0})|matrix({{0},{1},{0},{1}}))||ZeroN||TriDiag(m,{-1,0,1});
  G := G1|G2|G3|G4;
  return G;
);

AutocatGamma=method(TypicalValue=>Matrix);
AutocatGamma ZZ:= m->(
  G1 := TriDiag(m,{-1,0,1})+(((transpose matrix{for i from 1 to m list 0})**matrix{for i from 1 to m-1 list 0})|matrix(ZZ^m_0));
  G := G1|id_(ZZ^m)|(-id_(ZZ^m));
  return G;
  );

DiffusionGamma=method(TypicalValue=>Matrix);
DiffusionGamma ZZ := m->(
  G1 := submatrix(TriDiag(m,{1,-1,0}),,toList(1..m-1));
  G := G1|(-G1);
  return G;
  );

DistR=method(TypicalValue=>Matrix);
DistR ZZ:=m->(
  k:= symbol k;
  x:= symbol x;
  -- S := QQ[k_1..k_m,kp_1..kp_m,kb_1..kb_m,kbp_1..kbp_m,l_1..l_m,lb_1..lb_m, x_1..x_(3*m+3)];
  S := QQ[k_1..k_(6*m), x_1..x_(3*m+3)];


  R1 := matrix({for i from 1 to m list k_i*x_1*x_(i+2)});
  R2 := matrix({for i from 1 to m list k_(m+i)*x_(i+m+3)});
  R3 := matrix({for i from 1 to m list k_(2*m+i)*x_(i+m+3)});
  R4 := matrix({for i from 1 to m list k_(3*m+i)*x_2*x_(i+3)});
  R5 := matrix({for i from 1 to m list k_(4*m+i)*x_(2*m+3+i)});
  R6 := matrix({for i from 1 to m list k_(5*m+i)*x_(2*m+3+i)});
  R := transpose(R1|R2|R3|R4|R5|R6);
  return R;
  );

ProcR=method(TypicalValue=>Matrix);
ProcR ZZ:=m->(
  k:= symbol k;
  x:= symbol x;
  S := QQ[k_1..k_(4*m+2),x_1..x_(2*m+4)];
  R1 := matrix{{k_1*x_1*x_3}}|matrix({for i from 2 to m+1 list k_i*x_(i+3)})|matrix({for i from 1 to m list k_(m+1+i)*x_(i+4)});
  R2 := matrix({for i from 1 to m list k_(2*m+1+i)*x_(m+4+i)})|matrix{{k_(3*m+2)*x_2*x_4}}|matrix({for i from 1 to m list k_(3*m+2+i)*x_(m+i+4)});
  R := transpose(R1|R2);
  return R;
  );

PoreGamma=method(TypicalValue=>Matrix);
PoreGamma ZZ:=m->(
 G1u := matrix(-2)|matrix{for i from 1 to (m-2) list (-1)};
 G1d := TriDiag(m-1,{1,-1,0});
 G1 := G1u||G1d;
 G := G1|(-G1);
 return G;
 );

PoreR=method(TypicalValue=>Matrix);
PoreR ZZ := m->(
 k:= symbol k;
 x:= symbol x;
 S := QQ[k_1..k_(2*m-2),x_1..x_m];
 R1 := matrix({for i from 1 to (m-1) list k_i*x_1*x_i});
 R2 := matrix({for i from 1 to (m-1) list k_(m-1+i)*x_(i+1)});
 R := transpose(R1|R2);
 return R;
 );

McKGamma = method(TypicalValue=>Matrix);
McKGamma ZZ := m->(
  G1u := (matrix(-1)|matrix{for i from 1 to m-1 list 0})||(matrix(-1)|matrix{for i from 1 to m-1 list 0});
  G1d := TriDiag(m,{1,-1,0});
  G1 := G1u||G1d;
  G2u := (matrix{for i from 1 to m list 1})||(matrix{for i from 1 to m list 1});
  G2d := -id_(ZZ^m);
  G2 := G2u||G2d;
  G := G1|G2;
  return G;
  );

McKR = method(TypicalValue=>Matrix);
McKR ZZ := m->(
 k:= symbol k;
 x:= symbol x;
 S := QQ[k_1..k_(2*m),x_1..x_(m+2)];
 R1 := matrix(k_1*x_1*x_2)|matrix({for i from 2 to m list k_i*x_(i+1)});
 R2 := matrix({for i from 1 to m list k_(m+i)*x_(i+2)});
 R := transpose(R1|R2);
 return R;
 );

SeqGamma = method(TypicalValue=>Matrix);
SeqGamma ZZ := m->(
  G1 := submatrix(TriDiag(m,{-1,0,-1}),{0..m-2});
  G2 := -matrix(ZZ^m_0)+(matrix(ZZ^m_(m-1)));
  G := G1|G2;
  return G;
  );

SeqR = method(TypicalValue=>Matrix);
SeqR ZZ := m->(
  k:= symbol k;
  x:= symbol x;
  S := QQ[k_1..k_m,x_1..x_m];
  R := transpose(matrix({for i from 1 to (m-1) list k_i*x_i*x_(i+1)})|matrix(k_m*x_1));
  return R
  );

AutocatR = method(TypicalValue=>Matrix);
AutocatR ZZ := m->(
  k := symbol k;
  l := symbol l;
  lb := symbol lb;
  x := symbol x;
  S := QQ[k_1..k_(3*m),x_1..x_m];
  R1 := matrix({for i from 1 to m-1 list k_i*x_i*x_(i+1)})|matrix({{k_m*x_m*x_1}});
  R2:=matrix({for i from 1 to m list k_(m+i)});
  R3 := matrix({for i from 1 to m list k_(2*m+i)*x_i});
  R := transpose(R1|R2|R3);
  return R;
  );

DiffusionR=method(TypicalValue=>Matrix);
DiffusionR ZZ := m->(
  x := symbol x;
  d := symbol d;
  S:=QQ[d_1..d_(2*m-2),x_1..x_m];
  R := matrix({for i from 1 to m-1 list d_i*x_i})|matrix({for i from 1 to m-1 list d_(m-1+i)*x_(i+1)});
  -- R := d*(transpose matrix{toList(x_1..x_(m-1),x_2..x_m)});
  return transpose R;
  );

nSiteProcessiveModification = method()
installMethod(nSiteProcessiveModification, ZZ, N -> reactionNetwork({ProcGamma N, ProcR N},Input=>"Stoichiometric"))
nSiteDistributiveModification = method(TypicalValue=>ReactionNetwork)
installMethod(nSiteDistributiveModification, ZZ, N -> reactionNetwork({DistGamma N, DistR N},Input=>"Stoichiometric"))
nSiteImmuneReaction = method(TypicalValue=>ReactionNetwork)
installMethod(nSiteImmuneReaction, ZZ, N -> reactionNetwork({McKGamma N, McKR N},Input=>"Stoichiometric"))
nSiteSequestration = method(TypicalValue=>ReactionNetwork)
installMethod(nSiteSequestration, ZZ, N -> reactionNetwork({SeqGamma N, SeqR N},Input=>"Stoichiometric"))
nSiteDiffusion = method(TypicalValue=>ReactionNetwork)
installMethod(nSiteDiffusion, ZZ, N -> reactionNetwork({DiffusionGamma N, DiffusionR N},Input=>"Stoichiometric"))
nSitePoreForming = method(TypicalValue=>ReactionNetwork)
installMethod(nSitePoreForming, ZZ, N -> reactionNetwork({PoreGamma N, PoreR N},Input=>"Stoichiometric"))
nSiteAutocatalytic = method(TypicalValue=>ReactionNetwork)
installMethod(nSiteAutocatalytic, ZZ, N -> reactionNetwork({AutocatGamma N, AutocatR N},Input=>"Stoichiometric"))
