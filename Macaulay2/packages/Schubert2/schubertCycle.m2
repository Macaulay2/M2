-- experiment with getting Schubert cycles from flag bundles

A = OO_point^4
F = flagBundle({2,2},A,VariableNames => {s,q})
Q = last F.Bundles
euler F
AF = intersectionRing F
netList {{ F_{2,2}, F_{2,1}, F_{2,0} }, { , F_{1,1}, F_{1,0} }, { , , F_{0,0} }}
W = flagBundle({4:1},(F/point)^* A, VariableNames => {{symbol a},{symbol b},{symbol c},{symbol d}})
AW = intersectionRing W
F_{2,2}
(W/F)_* ( ctop Hom(W.SubBundles#2,Q) * (W/F).SectionClass )
F_{1,1}
(W/F)_* ( ctop Hom(W.SubBundles#1,Q) * (W/F).SectionClass )
F_{0,0}
(W/F)_* ( (W/F).SectionClass )
F_{1,0}
(W/F)_* ( ctop Hom(det W.SubBundles#2,det Q) * (W/F).SectionClass )

(W/F)_* ( chern_2 Q * (W/F).SectionClass )
(W/F)_* ( chern_1 Q * (W/F).SectionClass )
(W/F)_* (             (W/F).SectionClass )
