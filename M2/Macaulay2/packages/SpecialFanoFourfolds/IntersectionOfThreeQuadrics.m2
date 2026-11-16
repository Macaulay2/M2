
------------------------------------------------------------------------
----------- Complete intersections of three quadrics in PP^7 -----------
------------------------------------------------------------------------

IntersectionOfThreeQuadricsInP7 = new Type of HodgeSpecialFourfold;

globalAssignment IntersectionOfThreeQuadricsInP7;

IntersectionOfThreeQuadricsInP7.synonym = "complete intersection of three quadrics in PP^7";

expression IntersectionOfThreeQuadricsInP7 := X -> expression("complete intersection of three quadrics in PP^7 containing a surface of degree "|toString(degree surface X)|" and sectional genus "|toString(sectionalGenus surface X));

describe IntersectionOfThreeQuadricsInP7 := X -> (
    descr := "Complete intersection of 3 quadrics in PP^7";
    disc := discriminant X;
    A := X.cache#(surface X,"LatticeIntersectionMatrix");
    descr = descr|newline|toString("of discriminant "|(toString disc)|" = det"|(net A));
    descr = descr|newline|"containing a "|surfaceDescription(0,surface X);
    if member(recognize X,{"surf-5-7-0-1","surf-5-10-1","internal-projection-K3-genus-8","surf-4-3-1-external","surf-5-6-2-nodal","surf-7-1-9"}) then descr = descr|newline|"(This is a rational fourfold discovered in August 2022)";
    if recognize X === "planeInPP7" then descr = descr|newline|"(This is a classical example of rational fourfold)";
    if computationStatus X >= 0 then descr = descr|newline|(computationStatusLog X)|newline|toString(describeMirrorFourfoldAndK3 X);
    net expression descr
);

shortDescriptionFourfold (IntersectionOfThreeQuadricsInP7,Boolean) := (X,UseAttribute) -> (
    if UseAttribute and hasAttribute(X,ReverseDictionary) then return toString getAttribute(X,ReverseDictionary);
    "complete intersection of three quadrics in PP^7"
);

recognize3QuadricsP7 = X -> (
    S := surface X;
    d := discriminant X;
    e := eulerCharacteristic S;
    invS := (degree S,sectionalGenus S,euler hilbertPolynomial S);
    degs := flatten degrees ideal S;
    if (d == 31 and e == 3 and invS === (1,0,1) and degs == toList(5:1)) then return "planeInPP7";
    if (d == 47 and e == 11 and invS === (9,3,1) and degs == toList(12:2)) then return "surf-5-7-0-1";
    if (d == 55 and e == 14 and invS === (11,5,1) and degs == toList(10:2)) then return "surf-5-10-1";
    if (d == 55 and e == 25 and invS === (13,8,2) and degs == toList(9:2)) then return "internal-projection-K3-genus-8";
    if (d == 79 and e == 7 and invS === (9,2,1) and degs == {2,2,2,2,2,2,2,2,2,2,3}) then return "surf-4-3-1-external";
    if ((d == 71 or d == 87) and e == 5 and invS === (11,4,0) and degs == toList(9:2)) then if numberNodes(S,Verbose=>false) == 1 and discriminant X == 87 then return "surf-5-6-2-nodal";
    if (d == 96 and e == 13 and invS === (12,6,1) and degs == {2,2,2,2,2,2,2,2,2,3}) then return "surf-7-1-9";
    "NotRecognized"
);

infoAboutParameterCountInAmbientP7 = (x,y) -> (
    --------------------------------------------------
        -- Conversion between parameter counts
        -- X : c. i. of 3 quadrics in PP^7; Y = ambientFivefold X; S = surface X;
        -- Suppose we have computed:
        -- h^1(N_{S,Y}) = 0, h^0(N_{S,Y}) = a
        -- h^1(O_S(2)) = 0, and h^0(I_{S,Y}(2)) = b = h^0(O_Y(2)) - \chi(O_S(2));
        -- h^0(N_{S,X}) = c, dim P(H^0(O_Y(2))) = 33
        -- codim{[X] : S ⊂ X ⊂ Y} <= 33 - (a + (b-1) - c)
        -- parameterCount(X,Y) ----> (34-a-b+c, (b, a, c))
        -- Now from the exact sequence 0 -> N_(S,Y) -> N_(S,PP^7) -> N_(Y,PP^7)|_S -> 0
        -- that is 0 -> N_(S,Y) -> N_(S,PP^7) -> O_S(2)++O_S(2) -> 0
        -- we deduce h^0(N_(S,PP^7)) = h^0(N_{S,Y}) + h^0(O_S(2)++O_S(2)) = a + 2*(34 - b) = 68 + a - 2b
        -- Further, from the exact sequence 0 -> I_(Y,PP^7) -> I_(S,PP^7) -> I_(S,Y) -> 0
        -- we deduce h^0(I_{S,PP^7}(2)) = h^0(I_{S,Y}(2)) + 2 = b + 2
        -- from which we have dim GG(2, PP(h^0(I_{S,PP^7}(2)))) = 3 * (b-1)
        -- Since dim GG(2,P(H^0(O_(P^7)(2)))) = 99 we obtain
        -- 99 - ( (68 + a - 2b) + 3*(b-1) - c ) = 31 - a + 2b - 3b + 3 + c = 34 - a - b + c
        -- Therefore parameterCount(X) ----> (34-a-b+c, (b+2, 68 + a - 2b , c))
    --------------------------------------------------
    (b,a,c) := y;
    if 34-a-b+c != x then <<"--warning: something went wrong"<<endl;
    <<"[parameterCount in the ambient PP^7: "<<(34-a-b+c, (b+2, 68 + a - 2*b , c))<<"]"<<endl;
);

beauvilleMap = method();
beauvilleMap IntersectionOfThreeQuadricsInP7 := X -> (
    R := ring ambient X;
    l := projectiveVariety ideal submatrix'(vars R,{0,1});
    j := rationalMap(((line X) ===> l)|X,Dominant=>true);
    X' := target j;
    if not isSubset(l,X') then error "something went wrong";
    K := coefficientRing R;
    x := local x; P7 := projectiveVariety(K[x_0..x_7]);
    t := local t; P2 := projectiveVariety(K[t_0..t_2]);
    u := local u; P5 := projectiveVariety(K[u_0..u_5]);
    Q := flatten entries sub(gens ideal X',vars ring P7);
    S := (K[x_2..x_7])[x_0,x_1];
    L := apply(3,i -> coefficient((x_0)_S,sub(Q_i,S)));
    M := apply(3,i -> coefficient((x_1)_S,sub(Q_i,S)));
    F := apply(3,i -> coefficient(1_S,sub(Q_i,S)));
    if F != apply(3,i -> sub(Q_i,S) - (x_0)_S * L_i - (x_1)_S * M_i) then error "something went wrong";
    P2xP5 := P2 ** P5;
    A := sub(matrix {L,M,F},for i to 5 list (gens coefficientRing S)_i => u_i);
    Z := projectiveVariety ideal det A;
    Y := projectiveVariety ideal(((map last projections P2xP5) A) * transpose matrix {first P2xP5#"multigens"});
    fromXtoZ := j * rationalMap(ring X',ring P5,submatrix'(vars R,{0,1}));
    if image fromXtoZ != Z then error "something went wrong";
    fromXtoZ = rationalMap(fromXtoZ,Z);
    fromYtoZ := (last projections P2xP5)|Y;
    if image fromYtoZ != Z then error "something went wrong";
    fromYtoZ = rationalMap(fromYtoZ,Z);
    Y.cache#"projection maps" = {quadricFibration first projectionMaps Y, fromYtoZ};
    f := fromXtoZ * inverse fromYtoZ;
    g := fromYtoZ * inverse fromXtoZ;
    if not(f * g == 1 and g * f == 1) then error "something went wrong";
    forceInverseMap(f,g);
    f
);
