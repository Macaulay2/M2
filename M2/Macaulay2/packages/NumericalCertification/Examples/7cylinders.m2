-- We certify a solution of a problem of Littlewood: there exist seven infinite circular cylinders of unit radius which mutually touch each other.
-- solved by Bozoki, Lee and Ronyal

needsPackage "NumericalCertification"

FF = CC
--FF = QQ
--FF = QQ[i]/ideal(i^2+1)
-- Derive Eqs
R = FF[x_1..x_7,y_1..y_7,z_1..z_7,t_1..t_7,u_1..u_7,v_1..v_7];
ind = {1,2,3,4,5,6,7};
ind2 = {3,4,5,6,7};
apply(ind, i-> P_i=matrix{{x_i,y_i,z_i}});
apply(ind, i-> w_i=matrix{{t_i,u_i,v_i}});
eqs = matrix apply(ind, i-> apply(ind, j-> (det matrix {{x_j - x_i, y_j - y_i, z_j - z_i},{t_i, u_i, v_i}, {t_j, u_j, v_j}})^2 - 4 * ((u_i * v_j - v_i * u_j)^2 + (v_i * t_j - t_i * v_j)^2 + (t_i * u_j - u_i * t_j)^2)));
eqsRemain = submatrix'(eqs,,{0,1});
submatrix'(matrix eqs_0,{0},);
eqsEval1 = sub(submatrix'(matrix eqs_0,{0,1},), {x_1=>0, y_1=>0, z_1=>-1, z_3=>0, z_4=>0, z_5=>0, z_6=>0, z_7=>0,
	 t_1=>1, u_1=>0, v_1=>0, v_3=>1-t_3-u_3, v_4=>1-t_4-u_4, v_5=>1-t_5-u_5, v_6=>1-t_6-u_6, v_7=>1-t_7-u_7});
eqsEval2 = sub(submatrix'(matrix eqs_1,{0,1},), {x_2=>0, y_2=>0, z_2=>1, z_3=>0, z_4=>0, z_5=>0, z_6=>0, z_7=>0,
	 t_2=>0, u_2=>1, v_2=>0, v_3=>1-t_3-u_3, v_4=>1-t_4-u_4, v_5=>1-t_5-u_5, v_6=>1-t_6-u_6, v_7=>1-t_7-u_7});
eqsEval3 = flatten {flatten entries eqsEval1, flatten entries eqsEval2, flatten apply(ind2, i -> for j from i+1 to 7 when j < 8 list sub(eqs_(i-1,j-1), {z_3=>0, z_4=>0, z_5=>0, z_6=>0, z_7=>0,
	 v_3=>1-t_3-u_3, v_4=>1-t_4-u_4, v_5=>1-t_5-u_5, v_6=>1-t_6-u_6, v_7=>1-t_7-u_7}))};
eqsEval3#0;



end ---------------------------------------------
restart
load "7cylinders.m2"

-- Define polySys
R1 = FF[x_3..x_7,y_3..y_7,t_3..t_7,u_3..u_7];
f = polySystem sub(transpose matrix {eqsEval3},R1)
sol1 = apply({11.675771704477,3.802878122730,8.311818491659,-6.487945444917,-3.168475045360,
    -4.124414157636, -2.910611127075, -1.732276613733, -8.537495065091, -2.459640638529,
    0.704116159640, 0.895623427074, 2.515897624878, 0.785632006191, 0.192767499267,
    0.235129952793, -0.149726023342, -0.566129665502, 0.338461562103, 0.536724141124}, x->sub(promote(x,QQ),FF));
sol2 = apply({2.075088491891, -2.688893665930, -4.033142850644, 6.311134419772, 3.914613907006,
	-2.036516392124, 4.070505903499, -2.655943449984, -5.229892181735, -7.881492743224,
	-0.030209763440, 0.184499043058, 0.251380280590, -0.474742889365, 1.698198197367,
	0.599691085438, 0.426965115851, 0.516678258430, 1.230302197822, -1.164062857743}, x->promote(x,QQ));


-- alpha-theory
sol1point = point {sol1};
sol2point = point {sol2};
L = {sol1point,point random(FF^1,FF^(#coordinates sol1point)), sol2point}
certifySolutions(f, L)
peek oo

-- Krawczyk (faster IntervalArithmetic needed?)
certifySolutions(f, L, Strategy => "intervalArithmetic")
peek oo
