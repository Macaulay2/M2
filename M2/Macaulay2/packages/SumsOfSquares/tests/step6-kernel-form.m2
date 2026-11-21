-- LOG.md 6.5, 6.6 (step 6): the SDP in kernel form for feasibility problems
-- where it is smaller than the image form, with the fallback to the image
-- form when the solver fails.  Both forms must give exact certificates of the
-- same kind; the private switch kernelFormDefault forces a form.
debug SumsOfSquares;
checkIdeal = (h, D) -> ((sol, mult) := sosInIdeal(h, D);
    ring sol#GramMatrix === QQ and (h*mult)_(0,0) == value sosPoly sol and value sosPoly sol != 0);
R = QQ[x,y,z];
M = library("Motzkin", R);
Rb = library("Robinson", R);
h = matrix{{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
S = QQ[x,y,z][s,t];
g = library("Robinson", {x,y,z}) + s*x^6 + t*y^6;
T = QQ[u,v];
for form in {false, true, "auto"} do (
    kernelFormDefault = form;
    assert checkIdeal(matrix{{M}}, 8);
    assert checkIdeal(matrix{{Rb}}, 8);
    assert checkIdeal(h, 2);
    -- a feasibility problem with parameters
    sol := solveSOS g;
    assert(ring sol#Parameters === QQ and ring sol#GramMatrix === QQ);
    -- an optimisation problem: in kernel form CSDP fails here (the optimum
    -- on the boundary of the primal cone); the fallback gives the image
    -- form's exact optimum
    assert(first lowerBound(T_0^2 + T_0*T_1 + T_1^2 + T_0 + 9) == 26/3);
    );
kernelFormDefault = "auto";
-- sosdecTernary of the Motzkin form in the default mode
(p, q) = sosdecTernary M;
assert(M * product(q, value) == product(p, value));
-- the review of step 6 (notes/review-step6-code.md 1): with RoundTol =>
-- infinity the elimination must be exact; h_2 = (3/5) h_1, the minimum of
-- 1/x on {h = 0} is 1/(-0.3502...) = -2.8555...
R = QQ[x];
h = matrix{{x^2/3 - x/7 - 1/11, x^2/5 - 3*x/35 - 3/55}};
b = first lowerBound(1/x, h, 2, RoundTol => infinity);
assert(b =!= null and b <= -2.85 and b > -2.87)
S = QQ[x][s,t];
f = x^2 + 1 + s*(x^2/3 + x/7 + 1/11) + t*(x^2/5 + 3*x/35 + 3/55);
sol = solveSOS(f, RoundTol => infinity);
assert(sol#Parameters =!= null and max(abs \ flatten entries sol#Parameters) < 1e6)
-- (review 2): f_0 = 0 with parameters, no trace normalisation: not the zero
-- solution (the image form, as before step 6)
T = QQ[x,y][s,t,u];
sol = solveSOS(s*x^2 + t*y^2 + u*x*y);
assert(sol#GramMatrix =!= null and sol#GramMatrix != 0)
