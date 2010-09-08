I = Grassmannian(1, 6, CoefficientRing => ZZ/101);
R = ring I
RZ = R/I;
IC1 = ideal(p_(0,1), p_(0,2), p_(0,3), p_(1,2), p_(1,3), p_(2,3));
IC2 = ideal(p_(3,4), p_(3,5), p_(3,6), p_(4,5), p_(4,6), p_(5,6));
gbTrace = 1;
E2 = Ext^2(IC1, IC2);
E2 = Ext^3(comodule IC1, IC2);
Gr = Proj RZ;
SE2 = sheaf(E2);
HH^0(SE2(-1))
HH^1(SE2(-2))
HH^2(SE2(-3))
HH^3(SE2(-4))
HH^4(SE2(-5))
HH^1(SE2(-1))

end

Dear Mike,

I am attaching below the code that I'm tying to run. The line that runs almost
10 times slower in 0.9.8 from 0.9.2 is the one that starts E2 = Ext^2... (on my
PPC machine, the Ext line runs in about 4 minutes of processor time in 0.9.2,
and takes something like 80 or 90 minutes in 0.9.8).

Do you have any idea why the computation would abort when exceeding memory (but
still below the RAM availability of the machine)? If you try to run the code
below, it will crash after about 180 minutes, in one of the HH^* lines (second
or third).

Where can the newest version of PPC code be downloaded from? (You said it's
already available, have I understood correctly?) If not, what architecture will
the binary be ready for the soonest, and where is it downloadable from?

Thanks a lot,

Andrei.

R = ZZ/101[apply(subsets(7, 2), i -> p_i)];
I = Grassmannian(1, 6, R);
RZ = R/I;
IC1 = ideal(p_{0,1}, p_{0,2}, p_{0,3}, p_{1,2}, p_{1,3}, p_{2,3});
IC2 = ideal(p_{3,4}, p_{3,5}, p_{3,6}, p_{4,5}, p_{4,6}, p_{5,6});
gbTrace = 1;
time E2 = Ext^2(IC1, IC2);
Gr = Proj RZ;
SE2 = sheaf(E2);
HH^0(SE2(-1))
HH^1(SE2(-2))
HH^2(SE2(-3))
HH^3(SE2(-4))
HH^4(SE2(-5))
HH^1(SE2(-1))

-- MES: I changed/added the stuff below:
J = cokernel lift(presentation E2, R);
time ann J;
C = res J;

time E3 = Ext^3(coker gens IC1, IC2);

M = coker gens IC1
N = prune module IC2;
i = 3
time C = resolution(M, LengthLimit=>4)
time b = C.dd;
time complete b;
time H1 = Hom(b_(i+1),N);
bt = transpose b_(i+1);
bt ** N
H2 = Hom(b_i,N)
homology(H1,H2)
homology(Hom(b_(i+1),N), Hom(b_i,N))

G0 = target presentation N
G1 = source presentation N
F = C_3
F' = C_4
F'' = C_2
Hom(F,G0)
a = Hom(C.dd_4,G0);
b = Hom(F',presentation N);
c = modulo(a,b);
