doc ///
Key
    "examples from physics"
Description
  Text
    The following D-ideal corresponds to the GKZ system for the two-site chain of cosmologial correlators, see Figure 1 in https://arxiv.org/pdf/2410.14757.
  Example
    v = {1,1,1};
    D = makeWeylAlgebra(frac(QQ[eps,DegreeRank=>0])[x,y,z],v);
    delta1 = (x^2-z^2)*dx^2+2*(1-eps)*x*dx-eps*(1-eps);
    delta2 = (y^2-z^2)*dy^2+2*(1-eps)*y*dy-eps*(1-eps);
    delta3 = (x+z)*(y+z)*dx*dy-eps*(x+z)*dx-eps*(y+z)*dy+eps^2;
    h = x*dx+y*dy+z*dz-2*eps;
    I = ideal(delta1+delta3, delta2+delta3,h);

    assert(holonomicRank(I) == 4);

    -- Gauge transform to eps-factorized form:
    P = connectionMatrices I;
    G = flatten entries gens gb I;

    B = {1_D,dx,dy,dx*dy};
    g = gaugeMatrix(G,B);
    assert(g == gaugeMatrix(I,B));

    F = baseFractionField D;
    changeEps = transpose((1/(2*z*eps^2))*matrix({{2*z*eps^2, -eps^2*(x-z), -eps^2*(y-z), -eps^2*(x+y)},{0,eps*(x^2-z^2),0,eps*(x+y)*(x+z)},{0,0,eps*(y^2-z^2),eps*(x+y)*(y+z)},{0,0,0,-(x+y)*(x+z)*(y+z)}}));
    Peps = gaugeTransform(changeEps,gaugeTransform(g,P));

    assert(isEpsilonFactorized(Peps,eps));
References
  See the example in equation (11) from https://arxiv.org/pdf/2410.14757
SeeAlso

///
