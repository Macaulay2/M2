------------
-- Header --
------------
newPackage(
    "QthPower",
    Version => "0.0.1 pre-alpha", 
    Date => "January 08, 2010",
    Authors => {{Name => "Douglas Leonard", Email => "leonada@auburn.edu", HomePage => "http://www.dms.auburn.edu/~leonada"}},
    Headline => "An implementation of the Qth-Power algorithm for computing the integral closure of a ring.",
    DebuggingMode => true
)

export {qConductor, qthPower};

------------
--  Code  --
------------

-- Find the conductor element D in RP.
qConductor = method(TypicalValue => RingElement);
qConductor (Ideal, ZZ) := (I, deps) -> (
    R := ring I;
    q := char R;
    RP := ZZ/q[gens R, MonomialOrder => {Position => Up, {deps, #gens R - deps}}];
    IP := sub(I, RP);
    MP := transpose jacobian IP;
    SP := RP/IP;
    GP := gens gb sub(MP, SP);
    DP := 1;
    j := 0;
    for i from 0 to numRows GP - 1 do (
        gc = 0;
        while GP_(i,j) == 0 do j = j + 1;
        while max apply(deps,v -> degree(SP_v, leadMonomial(GP_(i,j)))) == 0 do (
            gc = gcd(gc, GP_(i,j));
            j = j + 1;
        );
        if gc != 0 then DP = DP * gc;
    );
    sub(DP, R)
);

-- Find the integral closure of the quotient ring using the Qth-Power algorithm.
qthPower = method(TypicalValue => List);
qthPower (Ideal, ZZ, List) := (I, deps, footprint) -> (
    -- Initialisation
    R := ring I;
    q := char R;
    qc := qConductor(I, deps);
    dq := qc^(q - 1);
    g := new MutableList from footprint;
    h := apply(g, s->fastq(s, I));
    e := apply(h, s->red(s, I, dq, footprint));
    s := new MutableList from apply(#footprint, i->-1);
    now := 0;
    before := -1;
    oldg := footprint;
    
    -- do stuff
    
    e
);

-------------
-- Helpers --
-------------

-- Reduction modulo a module.
red = method(TypicalValue => RingElement);
red (RingElement, Ideal, RingElement, List) := (g, I, dq, modfoot) -> (
    h := g;
    i := #modfoot - 1;
    while i >= 0 and g != 0 do (
        h = (g % (dq * modfoot#i)) % I;
        if g != h then (
            g = h;
            i = #modfoot - 1;
        );
        i = i - 1;
    );
    g % I
);

-- Fast qth powers of a RingElement modulo I
fastq = method(TypicalValue => RingElement);
fastq (RingElement, Ideal) := (g, I) -> (
    q := char ring I;
    res := 1;
    while q != 0 do (
        if q%2 == 1 then res = (res * g) % I;
        q = q//2;
        g = (g^2) % I;
    );
    res
);

-- Compare "logs".
geqlog = method(TypicalValue => Boolean);
geqlog (List, List) := (v, w) -> (
    for i to #v - 1 do if v#i < w#i then return false;
    true
);

-- "Log" of a polynomial.
logpoly = method(TypicalValue => List);
logpoly (RingElement, List, List) := (v, dep, ind) -> (
    lv := leadMonomial v;
    indlog := {};
    indprod := 1;
    for i to #ind - 1 do (
        indlog = append(indlog, degree(ind#i, lv));
        indprod = indprod * (ind#i)^indlog#i;
    );
    deplog := {};
    depprod := 1;
    for i to #dep - 1 do (
        deplog = append(deplog, degree(dep#i, lv));
        depprod = depprod * (dep#i)^deplog#i;
    );
    {deplog, depprod, indlog, indprod}
);

-------------------
-- Documentation --
-------------------

beginDocumentation()
doc ///
    Key
        QthPower
    Headline
        An implementation of the Qth-Power algorithm for computing the integral closure of a ring.
    Description
        Text
            This package is an alternative method for computing the integral closure of a ring.
///

doc ///
    Key
        qConductor
        (qConductor, Ideal, ZZ)
    Headline
        Computes the conductor of a ring where the number of dependent variables is known.
    Usage
        qConductor(I, deps)
    Inputs
        I:Ideal
        deps:ZZ
    Outputs
        D:RingElement
    Description
        Text
            {\tt D} is a conductor of the quotient ring of {\tt I} where there are {\tt deps} dependent variables.
        Example
            Rq = ZZ/23[y,x, MonomialOrder => {Weights => {11,7}, Weights => {1,0}}];
            deps = 1;
            Iq = ideal (y^7+y^6*(3*x+1)+y^5*(3*x^3+6*x^2)+y^4*9*x^4+y^3*(4*x^6-x^5)-3*y^2*x^7-3*y*x^9-x^11);
            D = qConductor(Iq, deps)
///

end

