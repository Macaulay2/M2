------------
-- Header --
------------
newPackage(
    "QthPower",
    Version => "0.0.1 pre-alpha", 
    Date => "January 08, 2010",
    Authors => {{Name => "Douglas Leonard", Email => "leonada@auburn.edu", HomePage => "http://www.dms.auburn.edu/~leonada"}},
    Headline => "An implementation of the Qth-Power algorithm for computing the integral closure of an ideal.",
    DebuggingMode => true
)

export {qConductor};

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

-------------------
-- Documentation --
-------------------

beginDocumentation()
doc ///
    Key
        QthPower
    Headline
        An implementation of the Qth-Power algorithm for computing the integral closure of an ideal.
    Description
        Text
            This package is an alternative method for computing the integral closure of an ideal.
///

doc ///
    Key
        qConductor
        (qConductor, Ideal, ZZ)
    Headline
        Computes the conductor of an ideal where the number of dependent variables is known.
    Usage
        qConductor(I, deps)
    Inputs
        I:Ideal
        deps:ZZ
    Outputs
        D:RingElement
    Description
        Text
            {\tt D} is a conductor of {\tt I} where there are {\tt deps} dependent variables.
        Example
            Rq = ZZ/23[y,x, MonomialOrder => {Weights => {11,7}, Weights => {1,0}}];
            deps = 1;
            Iq = ideal (y^7+y^6*(3*x+1)+y^5*(3*x^3+6*x^2)+y^4*9*x^4+y^3*(4*x^6-x^5)-3*y^2*x^7-3*y*x^9-x^11);
            D = qConductor(Iq, deps)
///

end

