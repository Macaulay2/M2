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
    inds := #gens R - deps;
    depq := take(gens R, deps);
    indq := take(gens R, -inds);
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
    
    -- local variables
    i := j := w := k := deplmei := indlmei := indlmj := deplmj := prod := mx := ll := ex := iii := gx := pos := 0;
    skip := skip1 := skip2 := true;
    logei := logj := logk := wi := ww := {};
    
    -- Compute the next module generating set; continue to loop until no changes.
    loop := true;
    while loop do (
        loop = false;
        before = now - 1;
        i = 0;
        skip = false;
        while i < #g do (	
            if s#i == before then (
	            skip2 = true;	 
	            while i > 0 and skip2 do (
	                j = i - 1;
	                skip2 = false;
	                while j >= 0 do (
	                    if leadMonomial(g#i) == leadMonomial(g#j) then (
		                    skip2 = true;
		                    break;
	                    );
	                    j = j - 1;
	                );
	                if skip2 then i = i + 1;
	            );
                e#i = red(e#i, I, dq, oldg);	      
                -- row-reduction
                j = 0;
                while j < i and e#i != 0 do (
	                if s#j == before and e#j != 0 then (
	                    logei = logpoly(e#i, depq, indq);
	                    logj = logpoly(e#j, depq, indq);	
	                    if e#j != 0 and logj#0 == logei#0 then (
                            if geqlog(logei#2, logj#2) then (
	                            wi = apply((logei#2 - logj#2), v->v%q);
		                        if wi == apply(inds, v->0) then (
		                            w = 1;
		                            for k to inds - 1 do w = w * (indq#k)^((logei#2 - logj#2)#k//q);
		                            lc = leadCoefficient(e#i)//leadCoefficient(e#j);
			                        if g#i > g#j * w then (
                                        g#i = g#i - lc * (g#j) * w;
	                                    h#i = (h#i - lc * (h#j) * w^q) % I;
                                        e#i = red(e#i - lc * (e#j) * w^q, I, dq, oldg);
	                                    j = 0;
			                            loop = true;
	    		                    )
		                            else (
			                            if g#i < g#j * w then (
                                            gx = (g#j) * w;
                                            pos = position(sort(toList(append(g,gx))), a->a==gx);
                                            g = new MutableList from insert(pos, gx, toList(g));
                                            h = new MutableList from insert(pos, (h#j)*w^q, toList(h));
                                            e = new MutableList from insert(pos, red((e#j)*w^q, I, dq, oldg), toList(e));
                                            s = new MutableList from insert(pos, before, toList(s));
                                            j = j + 1;
			                                loop = true;
 			                            )
		                                 else ( 
                                             j = j + 1;
                                         );
			                        );   
	                            ) 
		                        else (
                                    j = j + 1;
                                );
		                    )
	                        else (
                                j = j + 1;
                            );
	                    )
                        else (
                            j = j + 1;
                        );
	                )
                    else (
                        j = j + 1;
                    );
                );
	            if e#i == 0 then s#i = now;
                -- Extension
                if e#i != 0 then (
	                if s#i >= before then (
	                    loop = true;   
                        logei = logpoly(e#i, depq, indq);
	                    deplmei = logei#1;
	                    indlmei = logei#3;
	                    for j to #oldg-1 do (
	                        logj = logpoly(dq * oldg#j, depq, indq);
	                        deplmj = logj#1;
	                        indlmj = logj#3;
	                        if deplmj == deplmei then(
	                            prod = 1;
		                        for k to inds - 1 do (
		                            mx = -((-(((logj)#2)#k-((logei)#2)#k))//q);
		                            if mx > 0 then prod = prod * (indq#k)^mx;
		                        ); 
	                            skip1 = false;
		                        for l to #g - 1 do (
                                    if s#l >= before and g#l != 0 and leadMonomial(g#l) == leadMonomial(g#i*prod) then (
                                        skip1 = true;
		 	                            break;
		                            );
                                );
	                            if not skip1 then (
                                    gx = (g#i) * prod;
                                    pos = position(sort(toList(append(g,gx))), a->a==gx);
                                    g = new MutableList from insert(pos, gx, toList(g));
                                    h = new MutableList from insert(pos, (h#i)*prod^q, toList(h));
                                    e = new MutableList from insert(pos, red((e#i)*prod^q, I, dq, oldg), toList(e));
                                    s = new MutableList from insert(pos, before, toList(s));
	                                loop = true;
		                        );
		                    );          
	                    );
                        for k to #g - 1 do (
	                        if s#k >= before and g#k != 0 and e#k != 0 then (	 	 
	                            logk = logpoly(e#k, depq, indq);
	                            if k < i and logk#0 == logei#0 then (
	                                if apply((logei#2 - logk#2), v->v%q) == apply(inds, v->0) then (
		                                ll = lcm(indlmei, logk#3);
		                                w = ll//logei#3;
		                                ww = apply(inds, v->degree(indq#v,w));
			                            prod = 1;
			                            for iii to #ww - 1 do(
			                                ex = (ww#iii)//q;  
			                                prod = prod * (indq#iii)^ex;
			                            );
                                        gx = (g#i) * prod;
                                        pos = position(sort(toList(append(g,gx))), a->a==gx);
                                        g = new MutableList from insert(pos, gx, toList(g));
                                        h = new MutableList from insert(pos, (h#i)*prod^q, toList(h));
                                        e = new MutableList from insert(pos, red((e#i)*prod^q, I, dq, oldg), toList(e));
                                        s = new MutableList from insert(pos, before, toList(s));
			                            loop = true;
                                    );
	                            );
	                        );
	                    );
                    );
                );
            );
            i = i + 1;
        );    
        -- initialize next P-module generator
        now = now + 1;
        iii = #g - 1;
        while iii >= 0 do (
            if g#iii == 0 then (
	            g = drop(g,{iii,iii});
	            h = drop(h,{iii,iii});
	            e = drop(e,{iii,iii});
	            s = drop(s,{iii,iii});
            );
            iii = iii - 1;
        );
        before = before + 1;
        oldg = for iiii from 0 to #g - 1 list if s#iiii == before and e#iiii == 0 then g#iiii else continue;
        e = apply(h, s->red(s, I, dq, oldg));
    );

    -- Post-process
    i = #oldg - 1;
    while i > 0 do (
        logi = logpoly(oldg#i, depq, indq);
        j = i - 1;
        while j >= 0 do (
            logj=logpoly(oldg#j, depq, indq);
            if logi#0 == logj#0 and geqlog(logi#2, logj#2) then (
	            oldg = delete(oldg#i, oldg);
	            break;
            );
            j = j - 1;
        );
        i = i - 1;
    );
    oldg
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
    result := 1;
    while q != 0 do (
        if q%2 == 1 then result = (result * g) % I;
        q = q//2;
        g = (g^2) % I;
    );
    result
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
    de := 0;
    indlog := {};
    indprod := 1;
    for i to #ind - 1 do (
        de = degree(ind#i, lv);
        indlog = append(indlog, de);
        indprod = indprod * (ind#i)^de;
    );
    deplog := {};
    depprod := 1;
    for i to #dep - 1 do (
        de = degree(dep#i, lv);
        deplog = append(deplog, de);
        depprod = depprod * (dep#i)^de;
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
-- Happy Happy Joy Joy!
