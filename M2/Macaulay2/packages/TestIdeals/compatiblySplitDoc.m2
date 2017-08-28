doc ///
    Key
        compatibleIdeals 
        (compatibleIdeals, RingElement)
    Headline
        finds all ideals compatibly Frobenius split ideals
    Usage
        compatibleIdeals (u)
    Inputs
        u:RingElement
    Outputs
        :List
    Description
        Text
            Given an element u of the polynomial ring $R$ over a prime field, this function returns a list of all prime ideals $P$ such that: 
            
            (a) $u P \subseteq P^{[p]}$, and 
            
            (b) $u$ is not in $P^{[p]}$. 
            
            Condition (b) is equivalent to the non-vanishing of the corresponding Frobenius map on annihilator of $P$ on the injective hull of the residue field of $R$. This is an implementation of the algorithm described in Moty Katzman and Karl Schwede's paper "An algorithm for computing compatibly Frobenius split subvarieties" J. Symbolic Comput. 47 (2012), no. 8, 996-1008.      
            
            We begin with a simple example (what is split with the coordinate axes in A^2).
        Example
            R = ZZ/3[u,v];
            u = u^2*v^2;
            compatibleIdeals(u)
        Text
            Here is a more substantial example.
        Example
            R=ZZ/2[x_{21},x_{31},x_{32},x_{41},x_{42},x_{43}];
            u=x_{41}*(x_{31}*x_{42}-x_{41}*x_{32})*(x_{41}-x_{21}*x_{42}-x_{31}*x_{43}+x_{21}*x_{32}*x_{43});
            C=compatibleIdeals (u);
            apply(C, print);
///


