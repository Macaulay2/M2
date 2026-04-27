doc ///
	Key
		transferGW
		(transferGW, GrothendieckWittClass)
	Headline
		the transfer of Grothendieck-Witt from an étale algebras to a base field
	Usage
		transferGW(beta)
	Inputs
		beta: GrothendieckWittClass
			Grothendieck-Witt class over an étale algebra over field of characteristic not 2
	Outputs
		: GrothendieckWittClass
			the image of the Grothendieck-Witt class beta in $\text{GW}(k)$ under the canonical transfer map.
	Description
		Text
			Given a finite étale algebra $L/k$ and a Grothendieck-Witt class, $\beta$ in $\text{GW}(L)$, computes the image of $\beta$ under the canonical map $\text{GW}(L)\to\text{GW}(k)$ for a finite étale algebra $L/k$.
		Example
			R = QQ[x]/(x^2 - 1);
			beta = makeGWClass matrix(R, {{1,2},{2,x}});
			transferGW(beta)
	SeeAlso
		GrothendieckWittClass
		getTrace
///