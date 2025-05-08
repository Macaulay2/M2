document {
    Key => {makeDiagonalForm, (makeDiagonalForm, Ring, RingElement), (makeDiagonalForm, Ring, ZZ), (makeDiagonalForm, Ring, QQ), (makeDiagonalForm, Ring, Sequence), (makeDiagonalForm, InexactFieldFamily, RingElement), (makeDiagonalForm, InexactFieldFamily, ZZ), (makeDiagonalForm, InexactFieldFamily, QQ), (makeDiagonalForm, InexactFieldFamily, Sequence)},
    Headline => "the Grothendieck-Witt class of a diagonal form",     
	Usage => "makeDiagonalForm(k, a)
	          makeDiagonalForm(k, L)",
	Inputs => {
	    Ring => "k" => {"a field of characteristic not 2"},
	    RingElement => "a" => {"any element ", TEX///$a\in k$///},
	    Sequence => "L" => {"of elements ",TEX///$a_{i} \in k$///, ", where ", TEX///$i = 1,\dots, n$///},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the diagonal form ", TEX///$\langle a_1,\ldots,a_n\rangle \in \text{GW}(k)$///},
	    },
	PARA {"Given a sequence of elements ", TEX///$a_1,\ldots,a_n \in k$///, " we can form the diagonal form ", TEX///$\langle a_1,\ldots,a_n\rangle$///, " defined to be the block sum of each of the rank one forms ", TEX///$\langle a_i \rangle \colon k \times k \to k,$///, " ", TEX///$(x,y) \mapsto a_i xy$///, "."},
	EXAMPLE lines ///
    	    	 makeDiagonalForm(QQ, (3,5,7))
	 	 ///,
	PARA{"Inputting a ring element, an integer, or a rational number instead of a sequence will produce a rank one form instead. For instance:"},
	EXAMPLE lines ///
	makeDiagonalForm(GF(29), 5/13)
	makeDiagonalForm(RR, 2)
	///,
    SeeAlso => {"getDiagonalClass", "diagonalizeViaCongruence", "getDiagonalEntries"}
}


document {
    Key => {makePfisterForm, (makePfisterForm, Ring, RingElement), (makePfisterForm, Ring, ZZ),(makePfisterForm, Ring, QQ), (makePfisterForm, Ring, Sequence), (makePfisterForm, InexactFieldFamily, RingElement), (makePfisterForm, InexactFieldFamily, ZZ), (makePfisterForm, InexactFieldFamily, QQ), (makePfisterForm, InexactFieldFamily, Sequence)},
    Headline => "the Grothendieck-Witt class of a Pfister form",     
	Usage => "makePfisterForm(k, a)
	          makePfisterForm(k, L)",
	Inputs => {
	    Ring => "k" => {"a field of characteristic not 2"},
	    RingElement => "a" => {"any element ", TEX///$a\in k$///},
	    Sequence => "L" => {"of elements ", TEX///$L = (a_1,\ldots,a_n)$///, " with ", TEX///$a_i \in k$///},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the Pfister form ", TEX///$\langle\langle a_1,\ldots,a_n\rangle\rangle \in \text{GW}(k)$///},
	    },
	PARA {"Given a sequence of elements ", TEX///$a_1,\ldots,a_n \in k$///, " we can form the Pfister form ", TEX///$\langle\langle a_1,\ldots,a_n\rangle\rangle$///, " defined to be the rank ", TEX///$2^n$///, " form defined as the product ", TEX///$\langle 1, -a_1\rangle \otimes \cdots \otimes \langle 1, -a_n \rangle$///, "."},
	EXAMPLE lines ///
    	    	 makePfisterForm(QQ, (2,6))
	 	 ///,
	PARA{"Inputting a ring element, an integer, or a rational instead of a sequence will produce a one-fold Pfister form instead. For instance:"},
	EXAMPLE lines ///
	makePfisterForm(GF(13), -2/3)
	makePfisterForm(CC, 3)
	///
}


document {
    Key => {makeHyperbolicForm, (makeHyperbolicForm, Ring), (makeHyperbolicForm, Ring, ZZ), (makeHyperbolicForm, InexactFieldFamily), (makeHyperbolicForm, InexactFieldFamily, ZZ)},
    Headline => "the Grothendieck-Witt class of a hyperbolic form",     
	Usage => "makeHyperbolicForm k
	          makeHyperbolicForm(k, n)",
	Inputs => {
	    Ring => "k" => {"a field of characteristic not 2"},
	    ZZ => "n" => {"an even number, giving an optional rank ", TEX///$n$///, " for a totally hyperbolic form"},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the hyperbolic form ", TEX///$\mathbb{H} = \langle 1, -1\rangle \in \text{GW}(k)$///, " or the totally hyperbolic form ", TEX///$\left(\displaystyle\frac{n}{2}\right)\mathbb{H}$///, " if an optional rank ",TEX///$n$///," is specified"},
	    },
	PARA {"By default outputs the rank two hyperbolic form over the input field ", TEX///$k$///, "."},
	EXAMPLE lines ///
    	    	 makeHyperbolicForm GF(7)
	 	 ///,
	PARA{"Specifying a rank yields a copy of sums of the rank two hyperbolic form. Only even rank inputs are accepted."},
	EXAMPLE lines ///
        makeHyperbolicForm(RR, 4)
	///,
    SeeAlso => {"isAnisotropic", "getSumDecomposition", "getSumDecompositionString"}
}
