document {
    Key => {diagonalForm, (diagonalForm, Ring, RingElement), (diagonalForm,Ring,ZZ),(diagonalForm,Ring,QQ),(diagonalForm,Ring,Sequence), (diagonalForm,InexactFieldFamily,RingElement), (diagonalForm,InexactFieldFamily,ZZ), (diagonalForm,InexactFieldFamily,QQ),(diagonalForm,InexactFieldFamily,Sequence)},
    Headline => "the Grothendieck-Witt class of a diagonal form",     
	Usage => "diagonalForm(k,a)
	          diagonalForm(k,L)",
	Inputs => {
	    Ring => "k" => {"a field"},
	    RingElement => "a" => {"any element ", TEX///$a\in k$///},
	    Sequence => "L" => {"of elements ",TEX///$a_{i} \in k$///, ", where ", TEX///$i = 1,\dots, n$///},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the diagonal form ", TEX///$\langle a_1,\ldots,a_n\rangle \in \text{GW}(k)$///},
	    },
	PARA {"Given a sequence of elements ", TEX///$a_1,\ldots,a_n \in k$///, " we can form the diagonal form ", TEX///$\langle a_1,\ldots,a_n\rangle$///, " defined to be the block sum of each of the rank one forms ", TEX///$\langle a_i \rangle \colon k \times k \to k$///, " ", TEX///$(x,y) \mapsto a_i xy$///, "."},
	EXAMPLE lines ///
    	    	 diagonalForm(QQ,(3,5,7))
	 	 ///,
	PARA{"Inputting a ring element, an integer, or a rational instead of a sequence will produce a rank one form instead. For instance:"},
	EXAMPLE lines ///
	diagonalForm(GF(29),5/13)
	diagonalForm(RR,2)
	///,
    SeeAlso => {"diagonalClass", "congruenceDiagonalize", "diagonalEntries"}
}

document {
    Key => {PfisterForm, (PfisterForm, Ring, RingElement), (PfisterForm,Ring,ZZ),(PfisterForm,Ring,QQ),(PfisterForm,Ring,Sequence), (PfisterForm,InexactFieldFamily,RingElement), (PfisterForm,InexactFieldFamily,ZZ), (PfisterForm,InexactFieldFamily,QQ),(PfisterForm,InexactFieldFamily,Sequence)},
    Headline => "the Grothendieck-Witt class of a Pfister form",     
	Usage => "PfisterForm(k,a)
	          PfisterForm(k,L)",
	Inputs => {
	    Ring => "k" => {"a field"},
	    RingElement => "a" => {"any element ", TEX///$a\in k$///},
	    Sequence => "L" => {"of elements ", TEX///$L = (a_1,\ldots,a_n)$///, " with ", TEX///$a_i \in k$///},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the Pfister form ", TEX///$\langle\langle a_1,\ldots,a_n\rangle\rangle \in \text{GW}(k)$///},
	    },
	PARA {"Given a sequence of elements ", TEX///$a_1,\ldots,a_n \in k$///, " we can form the Pfister form ", TEX///$\langle\langle a_1,\ldots,a_n\rangle\rangle$///, " defined to be the rank ", TEX///$2^n$///, " form defined as the product ", TEX///$\langle 1, -a_1\rangle \otimes \cdots \otimes \langle 1, -a_n \rangle$///, "."},
	EXAMPLE lines ///
    	    	 PfisterForm(QQ,(2,6))
	 	 ///,
	PARA{"Inputting a ring element, an integer, or a rational instead of a sequence will produce a one-fold Pfister form instead. For instance:"},
	EXAMPLE lines ///
	PfisterForm(GF(13),-2/3)
	PfisterForm(CC,3)
	///
}


document {
    Key => {hyperbolicForm, (hyperbolicForm, Ring), (hyperbolicForm, Ring, ZZ), (hyperbolicForm, InexactFieldFamily), (hyperbolicForm, InexactFieldFamily, ZZ)},
    Headline => "the Grothendieck-Witt class of a hyperbolic form",     
	Usage => "hyperbolicForm(k)
	          hyperbolicForm(k,n)",
	Inputs => {
	    Ring => "k" => {"a field"},
	    ZZ => "n" => {"an even number, giving an optional rank ", TEX///$n$///, " for a totally hyperbolic form"},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the hyperbolic form ", TEX///$\mathbb{H} = \langle 1, -1\rangle \in \text{GW}(k)$///, " or the totally hyperbolic form ", TEX///$\frac{n}{2}\mathbb{H}$///, " if an optional rank is specified"},
	    },
	PARA {"By default outputs the rank two hyperbolic form over the input field ", TEX///$k$///, "."},
	EXAMPLE lines ///
    	    	 hyperbolicForm(GF(7))
	 	 ///,
	PARA{"Specifying a rank yields a copy of sums of the rank two hyperbolic form. Only even rank inputs are accepted."},
	EXAMPLE lines ///
        hyperbolicForm(RR,4)
	///,
    SeeAlso => {"isAnisotropic", "sumDecomposition", "sumDecompositionString"}
}
