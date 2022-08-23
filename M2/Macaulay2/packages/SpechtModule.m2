-- -*- coding: utf-8 -*-
newPackage(
        "SpechtModule",
        Version => "1.0", 
        Date => "October 22, 2019",
        Authors => {{Name => "Jonathan Niño", 
                  Email => "ja.nino937@uniandes.edu.co", 
                  HomePage => "http://www.uniandes.edu.co"}},
	Keywords => {"Representation Theory"},
        Headline => "invariants for permutation groups",
        DebuggingMode => false
        )
		

export {"CharacterTable"}
export {"characterTable"}
		
export {"YoungTableau"}
export {"youngTableau"}
export {"tableauToList"}
export {"listToTableau"}

export {"TableauList"}
export {"tableauList"}
export {"toListOfTableaux"}
export {"addTableau"}

export {"tabloids"}
export {"standardTableaux"}
export {"semistandardTableaux"}
export {"rowPermutationTableaux"}

export {"indexTableau"}

export {"hookLengthFormula"}
export {"cycleDecomposition"}
export {"conjugacyClass"}

export {"matrixRepresentation"}

export {"readingWord"}


export {"columnStabilizer"}

export {"rowStabilizer"}

export {"garnirElement"}
export {"sortColumnsTableau"}
export {"cardinalityOfConjugacyClass"}


export{"multinomial"}
export {"straighteningAlgorithm"}

export {"SpechtModuleElement"}
export {"spechtModuleElement"}

export {"permutePolynomial"}

export {"vandermondeDeterminant"}
export {"spechtPolynomial"}
export {"indexMonomial"}
export {"higherSpechtPolynomial"}

export {"spechtPolynomials"}
export {"higherSpechtPolynomials"}


export {"permutationSign"}
export {"firstRowDescent"}
export {"schurPolynomial"}
export {"generalizedVandermondeMatrix"}
export {"Robust","AsExpression"}
export {"generatePermutationGroup"}
export {"representationMultiplicity"}
export {"innerProduct"}


export {"elementarySymmetricPolynomials"}
export {"powerSumSymmetricPolynomials"}

export {"secondaryInvariants"}
export {"permutationMatrix"}

protect \ {row,column}


CharacterTable = new Type of MutableHashTable

characterTable = method(TypicalValue => CharacterTable)
characterTable ZZ := n -> (
    
    charTables := new MutableHashTable;
    
    
    
    
    for i from 1 to n do (
	
	 
	charTable := new CharacterTable;
	partis := partitions i;
	charTable#index = hashTable apply(#partis, i-> partis#i => i);
    	charTable#length = #(partis);
    	charTable#degree = i;
    	charTable#values = mutableMatrix(ZZ,charTable#length,charTable#length);
    	charTables#i = charTable;
	y:= partitions(i);
	for j  to #y-1 do (
	    
	    for k from j to #y-1 do (
		val:= calculateNumberOfEquals(y#(j),y#(k),charTables);
		(charTables#i)_(j,k)=val;
	    );
	);
    ); 
   charTable := reduceCharacterTable(charTables#n);
   charTable
)

    
       
	


CharacterTable_Sequence:=(charTable,seq)-> (
    if #seq != 2 then error "expected a sequence of length 2";
    (a,b) := seq;
    if(class a === Partition) then (
	if sum toList a != charTable#degree then (error "expected a partition of size "|charTable#degree)
	else a=charTable#index#a)
    else (if class a =!= ZZ then error "expected argument 1 to be a partition or an integer");	
     
     if(class b === Partition) then (
	if sum toList b != charTable#degree then (error "expected a partition of size "|charTable#degree)
	else b=charTable#index#b)
    else if class b =!= ZZ then error "expected argument 2 to be a partition or an integer";
    charTable#values_(a,b)
    )

CharacterTable_Sequence = (charTable,seq,e)-> (
    if #seq != 2 then error "expected a sequence of length 2";
    (a,b) := seq;
    if(class a === Partition) then (
	if sum toList a != charTable#degree then (error "expected a partition of size "|charTable#degree)
	else a=charTable#index#a)
    else (if class a =!= ZZ then error "expected argument 1 to be a partition or an integer");	
     
     if(class b === Partition) then (
	if sum toList b != charTable#degree then (error "expected a partition of size "|charTable#degree)
	else b=charTable#index#b)
    else if class b =!= ZZ then error "expected argument 2 to be a partition or an integer";
    charTable#values_(a,b)=e;
    e
    )



innerProduct = method(TypicalValue => ZZ)
innerProduct(ZZ,MutableMatrix,MutableMatrix) := (n,C,X) -> (
    prod:=0;
    p:=partitions(n);
    prod = sum apply( numColumns(C),i -> C_(0,i)*(X_(0,i))*(cardinalityOfConjugacyClass(p#(i))));
    prod//(n)!
)


net CharacterTable := charTable -> (
    net(charTable#values)
    )


reduceCharacterTable = method(TypicalValue => CharacterTable)
reduceCharacterTable CharacterTable  := charTable -> (
    for i to charTable#length-1 do(
    
        for j to  i-1 do(
            
            c := innerProduct(charTable#degree,(charTable#values)^{i},(charTable#values)^{j});
            for k to charTable#length-1 do(
		val:= charTable_(i,k)-c*charTable_(j,k);
                charTable_(i,k)=val;
            );
     );
    	
    );
   
    charTable
)



calculateNumberOfEquals = method(TypicalValue => ZZ )
calculateNumberOfEquals(Partition, Partition,MutableHashTable):= (partition1, partition2,charTables)->(
    
    z:=0;
    if(sum(toList partition1) == sum(toList partition2)) then (
    	if #partition1 == 1 then (z = 1;)
	else ( 
    	    border:= partition2#0;
	    partition2 = drop(partition2,1);
	    for i to #partition1-1 when partition1#i>=border do(
	    	c:= new MutableList from partition1;
		c#i = c#i-border;
	        newPartition := new Partition from reverse sort toList c;
		if(newPartition#(-1) == 0)
		    then (newPartition = drop(newPartition,-1););
		if(#newPartition == 0)
		    then (z= z+ 1;)
		else(
		    
		    currentTableNumber:=sum(toList newPartition);
		    z = z+(charTables#currentTableNumber)_(newPartition,partition2);
		);
		
	    );
	    
	);    
    ) else error "Partition dimensions do not match";
    z
)




YoungTableau = new Type of MutableHashTable
youngTableau = method(TypicalValue => YoungTableau)
youngTableau Partition := p -> (
    tableau:= new YoungTableau;
    tableau#partition = p;
    tableau#values = new MutableList from ((sum toList p):0) ;
    tableau
)

youngTableau(Partition,List):= (p,L)->(
    if(sum toList p != #L) then error " Partition size does not match with the length of the list L";
    tableau:= new YoungTableau;
    tableau#partition =p;
    tableau#values = new MutableList from L;
    tableau
)

youngTableau(YoungTableau):= (tableau)->(
    t:= new YoungTableau; 
    for i to #keys(tableau)-1 do t#((keys(tableau))#i) = tableau#((keys(tableau))#i);
    t#values = new MutableList from tableau#values; 
    t      
)

youngTableau(Partition,MutableList):= (p,L)->(
    if(sum toList p != #L) then error " Partition and List size do not match";
    tableau:= new YoungTableau;
    tableau#partition =p;
    tableau#values = L;
    tableau
)




tableauToList = method(TypicalValue => List)
tableauToList(YoungTableau):= (tableau)->(
    
    n:= #(tableau#partition);
    d:=0;
    s:= apply(n,i->(d=d+tableau#partition#i;d));
    s = prepend(0,s);
    L := apply(n,i->(toList tableau#values)_{(s#i..(s#(i+1))-1)}); 
    L
)

listToTableau = method(TypicalValue => YoungTableau)
listToTableau List := l -> (
    
    parti := new Partition from apply (l,i->#i);
    youngTableau(parti,flatten l)
    )


YoungTableau_Sequence:= (tableau,pos) -> (
    if #pos != 2 then error "expected a sequence of length two"
    
else(
    (i,j) := pos;
    ans:= 0;
    if(i < #(tableau#partition)) then (
        
        if(j < tableau#partition#i) then ( 
            ind := sum (toList tableau#partition)_{0..(i-1)};
            ans = tableau#values#(ind+j);
        )
        else (error "Index out of bounds ");
    
    )
    else( error "Index out of bounds" );
    ans  
    )

)


YoungTableau_Sequence = (tableau,pos,e)->(
    (i,j):=pos;
    if(i < #(tableau#partition)) then (
        if(j < tableau#partition#i) then ( 
            ind := sum (toList tableau#partition)_{0..(i-1)};
            tableau#values#(ind+j)= e;
        )
        else (error "Index out of bounds ");
    
    )
    else( error "Index out of bounds" );
    e
    
    )

YoungTableau^ZZ := (tableau,i) -> (
    ans:=0;
    if i < #(tableau#partition) then (
        ind := sum (toList tableau#partition)_{0..(i-1)};
    	ans = (toList tableau#values)_{(ind..(ind + (tableau#partition#i)-1))};   
    )
    else error "Index out of bounds";
    ans
    )

YoungTableau_ZZ := (tableau,i) -> (
    ans:= 0;
    if -1< i and i < tableau#partition#0 then (
        ind:= 0;
        ans = new MutableList;
        for j to #(tableau#partition)-1 when (tableau#partition#j > i) do(
            ans#j = tableau#values#(ind+i);
            ind = ind+(tableau#partition#j);
        );
        ans = toList ans;
	ans
    )
)

YoungTableau == YoungTableau := (S,T) -> (
    
    toList S#partition == toList T#partition and toList S#values == toList T#values
    )


entries YoungTableau := tableau -> toList tableau#values

numcols YoungTableau := tableau -> tableau#partition#0

numrows YoungTableau := tableau -> #tableau#partition

size YoungTableau := tableau -> sum toList tableau#partition

net YoungTableau := tableau ->(
    l := tableauToList tableau;
    corner := #(tableau#partition) ;
    tableauNet:= "|" ;
    for i to corner-2 do tableauNet = tableauNet || "|"; 
    
    for i to numcols tableau-1 do ( 
	column:= tableau_i;
	columnString := " "|column#0;
	for j from 1 to #column-1 do columnString = columnString|| " "|column#j;
	for j from #column to corner -1 do columnString = columnString || " |" ;
    	corner = #column;
	tableauNet = tableauNet|columnString;
	);
    columnString := " |";
    for i to corner-2 do columnString= columnString || " |"; 
    tableauNet = tableauNet | columnString;
    tableauNet
)



TableauList = new Type of MutableHashTable
tableauList = method(TypicalValue => TableauList)

tableauList Partition :=    p-> (
lista := new TableauList;
lista#partition = p;
lista#matrix = mutableMatrix(ZZ,multinomial(p),sum(toList p));
lista#length = 0;
lista
)


tableauList (Partition,ZZ) :=    (p,n)-> (
lista := new TableauList;
lista#partition = p;
lista#matrix = mutableMatrix(ZZ,n,sum(toList p));
lista#length = 0;
lista
)




toListOfTableaux = method()
toListOfTableaux TableauList := tableaux -> (
    apply(tableaux#length,i-> youngTableau(tableaux#partition,flatten entries tableaux#matrix^{i}))
    )

addTableau = method(TypicalValue => ZZ)
addTableau(TableauList,YoungTableau):= (tableaux,tableau) ->(
   scan(0..sum(toList tableau#partition)-1, i-> (tableaux#matrix)_(tableaux#length,i) = tableau#values#i);
   tableaux#length = tableaux#length+1;
   tableaux
)

addTableau(TableauList,List):= (tableaux,tableau) -> (
    scan(0..sum(toList tableaux#partition)-1, i-> (tableaux#matrix)_(tableaux#length,i) = tableau#i);
   tableaux#length = tableaux#length+1;
   tableaux
    )

net TableauList := tableaux -> (
    net toListOfTableaux tableaux
    )

TableauList_ZZ := (tableaux,n) -> (
     youngTableau(tableaux#partition,flatten entries tableaux#matrix^{n})
    ) 

getRow = method()
getRow (TableauList,ZZ) := (tableaux,i)-> flatten entries tableaux#matrix^{i}

previousElementInRow = method(TypicalValue => ZZ)
previousElementInRow(YoungTableau,HashTable):= (tableau,ind)->(
    
    e := -1;
    if ind#column!=0 then e = tableau#values#(ind#index-1);
    e
)

previousElementInColumn = method(TypicalValue => ZZ)
previousElementInColumn(YoungTableau,HashTable):= (tableau,ind)->(
    e:=-1;
    p:= tableau#partition;
    if ind#row!=0 then e = tableau#values#(ind#index-p#(ind#row-1));
    e
)

nextIndex = method()
nextIndex (HashTable,Partition)  := (ind,p)->(
    
    if p#(ind#row)-1==(ind#column)  then (
	ind = hashTable {row => ind#row+1,column => 0, index => ind#index+1 })
    else (
	ind = hashTable {row => ind#row,column => ind#column+1, index => ind#index+1 }
	);
    ind
)

maxPossibleNumber = method(TypicalValue => ZZ)
maxPossibleNumber(YoungTableau,HashTable):= (tableau,ind) ->(
  s:=(size tableau)-(tableau#partition)#(ind#row);
  s= s+ind#column;
  s
)



tabloids = method(TypicalValue => TableauList)
tabloids(Partition) := p->(
    size:= multinomial p;
    tableaux :=tableauList(p,size);
    if(size!= 0) then(
    nums := toList(0..sum toList p - 1);
    tableau:= youngTableau(p);
    ind := hashTable {row=> 0, column => 0, index => 0};
    recursiveTabloids(nums,tableau,tableaux,ind);
    );
    tableaux
)


recursiveTabloids = method(TypicalValue => TableauList)

recursiveTabloids(List,YoungTableau , TableauList,HashTable):= (numbers, tableau, tableaux,ind) -> (
    maximum:= maxPossibleNumber(tableau,ind);
    newInd:= nextIndex (ind,tableau#partition);
    for i from 0 to #numbers-1 when (numbers#i < maximum+1)  do (
        
            if(numbers#i>previousElementInRow(tableau,ind)) then
            (
		tableau#values#(ind#index) = numbers#i;
		numbers2 := delete(numbers#i,numbers);
                if newInd#index == sum toList tableau#partition then addTableau(tableaux,tableau)
		else recursiveTabloids(numbers2,tableau,tableaux,newInd);
            );
        );  
    tableaux
    )



maxPossibleNumberStandard = method(TypicalValue => ZZ)
maxPossibleNumberStandard(YoungTableau,HashTable):= (tableau,ind) ->(
  s:=sum(toList tableau#partition);
  for i from ind#row to #(tableau#partition)-1 when (tableau#partition#i > ind#column ) do (    
     s = s - (tableau#partition#i)+ind#column;
  );
  s
)



standardTableaux = method(TypicalValue => TableauList)
standardTableaux(Partition) := p->(
    size:=sum(toList p);
    tableaux :=tableauList(p,hookLengthFormula(p));
    if size != 0 then(
    nums := toList(0..size-1);
    tableau:= youngTableau(p);
    ind := hashTable {row=> 0, column => 0, index => 0};
    recursiveStandardTableaux(nums,tableau,tableaux,ind);
    );
    tableaux
)

recursiveStandardTableaux = method(TypicalValue => TableauList)
recursiveStandardTableaux(List,YoungTableau,TableauList,HashTable):= (numbers, tableau, tableaux,ind) -> (
    maximum:= maxPossibleNumberStandard(tableau,ind);
        newInd:= nextIndex (ind,tableau#partition);
	for i from 0 to #numbers-1 when (numbers#i < maximum+1)  do (
        
            if(numbers#i>previousElementInRow(tableau,ind) and numbers#i>previousElementInColumn(tableau,ind) ) then
            (
		tableau#values#(ind#index)= numbers#i;
		numbers2 := delete(numbers#i,numbers);
                if newInd#index == sum toList tableau#partition then addTableau(tableaux,tableau) 
		else recursiveStandardTableaux(numbers2,tableau,tableaux,newInd);
            );
        );
    tableaux  
)


maxPossibleNumbersSemistandard = method(TypicalValue => ZZ)
maxPossibleNumbersSemistandard(YoungTableau,HashTable,ZZ):= (tableau,ind,n)-> (
    
  s:=n;
  s = s - #(tableau_(ind#column))+ind#row;
  s
    )

semistandardTableaux = method(TypicalValue => TableauList)
semistandardTableaux(Partition,ZZ) := (p,n)->(
    size:=sum(toList p);
    tableaux :=tableauList(p,n^size);
    if size!=0 then (
    nums := toList(0..size-1);
    tableau:= youngTableau(p);
    ind := hashTable {row=> 0, column => 0, index => 0};
    recursiveSemistandardTableaux(n,tableau,tableaux,ind);
    );
    tableaux
)

recursiveSemistandardTableaux = method(TypicalValue => TableauList)
recursiveSemistandardTableaux(ZZ,YoungTableau,TableauList,HashTable):= (maxNumber, tableau, tableaux,ind) -> (
    newInd:= nextIndex (ind,tableau#partition);
    maximum:= maxPossibleNumbersSemistandard(tableau,ind,maxNumber);
    for i from max(previousElementInRow(tableau,ind),0 ,previousElementInColumn(tableau,ind)+1) to maximum do(   
	tableau#values#(ind#index)= i;
	if newInd#index == sum toList tableau#partition then tableaux = addTableau(tableaux,tableau)
	    else recursiveSemistandardTableaux(maxNumber,tableau,tableaux,newInd);
        );
    tableaux
    )


readingWord = method()
readingWord YoungTableau := tableau -> (
    
    flatten apply (numcols tableau, i-> reverse tableau_i)
    )

wordToTableau = method()
wordToTableau (Partition,List) := (p,word)->(
    
    conj := conjugate p;
    suma := 0;
    tableau := youngTableau p;
    for i to #conj-1 do(
	scan(conj#i, j -> tableau_((conj#i)-1-j,i)=word#(suma+j));
	suma = suma+conj#i;
	);
    tableau
    )
    

indexTableau = method()
indexTableau(YoungTableau):= tableau -> (
    
    word := readingWord tableau;
    ind := 0;
    m:=0;
    index := new MutableList;
    while m < sum(toList tableau#partition) do(
        for i to #word -1 do(
            if(word#i == m) then (
                m = m+1;
                index#i = ind;
                )
        );
            ind = ind +1;
        );
    wordToTableau (tableau#partition,toList index)
)



rowPermutationTableaux = method()
rowPermutationTableaux(YoungTableau) := (tableau)->(
    size:=sum(toList tableau#partition);	
    numbers:= apply (#(tableau#partition), i -> new MutableHashTable from tally tableau^i);
    maxTableaux:=product(numbers, tal->  multinomial( tally values tal));
    tableaux :=tableauList(tableau#partition,maxTableaux);
    newTableau:= youngTableau(tableau#partition,toList ( size:(-1) ) );
    recursiveRowPermutationTableaux((#tableau#partition-1,0),numbers,newTableau,tableaux);
    toListOfTableaux tableaux
)

recursiveRowPermutationTableaux = method(TypicalValue => TableauList)
recursiveRowPermutationTableaux(Sequence, List,YoungTableau,TableauList):= (pos,numbers, tableau, tableaux) -> (
    element:=0; 
    (row,col):= pos;
    nextPos := (0,0);
    if col + 1 == tableau#partition#row then nextPos = (row-1,0) else nextPos = (row,col+1);
    for j in keys(numbers#row) do (
	if not any (tableau_col, i-> i == j) then (
	    tableau_(row,col)=j;
	    numbers#row#j = numbers#row#j-1;
	    if(numbers#row#j == 0 ) then remove (numbers#row, j);
	    if nextPos#0 == -1 then addTableau(tableaux,tableau) else recursiveRowPermutationTableaux(nextPos,numbers,tableau,tableaux);
	    if numbers#row#?j then numbers#row#j = numbers#row#j+1 else numbers#row#j = 1;
	    tableau_(row,col)=-1;
	    );
	);   
)



hookLengthFormula = method(TypicalValue =>ZZ)
hookLengthFormula Partition := parti -> (
    
    prod := (sum toList parti)!;
    conj:= conjugate parti;
   
   for i to #parti-1 do (
       for j to parti#i-1 do(
	   prod = prod//(parti#i-j+conj#j-i-1);
	   );
       
       );
        prod
)


cycleDecomposition = method()
cycleDecomposition List := perm ->(
    visited:= new MutableList;
    for i to #perm-1 do (visited#i = 0);
    
    ind:= 0;
    visited#(ind) = 1;
    cycles:= {};
    while ind<#perm do (
        newInd:= perm#(ind);
        cycle := while newInd != ind list newInd do(
            visited#(newInd) = 1;
            newInd = perm#(newInd);
        );
    	cycle = prepend(ind,cycle);
    	cycles = append(cycles,cycle);
        
        for i from ind to #perm-1 when visited#i==1 do 
        (
            ind = i;
        );
        ind = ind+1;
        visited#(ind) = 1;
    );
    cycles
)

conjugacyClass = method()
conjugacyClass List := perm -> (
    
    cycles:= cycleDecomposition perm;
    new Partition from (reverse sort apply (cycles, c -> #c))
    )


multinomial = method(TypicalValue => ZZ)
multinomial(Tally) := (p)->(
    n:= sum p;
    r:= n!;
    r// product (keys p, i-> (i!)^(p#i))
  )

multinomial( List) := (c)->(
    r:= (sum c)!;
    for i to #c-1 do r = r//((c#i)!);  
    r
  )

multinomial Partition := p -> (
    multinomial toList p
    )    
 


extendPermutation = method(TypicalValue => List)
extendPermutation(ZZ, List) := (n,per) -> (
    numbers := sort(per);
    j := 0;
    result := new MutableList;
    result#(n-1) = 0;
    for i from 0 to n-1 do (
        if(j < #per and i == numbers#j) then 
        (
	    result#(i) = per#j;
            j = j+1;
        )
        else result#(i) = i;
    );
    result = toList result;
    result
)

extendedPermutations = method()
extendedPermutations(ZZ,List ):= (n,numbers) -> (
    perms:= permutations(numbers);
    apply(perms, p-> extendPermutation(n,p)) 
    )


directProductOfPermutations = method(TypicalValue =>List)
directProductOfPermutations(List,List):= (A,B) ->(
   flatten apply(A, a->apply(B,b->a_b))   
)

columnStabilizer=method(TypicalValue => List)
columnStabilizer(YoungTableau):= (tableau) ->(
    n:= sum toList tableau#partition;
    stab:=extendedPermutations(n,tableau_0);
    for i from 1 to tableau#partition#0-1 do(
	stab=directProductOfPermutations(stab,extendedPermutations(n,tableau_i));
	);
    stab
)

rowStabilizer=method(TypicalValue => List)

rowStabilizer(YoungTableau):= (tableau) ->(
    n:= sum toList tableau#partition;
    stab:=extendedPermutations(n,tableau^0);
    for i from 1 to #tableau#partition-1 do(
	stab=directProductOfPermutations(stab,extendedPermutations(n,tableau^i));
	);
    stab
)


permutationSign =method(TypicalValue=>ZZ)
permutationSign Partition := p -> (
    
    tal := tally toList p;
    product(keys tal, i->(-1)^((i+1)*tal#i))
)

permutationSign List := p -> (
    
    permutationSign conjugacyClass p
    )






combinations = method()
combinations(ZZ,ZZ):= (n,m)->(
    combs:=tabloids new Partition from {m,n-m};
    apply (combs#length, i-> flatten entries combs#matrix^{i})	
)   


SpechtModuleElement = new Type of HashTable 

spechtModuleElement = method()
spechtModuleElement (YoungTableau, QQ) := (tableau,coef)-> (
    new SpechtModuleElement from hashTable {partition => tableau#partition, 
	values => new MutableHashTable from hashTable {toList tableau#values => coef}} 
)

spechtModuleElement (YoungTableau, ZZ) := (tableau,coef)-> (
    new SpechtModuleElement from hashTable {partition => tableau#partition, 
	values => new MutableHashTable from hashTable {toList tableau#values => coef}} 
)

spechtModuleElement YoungTableau := tableau -> spechtModuleElement(tableau,1)

spechtModuleElement (Partition, MutableHashTable):= (p,v) ->(
    new SpechtModuleElement from hashTable {partition => p, values => v}
    )

netTerm = method()
netTerm (YoungTableau,ZZ) := (tableau,coef)-> (
    
    if coef  == 0 then 0 
    else if coef == 1 then net tableau
    else if coef == -1 then "- " | net tableau
    else coef | " " |net tableau    
    )

QQ * SpechtModuleElement := (c,element) ->(
     spechtModuleElement (element#partition, new MutableHashTable from applyValues (new HashTable from element#values,v-> if c!= 0 then v * c else continue))
    )

ZZ * SpechtModuleElement := (c,element) ->(
    spechtModuleElement (element#partition, new MutableHashTable from applyValues (new HashTable from element#values,v-> if c!= 0 then v * c else continue))
    )



trim SpechtModuleElement := A -> scan (keys(A#values), tabloid -> if A#tabloid == 0 then remove (A#values,tabloid))



SpechtModuleElement + SpechtModuleElement := (A,B)-> (
     if(A#partition===B#partition) then (
     	 v := merge(A#values,B#values,(i,j)->(if i+j != 0 then i+j else continue));
	spechtModuleElement(A#partition,v)
     ) else error "The elements do not belong to the same SpechtModule"
     
    )

SpechtModuleElement - SpechtModuleElement := (A,B)-> (
     A +(-1)*B
    )

terms SpechtModuleElement:= A -> (
    apply(keys A#values, tabloid-> (youngTableau(A#partition,tabloid),A#values#tabloid))
    )

List SPACE SpechtModuleElement:= (perm,element)->(
    vals := applyKeys(new HashTable from element#values, t->(
	    perm_t));
    spechtModuleElement(element#partition,new MutableHashTable from vals)
    )

net SpechtModuleElement := A -> (
    netElement :=  net {};
    tabloids := sort apply(keys A#values, t->  youngTableau(A#partition,t));
    if #tabloids > 0 then (
	t := first tabloids ; 
	netElement = netTerm(t,A#values#(toList t#values));
    for t in drop(tabloids,1)  do (
	if A#values#(toList t#values) >0 then netElement = netElement | " + " | netTerm (t,A#values#(toList t#values))
	else if A#values#(toList t#values) < 0 then netElement = netElement | " - " | netTerm (t,-(A#values#(toList t#values)));
	);
    );
    netElement
    )

straighteningAlgorithm = method(TypicalValue=> SpechtModuleElement)
straighteningAlgorithm(SpechtModuleElement) := (element)->(
    sortColumnsTableau(element); 
    notStandard := select(1, terms element, t-> firstRowDescent(t#0) > (-1,-1));
    while #notStandard != 0  do( 
	notStandard = first notStandard;
	garnir:= garnirElement(notStandard);
	element = element - garnir;
	notStandard = select(1, terms element, t-> firstRowDescent(t#0) > (-1,-1)); 	
	); 
    element 
)



straighteningAlgorithm(YoungTableau,ZZ):= (tableau,coef) ->(
    
    element := spechtModuleElement (tableau,coef);
    straighteningAlgorithm(element)
    )
 

straighteningAlgorithm(YoungTableau):= tableau -> straighteningAlgorithm(tableau,1)


garnirElement = method()

garnirElement(YoungTableau,ZZ,ZZ,ZZ):= (tableau,coef,a,b)-> (
    if(a >= #tableau#partition or b>=tableau#partition#a-1) then 
    error "Index out of bounds" else (
    	ans := {spechtModuleElement(tableau,coef)};
	if (a,b) >= (0,0) then ( 
    	conju:= conjugate tableau#partition;
    	combs:= combinations(conju#b+1,a+1);
    	AB:= (tableau_(b+1))_{0..a}|(tableau_(b))_{a..conju#b-1};
     	ans = apply (combs,comb->(
	    newTableau:= youngTableau tableau;
	    for j to a do(
		newTableau_(j,b+1)= AB#(comb#j);
	    	);
	    for j from a+1 to conju#b do (  
	    	newTableau_(j-1,b) = AB#(comb#j);
		);
	    sign:=sortColumnsTableau(newTableau);
	    spechtModuleElement (newTableau, (coef) *sign*permutationSign(conjugacyClass(comb)))
      	    ));
    	
	);
    sum ans	
	)
    )

garnirElement(YoungTableau,ZZ):= (tableau,coef) -> (
    newTableau := youngTableau tableau;
    ans:=  {spechtModuleElement(newTableau,coef)};
    (a,b):= firstRowDescent newTableau;
    garnirElement(tableau,coef,a,b)
    )

garnirElement(YoungTableau) := tableau -> garnirElement(tableau,1)

sortColumnsTableau = method()
sortColumnsTableau YoungTableau := tableau -> (
    product(tableau#partition#0,i->sortColumn(tableau,i))
    )

sortColumnsTableau SpechtModuleElement := element ->
(
    scan (keys element#values, t -> (
	    y := youngTableau(element#partition,t);
	    coef := element#values#t;
	    remove(element#values,t);
	    sign:= sortColumnsTableau(y);
	    if(element#values#?(entries y)) then element#values#(entries y) = element#values#(entries y)+sign*coef
	    else element#values#(entries y) = coef*sign;
	     )   
	);
    )

sortColumn = method()
sortColumn (YoungTableau,ZZ) := (tableau,i) -> (
    col:= tableau_i;
    sortedCol := sort col;
    scan (#col, j->(tableau_(j,i)= sortedCol#j));
    index := hashTable apply (#sortedCol,i-> sortedCol#i => i);
    permutation:= apply(col,a->index#a );
    permutationSign(permutation)
    )

rsortList = method()
rsortList List := l -> (
    sortedList := rsort l;
    index := hashTable apply (#sortedList,i-> sortedList#i => i);
    permutation:= apply(l,a->index#a);
    (sortedList,permutationSign(permutation))
    )

sortList = method()
sortList List := l -> (
    sortedList := sort l;
    index := hashTable apply (#sortedList,i-> sortedList#i => i);
    permutation:= apply(l,a->index#a);
    (sortedList,permutationSign(permutation))
    )


YoungTableau ? YoungTableau := (tableau1,tableau2)-> rowDescentOrder(tableau1,tableau2)

rowDescentOrder = method()
rowDescentOrder(YoungTableau,YoungTableau):= (tableau1,tableau2)-> (
    
    ans:= 0;
    if(firstRowDescent tableau1 < firstRowDescent tableau2) then (
	
	ans= symbol <;
	)
    else if ( firstRowDescent tableau1 > firstRowDescent tableau2) then (
	
	ans = symbol >;
	
	)
    else (
	
	ans = toList tableau1#values ? toList tableau2#values
	
	);
    
    ans
    
    )


firstRowDescent= method()
firstRowDescent YoungTableau := tableau -> (
    
    parti := conjugate(tableau#partition);
    (a,b):= (#parti,0);
    if not any(#parti,i->(b = i; any(parti#i, j-> (a = j;i+1 < tableau#partition#j and tableau_(j,i)>tableau_(j,i+1))))) then
    	(a,b) = (-1,-1);
    (a,b)
    )


toVector = method()
toVector (SpechtModuleElement,HashTable):= (element,index) -> (
    ans:=mutableMatrix (QQ,1,#index);
    scan(keys element#values, t-> ans_(0,index#t)= element#values#t);
    matrix ans
    )

toVector (SpechtModuleElement) := element -> (
    stan:= standardTableaux element#partition;
    index:= hashTable apply (stan#length,i->(flatten entries stan#matrix^{i} => i ));
    toVector(element,index)
    )



permutationMatrix = method()
permutationMatrix List := (perm) -> (
    n:= #perm;
    mat:=mutableMatrix(QQ,n,n);
    for  i to #perm-1 do (
	    mat_(i,perm#i)=1
	);
    matrix mat
    )

powerSumSymmetricPolynomials = method()
powerSumSymmetricPolynomials PolynomialRing := R -> (
    apply(numgens R, i-> (sum (gens R, x -> x^(i+1))))
    )

elementarySymmetricPolynomials = method()
elementarySymmetricPolynomials PolynomialRing := R -> (
    p := new Partition from {numgens R};
    ans := new MutableList from (numgens R):0;
    for i to numgens R -1 do (
	l := toList ( (i+1):1 );
	l = join(l , toList ( (numgens R - 1 -i):0));
	y := youngTableau(p,l);
	ele := rowPermutationTableaux y;
	ans#i = sum (ele, t-> product(0..numgens R -1, j-> R_j^(t_(0,j)) ))
	);
    toList ans
    )
cardinalityOfConjugacyClass = method(TypicalValue => ZZ)
cardinalityOfConjugacyClass(Partition) := p -> (
    tal := tally(toList p);
    base := keys(tal);
    prod := product ( base, n-> ( n^(tal#n) ) * (tal#n)! );
    (sum toList p)!//prod
)



matrixRepresentation = method()
matrixRepresentation(List,TableauList) := (permutation,standard)-> ( 
    
   index:= hashTable apply (standard#length,i->(flatten entries standard#matrix^{i} => i ));
   transpose matrix apply(standard#length,i->(
	   element:= spechtModuleElement(standard_i);
	   {toVector (straighteningAlgorithm (permutation element), index )})
       )
 
)

matrixRepresentation(TableauList) := (standard)-> (
    hashTable apply(permutations sum toList standard#partition, perm-> perm=> matrixRepresentation(perm,standard))
    )

matrixRepresentation(List,Partition) := (permutation, parti)->(
    standard := standardTableaux parti;
    matrixRepresentation(permutation,standard)
    )

matrixRepresentation(Partition) := (parti)->(
    standard := standardTableaux parti;
    matrixRepresentation(standard)
    )


permutationMap = method()
permutationMap(List,PolynomialRing) := (permutation,R)->(
    generatorList:= apply(permutation,i->R_(i) );
    map(R,R,matrix{generatorList})
)

permutePolynomial = method()
permutePolynomial (List, RingElement) := (permutation,polynomial)-> (
    if(isPolynomialRing ring polynomial) then
    (permutationMap(permutation,ring polynomial)) polynomial
    
    else error "argument is not a polynomial"
)


permutePolynomial(List,Product) := (permutation,polynomial) -> (
    new Product from apply (toList polynomial,f-> permutePolynomial(permutation,f))
    )

permutePolynomial(List,Sum) := (permutation,polynomial) -> (
    new Sum from apply (toList polynomial,f-> permutePolynomial(permutation,f))
    )

permutePolynomial(List,Power) := (permutation,polynomial) -> (
    new Power from {permutePolynomial(permutation,polynomial#0),polynomial#1}
    )

permutePolynomial(List,Minus) := (permutation,polynomial) -> (
    new Minus from {permutePolynomial(permutation,polynomial#0)}
    )
vandermondeDeterminant = method(Options => {AsExpression => false})
vandermondeDeterminant(List,PolynomialRing):= o-> (lista,R)->(
    variables := apply(lista,i-> R_i);
    if o.AsExpression then product flatten apply (#lista, i->toList apply( (i+1)..(#lista-1),j-> new Power from {(variables#j-variables#i),1} ) )
    else product flatten apply (#lista, i->toList apply( (i+1)..(#lista-1),j-> (variables#j-variables#i) ) )
    )


SpechtPolynomial = new Type of Expression

net SpechtPolynomial := polynomial -> (
    str := " "|net(toList polynomial#1);
    str = str||"S";
    str = str||(" "|net(polynomial#0));
    str^1
    )

ring SpechtPolynomial := exp -> exp#2

value SpechtPolynomial := exp ->
(
    spechtPolynomial(youngTableau(exp#1,exp#0),ring exp)
    ) 

permutePolynomial (List,SpechtPolynomial) := (permutation, polynomial) -> new SpechtPolynomial from {(polynomial#0)_permutation, polynomial#1,polynomial#2} 



spechtPolynomial = method(Options => {AsExpression => false})
spechtPolynomial ( YoungTableau, PolynomialRing ) := o->(tableau, R)-> (
    product (numcols tableau, i->vandermondeDeterminant(tableau_i,R,AsExpression => o.AsExpression))
    )

spechtPolynomials = method(Options => {AsExpression => false})
spechtPolynomials (Partition,PolynomialRing):= o->(partition,R)-> (
    standard:= standardTableaux partition;
    firstPolynomial:= spechtPolynomial(standard_0,R,AsExpression => o.AsExpression);
    hashTable apply (standard#length, i-> getRow(standard,i) => permutePolynomial(getRow(standard,i) , firstPolynomial) )
    )


indexMonomial = method()
indexMonomial(YoungTableau, YoungTableau, PolynomialRing) := (S,T,R) -> (
    ind := indexTableau S;
    monomial:= 1_R;
    if(toList S#partition == toList T#partition) then (
    	monomial = product(size S, i -> R_((entries T)#i)^( (entries ind)#i) )
    	) else error "tableaux 1 and 2 do not have the same shape";
    monomial
    )

higherSpechtPolynomial = method(Options => {AsExpression => false , Robust => false})
higherSpechtPolynomial(YoungTableau, YoungTableau, PolynomialRing) := o-> (S,T,R)->(
    if toList S#partition != toList T#partition then error "tableau shapes of S and T do not match";
    if size S != numgens R then error "number of generators must be equal to the size of the tableaux";
    ans:= R_0;
    
    if(o.Robust) then (
	monomial := indexMonomial(S,T,R);
    	sym:= sum (rowStabilizer T, sigma-> permutePolynomial(sigma,monomial));
	sym = sym//leadCoefficient sym;
    	polynomial:= sum (columnStabilizer T, tau -> permutationSign(tau)*permutePolynomial(tau,sym));
	if o.AsExpression then ans = factor polynomial else ans = polynomial 
    	)
    else (
	
	rowPermutations := rowPermutationTableaux indexTableau S;
	ans = spechtPolynomial(T,R,AsExpression =>o.AsExpression)*sum(rowPermutations, tab -> product apply (numcols S, i->
		(
		    (sortedList,sign) := sortList(tab_i);
		    sortedList = sortedList  -  toList (0..(#(tab_i)-1));
		    sortedList = reverse sortedList;
		    firstZero := position(sortedList,i->i==0);
		    lastNonZero:= 0;
	 	    if (firstZero === null) then lastNonZero = #sortedList-1 else lastNonZero = firstZero -1;
		    partition:= new Partition from sortedList_{0..lastNonZero};
		    sign*schurPolynomial(T_i,partition,R,AsExpression => o.AsExpression)
		    )))
	);
    ans
   )


    
higherSpechtPolynomials = method(Options => {AsExpression => false , Robust => false})
higherSpechtPolynomials(Partition,PolynomialRing):= o-> (partition,R) -> (
    
    standard:= standardTableaux partition;
    hashTable apply(standard#length, i-> getRow(standard,i) => higherSpechtPolynomials(standard_i,standard,R, Robust => o.Robust, AsExpression => o.AsExpression))
    )

higherSpechtPolynomials(YoungTableau,PolynomialRing):= o->(S,R)-> (
    standard:= standardTableaux S#partition;
    higherSpechtPolynomials(S,standard,R,AsExpression => o.AsExpression, Robust => o.Robust)
    )


higherSpechtPolynomials(YoungTableau,TableauList,PolynomialRing):= o->(S,standard,R)-> (
    firstPolynomial:= higherSpechtPolynomial(S,standard_0,R,Robust => o.Robust, AsExpression => o.AsExpression);
    hashTable apply (standard#length, i-> getRow(standard,i)=> permutePolynomial(getRow(standard,i),firstPolynomial))
    )

higherSpechtPolynomials PolynomialRing := o-> R -> (
     partis := partitions numgens R;
     hashTable apply(partis,p-> p=> higherSpechtPolynomials(p,R,Robust => o.Robust, AsExpression => o.AsExpression))
    )

generalizedVandermondeMatrix = method()
generalizedVandermondeMatrix(List,List,PolynomialRing):= (indices, exponents, R) -> (
    if #indices != #exponents then error "number of indices and exponents does not match";
    M := matrix apply (exponents, e-> apply (indices, i-> (R_i)^e));
    M
    )

SchurPolynomial = new Type of Expression

permutePolynomial(List,SchurPolynomial) := (permutation,polynomial) -> (
    new SchurPolynomial from {permutation_(polynomial#0),polynomial#1,polynomial#2}
    )


net SchurPolynomial := expr -> (
    str := " "|net(toList expr#1);
    str = str||"s";
    str = str||(" "|net(expr#0));
    str^1
    )

ring SchurPolynomial := exp -> exp#2

value SchurPolynomial := exp ->
(
    schurPolynomial(exp#0,exp#1,ring exp)
    ) 

schurPolynomial = method(Options => {AsExpression => false, Strategy=>"semistandard_tableaux"})
schurPolynomial(List,Partition,PolynomialRing) := o->(indices, partition, R) -> (
    ans := 0;
    ans= 1_R;
    if #indices < #partition then error "size of indices and exponents does not match"; 
    if o.Strategy == "determinant" then (
	exponents := join (toList (#indices - #partition:0), reverse toList partition) + toList 0..(#indices-1);
	ans = determinant generalizedVandermondeMatrix(indices,exponents,R)// vandermondeDeterminant(indices,R))
    else if o.Strategy == "semistandard_tableaux" then (
	if #partition == 0 then ans = 1_R else (
	    semistandard := semistandardTableaux(partition, #indices );
	    ans = sum(semistandard#length, i-> product(getRow(semistandard,i),j->R_(indices#j)))
	    );
	);
    if o.AsExpression then ans = factor(ans) ;
    ans
    )

increasing = method()
increasing List := lista -> (
    all(#lista-1, i-> lista#i <= lista#(i+1))
)

decreasing = method()
decreasing List := lista -> (
    all(#lista-1, i-> lista#i >= lista#(i+1))
)


generatePermutationGroup = method()
generatePermutationGroup List := gens -> (
    	group:= hashTable apply (gens , g -> g=> 0 );
	products:= group;
	for g in keys group do products = merge (products, applyKeys (group, h-> g_h), (i,j)-> i+j);
	while #group < #products do(
	  group = products;
	  products = group;
	  for g in keys group do products = merge (products, applyKeys (group, h-> g_h), (i,j)-> i+j);
	    );
    	keys group
    )

representationMultiplicity = method()

representationMultiplicity(Tally,Partition,CharacterTable):= (tal,partition,charTable)-> (
       partis:= partitions sum toList partition;
       sum(keys tal, p ->(charTable_(partition,p)*tal#p))// sum values tal
    )

representationMultiplicity(Tally,Partition):= (tal,partition)-> (
    charTable := characterTable sum toList partition;    
    representationMultiplicity(tal,partition,charTable)
    )


vectorToPolynomial = method()
vectorToPolynomial(List,HashTable,TableauList):= (vector, basis,standard)->(
   	sum ( #vector, i-> if(vector#i == 0) then 0 else vector#i*basis#( getRow(standard,i))  )	
    )

secondaryInvariants = method(Options => { AsExpression => false, Robust => false })
secondaryInvariants(List,PolynomialRing):= o->(gens,R)-> (
    	if #gens#0 != numgens R then error "the size of the elements does not match the number of generators of R";
	H := generatePermutationGroup gens;
	tal := tally apply (H,h->conjugacyClass h);
       	partis := partitions numgens R;
	charTable := characterTable numgens R;
	hashTable apply (partis, p-> (
		multi := representationMultiplicity(tal,p,charTable);
		print(p,"Ambient_Dimension",hookLengthFormula(p),"Rank",multi);
		if multi == 0 then p=> {}
		else (
		    standard := standardTableaux p;
		    isotypicalComponentBasis := higherSpechtPolynomials(p,R,AsExpression => o.AsExpression, Robust => o.Robust);
		    
		    if multi==standard#length then (
		       
			index:= hashTable apply (standard#length, i-> getRow(standard,i)=> i);
			p => hashTable apply(keys isotypicalComponentBasis, S->( S=> 
				applyKeys(isotypicalComponentBasis#S, T-> index#T) ) )			 
		    ) else
		    (
		    	V:=(coefficientRing R)^(standard#length);
			for h in gens do (
			    M:= matrixRepresentation(h,standard);
			    V = intersect(V,ker ( M - id_(source M ) ) );
			    );
			vectors:= generators V;
			p => hashTable apply (keys isotypicalComponentBasis,S-> ( S => 
			       hashTable apply (numColumns vectors, i-> i=> 
				   (vectorToPolynomial(flatten entries vectors_{i},isotypicalComponentBasis#S,standard ))) ))
				
		    )
		)
	    )
	)
    )
 
   
beginDocumentation()

multidoc ///
    Node
    	Key
	    SpechtModule
	Headline
    	    a package for constructing Specht Modules
	Description
	    Text
	    	{\em SpechtModule} calculates many objects related to the irreducible representations of the symmetric functions.
    		This construction is used to implement an algorithm in invariant theory which calculates efficiently the secondary
    		invariants of any permutation group.
		
		The main features of the package include a method for calculating the character table of $S_n$, algorithms for
    		calculating list of tableaux given a partition (tabloids, standard tableaux and semistandard tableaux among others)
    		an implementation of the straightening algorithm which includes an implementation of the Garnir element given a tableau
    		an a row descent. Methods for calculating Higher Specht Polynomials which give a basis of
    		the Specht Modules that arise in the coinvariant ring of $S_n$ which is the quotient $k[x_1,..,x_n]/({\rm Sym}(n)^+)$. 
    		And finally methods for calculating the secondary invariants described above.	    
	Caveat	  
	    An improvement can be made by finding an efficient way to calculate or represent Schur Polynomials
 
    Node
    	Key
	    CharacterTable
	    (net, CharacterTable)
	Headline
    	    the class of character tables
	Description
	    Text
	    	This type represents the character table of a symmetric group. It is implemented as a
    		hash table that stores the list of partitions, the size of the table and a
    		matrix which stores the values of the table.	    
	    
	    Example	
		charTable = characterTable 5
   		a = new Partition from {3,1,1}; b = new Partition from {1,1,1,1,1}
		peek charTable	
 	SeeAlso
 	    characterTable

    Node
    	Key
	    (symbol _,CharacterTable, Sequence)
	Headline
    	    retrieves an entry from the character table
	Usage
	    charTable_(a,b)
	Inputs
	    charTable:CharacterTable
	    pos:Sequence
	    	the position (a,b) to be consulted. It can be given by either a number or a Partition	
	Outputs
	    :ZZ
	    	the number in the position
	Description
	    Example			
		charTable = characterTable 5
   		a = new Partition from {3,1,1}; b = new Partition from {1,1,1,1,1}
		charTable_(0,0)
		charTable_(a,b)
 	SeeAlso
 	    characterTable
    Node
    	Key
	    (innerProduct,ZZ,MutableMatrix,MutableMatrix)
	    innerProduct
	Headline
	    calculates the inner product for the characters of S_n
	Usage
	    innerProduct(n,X,Y)
	Inputs
	    n:ZZ
	    	the degree of the symmetric group
	    X:MutableMatrix
	    	a matrix row that represents a character of S_n
	    Y:MutableMatrix
	    	a matrix row that represents a character of S_n
	Outputs
	    :ZZ
	    	the inner product of the two characters X and Y
	Description
	    Text
	    	The character table for two characters $X$ and $Y$ of $G$ is calculated using the formula 
	    	$<X,Y> = \sum_{g \in G} X(g)Y(g) = \sum_{C \in Cl(G)} |C|X(g_C)Y(g_C) $
	    	where the second sum is taken over all conjugacy classes of $G$ and $g_c$ is an element
		in the conjugacy class.
	    	
		 As an example we calculate the inner product between the character of the 
		 regular representation of $S_4$ and the character indexed by partition {2,1,1}.
	    Example
	    	n = 4
	    	X = mutableMatrix  {{0,0,0,0,24}} 
		Y = mutableMatrix  {{1,0,-1,-1,3}}
	        innerProduct(4,X,Y)
	    Text
	    	As expected this inner product is equal to 3.
    Node
    	Key
	    ((symbol _,symbol =),CharacterTable, Sequence)
	Headline
    	    modifies an entry from the character table
	Usage
	    charTable_(a,b)=e
	Inputs
	    charTable:CharacterTable
	    pos:Sequence
	    	the position (a,b) to be consulted. It can be given by either a number or a Partition	
	    e:Thing
	    	the number to be put in the position
	Outputs
	    :ZZ
	    	the number in the position
	Description
	    Example			
		charTable = characterTable 5
   		a = new Partition from {3,1,1}; b = new Partition from {1,1,1,1,1}
		charTable_(0,0)=-100
		charTable_(a,b)=100
		charTable
 	SeeAlso
 	    characterTable


    Node
    	Key
	    characterTable
    	    (characterTable,ZZ)
	Headline
    	    returns the character table of the symmetric group
	Usage
	    characterTable n
	Inputs
	    n:ZZ
	    	the degree of the symmetric group
	Outputs
	    :CharacterTable
	    	the character table with the irreducible characters of $S_n$ indexed by partitions
	Description
	    Text
	    	This method construct the irreducible characters of $S_n$. The method works by recursively calculating the
		character tables for the permutation modules of $S_n$. Then applying Gram-Schimdt algorithm to this
		characters using the inner product of characters we obtain the irreducible characters of $S_n$	    
 	SeeAlso
 	    CharacterTable
    Node
    	Key
	    YoungTableau
	    (net,YoungTableau)
    	Headline
    	    the class of Young Tableaux
    	Description
             Text
    	    	This type represents a Young Tableau. It is implemented as a MutableHashTable. This hash table stores
    		a partition that represents a shape of the tableau. The filling of the tableau is stored as a
    		mutable list.
    	    Example
    	    	p = new Partition from {3,2}
    		y = youngTableau(p,{1,0,2,3,4})
    		peek y
    Node
    	Key
	    youngTableau
	    (youngTableau,Partition)
    	    (youngTableau,Partition,List)
    	    (youngTableau,Partition,MutableList)
    	Headline
    	    the constructor method for the class YoungTableau
    	Usage
    	    youngTableau(p,l)
    	Inputs
    	    p:Partition
    	    	the shape of the tableau
    	    l:List
    	    	the filling of the tableau, if it is not provided then it is assume that the filling is zero.
    	Outputs
    	    :YoungTableau
    	    	a young tableau with the given shape and filling
    	SeeAlso
	    YoungTableau
    Node
    	Key
    	    (youngTableau,YoungTableau)
    	Headline
    	    creates a copy of a YoungTableau object
    	Usage
    	    youngTableau(y)
    	Inputs
    	    y:YoungTableau
    	    	a Young tableau
    	Outputs
    	    :YoungTableau
    	    	a copy of y
    	Description
	    Example
	    	p = new Partition from {3,2}
		l = {2,1,0,3,4}
		y = youngTableau(p,l)
		y1 = youngTableau y
		y == y1  
    		y === y1    	
	
    Node
    	Key
    	    (tableauToList,YoungTableau)
    	    tableauToList
    	Headline
    	    converts a YoungTableau to list form
    	Usage
    	    tableauToList(y)
    	Inputs
    	    y:YoungTableau
    	    	a Young tableau
    	Outputs
    	    :List
    	    	a doubly nested list, the list of rows of the tableau
    	Description
	    Example
	    	p = new Partition from {2,2,1}
		l = {2,1,0,3,4}
		y = youngTableau(p,l)
		tableauToList y

    Node
    	Key
    	    (listToTableau,List)
    	    listToTableau
    	Headline
    	    constructs a Young Tableau from a doubly nested list of numbers
    	Usage
    	    listToTableau(l)
    	Inputs
    	    l:List
    	    	a doubly nested list of numbers
    	Outputs
    	    :YoungTableau
    	    	a Young Tableau, such that the rows corresponds to the elements of l
    	Description
	    Example
	        l = {{0,1,2},{3,4},{5}}
        	listToTableau l

    Node
    	Key
    	    (symbol _,YoungTableau,Sequence)
    	Headline
    	    retrieves the entry in cell (a,b) from a Young Tableau
    	Usage
    	    y_(a,b)
    	Inputs
    	    y:YoungTableau

    	    pos:Sequence
    	    	the position (a,b) of the cell where a is the row and b the column	    
    	Outputs
    	    :ZZ
    	    	the number in the cell at the position (a,b) of the tableau y
    	Description
	    Example
	        y = youngTableau(new Partition from {2,2},{0,2,1,3})
		y_(0,0)
		y_(1,1)

    Node
    	Key
    	    ((symbol _,symbol =),YoungTableau,Sequence)
    	Headline
    	    changes the entry in cell (a,b) from a Young Tableau
    	Usage
    	    y_(a,b) = e
    	Inputs
    	    y:YoungTableau
    	    
    	    pos:Sequence
    	    	the position (a,b) of the cell where a is the row and b the column
	    e:Thing
	    	a number, the new entry of the cell
    	Outputs
    	    :YoungTableau
    	    	the number in the cell at the position (a,b) of the tableau y
    	Description
	    Example
	        y = youngTableau(new Partition from {2,2},{0,2,1,3})
		y_(0,0)=1
		y

    Node
    	Key
    	    (symbol ^,YoungTableau,ZZ)
    	Headline
    	    retrieves a row from a Young Tableau
    	Usage
    	    y^n
    	Inputs
    	    y:YoungTableau

    	    n:ZZ
    	    	the number of the row
    	Outputs
    	    :ZZ
    	    	the number added
    	Description
	    Example
	        y = youngTableau(new Partition from {3,2},{0,2,1,3,4})
		y^0
		y^1

    Node
    	Key
    	    (symbol _,YoungTableau,ZZ)
    	Headline
    	    retrieves a column from a Young Tableau
    	Usage
    	    y_n
    	Inputs
    	    y:YoungTableau
	    
    	    n:ZZ
    	    	the number of the row
    	Outputs
    	    :ZZ
    	    	a list of the numbers that appear in row n of y
  
    	Description
    	    Example
    	    	y = youngTableau(new Partition from {3,2},{0,2,1,3,4})
		y_0
		y_1

    Node
    	Key
	    (symbol ==,YoungTableau,YoungTableau)
    	Headline
    	    checks whether two tableaux are equivalent	 
        Usage
    	    y1 == y2	
        Inputs
      	    y1:YoungTableau
      	    
	    y2:YoungTableau
  	Outputs
      	    :ZZ
            	true if the shape and filling of tableaux y1 and y2 are the same
  	Description
   	    Example
	    	y = youngTableau(new Partition from {3,2},{0,2,1,3,4})
		y1 = youngTableau(new Partition from {3,2},{0,2,1,3,4})
		y == y1
		y2 =  youngTableau(new Partition from {2,2,1},{0,2,1,3,4})
		y == y2

    Node
    	Key
    	    (entries,YoungTableau)
  	Headline
	    returns the filling of the tableau
  	Usage
    	    entries y
  	Inputs
      	    y:YoungTableau
  	Outputs
      	    :List
            	returns the filling of the tableau
  	Description
   	    Example
	    	y = youngTableau(new Partition from {3,1,1},{2,0,1,4,3})
		entries y

    Node
    	Key
    	    (numcols,YoungTableau)
  	Headline
    	    returns the number of columns of a tableau
  	Usage
    	    numcols y
  	Inputs
      	    y:YoungTableau
  	Outputs
      	    :ZZ
            	the number of columns of the tableau
       	Description
    	    Example
	    	y = youngTableau(new Partition from {2,1,1,1},{2,0,1,4,3})
		numcols y

    Node
    	Key
    	    (numrows,YoungTableau)
       	Headline
    	    returns the number of rows of a tableau
	Usage
	    numrows y
  	Inputs
    	    y:YoungTableau
	Outputs
      	    :ZZ
            	the number of rows of the tableau
  	Description
   	    Example
	    	y = youngTableau(new Partition from {2,1,1,1},{2,0,1,4,3})
		numrows y

    Node
    	Key
    	    (size,YoungTableau)
  	Headline
    	    returns the number of cells of a tableau
  	Usage
    	    size y
  	Inputs
      	    y:YoungTableau
  	Outputs
      	    :ZZ
            	the number of cells rows of the tableau
       	Description
   	    Text
       	    	The size is calculated as the sum of the numbers in the partition associated to the tableau
   	    Example
	    	y = youngTableau(new Partition from {2,1,1,1},{2,0,1,4,3})
		size y

    Node
    	Key
      	    (symbol ?, YoungTableau,YoungTableau)  
	    
  	Headline
    	    an order of YoungTableaux
  	Usage
	    y1 ? y2
	Inputs
	    y1:YoungTableau
	    y2:YoungTableau
	Outputs
	    :Boolean
	    	either "=", "<" or  ">"
	Description
    
    	    Text
    	    	The order implemented checks where is the first row descent of the tableau. Then it applies
		lexicographical order to the coordinates of these cells.
		
		If the row descent is in the same cell then the lexicographical order for the filling is outputted.
		
		This order is implemented for th net of SpechtModuleElement
		so that the terms with some row descent appear last.
		
  	    Example
    	    	p = new Partition from {2,1}
    		y1 = youngTableau(p,{1,0,2})
		y2 = youngTableau(p,{0,2,1})
    		y1 ? y2
		sort {y1,y2} 

    Node
    	Key
      	    (addTableau,TableauList,YoungTableau)  
	    (addTableau,TableauList,List)
	    addTableau
	    
  	Headline
    	    adds a Young Tableau in the list
  	Description
    
    	    Text
    	    	The following is an example of how does the method addTableau work
  	    Example
    	    	p = new Partition from {2,1}
    		y1 = youngTableau(p,{0,1,2})
		y2 = youngTableau(p,{0,2,1})
    		t = tableauList p
		addTableau(t, y1)
		addTableau(t, y2)
		addTableau(t, {1,2,0})

		peek t
 

    
    Node
    	Key
    	    TableauList
	    (net,TableauList)
	    
  	Headline
    	    the class of list of tableaux
  	Description
    
    	    Text
            	This type represents a list of tableaux of the same size. They are represented as a MutableHashTable.
		A matrix in this hash table stores the filling of every tableau in the list. This representation
		is particularly useful when only the filling of the tableau is needed.
  	    Example
    	    	p = new Partition from {2,1}
    		y1 = youngTableau(p,{0,1,2})
		y2 = youngTableau(p,{0,2,1})
		y3 = youngTableau(p,{1,2,0})
    		t = tableauList p
		addTableau(t, y1)
		addTableau(t, y2)
		addTableau(t, y3)
		peek t
 

    Node
    	Key
    	    (tableauList,Partition,ZZ)
    	    (tableauList,Partition)
    	    tableauList
  	Headline
    	    the constructor for the type TableauList
  	Usage
    	    tableauList(p,n)
    	    tableauList(p)
  	Inputs
    	    p:Partition
    	    	the shape for the tableaux
    	    n:ZZ
    	    	the number of tableaux in the list, if is not provided then the default value
	    	multinomial p is used. See multinomial.
  	Outputs
      	    :TableauList
            	an empty TableauList with space for n tableaux
  	Description
        
    	    Example
    	    	p = new Partition from {2,1}
    		y = youngTableau(p,{0,1,2})
		t = tableauList p
		addTableau(t,y)
		peek t 
		t1 = tableauList (p,5)
		addTableau(t1,y)
		peek t1

    Node
    	Key
    	    (symbol _,TableauList,ZZ)
    	    
  	Headline
    	    a method that retrieves the tableaux from the list
  	Usage
    	    tableaux_i
    	    
  	Inputs
    	    tableaux:TableauList
    	    	
    	    i:ZZ
    	    	the index of the tableau to be retrieved
  	Outputs
      	    :YoungTableau
            	the YoungTableau stored at position i
  	Description
        
    	    Example
    	    	p = new Partition from {2,1}
    		y1 = youngTableau(p,{0,1,2})
		y2 = youngTableau(p,{0,2,1})
		y3 = youngTableau(p,{1,2,0})
    		t = tableauList p;
		addTableau(t, y1);
		addTableau(t, y2);
		addTableau(t, y3);
		t_0
		t_2
		
    Node
    	Key
	    (toListOfTableaux,TableauList)
	    toListOfTableaux
	Headline
	    converts an object of type TableauList into a list of YoungTableau objects
	Usage
    	    toListOfTableaux(tableaux)
  	Inputs
    	    tableaux:TableauList
  	Outputs
    	    :List
    		a list of the tableaux stored in the TableauList  
   	Description
    	    Example
        	p = new Partition from {2,1}
    		y1 = youngTableau(p,{0,1,2})
		y2 = youngTableau(p,{0,2,1})
		y3 = youngTableau(p,{1,2,0})
    		t = tableauList p
		addTableau(t, y1)
		addTableau(t, y2)
		addTableau(t, y3)
		toListOfTableaux t

    Node 
    	Key
    	    (tabloids, Partition)
	    tabloids
    	Headline
    	    the list of tabloids for a given partition
    	Usage
    	    tabloids(p)
    	Inputs
    	    p:Partition
    	Outputs
    	    :TableauList
	    	the list of tabloids
    	Description
    	
	    Text
	    	Tabloids are the equivalence class of tableaux under the row permutation equivalence relation.
	   	Two tabloids are row permutation equivalent if one can be obtained from the other by permuting elements in its rows.
	   	For every tabloid there is a unique representative such that its rows
	   	are increasing. This representatives are the ones calculated by the method
	   	tabloids().
		
		Tabloids are the basis of the permutation modules from which the Specht Modules are constructed.	
	    Example	    
	    	p = new Partition from {3,2}
	    	tabloids p

    Node 
        Key
    	    (standardTableaux, Partition)
	    standardTableaux
    	Headline	
    	    the list of standard tableaux of shape p
    	Usage
    	    standardTableaux(p)
    	Inputs
    	    p:Partition
    	Outputs
    	    :TableauList
	    	the list of standard tableaux
    	Description    	
	    Text
    	    	The standard tableaux of a given partition $\lambda$ are tableaux of shape p.
	    	Such that they are both row and column increasing. This set of tableaux are
 	    	very important because they are in bijection with the basis of the Specht module
	    	$S^\lambda$.	    
	    	
		The method calculates this tableaux recursively filling the cells of the Ferrer diagram
	    	and checking if the rows and columns are still increasing.	
	    Example	    
	    	p = new Partition from {3,2}
	    	standardTableaux p


    Node
    	Key
    	    (semistandardTableaux, Partition, ZZ)
	    semistandardTableaux
    	Headline
    	    the list of semistandard tableaux of shape p and filling with the numbers from 0 to n-1.
    	Usage
    	    standardTableaux(p,n)
    	Inputs
    	    p:Partition
    	    	the shape of the tableaux
	    n:ZZ
	    	a limit of the range of numbers that appear in the tableaux
    	Outputs
    	    :TableauList
		the list of semistandard tableaux
    	Description
	    Text
    	    	The semistandard tableaux are tableaux that are strictly decreasing in rows and
		weakly decreasing in rows. 	
	    Example
    		p = new Partition from {3,2}
	    	semistandardTableaux (p,4)

    Node
    	Key
	    (readingWord,YoungTableau)
	    readingWord
	Headline
	    gives the reading word of a given tableau
	Usage
	    readingWord(y)
	Inputs
	    y:YoungTableau
	    	a young tableau
	Outputs
	    :List
	    	the reading word of the Young tableau
	Description
	    Text
	    	The reading word of a tableau is word obtain by reading each column from the bottom up and reading 
		the columns from left to right. The reading word is used to calculate the cocharge statistic of the given tableau.
		
	    Example
	    	p = new Partition from {3,2}
	    	y = youngTableau(p,{0,2,3,1,4})
	    	readingWord(y)


    Node
    	Key
	    (indexTableau,YoungTableau)
	    indexTableau
	Headline
	    the index tableau for a given tableau
	Usage
	    indexTableau(y)
	Inputs
	    y:YoungTableau
	Outputs
	    :YoungTableau
	    	the index tableau
    	Description
	    Text
	    	The index tableau is a filling obtained by the original tableau using the reading word.
		To every element in the reading word a number is given recursively in the following way.
		f(0) = 0 and f(k+1) = f(k) if k+1 appear to the right of k. Otherwise f(k+1)= f(k)+1.
		
		Finally the entries in the original tableau are replaced by the values of the function f.
	    
	    Example
	    	p = new Partition from {3,2}
	    	y = youngTableau(p,{0,2,3,1,4})
	    	readingWord(y)
		indexTableau(y)

    Node
    	Key
    	    (rowPermutationTableaux, YoungTableau)
	    rowPermutationTableaux
    	Headline
    	    the list of row permutations without repetitions in columns
    	Usage
    	    rowPermutationTableaux(y)
    	Inputs
    	    y:YoungTableau
    	    	a tableau, generally the index tableau of a standard tableau	

    	Outputs
    	    :List
		the list of all row permutations of the tableau
    	Description
	    Text
    	    	This list of tableaux is used to calculate more efficiently higher Specht polynomials.
		If any of the columns has a repetition then the associated term in the higher Specht polynomial
		for this row permutation is zero. This is why such permutations are omitted. 	

	    Example
		p = new Partition from {3,2}
	    	y = youngTableau(p, {0,2,1,3,4})
		ind = indexTableau y
		rowPermutationTableaux ind

    Node
    	Key
    	    (hookLengthFormula, Partition)
	    hookLengthFormula
    	Headline
    	    a formula for the number of standard tableaux
    	Usage
    	    hookLengthFormula(p)
    	Inputs
    	    p:Partition
    	    	a partition that indexes a Specht Module	

    	Outputs
    	    :ZZ
		the dimension of the Specht module S_\lambda
    	Description
	    Text
    	    	The hook length formula is a method that counts the number of
		standard tableaux of a given shape p. Therefore it counts the
		dimension of the associated Specht module.
		
	        For each Ferrer diagram and each cell (a,b) the hook at (a,b) is
		the set of cells that comprise (a,b) the cells that are below (a,b),
		and the cells that are to right of (a,b). The hook length of a hook h(a,b) is
		defined of the number of cells in the hook.
		
		If p is a partition of n then the hook length formula for p is
		$ n!/\prod_{(a,b)} h(a,b) $ 	

	    Example
		p = new Partition from {3,2}
	    	standardTableaux p
		hookLengthFormula p

    Node
    	Key
    	    (cycleDecomposition, List)
	    cycleDecomposition
    	Headline
    	    the cycle decomposition of a permutation
    	Usage
    	    cycleDecomposition perm
    	Inputs
    	    perm:List
    	    	a permutation of the list of numbers from 0 to n-1	

    	Outputs
    	    :List
		a doubly nested list with cycles of the permutation
    	Description
	    Text
    	    	Every permutation has a decomposition as the concatenation of disjoint cycles.
		This decomposition is used to calculate the conjugacy class of the permutation.
		
	    Example
		cycleDecomposition {0,1,2,3,4}		
		cycleDecomposition {1,3,2,0,4} 
		
    Node
    	Key
    	    (conjugacyClass, List)
	    conjugacyClass
    	Headline
    	    the conjugacy class of a permutation
    	Usage
    	    conjugacyClass perm
    	Inputs
    	    perm:List
    	    	a permutation of the list of numbers from 0 to n-1	

    	Outputs
    	    :Partition
		a partition that represents the conjugacy class of the permutation
    	Description
	    Text
    	    	The method first calculates the cycle decomposition of the permutation
		Then the conjugacy class is the partition given by the lengths of the
		cycles in the decomposition
		
	    Example
		cycleDecomposition {0,1,2,3,4}
		conjugacyClass 	{0,1,2,3,4}	
		cycleDecomposition {1,3,2,0,4} 
    	    	conjugacyClass 	{0,1,2,3,4}

    Node
    	Key
    	    multinomial
	    (multinomial, Tally)
	    (multinomial, List)
	    (multinomial, Partition)
    	Headline
    	    a formula for the multinomial coefficient
    	Usage
    	    multinomial(tal)
	    multinomial(l)
	    multinomial(p)
    	Inputs
    	    p:Partition
    	    	a partition	
    	    l:List
	    	a list of non negative numbers
	    tal:Tally
    	    	a tally from a list
		
    	Outputs
    	    :ZZ
		the multinomial coefficient of the given list
    	Description
	    Text
    	    	The multinomial coefficient is a generalization of the binomial coefficient.
		Given a list of number $k_1,\ldots,k_l$, the multinomial coefficient is
		$n!/(k_1!\ldots,k_l!)$ where $n = \sum k_i$. The multinomial coefficient is calculated
		because it gives the numbers of tabloids for a given partition.
		
		The list of numbers used to calculate the multinomial can be given
		as a list, a partition or a tally. This last option was added to optimize
		this calculation.

	    Example
		p = new Partition from {2,2}
	    	tabloids p
		multinomial {2,2}
		multinomial tally {2,2}


    Node
    	Key
    	    (rowStabilizer,YoungTableau )
	    rowStabilizer
    	Headline
    	    the row stabilizer of the tableau
    	Usage
    	    rowStabilizer(y)
    	Inputs
    	    y:YoungTableau
    	Outputs
    	    :List
		a dubly nested list with the permutations in the row stabilizer
    	Description
	    Text
    	    	The row stabilizer of a tableau is the group of the permutations that fixes the rows of
		the tableau. In terms of tabloids it is the stabilizer of a tabloid under the action of the
		group of permutations S_n. This group is used in the calculation of polytabloids and Specht polynomials.
		

	    Example
		p = new Partition from {2,2,1}
	    	y = youngTableau(p,{0,3,1,4,2})
		rowStabilizer y

    Node
    	Key
    	    (columnStabilizer,YoungTableau )
	    columnStabilizer
    	Headline
    	    the column stabilizer of the tableau
    	Usage
    	    columnStabilizer(y)
    	Inputs
    	    y:YoungTableau
    	Outputs
    	    :List
		a doubly nested list with the permutations in the column stabilizer
    	Description
	    
	    Example
		p = new Partition from {2,2,1}
	    	y = youngTableau(p,{0,3,1,4,2})
		columnStabilizer y
    	SeeAlso
	    rowStabilizer

    Node
    	Key
    	    permutationSign
	    (permutationSign,List )
	    (permutationSign,Partition)
    	Headline
    	    the sign of a permutation
    	Usage
    	    permutationSign(perm)
    	Inputs
    	    perm:List
	    	a permutation of the numbers from 0 to n-1
	    p:Partition
	    	a partition that represents the conjugacy class of the permutation 
    	Outputs
    	    :ZZ
		1 or -1, the sign of the permutation
    	Description
	    Text
	    	Every permutation can be decompose as a product of transpositions.
		This decomposition is not unique, however the parity of the number
		of transpositions that appears in the decomposition is always the same.
		Thus the sign is defined as $(-1)^l$ where $l$ is the number of transposition.
	    	
		The sign can be calculated if the cycle decomposition if known because
		the sign is multiplicative and the sign of a $k$-cycle is $(-1)^(k+1)$.
		This is the way the method permutationSign calculates the sign.
		
		The sign permutation is used to calculate polytabloids and higher Specht polynomials.
		
	    Example
		perm = {2,1,4,3,0}
		c = cycleDecomposition perm
		permutationSign perm
		perm2 = {4,2,1,0,3}
    	    	c2 = cycleDecomposition perm2
		permutationSign perm2
    	    	
    Node
    	Key
	    SpechtModuleElement
	    (symbol *,QQ, SpechtModuleElement)
	    (symbol *,ZZ, SpechtModuleElement)
	    (trim,SpechtModuleElement)
	    (symbol +,SpechtModuleElement, SpechtModuleElement)
	    (symbol -,SpechtModuleElement, SpechtModuleElement)
	    (terms,SpechtModuleElement)
	    (symbol SPACE,List, SpechtModuleElement)
    	    (net, SpechtModuleElement)
	Headline
    	    the class of Specht Module elements
	Description
	    Text    	
		Polytabloids of shape $p$ are elements of the module of tabloids of the form 
		$\sum_{\tau \in C(T)}\sum_{\sigma \in R(T)}sgn(\tau) \tau\sigma(T)$
		where T is a tabloid of shape $p$.
		
		The set of polytabloids generates the Specht Module of shape $p$.
		
		In other words the element in a SpechtModule are linear combinations of
		polytabloids. This is the way such elements are implemented in this package.
	    
	    	The constructor takes just one polytabloid and a coefficient
	    Example
	    	p = new Partition from {3,2,1}
		y = youngTableau(p,{2,0,3,4,5,1})
		e = spechtModuleElement(y,-2)
	    Text
	    	More complex elements can be made by adding or subtracting previously build elements
		and multiplying by any element of the base field (which is assumed to be \mathbb{Q}).
	    Example
	    	y2 = youngTableau(p,{5,0,2,4,1,3})
		e2 = spechtModuleElement(y2)
		e+e2
		e-e2
		3*oo
	    Text
	    	The element SpechtModuleElement is implemented as a MutableHashTable.
		The keys are the filling of the tableaux that label the polytabloids and they
		point to their respective coefficients
	    Example
	    	peek oo
	        peek ooo#values 
	    Text
	    	The method terms is used to retrieve the polytabloid with their respective coefficient.
		This is given as a list of pairs of tableaux and coefficients.
	    Example
	    	terms (3*(e-e2))
	    Text
	    	A method was implemented to apply a permutation to a SpechtModuleElement.
		The action is defined by permuting the entries of the tableaux that label the 
		polytabloids.
	    Example
	    	{0,1,2,3,4,5} (3*(e-e2))
		{1,0,2,3,4,5} (3*(e-e2))
 	SeeAlso
 	    spechtModuleElement

	        
    Node
    	Key
    	    spechtModuleElement
	    (spechtModuleElement,YoungTableau)
	    (spechtModuleElement,YoungTableau,QQ )
	    (spechtModuleElement,YoungTableau,ZZ )
	    (spechtModuleElement,Partition,MutableHashTable)
	    
	    
    	Headline
    	    the constructor for the class SpechtModuleElement
    	Usage
    	    spechtModuleElement(p,v)
	    spechtModuleElement(y,n)
	    spechtModuleElement(y,m)
	    spechtModueElement(y)
	    
    	Inputs
    	    y:YoungTableau
	    	the label of the polytabloid
	    n:ZZ
	    	a number. If not specified then it is assumed to be a 1.
	    m:QQ
	    	a number. If not specified then it is assumed to be a 1.
	    p:Partition
	    	A partition that index a module
	    v:MutableHashTable
	    	A mutable hash table from a SpechtModuleElement
	       
    	Outputs
    	    :SpechtModuleElement
		an element of the form n*poly_y, where poly_y is the polytabloid labeled by the tableau y.
    	Description
	    Text	
		The basic constructor builds a SpechtModuleElement from just one polytabloid and
		its respective coefficient.
	    Example
		p = new Partition from {3,2,1}
		y = youngTableau(p,{2,0,3,4,5,1})
		spechtModuleElement(y,-2)
		spechtModuleElement(y)

    Node
    	Key
    	    garnirElement
	    (garnirElement,YoungTableau, ZZ,ZZ,ZZ )
	    (garnirElement,YoungTableau,ZZ )
	    (garnirElement,YoungTableau)
	    
    	Headline
    	    a SpechtModuleElement that is equal to zero
    	Usage
    	    garnirElement(y,coef,a,b)
	    garnirElement(y,coef)
	    garnirElement(y)
    	Inputs
    	    y:YoungTableau
	    	a tableau that labels a polytabloid
	    a:ZZ
	    	the row of the descent
	    b:QQ
	    	the column of the descent
	    coef:ZZ
	    	the coefficient of the polytabloid
    	Outputs
    	    :SpechtModuleElement
		an element which is equal to zero.
    	Description
	    Text	
		A Garnir element is an element which is constructed to remove row descents from a tableau.
		Given a tableau $T$, the Garnir element is defined for a subset $A$ of the $i$th column and a subset $B$ of the $i+1$ column.
		It is defined as $ \sum_{\pi} sgn(\pi)\pi(T)$. The  $\pi$ are called transversals. They are a set of permutations such that
		$S_{A \cup B}$  is the disjoint union of  $\pi(S_A \times S_B)$. 
		  
		The identity can always be chosen as a transversal for any pair of sets. Therefore the original tableau $T$ appears along side other tableaux which are
		closer to being standard. Another property is that this element is equal to zero. Therefore the original polytabloid $e_T$ can be written as
		$ e_T = -\sum_{\pi \neq id} sgn(\pi)\pi(e_T)  $ 
	    
	    	In this implementation the $i$th column is taken to be the parameter b. The set $A$ is all the cells in the $i$th column from the a-th row to the bottom.
		The set $B$ is all the cells in the $i+1$ column from the a-th row to the top.
		
		If the number (a,b) are not specified then they are taken as the coordinates of the first row descent of $T$
	    Example
		p = new Partition from {3,2,1}
		y = youngTableau(p,{1,2,3,5,4,6})
		garnirElement y
    	SeeAlso
    	    firstRowDescent

    Node
    	Key
    	    straighteningAlgorithm
	    (straighteningAlgorithm, SpechtModuleElement )
	    (straighteningAlgorithm,YoungTableau,ZZ )
	    (straighteningAlgorithm,YoungTableau)
	    
    	Headline
    	    an algorithm for expressing any polytabloid as linear combinations of standard polytabloids
    	Usage
    	    straighteningAlgorithm(ele)
	    straighteningAlgorithm(y,coef)
	    straighteningAlgorithm(y)
    	Inputs
	    ele:SpechtModuleElement
	    	a SpecthModuleElement
    	    y:YoungTableau
	    	a tableau that labels a polytabloid
	    coef:ZZ
	    	the coefficient of the polytabloid
    	Outputs
    	    :SpechtModuleElement
		the same SpechtModuleElement written as a linear combination of standard polytabloids 
    	Description
	    Text	
		The straigtening algorithm works by finding the first term that is not standard. Then, taking as coordinates
		the first row descent, it calculates the Garnir element of this tableaux. It then rewrites
		the SpechtModuleElement substituting the term by the linear combination given by the garnir element.
	    Example
		p = new Partition from {3,2,1}
		y = youngTableau(p,{1,2,3,5,4,6})
		garnirElement y
    	SeeAlso
	    garnirElement


    Node
    	Key
	    (sortColumnsTableau, YoungTableau)
	    
    	Headline
    	    a method for 
    	Usage
    	    sortColumnsTableau(y)
    	Inputs
    	    y:YoungTableau
    	Outputs
    	    :ZZ
		the sign of the permutation that sorts the columns of the tableau
    	Description
	    Text	
    	    	This method sorts the columns of the tableau and retrieves the sign of the associated permutation
	    Example
		p = new Partition from {2,2,1}
		y = youngTableau(p,{0,1,4,3,2})
		sortColumnsTableau y
		y

    Node
    	Key
	    (sortColumnsTableau, SpechtModuleElement)
    	    sortColumnsTableau
	    
    	Headline
    	    a method for sorting the columns of the tableaux in a SpechtModuleElement 
    	Usage
    	    sortColumnsTableau(ele)
    	Inputs
    	    ele:SpechtModuleElement
    	Outputs
    	    :null
    	Description
	    Text	
    	    	This method sorts the columns of every tableaux that appears as a term of the SpechtModuleElement.
		The corresponding sign of the sort is multiplied to the coefficient of the respective term.
		The method returns null but changes the SpechtModuleElement that was input as a parameter.
	    Example
		p = new Partition from {2,2,1}
		y1 = youngTableau(p,{0,1,4,3,2})
		y2 = youngTableau(p,{0,3,4,1,2})
		ele = spechtModuleElement(y1)-spechtModuleElement(y2)
		sortColumnsTableau ele
		ele
		
		
		
    Node
    	Key
	    (firstRowDescent, YoungTableau)
    	    firstRowDescent
	    
    	Headline
    	    retrieves the first row descent of a young tableau
    	Usage
    	    firstRowDescent y
    	Inputs
    	    y:YoungTableau
    	Outputs
    	    a:ZZ
	    	the row of the row descent or -1 if there is no row descent
	    b:ZZ
	    	the column of the row descent or -1 if there is no row descent
	    
    	Description
	    Text	
    	    	A row descent is defined to be a cell (a,b) in a tableau $T$ such that T_(a,b)>T_(a,b+1).
		This method reads by columns from left to right and each column is read from the top down until the first row descent is found.
		If no row descent is found the pair (a,b)= (-1,-1) is returned.
	    Example
		p = new Partition from {3,2,1}
		y = youngTableau(p,{1,2,3,5,4,6})
		firstRowDescent y
		y2 = youngTableau(p,{1,2,4,3,5,6})
		firstRowDescent y2

    Node
    	Key
	    (cardinalityOfConjugacyClass, Partition)
    	    cardinalityOfConjugacyClass
	    
    	Headline
    	    the size of the conjugacy classes of S_n
    	Usage
    	    cardinalityOfConjugacyClass p
    	Inputs
    	    p:Partition
	    	a partition that indexes a conjugacy class of S_n
    	Outputs
    	   :ZZ
	       the size of the conjugacy class  
    	Description
	    Text	
    	    	The formula for this classes is obtained by the Orbit-Stabilizer lemma applied for S_n
		with the action of conjugation.
		
		For a partition $p$ this formula is $n!/(\prod_i (\lambda_i )!i^\lambda_i$, where $\lambda_i$ denotes the number
		    of parts in $p$ that are equal to $i$.  
	    Example
		p1 = new Partition from {3,2,1}
		cardinalityOfConjugacyClass p1
		p2 = new Partition from {1,1,1,1,1}
		cardinalityOfConjugacyClass p2
		
		
    Node
    	Key
	    matrixRepresentation
	    (matrixRepresentation, List, TableauList)
    	    (matrixRepresentation, List, Partition)
	    (matrixRepresentation,TableauList)
	    (matrixRepresentation,Partition)
	    
    	Headline
    	    the matrix representation of a permutation in the Specht Module
    	Usage
    	    matrixRepresentation(perm,standard)
	    matrixRepresentation(perm,parti)
	    matrixRepresentation(standard)
	    matrixRepresentation(parti)
	    
    	Inputs
    	    perm:List
	    	a permutation
	    standard:TableauList
	    	a list of standard tableaux of a given partition
	    parti:Partition
	    	a partition
	    
    	Outputs
    	   :Matrix
	       the matrix representation of the given permutation in the Specht module index by the given partition
	   :HashTable
	       if no permutation is given then it calculates the representation for all the permutations in S_n
    	Description
	    Text	
    	    	The matrix representation for a permutation is calculated by studying the action of the permutation
		on the basis of standard polytabloids.
		
		The permuted polytabloids are then written as a linear combination of standard polytabloids using the
		straightening algorithm.
	    Example
		p = new Partition from {2,1}
		l = {0,2,1}
		matrixRepresentation (l,p)
		stan = standardTableaux p
		matrixRepresentation (l,stan)
    	    	matrixRepresentation stan
		
		
    Node
    	Key
	    permutePolynomial
	    (permutePolynomial, List, RingElement)
    	    (permutePolynomial, List, Product)
	    (permutePolynomial, List, Sum)
	    (permutePolynomial, List, Power)
	    (permutePolynomial, List, Minus)
    	Headline
    	    permutes a RingElement or a PolynomialExpression of RingElements
    	Usage
    	    permutePolynomial(perm,f)
	    permutePolynomial(perm,prod)
	    permutePolynomial(perm,s)
	    permutePolynomial(perm,pow)
	    permutePolynomial(perm,minu)
	    
    	Inputs
	    
    	    f:RingElement
	    	a ring element
	    prod:Product
	    	a Product expression
	    s:Sum
	    	a sum expression
	    pow:Power
	    	a power expression
	    minu:Minus
	    	a minus expression
	    perm:
	    	a permutation
    	Outputs
    	   :RingElement
	       the result of applying perm to f  
	   :Expression
	       the result of applying f to the given expression
    	Description
	    Text
	    	This method applies permutations to polynomial ring elements by permuting the variables.  
	    	Therefore the size of the permutation must be equal to the number of generators of the ring of the elements.
	    Example
		R = QQ[x_0..x_4]
		l = {1,0,2,3,4}
		f = x_1*x_2*x_3
		permutePolynomial(l,f)
	    Text
	    	This method can also permute polynomial expressions that are constructed from ring elements
		either by sums, products or powers.
	    Example
	    	ex = factor(x_1*x_2*x_3)+factor(x_1*x_3*x_4)
    	    	permutePolynomial(l,ex)		

    				
    Node
    	Key
	    vandermondeDeterminant
	    (vandermondeDeterminant, List,PolynomialRing)
    	    
    	Headline
    	   the vandermonde determinant for a set of generators of a ring
    	Usage
    	    vandermondeDeterminant(l,R)
    	
	Inputs
	    R:PolynomialRing
	    
    	    l:List
	    	a subset of the indices of the generators of R
    	    AsExpression=>Boolean
	    	a Boolean value, default value is false. If true it returns the determinant as a product expression
		This is a particularly useful way to reduce the size of the object since a Vandermonde determinant
		has n! terms but only n*(n-1)/2 factors.
	Outputs
    	   :RingElement
	       the determinant of the Vandermonde matrix formed by the generators indexed by l. 
    	Description
	    Text	
    	    	A Vandermonde matrix is a matrix of $n$ elements is constructed by putting in each column
		all the powers from 0 to $n-1$ of each of the elements.
		
		If $x_i$ are the elements used to construct the matrix then it can be proven that the determinant
		has the following form.
		
		$\prod_{0 \leq i < j < n} (x_j-x_i) $
		  
	    Example
		R = QQ[x_0..x_3]
		vandermondeDeterminant({0,2,3},R)
    	    	factor oo
		

    Node
    	Key
	    AsExpression
	    [vandermondeDeterminant, AsExpression]
    	    [schurPolynomial,AsExpression]
	    [spechtPolynomial,AsExpression]
	    [spechtPolynomials,AsExpression]
	    [higherSpechtPolynomial,AsExpression]
	    [higherSpechtPolynomials,AsExpression]
	    [secondaryInvariants,AsExpression]
    	Headline
    	    an optional argument that returns polynomials as expressions	
    	Description
	    Text	
    	    	The optional argument AsExpression specifies whether the polynomials
		should be outputted as RingElement objects or as elements of type Expression 
		  
	    Example
		R = QQ[x_0..x_3]
		vandermondeDeterminant({0,2,3},R,AsExpression => true)
		
    	    Text
	    	This allows to visualize some of the polynomials in a clearer way.
	   Example
	       p = new Partition from {2,2}
	       S = youngTableau(p,{0,2,1,3})
	       T = youngTableau(p,{0,1,2,3})
	       higherSpechtPolynomial(S,T,R,AsExpression => true)
	       higherSpechtPolynomials(R,AsExpression => true)
	   Text
	    	In some cases it also allows to work with polynomials whose term expansion
		is very big.
		
	    Example
	     	 R = QQ[x_1..x_10]
		 p = new Partition from {1,1,1,1,1,1,1,1,1,1};
		 spechtPolynomial(youngTableau(p,{0,1,2,3,4,5,6,7,8,9}),R,AsExpression => true)
    	    SeeAlso
	    	higherSpechtPolynomial

    Node
    	Key
	    Robust
	    [higherSpechtPolynomial,Robust]
	    [higherSpechtPolynomials,Robust]
	    [secondaryInvariants,Robust]
    	Headline
    	    an optional argument for specifying the algorithm for calculating higherSpechtPolynomials	
    	Description
	    Text	
    	    	This optional argument decides between two ways to calculate higherSpechtPolynomials.
	    	If it is set to to true then a calculation involving the row and column stabilizers
	    	is used.                                                                                                                                                                                                      
    	    	If it is set to false then another strategy is used. This strategy is based on a
	    	representation of higher specht polynomials as a multiplication
	    	of simpler Specht polynomials and Schur polynomials.
	    
	    Example
	       R = QQ[x_1..x_4]
	       p = new Partition from {2,2}
	       S = youngTableau(p,{0,2,1,3})
	       T = youngTableau(p,{0,1,2,3})
	       higherSpechtPolynomial(S,T,R,Robust => true)
	       higherSpechtPolynomial(S,T,R,Robust => false)
    	    Text
	    	This option is used mainly to check that the alternative algorithm proposed
		was correct.
	SeeAlso
	     higherSpechtPolynomial
	        
    Node
    	Key
	    (spechtPolynomial,YoungTableau, PolynomialRing)
    	    spechtPolynomial
    	Headline
    	   the Specht polynomial indexed by a standard tableau 
    	Usage
    	    spechtPolynomial(y,R)
    	
	Inputs
	    y:YoungTableau
	    	
	    R:PolynomialRing
	    	
	Outputs
    	   :RingElement
	     the Specht polynomial  
    	Description
	    Text	
    	    	Specht polynomials were the original objects that gave rise to the Specht modules.
		The Specht polynomial of a tableau $T$ is product of the Vandermonde determinant of the variables
		index by the columns of the tableau.
		  
	    Example
		R = QQ[x_0..x_4]
		p = new Partition from {2,2,1}
		y = youngTableau(p,{0,3,1,4,2})
		spechtPolynomial(y,R)
		factor oo
    Node
    	Key
	    (spechtPolynomials,Partition, PolynomialRing)
    	    spechtPolynomials
	    
    	Headline
    	   the set of all Specht polynomial indexed by standard tableaux of shape p 
    	Usage
    	    spechtPolynomials(p,R)
    	
	Inputs 
    	    p:Partition
	  
	    R:PolynomialRing
	   
	Outputs
    	   :HashTable
	     a hash table with the polynomials index by the filling of their respective tableaux 
    	Description
	    Text
	    	The set of all the Specht polynomials for standard tableaux of a given shape p forms a basis for a module which is isomorphich to 
		the Specht module indexed by p.
	   
	   Example
		R = QQ[x_0..x_4]
		p = new Partition from {2,2,1}
		specht = spechtPolynomials(p,R)
			

    Node
    	Key
	    (indexMonomial,YoungTableau, YoungTableau,PolynomialRing)
    	    indexMonomial
	    
    	Headline
    	   a monomial that represents an index tableau 
    	Usage
    	    indexMonomial(S,T,R)
    	
	Inputs
	    S:YoungTableau
	    
    	    T:YoungTableau
	    
	    R:PolynomialRing
	    
	Outputs
    	   :RingElement 
    	Description
	    Text
	    	The index monomial is used in the construction of higher Specht polynomials.
	        To calculate the index monomial first the index tableau of $S$, $i(S)$ is calculated.
		Then the monomial is calculated as $x_T^{i(S)}$. This is a monomial with the variables as they appear in T
		with the exponents that appear in $i(S)$.
	   
	   Example
		R = QQ[x_0..x_4]
		p = new Partition from {2,2,1}
		S  = youngTableau(p,{0,2,1,3,4})
		T  = youngTableau(p,{0,1,2,3,4})
		ind = indexTableau(S)
		indexMonomial(S,T,R)
    	SeeAlso
	    indexTableau

    Node
    	Key
	    (permutationMatrix,List)
	    permutationMatrix
	Headline
	    a permutation matrix generator
	Usage
	    permutationMatrix (permutation)
	Inputs
	    permutation:List
	    	a list of numbers from 0..n-1 that represents a permutation
	Outputs
	    :Matrix
	    	the matrix that represents the given permutation
	Description
	    Example
	    	permutationMatrix {0,1,2}
		permutationMatrix {1,0,2}
		permutationMatrix {1,2,0}
	    
	
	    
    Node
    	Key
	    (higherSpechtPolynomial,YoungTableau, YoungTableau,PolynomialRing)
    	    higherSpechtPolynomial
	    
    	Headline
    	   the higher Specht polynomial index by the pair of standard tableaux (S,T) 
    	Usage
    	    higherSpechtPolynomial(S,T,R)
	Inputs
	    S:YoungTableau
	    
    	    T:YoungTableau
	    
	    R:PolynomialRing
	    
	Outputs
    	   :RingElement
	    	the higher Specht polynomial	
    	Description
	    Text
	    	Higher Specht polynomials are a family of polynomials that form a basis of the coinvariant algebra for the symmetric group.
		The coinvariant algebra is isomorpich as a $S_n$ module to the regular representation of $S_n$. Therefore
		every Specht modules appears as an irreducible module in this algebra with multiplicity $f^\lambda= {\rm dim} \, S^\lambda $. 
		Higher Specht polynomials decompose this algebra into its irreducible submodules. 
		
		Higher Specht polynomials are indexed by pairs of standard tableaux of the same size.
		The usual construction of these polynomials is as follows.
		
		1. Given two tableaux (S,T) of shape $\lambda$ the index tableau i(S) is calculated and the index monomial $x_T^{i(S)}$ is calculated.
		2. The Young symmetrizer $\sum_{\tau \in C(T)} \sum_{R(T)} sgn(\tau)\sigma$ is applied to the index monomial.  
		
		The algorithm based on this construction can be used in this method with the optional input
		Robust => true
		
		A second algorithm  for this polynomials is based on a study on the structure of this polynomials.
		
		The outline of this construction is as follow.
		
	        1. Calculate the index tableau $i(S)$.
    		2. Calculate all row permutations of $i(S)$ such that no entries in the same column are equal.
    		3. For each different tableau $\sigma(i(S))$ in the previous step order the columns in descending order making sure to calculate the sign of the permutation used. 
    		4. For each column in $\sigma(i(S))$ determine the Schur polynomial with partition $\lambda = (a_p-p, \ldots,a_i-i ,\ldots ,a_0) $.
    		5. For all columns multiply the polynomials obtained in Step 4. Multiply this by the sign obtained in Step 3.
    		6. For all tableaux $\sigma(i(S))$, add all polynomials obtained in Step 5.
    		7. Multiply the polynomial in Step 6 by the Specht polynomial of T.  
	   
	   Example
		R = QQ[x_0..x_4]
		p = new Partition from {2,2,1}
		S  = youngTableau(p,{0,2,1,3,4})
		T  = youngTableau(p,{0,1,2,3,4})
		time higherSpechtPolynomial(S,T,R)
		time higherSpechtPolynomial(S,T,R, Robust => false)
		time higherSpechtPolynomial(S,T,R, Robust => false, AsExpression => true)

    	SeeAlso
	    spechtPolynomial
	    indexMonomial
	    columnStabilizer
	    rowStabilizer
	    rowPermutationTableaux
	    
    Node
    	Key
	    higherSpechtPolynomials
	    (higherSpechtPolynomials,YoungTableau,PolynomialRing)
	    (higherSpechtPolynomials,YoungTableau,TableauList,PolynomialRing)
	    (higherSpechtPolynomials,Partition,PolynomialRing)
	    (higherSpechtPolynomials,PolynomialRing)
    	    
    	Headline
    	   a method that gives sets of higher Specht polynomials 
    	Usage
    	    higherSpechtPolynomial(S,R)
	    higherSpechtPolynomial(S,standard,R)
	    higherSpechtPolynomial(p,R)
	    higherSpechtPolynomial(R)
	Inputs
	    S:YoungTableau
	    	    
	    R:PolynomialRing
	    
	    standard:TableauList
	    	The list of standard tableaux of the same shape as S
	    p:Partition
	Outputs
    	   :HashTable
	       a hash table with multiple levels depending on the input
    	Description
	   Text
	    	 This methods returns higher Specht polynomials sorted in hash tables depending on the input received.
		 
		 If the input is just a YoungTableau $S$ of shape $\lambda$ and a PolynomialRing then it calculates the 
		 standard tableaux $ST(\lambda)$ and then stores all polynomials $F_T^S$ such that $T \in ST(\lambda)$.
		 The polynomials are stored in a hash table with the filling of $T$ as the key.
		 
		 The list $ST(\lambda)$ can be provided as an input. This is used to avoid repeating this calculation
		 when this method is called multiple times with the same shape $\lambda$.
		 
		 This set forms a basis for one of the copies of the Specht module $S^\lambda$.
	   
	   Example
		R = QQ[x_0..x_3]
	        p = new Partition from {2,2}
		S  = youngTableau(p,{0,2,1,3})
		higherSpechtPolynomials(S,R)
		stan = standardTableaux p
		higherSpechtPolynomials(S, stan,R)
			
	   Text	
	    	If only a partition $\lambda$ and a polynomial ring is given then the method calculates $ST(\lambda)$.
		Then it calculates all polynomials $F_T^S$ such that $S,T \in ST(\lambda)$.
		
		This is a basis for the isotypical component $X_\lambda$ in the coinvariant algebra of the symmetric group.

    	    	The polynomials are stored by calling for each $S \in ST(\lambda) $ the previous method. The output is stored
		in another hash table with the key being the filling of the tableau $S$.
		  
	   Example
	       higherSpechtPolynomials(p,R)
	   Text
	       Finally if just a polynomial ring $R$ with $n$ elements is provided then the method calculates all higher Specht polynomials 
	       for all partitions $\lambda$ of $n$.
	       
	       The polynomials are calculated by calling the previous method for every partition of $n$ and storing the values in
	       a new hash table with the key being the partition.
	   Example
	       higherSpechtPolynomials(R)
	       
	       
    Node
    	Key
	    (generalizedVandermondeMatrix,List,List,PolynomialRing)
	    generalizedVandermondeMatrix
	Headline
	    the method for calculating generalized Vandermonde matrices
	Usage
	    generalizedVandermondeMatrix(indices,exponents,R)
	Inputs
	    indices:List
	    	a list of the variables that appear in each column of the matrix
	    exponents:List
	    	a list of the powers that appear in each row of the matrix
	    R:PolynomialRing
	    
	Outputs
	    :Matrix
	Description
	    Text
    	    	Generalized vandermonde matrices allow the power in the rows to be different from the numbers
		from 0 to n-1.
	    Example
	    	R = QQ[x_0..x_4]
	    	M = generalizedVandermondeMatrix({0,2,3},{1,3,5},R)
		
	   Text
	       The determinant of these matrices divided by the Vandermonde determinant of the same rank is equal
		to a schur polynomial .
	   Example
		(determinant M)//vandermondeDeterminant({0,2,3},R) 
		
    Node
    	Key
	    (schurPolynomial,List,Partition,PolynomialRing)
	    schurPolynomial
	Headline
	    a method for constructing Schur polynomials
	Usage
	    schurPolynomial(indices,parti,R)
	Inputs
	    indices:List
	    	a list of the variables that appear in each column of the matrix
	    parti:Partition
	    	a partition that indexes the schur polynomial
	    R:PolynomialRing
	    
	Outputs
	    :Matrix
	Description
	    Text
    	    	Generalized vandermonde matrices allow the power in the rows to be different from the numbers
		from 0 to n-1.
	    Example
	    	R = QQ[x_0..x_4]
	    	M = generalizedVandermondeMatrix({0,2,3},{1,3,5},R)
		
	   Text
	       The determinant of these matrices divided by the Vandermonde determinant of the same rank is equal
		to a schur polynomial .
	   Example
		(determinant M)//vandermondeDeterminant({0,2,3},R)
		
		
    Node
    	Key
	    (generatePermutationGroup,List)
	    generatePermutationGroup
	Headline
	    a method for generating a permutation group given a set of generators
	Usage
	    generatePermutationGroup(gens)
	Inputs
	    gens:List
	    	a list of permutations
	    
	Outputs
	    :List
	    	the group generated by the given set of generators
	Description
	    Text
    	    	The method works by taking all multiplications of the elements in the set of generators. New elements
		that are found are added and the process is repeated until no new elements are found.
	    Example
	    	generatePermutationGroup {{1,0,2,3},{1,2,3,0}}
				
	    Text
	    	This method is used to calculate the size of each conjugacy classes for the groups.	
    Node
    	Key
	    representationMultiplicity
	    (representationMultiplicity,Tally,Partition,CharacterTable)
	    (representationMultiplicity,Tally,Partition)    
	Headline
	    the number of secondary invariants in a given irreducible representation			
	Usage
	    representationMultiplicity(tal,p,charTable)
	    representationMultiplicity(tal,p)
	Inputs
	    tal:Tally
	    	a tally with the number of elements in each conjugacy class of the group
	    p:Partition
	    	a partition that indexes an irreducible representation
	    charTable:CharacterTable
	    	optionally the character table of S_n. If it is not provided then it is calculated by the method
	Outputs
	    :ZZ
	    	the multiplicity of the trivial representation of the group described by tal in the irreducible representation of S_n indexed by p

	Description
	    Text
    	    	Since the given group $H$ is a subgroup of $S_n$, the restrictions of the Specht modules to $H$
		are also $H$-modules. The number of copies of the trivial representation of $H$ in each of these modules
		can be found by the formula for the inner product for characters applied to the characters of the previous modules.
		
		$\frac{1}{|H|}\sum_{C \in Cl(H)} |C|X_\lambda(\sigma_c)$ 
		
		$Cl(H)$ is the set of conjugacy classes of $H$, $|C|$ is the size of the conjugacy class and $\sigma_c$ is a representative
		of the conjugacy class $C$ and $X$ is the character of the representation.
		
		Therefore it is necessary to calculate the cardinality of each conjugacy class. This is done by checking the conjugacy class of each element
		in the group. For the following example a subgroup of $S_6$ isomorphic to $S_4$ is taken.	
	   Example
	    	genList = {{1,2,3,0,5,4},{0,4,2,5,1,3}}
    	    	H = generatePermutationGroup(genList)
		
	   Text
    	    	For the given group a tally with the size of each conjugacy class must be provided. This tally
		is inputted to the representationMultiplicityMethod
    	   Example
	       tal := tally apply (H,h->conjugacyClass h);
	   Text
	       The number of secondary invariants is equal to the index of the group $[S_6:H] = 30$.
	       We check that this is true by calculating the number of trivial representations of $H$ in each 
	       irreducible representation of $S_6$. We take into account that there are multiple copies of each
	       representation by multiplying the values with the number of copies which is given by the hookLengthFormula.
	   Example
	       	partis = partitions 6;
	       	time multi = hashTable apply (partis, p-> p=> representationMultiplicity(tal,p))
		sum (partis, p -> multi#p * hookLengthFormula p)
	   Text
	      The submodules where the multiplicity is zero will not be taken into account when applying the secondaryInvariants
	      algorithm.
      	      The character table can be inputted to the method as well. This is made to avoid calculating the same character table for every partition of $n$. 	
	    Example
	    	charTable = characterTable 6
		time multi2 = hashTable apply (partis, p-> p=> representationMultiplicity(tal,p,charTable))
	SeeAlso
	    generatePermutationGroup
	    conjugacyClass
    Node
    	Key
	    (secondaryInvariants,List,PolynomialRing)
	    secondaryInvariants   
	Headline
	    the set of secondaryInvariants of a permutation group			
	Usage
	    secondaryInvariants(gens,R)
	Inputs
	    gens:List
	    	a list of generators of a permutation group H
	    R:PolynomialRing
	    	a  polynomial ring
	Outputs
	    :HashTable
	    	the set of secondary invariants indexed by the representation in which they are found
	Description
	    Text
    	    	Let $R$ be a polynomial ring with $n$ generators. The secondary invariants of a group $H$ in $GL(n)$ are the set of generators of the ring of invariants $R^H$
		as a $K[\theta_1,\ldots,\theta_n]$-module. For this algorithm we always take the primary invariants
		 $\theta_1,\ldots,\theta_n$ to be the elementary symmetric polynomials $e_1,\ldots e_n$ so that
		 the ring $K[\theta_1,\ldots,\theta_n]$ is the ring of symmetric polynomials.
	    
	    	The secondary invariants are obtained by considering the quotient ring $R/(e_1,\ldots,e_n)$.
		This quotient ring is called the coinvariant algebra of $S_n$. This quotient is isomorphic to the regular representation of $S_n$. In particular as
		a K-vector space it is finite dimensional.
		In this space we find the subspace that is invariant under the action of $H$. The secondary invariants
		correspond to a basis for this space.
		
	    	The advantage of this algorithm is that it decomposes the regular representation into its
		irreducible representation by means of the higher Specht polynomials basis. This reduces
		significantly the dimension of the vector spaces in which the invariant spaces must be found.
	        
		To illustrate we calculate the secondary invariants for a subgroup of cardinality 24 in $S_6$.   
	    Example
	    	R = QQ[x_1..x_6]
	    	genList = {{1,2,3,0,5,4},{0,4,2,5,1,3}}
		time seco = secondaryInvariants(genList,R);
		seco#(new Partition from {2,2,2})
		
		
   Node
    	Key
	    (powerSumSymmetricPolynomials,PolynomialRing)
	    powerSumSymmetricPolynomials
	Headline
	    the power sum symmetric polynomials			
	Usage
	    powerSumSymmetricPolynomials(R)
	Inputs
	    R:PolynomialRing
	    	a  polynomial ring
	Outputs
	    :List
	    	the list of power sum symmetric polynomials
	Description
	    Text
	   	 As an example the power sum symmetric polynomials of a ring with three variables
	     	 are calculated. These polynomials form a basis for the ring of symmetric polynomials.
	    Example
	     	 R = QQ[x_1..x_3]
		 powerSumSymmetricPolynomials R
        SeeAlso
	    elementarySymmetricPolynomials

   Node
    	Key
	    (elementarySymmetricPolynomials,PolynomialRing)
	    elementarySymmetricPolynomials
	Headline
	    the elementary symmetric polynomials			
	Usage
	    elementarySymmetricPolynomials(R)
	Inputs
	    R:PolynomialRing
	    	a  polynomial ring
	Outputs
	    :List
	    	the list of power elementary symmetric polynomials
	Description
	    Text
	   	 As an example the elementary symmetric polynomials of a ring with three variables
	     	 are calculated. These polynomials form a basis for the ring of symmetric polynomials.
	    Example
	     	 R = QQ[x_1..x_3]
		 elementarySymmetricPolynomials R
    	SeeAlso
	    powerSumSymmetricPolynomials
///

-*
Tests that the representationMultiplicity correctly founds the number of secondary invariants in 
each irreducible representation 

The test is made for H1 = D_4 subset of S_4 and H2 = S_5 as a subset of S_10.
*-

TEST ///


testMultiplicity = method()
testMultiplicity List := (listGens) -> (
    n := #listGens#0;
    p:= partitions n;
    total:= 0;
    charTable := characterTable n;
    group:=generatePermutationGroup(listGens);
    for i to #p-1 do(
    	tal := tally apply (group, g-> conjugacyClass g);
    	multiplicity:= representationMultiplicity(tal,p#i,charTable);
    	total = total + multiplicity*hookLengthFormula(p#i);
    	);
    	total == n!/#group
    	)
listGens = {{0,3,2,1},{1,2,3,0}};    
assert testMultiplicity(listGens);
listGens2 = {{5,1,8,3,4,0,7,6,2,9},{4,0,1,2,3,7,8,9,5,6}};    
assert testMultiplicity(listGens2);

///


-*
Test that the output of the straightening algorithm correctly represents the same polynomial as the input.
It is done for the Modules index by partitions {3,2} and {2,2,2}, and for all permutations of S_5 and S_6.
*-

TEST ///

testStraighteningAlgorithm = method()
testStraighteningAlgorithm(List,TableauList,PolynomialRing):= (perm,standard,R)-> (
    for i to standard#length-1 do (
    	perm2 := perm_(flatten entries standard#matrix^{i});
	polynomial := spechtPolynomial(youngTableau(standard#partition,perm2),R);
	y:= youngTableau(standard#partition,perm2);
	lineal := straighteningAlgorithm y;
	ini := 0;
	suma:=0_R;
	for term in terms lineal do (
	    suma = suma + term#1* (spechtPolynomial(term#0,R)); 
	    );
	assert (suma === polynomial)
    )
)



p = new Partition from {3,2};
standard = standardTableaux p;
R := QQ[x_0..x_4];
perms = permutations 5;
for perm in perms do testStraighteningAlgorithm(perm,standard,R);

p = new Partition from {2,2,2};
standard = standardTableaux p;
R:= QQ[x_0..x_5];
perms = permutations 6;
for perm in perms do testStraighteningAlgorithm(perm,standard,R);


///

-*
Test whether the algorithm proposed for calculating higher Specht polynomials coincides with the 
standard method that is closest to the definition of higher Specht polynomials. It also checks whether the outputs
as expressions coincide with the normal outputs of this method
*-

TEST /// 

n:=6;
R := QQ[x_1..x_n];


specht0 := higherSpechtPolynomials(R,Robust=> true, AsExpression => false);
specht1 := higherSpechtPolynomials(R,Robust=>true, AsExpression => true);
specht2 := higherSpechtPolynomials(R,Robust=>false, AsExpression => false);
specht3 := higherSpechtPolynomials(R,Robust=>false, AsExpression => true);


for p in keys specht0 do (
    for S in keys (specht0#p) do (
        for T in keys(specht0#p#S) do (
	    assert (specht0#p#S#T == value specht1#p#S#T);
	     assert (specht0#p#S#T == specht2#p#S#T);
	     assert (specht0#p#S#T == value specht3#p#S#T);
	    );
	);
    );

///


-*
Tests that the rows in the character table are 
orthogonal with respect to the inner product of characters.
*-

TEST ///

for n from 1 to 10 do (
    charTable := characterTable n;
    for i to charTable#length-1 do (
       	assert ( innerProduct(n,(charTable#values)^{i},(charTable#values)^{i})== 1 );
	for j to i-1 do (	
	    assert (innerProduct(n,(charTable#values)^{i},(charTable#values)^{j}) == 0);
    	    );
	);
    );
///



-*
Test that the secondary invariants are effectively invariant under the action of the given ring.
*-

TEST ///

testInvariance = method()
testInvariance (List,HashTable):= (lista,hashTab)->  (
    for k in values (hashTab) do (
	assert testInvariance(lista,k);
    	);
    true
    )

testInvariance (List,RingElement):= (gens,s) -> (
    for g in gens do (
	assert (permutePolynomial(g,s) == s);
	);
    true
    )

testInvariance (List,List):= (gens,l) -> (
    assert (#l == 0);
    true
    )

listGens = {{0,3,2,1},{1,2,3,0}};    
R = QQ[x_0..x_3];
testInvariance(listGens,secondaryInvariants(listGens,R));

R = QQ[x_1..x_6];
listGens = {{1,2,3,0,5,4},{0,4,2,5,1,3}}		
testInvariance(listGens,secondaryInvariants(listGens,R));


///
end

