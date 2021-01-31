-- -*- coding: utf-8 -*-

newPackage(
        "MultiplierIdealsDim2",
        Version  =>  "1.0", 
        Date  =>  "Aug 27, 2015",
        Authors  =>  {{Name  =>  "Ferran Dachs-Cadefau", 
                     Email  =>  "ferran.dachscadefau@wis.kuleuven.be",
                     HomePage  =>  "http://perswww.kuleuven.be/ferran_dachs_cadefau"}},
        Headline  =>  "multiplier ideals in 2-dimensional rings",
	Keywords => {"D-modules"},
        DebuggingMode  =>  false
        )

export {
     "MultiplierIdeals", "Unloading",
     "RelativeCanonicalDivisor", "MultIdeal", "MultiplicityJN"
     }

exportMutable{ "JNandMI", "UnloadingValue", "algorithm", "MaxIterations", "SmallestJN", "BiggestJN", "JumpingDivisor"}



-- Main function
MultiplierIdeals = {SmallestJN => 0,MaxIterations => 10000,BiggestJN => 2,algorithm => "AlbAlvDac",JumpingDivisor => true} >> o -> (F,IntersectionMatrix) -> (
  NumExceptionalDiv := numgens target IntersectionMatrix;
  NumDiv := numgens source IntersectionMatrix;
  if o.algorithm == "AlbAlvDac" then(
     JNandMI = AlbAlvDac(F,IntersectionMatrix,SmallestJN => o.SmallestJN,MaxIterations => o.MaxIterations,BiggestJN => o.BiggestJN);
  )else if o.algorithm == "Tucker" then(
     JNandMI = Tucker(F,IntersectionMatrix,SmallestJN => o.SmallestJN,MaxIterations => o.MaxIterations,BiggestJN => o.BiggestJN);
  )else if o.algorithm == "Mult" and NumExceptionalDiv == NumDiv then(
     JNandMI = Multiplicities(F,IntersectionMatrix,SmallestJN => o.SmallestJN,MaxIterations => o.MaxIterations,BiggestJN => o.BiggestJN);
  )else(
     print "Please choose another algorithm, this ideal is not m-primary";
     JNandMI = 0;
  );
JumpingNumbersTable := new Type of HashTable;
  if (o.JumpingDivisor == false) then(
           net JumpingNumbersTable  :=  CollectionJN -> stack apply (sort keys CollectionJN, k -> " Jumping number: " |net k_0 | " Multiplicity: " | net ((CollectionJN#k)_0)| " Multiplier ideal: " | net matrix (CollectionJN#k)_1);
  )else if (o.JumpingDivisor == true) and o.algorithm == "Mult" then(
           net JumpingNumbersTable  :=  CollectionJN -> stack apply (sort keys CollectionJN, k -> " Jumping number: " |net k_0 | "  Multiplicity: " | net ((CollectionJN#k)_0)| " Multiplier ideal: " | net (CollectionJN#k)_1| " Maximal jumping divisor: "| net matrix ((CollectionJN#k)_2));
  )else if (o.JumpingDivisor == true) and o.algorithm == "AlbAlvDac" then(
           net JumpingNumbersTable  :=  CollectionJN -> stack apply (sort keys CollectionJN, k -> " Jumping number: " |net k_0 | "  Multiplicity: " | net (CollectionJN#k)_0| " Multiplier ideal: " | net matrix((CollectionJN#k)_1)| " Maximal jumping divisor:\n Minimal jumping divisor: "| net (VerticalList (new List from(matrix ((CollectionJN#k)_2),matrix ((CollectionJN#k)_3)))));
  )else if (o.JumpingDivisor == true) and o.algorithm == "Tucker" then(
           net JumpingNumbersTable  :=  CollectionJN -> stack apply (sort keys CollectionJN, k -> " Jumping number: " |net k_0 | "  Multiplicity: " | net (CollectionJN#k)_0| " Multiplier ideal: " | net matrix((CollectionJN#k)_1)| " Maximal jumping divisor:\n Minimal jumping divisor:\n Critical divisor(s): "| net (VerticalList (new List from(matrix ((CollectionJN#k)_2),matrix ((CollectionJN#k)_3),matrix ((CollectionJN#k)_4)))));
  );
new JumpingNumbersTable from JNandMI
)


-- Main function
Unloading = {MaxIterations => 10000,UnloadingValue => false}>>p -> (D,IntersectionMatrix) -> (
  NumExceptionalDiv := numgens target IntersectionMatrix;
  NumDiv := numgens source IntersectionMatrix;
  Tame := 0;
  isUnloaded := false;
  UnloadedDiv := mutableMatrix(ZZ,1,NumDiv);
  for j from 0 to NumDiv - 1 list(
     UnloadedDiv_(0,j) = ceiling(D_(0,j));
  );
  for cont from 0 to p.MaxIterations when (not isUnloaded) list(
    NumUnloadedDiv := 0;
    Condition := transpose(IntersectionMatrix * transpose(matrix(UnloadedDiv)));
    for j from 0 to NumExceptionalDiv - 1 list(
      if (Condition_(0,j) > 0) then(
	UnloadedDiv_(0,j) = UnloadedDiv_(0,j) + ceiling( - Condition_(0,j) / IntersectionMatrix_(j,j));
        if (ceiling( - Condition_(0,j) / IntersectionMatrix_(j,j)) > Tame) then( 
            Tame = ceiling( - Condition_(0,j) / IntersectionMatrix_(j,j));
        );
	NumUnloadedDiv = 0; 
      ) else( 
	NumUnloadedDiv = NumUnloadedDiv + 1; 
      ); 
    ); 
    if (NumUnloadedDiv == NumExceptionalDiv) then(
      isUnloaded = true;
    ); 
  );
  if (not isUnloaded) then(
    print "Not Unloaded, you need more iterations";
  ); 
  if (p.UnloadingValue == true) then(
    return(UnloadedDiv,Tame)
  )else if (p.UnloadingValue == false) then(
    UnloadedDiv
  )
)

-- Main function
RelativeCanonicalDivisor = (IntersectionMatrix) -> (
  NumExceptionalDiv := numgens target IntersectionMatrix;
  NumDiv := numgens source IntersectionMatrix;
  AuxRelCanDivisor := mutableMatrix(ZZ,1,NumExceptionalDiv);
  for i from 0 to NumExceptionalDiv - 1 do(
     AuxRelCanDivisor_(0,i) =  - 2 - IntersectionMatrix_(i,i);
  );
  matrix(AuxRelCanDivisor) * transpose(inverse(promote(submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}),QQ)))|matrix(mutableMatrix(QQ,1,NumDiv - NumExceptionalDiv))
)

-- Tecnical routine to compute the multiplicity of a jumping number (similar to MultiplicityJN)
Multiplicity = (F,IntersectionMatrix,RelCanDivisor,Excess,JumpingNumber) -> (
    MultJN := 0;
    NumDiv := numgens source IntersectionMatrix;
    FracPart := mutableMatrix(QQ,1,NumDiv);
    MaxJumpingDivisor := mutableMatrix(QQ,1,NumDiv);
    for j from 0 to NumDiv - 1 list( 
       FracPart_(0,j) =  - RelCanDivisor_(0,j) + JumpingNumber * F_(0,j) - floor( - RelCanDivisor_(0,j) + JumpingNumber * F_(0,j));
    );
    NumDivIntersect := 0;
    SumExcess := 0;
    for j from 0 to NumDiv - 1 list( 
       if FracPart_(0,j) == 0 then(
          MaxJumpingDivisor_(0,j) = 1;
          for k from 0 to NumDiv - 1 list( 
              if IntersectionMatrix_(k,j) == 1 then(
                   if FracPart_(0,k) == 0 then(
                       NumDivIntersect = NumDivIntersect + 1;
                   )else(
                       MultJN = MultJN + FracPart_(0,k);
                   );
              );
          );
          MultJN = MultJN + JumpingNumber * Excess_(0,j) - 1;
          SumExcess = SumExcess + Excess_(0,j);
       );
    );
    return(MultJN + NumDivIntersect / 2,SumExcess,MaxJumpingDivisor)
)

-- Tecnical routine to compute the ideal associated to a jumping number (similar to MultIdeal)
CompIdeal = {MaxIterations => 10000,UnloadingValue => false} >> o -> (JumpingNumber,IntersectionMatrix,F,RelCanDivisor) -> (
   NumDiv := numgens source IntersectionMatrix;
   Divisor := mutableMatrix(ZZ,1,NumDiv);
   for j from 0 to NumDiv - 1 list( 
       Divisor_(0,j) =  - (ceiling(RelCanDivisor_(0,j) - JumpingNumber * F_(0,j))); 
   );
   Unloading(Divisor,IntersectionMatrix,MaxIterations => o.MaxIterations,UnloadingValue => false)
)


-- Algorithm from [AADG14]
Multiplicities = {SmallestJN => 0,MaxIterations => 10000,BiggestJN => 2} >> o -> (F,IntersectionMatrix) -> (
  NumDiv := numgens source IntersectionMatrix;
  RelCanDivisor := RelativeCanonicalDivisor(IntersectionMatrix);
  Excess :=  - F * IntersectionMatrix;
  JN := new MutableHashTable;
  Candidates := ();
  for j from 0 to NumDiv - 1 list(
     for k from ceiling(RelCanDivisor_(0,j) - F_(0,j)) to floor(RelCanDivisor_(0,j)) list( 
        Candidates = append(Candidates,( - k + RelCanDivisor_(0,j)) / F_(0,j));
     );
  );
  for i from 0 to #Candidates - 1 list(
    JumpingNumber := Candidates#i;
    (MultJN,Increase,MaxJumpingDivisor) := Multiplicity(F,IntersectionMatrix,RelCanDivisor,Excess,JumpingNumber);
    for j from 0 to ceiling(o.BiggestJN) list(
        if MultJN + j * Increase > 0 and JumpingNumber + j <= o.BiggestJN and o.SmallestJN < JumpingNumber + j then( 
           JN#{JumpingNumber + j} = {MultJN + j * Increase,CompIdeal(JumpingNumber + j,IntersectionMatrix,F,RelCanDivisor,MaxIterations => o.MaxIterations,UnloadingValue => false),MaxJumpingDivisor}; 
        );
    );
  );
new HashTable from JN
)




-- Algorithm from [AAD14]
AlbAlvDac = {SmallestJN => 0,MaxIterations => 10000,BiggestJN => 2} >> o -> (F,IntersectionMatrix) -> (
  NumExceptionalDiv := numgens target IntersectionMatrix;
  NumDiv := numgens source IntersectionMatrix;
  RelCanDivisor := RelativeCanonicalDivisor(IntersectionMatrix);
  JN := new MutableHashTable;
  StartingDiv := mutableMatrix(ZZ,1,NumDiv);
  for i from 0 to NumDiv - 1 do(
    StartingDiv_(0,i) = floor(o.SmallestJN * F_(0,i) - RelCanDivisor_(0,i));
  );
  StartingDiv = Unloading(StartingDiv,IntersectionMatrix);
  k := 1;
  if NumExceptionalDiv =!= NumDiv then( 
      CodimPrevMI := ( - (submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,submatrix(matrix(StartingDiv), {NumExceptionalDiv..NumDiv - 1}));
  ) else (
      CodimPrevMI = ( - (submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,0);
        );
  isLastJN := false;
  while (not isLastJN) do(
    JumpingNumber := o.BiggestJN + 1;
    MinJumpingDivisor := mutableMatrix(ZZ,1,NumDiv);
    for i from 0 to NumDiv - 1 list(
        CandidateJN := (RelCanDivisor_(0,i) + StartingDiv_(0,i) + 1) / F_(0,i);
        if (JumpingNumber > CandidateJN) then (
            MinJumpingDivisor = mutableMatrix(ZZ,1,NumDiv);
            JumpingNumber = CandidateJN;
            MinJumpingDivisor_(0,i) = 1;
        )else if (JumpingNumber == CandidateJN) then (
            MinJumpingDivisor_(0,i) = 1;   
        );  
     );
     if (JumpingNumber > o.BiggestJN) then(
	isLastJN = true;
     )else(
        FracPart := mutableMatrix(QQ,1,NumDiv);
        MaxJumpingDivisor := mutableMatrix(QQ,1,NumDiv);
        for j from 0 to NumDiv - 1 list( 
            FracPart_(0,j) =  - RelCanDivisor_(0,j) + JumpingNumber * F_(0,j) - floor( - RelCanDivisor_(0,j) + JumpingNumber * F_(0,j));
            if FracPart_(0,j) == 0 then(
                MaxJumpingDivisor_(0,j) = 1;
            );
        );
     	DivJN := CompIdeal(JumpingNumber,IntersectionMatrix,F,RelCanDivisor,MaxIterations => o.MaxIterations,UnloadingValue => false);
        if NumExceptionalDiv =!= NumDiv then( 
            CodimActMI := ( - (submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,submatrix(matrix(DivJN), {NumExceptionalDiv..NumDiv - 1}));
        )else(
            CodimActMI = ( - (submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,0);
        );
        if CodimPrevMI_1 =!= CodimActMI_1 then(
            JN#{JumpingNumber} = {infinity,DivJN,MaxJumpingDivisor,MinJumpingDivisor};
        )else(
            JN#{JumpingNumber} = {CodimActMI_0 - CodimPrevMI_0,DivJN,MaxJumpingDivisor,MinJumpingDivisor};
        );
        CodimPrevMI = CodimActMI;
        k = k + 1;
        StartingDiv = DivJN;
     );
  );
new HashTable from JN
)




-- Recursive routine to generate the critical chains
Chain = (actp, Combinations, Divisor, IntersectionMatrix, RuptureDivisors,NumDiv) -> (
  DivisorMutable := mutableMatrix Divisor;
  NumExceptionalDiv := numgens target IntersectionMatrix;
  if RuptureDivisors_(0,actp) == 1 then(
    isInCombination := false;
    for i from 0 to #Combinations - 1 when not isInCombination do(
        AreEqual := true;
        for j from 0 to NumExceptionalDiv - 1 when AreEqual do(
            AreEqual = (Combinations#i_(0,j) == DivisorMutable_(0,j));
        );
        isInCombination = AreEqual;
    ); 
    if not isInCombination then(
       Combinations#(#Combinations) = matrix DivisorMutable;
    );
  );
  for i from 0 to NumExceptionalDiv - 1 do(
      if IntersectionMatrix_(i,actp) == 1 and DivisorMutable_(0,i) == 0 then(
          DivisorMutable_(0,i) = 1;
          Combinations = Chain(i, Combinations, DivisorMutable, IntersectionMatrix, RuptureDivisors,NumDiv);
          --DivisorMutable_(0,i) = 0;
      );
  );
Combinations 
)

-- Tecnical routine to generate the critical chains
GenChains = (IntersectionMatrix,F,NumDiv) -> (
  NumExceptionalDiv := numgens target IntersectionMatrix;
  Combinations := new MutableList;
  Divisor := mutableMatrix(ZZ,1,NumDiv);
    RuptureDivisors := mutableMatrix(ZZ,1,NumDiv);
    Excess :=  - F * IntersectionMatrix;
    for i from 0 to NumExceptionalDiv - 1 list(
       if Excess_(0,i) > 0 then(
          RuptureDivisors_(0,i) = 1;
       );
       s  :=  0;
       for j from 0 to NumExceptionalDiv - 1 list(
          s = s + IntersectionMatrix_(j,i);
       ); 
       s = s - IntersectionMatrix_(i,i); 
       if s > 2 then(
          RuptureDivisors_(0,i) = 1;
       );
       RuptureDivisors_(0,0) = 1;
    );
    for i from 0 to NumExceptionalDiv - 1 do(
       if RuptureDivisors_(0,i) == 1 then(
          Divisor_(0,i) = 1;
          Combinations = Chain(i,Combinations,Divisor,IntersectionMatrix,RuptureDivisors,NumDiv);
          Divisor_(0,i) = 0;
       );
    );
  Combinations
)

-- Algorithm form [Tuc10]
Tucker = {SmallestJN => 0,MaxIterations => 10000,BiggestJN => 2} >> o -> (F,IntersectionMatrix) -> (
  NumExceptionalDiv := numgens target IntersectionMatrix;
  NumDiv := numgens source IntersectionMatrix;
  RelCanDivisor := RelativeCanonicalDivisor(IntersectionMatrix);
  Excess :=  - F * transpose(IntersectionMatrix);
  CandidateJumpingNumbers := new MutableList;
  JNList := new MutableHashTable;
  for k from 0 to NumDiv - 1 do(
    Candidates := new MutableList;
    NextDivisor := false;
    j := 0;
    while not NextDivisor do(
      j = j + 1;
      CandidateJN := (RelCanDivisor_(0,k) + j) / F_(0,k);
      if (CandidateJN <= o.SmallestJN) then(
      ) else if (CandidateJN <= o.BiggestJN) and (CandidateJN > o.SmallestJN) then(
        Candidates#(#Candidates) = CandidateJN;
        if k >= NumExceptionalDiv then(
            Chain := mutableMatrix(ZZ,1,NumDiv);
            Chain_(0,k) = 1;
            if not JNList#?({CandidateJN}) then(
                  JNList#({CandidateJN}) = new List from entries Chain;
            ) else(
                   JNList#({CandidateJN}) = VerticalList((JNList#({CandidateJN}))|new List from entries Chain);               
            );
        );
      )else (
	NextDivisor = true;
      );
    );
    if k <  NumExceptionalDiv then( 
       CandidateJumpingNumbers#k = Candidates;
    );
  );
  NumIntersectingDiv := mutableMatrix(ZZ,1,NumDiv);
  for i from 0 to NumExceptionalDiv - 1 do(
    for j from 0 to NumDiv - 1 do(
      if i =!= j then(
        NumIntersectingDiv_(0,i) = NumIntersectingDiv_(0,i) + IntersectionMatrix_(i,j);
      );
    );
  );
  for i from 0 to NumExceptionalDiv - 1 do(
    if Excess_(0,i) >= 1 then(
       NumIntersectingDiv_(0,i) = NumIntersectingDiv_(0,i) + 1;
    );
  );
  Chains := new List from GenChains(submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}),submatrix(F, {0..NumExceptionalDiv - 1}),NumDiv);
  AdmissibleChains := new MutableList;
  for i from 0 to #Chains - 1 do(
    Chain := mutableMatrix(ZZ,1,NumDiv);
    isAdmissibleChain := true;
    for j from 0 to NumExceptionalDiv - 1 when isAdmissibleChain do(
      if Chains#i_(0,j) == 1 then(
        for k from 0 to NumDiv - 1 do(
          if j =!= k then( Chain_(0,j) = Chain_(0,j) + Chains#i_(0,k) * IntersectionMatrix_(j,k););
        );
        if Chain_(0,j) == 1 and NumIntersectingDiv_(0,j) < 3 then( isAdmissibleChain = false; );
      );
    );
    if isAdmissibleChain then(
      NElems := 0;
      StartingElem := NumExceptionalDiv;
      for k from 0 to NumExceptionalDiv - 1 do(
        NElems = NElems + Chains#i_(0,k);
        if Chains#i_(0,k) > 0 and k < StartingElem then( StartingElem = k;);
      );
      AdmissibleChains#(#AdmissibleChains) = (Chains#i,NElems,StartingElem);
    );
  );
  JN := new MutableList;
-- Part of JN
  for jj from 0 to #AdmissibleChains - 1 do(
    JNOptions := CandidateJumpingNumbers#(AdmissibleChains#jj#2); 
    for i from (AdmissibleChains#jj#2) to NumExceptionalDiv - 1 do(
      if AdmissibleChains#jj#0_(0,i) == 1 then(
         Candidates := new MutableList;
         for j from 0 to #JNOptions - 1 do(
           SearchedJN := JNOptions#j; 
           for k from 0 to #CandidateJumpingNumbers#i - 1 when SearchedJN >= CandidateJumpingNumbers#i#k do(
             if SearchedJN == CandidateJumpingNumbers#i#k then(
               Candidates#(#Candidates) = SearchedJN;
             );
           ); 
         );
         JNOptions = Candidates;
      );
    );
-- Computation of  - G * E_i
    Candidates := new MutableList;                             
    for i from 0 to #JNOptions - 1 do(
      JNandValues := new MutableList;  
      if AdmissibleChains#jj#1 > 1 then(
        Values := mutableMatrix(ZZ,1,AdmissibleChains#jj#1);
        CandidateJN := JNOptions#i;
        a  :=  0;
        for j from 0 to NumDiv - 1 do(
          if AdmissibleChains#jj#0_(0,j) == 1 then( 
            R := 0;
            for k from 0 to NumDiv - 1 do(
              R = R + ceiling(RelCanDivisor_(0,k) - CandidateJN * F_(0,k)) * IntersectionMatrix_(j,k);
            ); 
            Values_(0,a) = R;
            a = a + 1;
          );
        );
        JNandValues#(#JNandValues) = (CandidateJN,Values);
-- Check if  - G * E_i satisfies the conditions of the reference
        IntersectionVal := new MutableList;
        for kk from 0 to NumDiv - 1 do(
          if AdmissibleChains#jj#0_(0,kk) == 1 then(
           R := 0;
           for k from 0 to NumDiv - 1 do(
               R = R - AdmissibleChains#jj#0_(0,k) * IntersectionMatrix_(kk,k);
           ); 
           IntersectionVal#(#IntersectionVal) = R; 
          );
        );
        for kk from 0 to #JNandValues - 1 do(
          A := 0;
          for j from 0 to #IntersectionVal - 1 do(
            if IntersectionVal#j == JNandValues#kk#1_(0,j) then(
                 A = A + 1;
            );
          );
          if A == #IntersectionVal then(
            Candidates#(#Candidates) = JNandValues#kk#0;
          );
        );
--  Part for G = E_i
      )else(
        R := 0;
        for l from 0 to NumDiv - 1 do(
          R = R + ceiling(RelCanDivisor_(0,l) - (JNOptions#i) * F_(0,l)) * IntersectionMatrix_(AdmissibleChains#jj#2,l);
        ); 
        if R  >=   - IntersectionMatrix_(AdmissibleChains#jj#2,AdmissibleChains#jj#2) then( 
          Candidates#(#Candidates) = JNOptions#i;
        ); 
      );
    ); 
    JN#jj = (AdmissibleChains#jj#0,Candidates);
-- Tecnical part for the output
  );
  for i from 0 to #JN - 1 do(
    for j from 0 to #(JN#i#1) - 1 do(
      if not JNList#?({JN#i#1#j}) then(
         JNList#({JN#i#1#j}) = new List from entries JN#i#0;
      ) else(
         JNList#({JN#i#1#j}) = VerticalList((JNList#({JN#i#1#j}))|new List from entries JN#i#0);         
      );
    );
  );
  JN = new MutableHashTable;
  OrderedJN := sort keys JNList;
  StartingDiv := mutableMatrix(ZZ,1,NumDiv);
  for i from 0 to NumDiv - 1 do(
    StartingDiv_(0,i) = floor(o.SmallestJN * F_(0,i) - RelCanDivisor_(0,i));
  );
  StartingDiv = Unloading(StartingDiv,IntersectionMatrix);
  k := 1;
  if NumExceptionalDiv =!= NumDiv then( 
      CodimPrevMI := ( - (submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,submatrix(matrix(StartingDiv), {NumExceptionalDiv..NumDiv - 1}));
  )else(
      CodimPrevMI = ( - (submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(StartingDiv), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,0);
  );
    StartingDiv = CompIdeal(o.SmallestJN,IntersectionMatrix,F,RelCanDivisor,MaxIterations => o.MaxIterations,UnloadingValue => false);
  for i from 0 to #OrderedJN - 1 do(
    MinJumpingDivisor := mutableMatrix(ZZ,1,NumDiv);
    for j from 0 to NumDiv - 1 list(
        CandidateJN := (RelCanDivisor_(0,j) + StartingDiv_(0,j) + 1) / F_(0,j);
        if (OrderedJN#i_0 == CandidateJN) then (
            MinJumpingDivisor_(0,j) = 1;   
        );  
    );
        FracPart := mutableMatrix(QQ,1,NumDiv);
        MaxJumpingDivisor := mutableMatrix(QQ,1,NumDiv);
        for j from 0 to NumDiv - 1 list( 
            FracPart_(0,j) =  - RelCanDivisor_(0,j) + (OrderedJN#i_0) * F_(0,j) - floor( - RelCanDivisor_(0,j) + (OrderedJN#i_0) * F_(0,j));
            if FracPart_(0,j) == 0 then(
                MaxJumpingDivisor_(0,j) = 1;
            );
        );
    DivJN := CompIdeal(OrderedJN#i_0,IntersectionMatrix,F,RelCanDivisor,MaxIterations => o.MaxIterations,UnloadingValue => false);
        if NumExceptionalDiv =!= NumDiv then( 
            CodimActMI := ( - (submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,submatrix(matrix(DivJN), {NumExceptionalDiv..NumDiv - 1}));
        )else(
            CodimActMI = ( - (submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) * submatrix(IntersectionMatrix, {0..NumExceptionalDiv - 1}) * transpose(submatrix(matrix(DivJN), {0..NumExceptionalDiv - 1}) + submatrix(RelCanDivisor, {0..NumExceptionalDiv - 1})))_(0,0) / 2,0);
        );
        if CodimPrevMI_1 =!= CodimActMI_1 then(
            JN#(OrderedJN#i) = {infinity,DivJN,MaxJumpingDivisor,MinJumpingDivisor,JNList#(OrderedJN#i)};
        )else(
            JN#(OrderedJN#i) = {CodimActMI_0 - CodimPrevMI_0,DivJN,MaxJumpingDivisor,MinJumpingDivisor,JNList#(OrderedJN#i)};
        );
        CodimPrevMI = CodimActMI;
        k = k + 1;
  ); 
new HashTable from JN
)


-- Main algorithm
MultiplicityJN = (F,IntersectionMatrix,JumpingNumber) -> (
  RelCanDivisor := RelativeCanonicalDivisor(IntersectionMatrix);
  NumDiv := numgens source IntersectionMatrix;
  Excess :=  - F * IntersectionMatrix;
    MultJN:= 0;
    FracPart := mutableMatrix(QQ,1,NumDiv);
    for j from 0 to NumDiv - 1 list( 
       FracPart_(0,j) =  - RelCanDivisor_(0,j) + JumpingNumber * F_(0,j) - floor( - RelCanDivisor_(0,j) + JumpingNumber * F_(0,j));
    );
    NumDivIntersect := 0;
    for j from 0 to NumDiv - 1 list( 
       if FracPart_(0,j) == 0 then(
          for k from 0 to NumDiv - 1 list( 
              if IntersectionMatrix_(k,j) == 1 then(
                   if FracPart_(0,k) == 0 then(
                       NumDivIntersect = NumDivIntersect + 1;
                   )else(
                       MultJN = MultJN + FracPart_(0,k);
                   );
              );
          );
          MultJN = MultJN + JumpingNumber * Excess_(0,j) - 1;
       );
    );
    return(MultJN + NumDivIntersect / 2)
)

-- Main algorithm
MultIdeal = {MaxIterations => 10000} >> o -> (F,IntersectionMatrix,JumpingNumber) -> (
  RelCanDivisor := RelativeCanonicalDivisor(IntersectionMatrix);
   NumDiv := numgens source IntersectionMatrix;
   Divisor := mutableMatrix(ZZ,1,NumDiv);
   for j from 0 to NumDiv - 1 list( 
       Divisor_(0,j) =  - (ceiling(RelCanDivisor_(0,j) - JumpingNumber * F_(0,j))); 
   );
   Unloading(Divisor,IntersectionMatrix,MaxIterations => o.MaxIterations)
)


beginDocumentation()

document {
     Key  =>  MultiplierIdealsDim2,
        Headline  =>  "A package for Multiplier Ideals",

        EM "MultiplierIdealsDim2", " is a package that contains several tools related 
        with the computations of multiplier ideals. Given the self intersection matrix  and the divisor associated to this ideal,
        using the function ", TO MultiplierIdeals, ", one can compute the jumping numbers and their
        associated multiplier ideals in the interval (", TO SmallestJN, "," , TO BiggestJN ,"] using either
        the algorithms presented on [Tuc10], [AAD14] or [AADG14]. However, if we want to know 
        the multiplicity of a given number as a jumping number for a given ideal, one can use ", TO MultiplicityJN , 
        ". Another option that this package offers is to compute the Multiplier Ideal associated 
        to a given number, thanks to the function ", TO MultIdeal , ". The package also contains two
        extra functions: to compute the relative Canonical divisor of a resolution 
        (", TO RelativeCanonicalDivisor, "). And to compute the antinef closure of a given 
        divisor (", TO Unloading, ").",
	
	BR{},BR{},
	BOLD "Literature \n",
	UL {
	  LI {"[AAD14] ", EM "Multiplier ideals in two-dimensional local rings with rational singularities", " (M. Alberich-Carramiñana, J. Àlvarez Montaner, F. Dachs-Cadefau, 2014).\n"},
	  LI {"[AADG14] ", EM "Poincaré series of multiplier ideals in two-dimensional local rings with rational singularities", " (M. Alberich-Carramiñana, J. Àlvarez Montaner, F. Dachs-Cadefau, V. González-Alonso, 2014)\n"},
	  LI {"[Tuc10] ", EM " Jumping numbers on algebraic surfaces with rational singularities", " (K. Tucker, 2010)"}
	  }
}

document {
     Key  =>  MultiplierIdeals,
     Headline  =>  "Computes the jumping numbers and their ideals",
     Usage  =>  "MultiplierIdeals(F,E)",
     Inputs  =>  {
        "F"  =>   {"Matrix"}, 
        "E"  =>   {"Intersection matrix."} 
          },
     Outputs  =>  {
      "A table that contains at least the jumping number, their multiplicities and the ideals"
          },
     "Starting form the divisor encoded as a matrix of dimensions 1 x m, and the intersection matrix as presented in [AAD14], the algorithm computes the jumping numbers for this ideal with their multiplicities and associated ideals in the interval (", TO SmallestJN, ",", TO BiggestJN, "].",
     EXAMPLE {
      "E = matrix({{ -5,  0,  1,  0,  1},
                {  0, -2,  1,  0,  0},
                {  1,  1, -1,  0,  0},
                {  0,  0,  0, -2,  1},
                {  1,  0,  0,  1, -1}})",
      "F = matrix({{4,5,10,5,10}})",
      "MultiplierIdeals(F,E,BiggestJN => 1)" 
          },
     }  

document {
     Key  =>  RelativeCanonicalDivisor,
     Headline  =>  "computes the relative canonical divisor",
     Usage  =>  "RelativeCanonicalDivisor(E)",
     Inputs  =>  {
        "E"  =>   {"Intersection matrix."} 
          },
     Outputs  =>  {
          "The relative canonical divisor of the resolution."
          },
          "Starting form the intersection matrix as presented in [AAD14], it returns the relative canonical divisor of the resolution as a matrix 1 x m.",
     EXAMPLE {
      "E = matrix({{ -5,  0,  1,  0,  1},
                {  0, -2,  1,  0,  0},
                {  1,  1, -1,  0,  0},
                {  0,  0,  0, -2,  1},
                {  1,  0,  0,  1, -1}})",
      "RelativeCanonicalDivisor(E)" 
          },
     }   

document {
     Key  =>  Unloading,
     Headline  =>  "Computes the antinef closure of a divisor.",
     Usage  =>  "Unloading(D,E)",
     Inputs  =>  {
        "E"  =>   {"Intersection matrix."},
        "D"  =>   "Matrix"
          },
     Outputs  =>  {
          "Starting form the divisor encoded as a matrix of dimensions 1 x m, and the intersection matrix as presented in [AAD14], it returns the antinef closure of the divisor.",
          },
     EXAMPLE {
      "E = matrix({{ -5,  0,  1,  0,  1},
                {  0, -2,  1,  0,  0},
                {  1,  1, -1,  0,  0},
                {  0,  0,  0, -2,  1},
                {  1,  0,  0,  1, -1}})",
      "D = matrix({{1,9,8,8,5}})",
      "Unloading(D,E)" 
          },
     } 

document {
     Key  =>  MultIdeal,
     Headline  =>  "Computes the multiplier ideal of a given number.",
     Usage  =>  "MultIdeal(F,E,jn)",
     Inputs  =>  {
        "F"  =>   "Matrix", 
        "E"  =>   "Matrix",
        "jn"  =>   "Real number"
          },
     Outputs  =>  {
          "The multiplier ideal associated to jn."
          },
          "Starting form the divisor encoded as a matrix of dimensions 1 x m, the intersection matrix as presented in [AAD14] and a real number, it returns the multiplier ideal associated to this number.",
     EXAMPLE {
      "E = matrix({{ -5,  0,  1,  0,  1},
                {  0, -2,  1,  0,  0},
                {  1,  1, -1,  0,  0},
                {  0,  0,  0, -2,  1},
                {  1,  0,  0,  1, -1}})",
      "F = matrix({{4,5,10,5,10}})",
      "MultIdeal(F,E,1 / 2)" 
          },
     } 


document {
     Key  =>  MultiplicityJN,
     Headline  =>  "Computes the multiplicity as a jumping number.",
     Usage  =>  "MultIdeal(F,E,jn)",
     Inputs  =>  {
        "F"  =>   "Matrix", 
        "E"  =>   "Matrix",
        "jn"  =>    "Real number",
          },
     Outputs  =>  {
          "The multiplicity of jn as a jumping number."
          },
          "Starting form the divisor encoded as a matrix of dimensions 1 x m, the intersection matrix as presented in [AAD14] and a real number jn, it returns the multiplicity of jn as a jumping number. It is important to notice that if jn is not a jumping number, then the multiplicity will be zero.",
     EXAMPLE {
      "E = matrix({{ -5,  0,  1,  0,  1},
           {  0, -2,  1,  0,  0},
           {  1,  1, -1,  0,  0},
           {  0,  0,  0, -2,  1},
           {  1,  0,  0,  1, -1}})",
      "F = matrix({{4,5,10,5,10}})",
      "MultiplicityJN(F,E,1 / 2)" 
          },
     } 

document {
     Key  =>  JNandMI,
     Headline  =>  "HashTable containing all the information about the Jumping Numbers",
     "This HashTable contains all the information about the jumping numbers that computes ",TO MultiplierIdeals, ". As a key, it contains the Jumping Number and for each Jumping Number it contains the multiplicity, the divisor associated to the ideal, the Maximal and Minimal Jumping Divisors and (if it applies) the critical chains.",
     EXAMPLE {
      "E = matrix({{ -5,  0,  1,  0,  1},
           {  0, -2,  1,  0,  0},
           {  1,  1, -1,  0,  0},
           {  0,  0,  0, -2,  1},
           {  1,  0,  0,  1, -1}})",
      "F = matrix({{4,5,10,5,10}})",
      "MultiplierIdeals(F,E,BiggestJN => 1)",
      "JNandMI",
          },
}
     
     
document {
     Key  =>  {algorithm,
	  [MultiplierIdeals,algorithm]},
     Headline  =>  "Method used to compute the jumping numbers and multiplier ideals",
     "Default value \"AlbAlvDac\". This variable is used to choose which method we want to use to compute the jumping numbers, the three options are \"AlbAlvDac\" for the algorithm in [AAD14], \"Mult\" for the one in [AADG14] and \"Tucker\" for [Tuc10].",
     
     	BR{},BR{},
	BOLD "Literature \n",
	UL {
	  LI {"[AAD14] ", EM "Multiplier ideals in two-dimensional local rings with rational singularities", " (M. Alberich-Carramiñana, J. Àlvarez Montaner, F. Dachs-Cadefau, 2014).\n"},
	  LI {"[AADG14] ", EM "Poincaré series of multiplier ideals in two-dimensional local rings with rational singularities", " (M. Alberich-Carramiñana, J. Àlvarez Montaner, F. Dachs-Cadefau, V. González-Alonso, 2014)\n"},
	  LI {"[Tuc10] ", EM " Jumping numbers on algebraic surfaces with rational singularities", " (K. Tucker, 2010)"}
	  }
}

document {
     Key  =>  {MaxIterations,
	  [MultiplierIdeals,MaxIterations],
	  [Unloading,MaxIterations],
	  [MultIdeal,MaxIterations]},
     Headline  =>  "Limits the number of iterations of the Unloading algorithm.",
     "Default value 10000. This variable is used to limitate the number of iterations of the Unloading algorithm. If the resulting divisor is not unloaded, a warning will appear.",
}

document {
     Key  =>  {BiggestJN,
	  [MultiplierIdeals,BiggestJN]},
     Headline  =>  "Upper bound of the interval where we want to compute the JN.",
     "Default value 2. Upper bound of the interval where be computed the jumping numbers. The lower bound is ", TO SmallestJN, ".",
}

document {
     Key  =>  {SmallestJN,
	  [MultiplierIdeals,SmallestJN]},
     Headline  =>  "Lower bound of the interval where we want to compute the JN.",
     "Default value 0. Lower bound of the interval where be computed the jumping numbers. The upper bound is ", TO BiggestJN, ".",
}

document {
     Key  =>  {JumpingDivisor,
	  [MultiplierIdeals,JumpingDivisor]},
     Headline  =>  "Show or not the jumping divisors.",
     "Default value true, whether to show the jumping divisors associated to each jumping number or not.",
     }

document {
     Key  =>  {UnloadingValue,
	  [Unloading,UnloadingValue]},
     Headline  =>  "Show the maximum of the unloaded values",
     "Default value false, whether to show the maximum exces in the unloading procedure.",
}


TEST ///
E = matrix(
{{ -5,  0,  1,  0,  1},
 {  0, -2,  1,  0,  0},
 {  1,  1, -1,  0,  0},
 {  0,  0,  0, -2,  1},
 {  1,  0,  0,  1, -1}})
F = matrix({{4,5,10,5,10}})
assert(#MultiplierIdeals(F,E,BiggestJN => 1) == 4)
///

TEST ///
E = matrix(
{{ -5,  0,  1,  0,  1},
 {  0, -2,  1,  0,  0},
 {  1,  1, -1,  0,  0},
 {  0,  0,  0, -2,  1},
 {  1,  0,  0,  1, -1}})
F = matrix({{4,5,10,5,10}})
assert(#MultiplierIdeals(F,E,SmallestJN => 1 / 2,BiggestJN => 1,algorithm => "Tucker") == 3)
///

TEST ///
E = matrix(
{{ -5,  0,  1,  0,  1},
 {  0, -2,  1,  0,  0},
 {  1,  1, -1,  0,  0},
 {  0,  0,  0, -2,  1},
 {  1,  0,  0,  1, -1}})
D = matrix({{0,0,1,0,1}})
assert(Unloading(D,E) == mutableMatrix{{1,1,2,1,2}})
///

TEST ///
E = matrix(
{{ -5,  0,  1,  0,  1},
 {  0, -2,  1,  0,  0},
 {  1,  1, -1,  0,  0},
 {  0,  0,  0, -2,  1},
 {  1,  0,  0,  1, -1}})
assert(RelativeCanonicalDivisor(E) == matrix({{promote(1,QQ),2,4,2,4}}))
///

TEST ///
E = matrix(
{{ -5,  0,  1,  0,  1},
 {  0, -2,  1,  0,  0},
 {  1,  1, -1,  0,  0},
 {  0,  0,  0, -2,  1},
 {  1,  0,  0,  1, -1}})
F = matrix({{4,5,10,5,10}})
assert(MultiplicityJN(F,E,1 / 2) == 1)
///

TEST ///
E = matrix(
{{ -5,  0,  1,  0,  1},
 {  0, -2,  1,  0,  0},
 {  1,  1, -1,  0,  0},
 {  0,  0,  0, -2,  1},
 {  1,  0,  0,  1, -1}})
F = matrix({{4,5,10,5,10}})
assert(MultIdeal(F,E,1 / 2) == mutableMatrix({{1,1,2,1,2}}))
///

