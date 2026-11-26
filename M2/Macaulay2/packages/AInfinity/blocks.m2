-- Schematic display of the blocks of a map

picture = method(Options => {ShowRanks => false})

-- A typed Burke term stores its words on the enclosing direct sum. A
-- global Burke term can instead be an unlabeled sum of labeled summands.
-- Preserve explicitly assigned outer labels, including integer labels.
moduleForBlocks = M -> (
   if M == 0 or not isDirectSum M then return label(null,M);
   if isLabeled M then (
       if not areLabelsUnique M then error "expected distinct summand labels";
       return M);
   (parts,words) := componentsAndIndices M;
   if all(words,w -> w === null) then return M;
   if #set words != #words then error "expected distinct summand labels";
   directSum apply(words,parts,(w,N) -> w => N))

labeledBlockMap = M -> map(moduleForBlocks target M,moduleForBlocks source M,M,Degree => degree M)

pictureAt = (M,showRanks,point) -> (
   -- Give a matrix of ordinary modules a single block on each unlabeled side.
   M = labeledBlockMap M;
   src := indices source M;
   tar := indices target M;
   R := ring M;
   -- A standalone matrix uses the origin over a coefficient field. Some
   -- affine quotients do not contain it and have no residue rank there.
   red := if point =!= null then (
       if point == ideal(1_R) then null else map(R/point,R))
       else if isField R then map(R,R)
       else if (try isField coefficientRing R else false) then (
           m := ideal vars R;
           if m == ideal(1_R) then null else map(R/m,R))
       else null;
   caption := (u,r) -> if showRanks then (u,r) else u;
   netList(prepend(prepend("",apply(src,s -> caption(s,numcols M_[s]))),
       for t in tar list prepend(caption(t,numrows M^[t]),for s in src list (
           block := M^[t]_[s];
           if block == 0 then "." else if numrows block == numcols block and block == 1 then "id"
           else if red === null then "*"
           else (r := rank(red block); if r == 0 then "*" else toString r)))),Alignment => Center)
)

picture Matrix := o -> M -> pictureAt(M,o.ShowRanks,null)

picture Module := o -> M -> picture(id_M,o)
picture Complex := o -> C -> (
    point := if C.cache#?"AInfinityModelPoint" then C.cache#"AInfinityModelPoint" else null;
    netList apply(toList(min C+1..max C),i -> pictureAt(C.dd_i,o.ShowRanks,point)))
picture ChainComplex := o -> C -> netList apply(toList(min C+1..max C), i-> picture(C.dd_i,o))

-- Display the blocks of a map

displayBlocks = method()
displayBlocks Matrix := M -> (
   M = labeledBlockMap M;
   src := indices source M;
   tar := indices target M;
   netList(prepend(prepend("",src),
       for t in tar list prepend(t,for s in src list (
           block := M^[t]_[s];
           if block == 0 then "." else if numrows block == numcols block and block == 1 then "1" else net block))),Alignment => Center)
)

-- Return a block of the map corresponding to labels of summands

extractBlocks = method()
blockSelection = (M,L) -> (
   if not areLabelsUnique M then error "expected distinct summand labels";
   keys := labels M;
   -- A single integer word, or a list of words in the requested order.
   chosen := if member(L,keys) then {L} else L;
   if not all(chosen,k -> member(k,keys)) then error "unknown summand label";
   new Array from chosen)

extractBlocks(Matrix,List) := Matrix => (phi,src) -> (
   f := labeledBlockMap phi;
   if src === {} then return map(target f,(ring f)^0,0,Degree => degree f);
   f_(blockSelection(source f,src)))
extractBlocks(Matrix,List,List) := Matrix => (phi,tar,src) -> (
   f := labeledBlockMap phi;
   if tar === {} then (
       sub := extractBlocks(f,src);
       return map((ring f)^0,source sub,0,Degree => degree f));
   g := f^(blockSelection(target f,tar));
   extractBlocks(g,src))
