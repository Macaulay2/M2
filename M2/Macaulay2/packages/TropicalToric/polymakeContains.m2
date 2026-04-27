polymakeConeContains = method(TypicalValue => Boolean)
polymakeConeContains (List, List) := (v,C) ->(
  --v is a list
  --C is a list
  v = toExternalString v;
  v = replace("\\{","[",v);
  v = replace("\\}","]",v);

  C = toExternalString C;
  C = replace("\\{","[",C);
  C = replace("\\}","]",C);
  C = replace("\\],","],\n",C);

  outputFile := temporaryFileName();

  codeString := "use application \"fan\";
  my $v = new Vector("|v|");
  my $C = new Cone(INPUT_RAYS=>"|C|");
  open(OUT,\">\",\""|outputFile|"\");
  print OUT $C->contains($v);
  close OUT;
  ";

  polymakeCode := temporaryFileName();
  polymakeCode << codeString << close;

  run ("polymake --script "|polymakeCode);

  o := get outputFile;
  if o == "false" then(
    return false;
  ) else(
    return true;
  );
);
