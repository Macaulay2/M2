@polytopeFiles = `grep -R "polytope::Polytope&lt;Rational&gt;" /home/lars/polymake-source/apps/polytope/testsuite`;
@coneFiles = `grep -R "polytope::Cone&lt;Rational&gt;" /home/lars/polymake-source/apps/polytope/testsuite`;


sub m2_matrix{
   my($name, $A) = @_;
   my $result = $name." = matrix {{";
   my @rows = map(join(",",@$_), @$A);
   $result .= join("},{", @rows);
   $result .= "}}";
   return $result;
}

sub convex_hull_test{
   my($P) = @_;
   $result = m2_matrix("vertices", $P->VERTICES);
   if($P->BOUNDED){

   } else {

   }
}

sub lattice_points_test_inner{
   my($P) = @_;
   $result = m2_matrix("verticesP", transpose($P->VERTICES->minor(All, ~[0])));
   $result .= ";\n";
   $result .= m2_matrix("desiredLP", transpose($P->LATTICE_POINTS->minor(All, ~[0])));
   $result .= ";\n";
   $result .= "desiredLP = sort desiredLP;\n";
   $result .= "P = convexHull(verticesP)\n";
   $result .= "computedLP = sort matrix {latticePoints P};\n";
   $result .= "assert(desiredLP == computedLP);\n";
}

sub test_wrapper_start{
   my($method, $name) = @_;
   my $result = "-- Test ".$name."\n";
   $result .= "-- Checking ".$method."\n";
   $result .= "TEST ///\n";
   return $result;
}

sub test_wrapper_end{
   return "///\n\n";
}

sub split_off_name{
   my($file) = @_;
   my($a, $b) = $file =~ m/(.*)\/(.*\/[^\/]*)/;
   return $b;
}

sub lattice_point_test{
   my($file) = @_;
   my $name = split_off_name($file);
   my $result = test_wrapper_start("latticePoints", $name);
   my $P = load($file);
   $result .= lattice_points_test_inner($P);
   $result .= test_wrapper_end();
   return $result;
}

sub hilbert_basis_test_inner{
   my($C) = @_;
   print "Entered.\n";
   $result = m2_matrix("raysC", transpose(new Matrix($C->RAYS)));
   print "Rays ok.\n";
   $result .= ";\n";
   $result .= m2_matrix("desiredHB", transpose(new Matrix($C->HILBERT_BASIS)));
   print "Desired ok.\n";
   $result .= ";\n";
   $result .= "desiredHB = sort desiredHB;\n";
   $result .= "C = coneFromVData(raysC)\n";
   $result .= "computedHB = sort matrix {hilbertBasis C};\n";
   $result .= "assert(desiredHB == computedHB);\n";
}

sub hilbert_basis_test{
   my($file) = @_;
   my $name = split_off_name($file);
   my $result = test_wrapper_start("hilbertBasis", $name);
   print "0: ",$result;
   my $P = load($file);
   my $C = new Cone($P);
   $result .= hilbert_basis_test_inner($C);
   print "1: ",$result;
   $result .= test_wrapper_end();
   print "2: ",$result;
   return $result;
}

@a = split(":", $polytopeFiles[0]);
$file = $a[0];
print "LPTest\n",lattice_point_test($file);
print "HBTest\n",hilbert_basis_test($file);


open(FILE, ">hilbert_basis_tests.m2");
$i=0;
foreach my $file (@polytopeFiles){
   print $i,": ",$file,"\n";
   my @split = split(":",$file);
   eval{
      my $P = load($split[0]);
      my $C = new Cone($P);
      if($C->POINTED && $C->DIM > 3){
         print $C->HILBERT_BASIS;
         print FILE hilbert_basis_test($split[0]);
      }
   };
   $i++;
}
close(FILE);



open(FILE, ">lattice_point_tests.m2");
$i=0;
foreach my $file (@polytopeFiles){
   print $i,": ",$file,"\n";
   my @split = split(":",$file);
   eval{
      my $P = load($split[0]);
      print $P->FEASIBLE,"\n";
      if($P->BOUNDED && $P->DIM > 3){
         print FILE lattice_point_test($split[0]);
      }
   };
   $i++;
}
close(FILE);


