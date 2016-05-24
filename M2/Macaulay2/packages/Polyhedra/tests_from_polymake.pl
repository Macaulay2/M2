use TryCatch;

@polytopeFiles = `grep -R "polytope::Polytope&lt;Rational&gt;" /home/lars/polymake-source/apps/polytope/testsuite`;

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
   $result .= "P = convexHull(verticesP)\n";
   $result .= "computedLP = latticePoints P;\n";
   $result .= "assert(numColumns(desiredLP) == #computedLP);\n";
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

sub lattice_point_test{
   my($file) = @_;
   my $result = test_wrapper_start("latticePoints", $file);
   my $P = load($file);
   $result .= lattice_points_test_inner($P);
   $result .= test_wrapper_end();
   return $result;
}

@a = split(":", $polytopeFiles[0]);
$file = $a[0];


$i=0;
foreach my $file (@polytopeFiles){
   print $i,": ",$file,"\n";
   my @split = split(":",$file);
   try{
      my $P = load($split[0]);
      print $P->FEASIBLE,"\n";
   } catch($e){}
   $i++;
}
