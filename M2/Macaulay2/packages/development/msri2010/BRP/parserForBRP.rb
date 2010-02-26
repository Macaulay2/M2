suffix = ARGV[0]

open("long#{ARGV[0]}.cpp","w") { |out|
  out << "#include \"brp.h\"\n\nmap<int,BRP> testLong#{ARGV[0]}Example() {\n\ \ \ map<int,BRP> G;\n"
  File.open("tmp#{ARGV[0]}.m2").each { |line|
    line.gsub!(/\},/, '} +')
    line.gsub!(/,\ /, '')
    line.gsub!(/(\d*) =>/) {|i| "G[" + $1.to_s + "] ="}
    line.gsub!(/\{([01]+)\}/) {|m| "BRP(" + $1.to_i(2).to_s+")"}
    line.gsub!(/\{/, '')
    line.gsub!(/\}/, ';')
    line.gsub!(/\ \ \ \ \ \ \ \ \ \ \ /, '')
    line.gsub!(/gbComputation/, '   ')
    line.gsub!(/;*;/,';')
    out << line
  }
  out << "   return G;\n}\n"

  out << "map<int,BRP> testLong#{ARGV[0]}ExampleCorrect() {\n\ \ \ map<int,BRP> correct;\n"
  File.open("tmp#{ARGV[0]}Solution.m2").each { |line|
    line.gsub!(/\},/, '} +')
    line.gsub!(/,\ /, '')
    line.gsub!(/(\d*) =>/) {|i| "correct[" + $1.to_s + "] ="}
    line.gsub!(/\{([01]+)\}/) {|m| "BRP(" + $1.to_i(2).to_s+")"}
    line.gsub!(/\{/, '')
    line.gsub!(/\}/, ';')
    line.gsub!(/\ \ \ \ \ \ \ \ \ \ \ /, '')
    line.gsub!(/gbComputation/, '   ')
    line.gsub!(/;*;/,';')
    out << line
  }
  out << "   return correct;\n}\n"
}

open("long#{ARGV[0]}.h","w") { |out|
  out << "//This takes -- 
// -- used #{ARGV[1]} seconds
//  in M2\n\n"
  out << "map<int,BRP> testLong#{ARGV[0]}Example();\n"
  out << "map<int,BRP> testLong#{ARGV[0]}ExampleCorrect();\n"
}
