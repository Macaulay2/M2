BEGIN { FS = "\n"; RS = ""}
{printf "const int %s = %d;\n", $1, i++}
END {printf "const int NCOMMANDS = %d;\n", i}
