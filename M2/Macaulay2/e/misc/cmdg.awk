BEGIN { FS = "\n"; RS = ""}
{printf "%s = gg %d;\n", $1, i++}
