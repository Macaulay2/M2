BEGIN { FS = "\n"; RS = ""}
{printf "install_name(%s,\"%s\");\n", $1, $1}

