BEGIN { FS = "\n"; RS = ""}
      {
	   printf "document { quote %s,\n     TT \"%s\", \" -- gbengine command string.\",\n     PARA,\n     \"arguments: %s\",\n     PARA,\n     \"%s\"\n     }\n", $1,$1,$3,$4
	   }
