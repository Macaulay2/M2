BEGIN { 
      FS = "\n" 
      RS = ""
      printf "document { \"low level gb engine commands\",\n"
      printf "     MENU {\n"
      }

      {
      printf "          TO \"%s\",\n", $1 
      }

END {
    printf "          }\n"
    printf "     }\n"
    }
