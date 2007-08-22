regular expressions don't work past null characters:

    i31 : match("a"," a")

    o31 = true

    i32 : match("a","\0 a")

    o32 = false

