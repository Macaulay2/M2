i = 0 ; while i < 10 list i^2 do i = i+1
i = 0 ; while i < 4 do (print i; i = i+1)
i = 0 ; while i < 10 list (i = i+1; i^2)
i = 0 ; while i < 10 list (i = i+1; if odd i then continue; i^2)
i = 0 ; while i < 10 list (i = i+1; if odd i then continue x; i^2)
i = 0 ; while i < 10 list (i = i+1; if i == 5 then break i; i^2)
i = 0 ; while i < 10 list (i = i+1; if i == 5 then break; i^2)
