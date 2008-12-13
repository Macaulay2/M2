$0 ~ /^turn-off[ \t]*$/ {on = 0}
on {print $0}
$0 ~ /^turn-on[ \t]*$/ {on = 1}
