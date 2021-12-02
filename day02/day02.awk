/down/ {aim += $2}
/up/ {aim -= $2}
/forward/ {pos += $2; depth += aim * $2}
END {print pos * aim, pos * depth}
# one-liner
# /^d/{a+=$2}/u/{a-=$2}/f/{p+=$2;d+=a*$2}END{print p*a,p*d}
