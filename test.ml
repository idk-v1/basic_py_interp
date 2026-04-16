#use "python.ml"

let input1 =
  "print \"primes 0 to 100:\n\"\n"            ^
  "max = 100\n"                               ^
  "i = 0\n"                                   ^
  "while i < max:\n"                          ^
  "    if i == 2:\n"                          ^
  "        print i\n"                         ^
  "    elif i > 2:\n"                         ^
  "        ii = 2\n"                          ^
  "        found = False\n"                   ^
  "        while ii <= i / 2 and not found:\n"^
  "            if i % ii == 0:\n"             ^
  "                found = True\n"            ^
  "            else:\n"                       ^
  "                ii += 1\n"                 ^
  "        if not found:\n"                   ^
  "            print i\n"                     ^
  "    i += 1\n"
;;

let input2 =
  "print \"even numbers 0 to 100:\n\"\n"^
  "max = 100\n"                         ^
  "i = 0\n"                             ^
  "while i < max:\n"                    ^
  "    if i % 2 == 0:\n"                ^
  "        print i\n"                   ^
  "    i = (i += 1)\n"
;;

execPython input1;;
