#use "python.ml"

let primes =
  "print \"primes 0 to \"\n"                  ^
  "input max\n"                               ^
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
  "    i += 1\n"                              ^
  "print \"\n\""
;;

let testUnknownVar =
  "i\n";;

let testNoOp =
  "1 2\n";;

let testNoValue =
  "1 *\n";;

let testBadParentheses =
  "(1 + (2)\n";;

let testNotVariable =
  "True = 5\n";;

let testIllegalElse =
  "else:\n"^
  "    1 + 1\n";;

let testEmptyControl =
  "if True:\n"^
  "1 + 1\n";;

let testIllegalControlBlock =
  "1 + 1\n"^
  "    2 + 2\n";;

let testDivZero =
  "1 / False\n";;

let testBadChar =
  "1 + 112&\n";;

let testInput =
  "print input i\n";;

execPython testInput;;
Printf.printf "\n";;
execPython testUnknownVar;;
execPython testNoOp;;
execPython testNoValue;;
execPython testBadParentheses;;
execPython testNotVariable;;
execPython testIllegalElse;;
execPython testEmptyControl;;
execPython testIllegalControlBlock;;
execPython testDivZero;;
execPython testBadChar;;

execPython primes;;
