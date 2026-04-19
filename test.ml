(*
   Name: Ben Hamilton
   File: test.ml
*)

#use "execpython.ml"

let primes =
  "print \"primes from \""                ^"\n"^
  "input i"                               ^"\n"^
  "print \"to \""                         ^"\n"^
  "input max"                             ^"\n"^
  "while i <= max:"                       ^"\n"^
  "    if i == 2:"                        ^"\n"^
  "        print i"                       ^"\n"^
  "        print \" \""                   ^"\n"^
  "    elif i > 2:"                       ^"\n"^
  "        ii = 2"                        ^"\n"^
  "        found = False"                 ^"\n"^
  "        sq = sqrt i"                   ^"\n"^
  "        while ii <= sq and not found:" ^"\n"^
  "            if i % ii == 0:"           ^"\n"^
  "                found = True"          ^"\n"^
  "            else:"                     ^"\n"^
  "                ii += 1"               ^"\n"^
  "        if not found:"                 ^"\n"^
  "            print i"                   ^"\n"^
  "            print \" \""               ^"\n"^
  "    i += 1"                            ^"\n"^
  "println \"\""                          ^"\n"
;;

let testUnknownVar =
  "println \"\nTest: Unknown Variable\"\n"^
  "i\n";;

let testNoOp =
  "println \"\nTest: No Operator\"\n"^
  "1 2\n";;

let testNoValue =
  "println \"\nTest: No Value\"\n"^
  "1 *\n";;

let testBadParentheses =
  "(1 + (2)\n";;

let testNotVariable =
  "println \"\nTest: Not a Variable\"\n"^
  "True = 5\n";;

let testIllegalElse =
  "println \"\nTest: Illegal Else\"\n"^
  "else:\n"^
  "    1 + 1\n";;

let testEmptyControl =
  "println \"\nTest: Empty Control Block\"\n"^
  "if True:\n"^
  "1 + 1\n";;

let testIllegalControlBlock =
  "println \"\nTest: Illegal Control Block\"\n"^
  "1 + 1\n"^
  "    2 + 2\n";;

let testDivZero =
  "println \"\nTest: Divide by Zero\"\n"^
  "1 / False\n";;

let testBadChar =
  "1 + 112&\n";;

let testInput =
  "println \"\nTest: Input\"\n"^
  "input in\n"^
  "println in\n";;

print_endline "started";;
execPython testInput false;;
execPython testUnknownVar false;;
execPython testNoOp false;;
execPython testNoValue false;;
Printf.printf "\nTest: Bad Parentheses\n%!";;
execPython testBadParentheses false;;
execPython testNotVariable false;;
execPython testIllegalElse false;;
execPython testEmptyControl false;;
execPython testIllegalControlBlock false;;
execPython testDivZero false;;
Printf.printf "\nTest: Bad Character\n%!";;
execPython testBadChar false;;
Printf.printf "\n%!";;

execPython primes true;;
