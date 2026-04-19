`ocaml python.ml <args>`<br>
`args` can be any number of file names followed by an optional "debug" string that prints the generated token tree.<br>
ex:<br>
`ocaml python.ml primes.py debug tictactoe.py`<br>
will run `primes.py` with debug printing, then runs `tictactoe.py` normally.<br>
<br>
No functions, lists, strings or any special things are supported.<br>
Just math, conditionals, loops, and variables.<br>
<br>
Extensions:<br>
- operators `print`, `println`, and `input`.<br>
    - `print` and `println` take 1 argument to print out. Printing is the only use of strings.<br>
    - `input` takes 1 argument which has to be a variable, and sets its value to user input.
