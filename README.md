`ocaml python.ml <args>`

`args` can be any number of file names followed by an optional "debug" string that prints the generated token tree.

ex:
`ocaml python.ml primes.py debug tictactoe.py`

will run `primes.py` with debug printing, then runs `tictactoe.py` normally.
