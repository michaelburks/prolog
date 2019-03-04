Sudoku puzzle solver written entirely in Prolog.
Takes just over 1 second to solve the "world's hardest puzzle", and 35-40 seconds to solve a maximally sparse sudoku puzzle with 17 provided entries.

To run, enter the following in a SWI-Prolog terminal:

```
?- consult(sudoku).
?- solve_puzzle(worlds_hardest_sudoku).
```
