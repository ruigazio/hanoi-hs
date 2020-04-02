# Solve the hanoi towers game for three rods

To represent a game state use the Hanoi (record) type:

```
initial = Rods [1,2,3] [] []
```
The above describes three discs in the leftmost rod (A) and the other rods (B and C) being empty.

To get the list of moves use:

```
getMoves initial
```
It returns a list of moves `AB`, `AC`, ... representing a move from rod A to B and A to C, respectively.

To apply a list of moves to a game state, use `play`, it'll output the list of the game state after each move.

To solve and play from an initial game set with _n_ discs, use `playWith n`.
