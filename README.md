# Solve the _Towwer of Hanoi_ game for three rods

To represent a game state use the Hanoi (record) type:

```
initial = Rods [1,2,3] [] []
```
The above describes three discs in the leftmost rod (A) and the other rods (B and C) being empty.

To get the list of minimal moves from an *initial state* only, use:

```
getMoves initial
```
It returns a list of moves `AB`, `AC`, ... representing a move from rod A to B and A to C, respectively.

To apply the list retuned frrom `getMoves` to an intial game state, use `play`, it will output the list of the game state after each move.
That is, you can simply use `play initial`.

To solve and play from an initial game set with _n_ discs, use `playWith n`.
