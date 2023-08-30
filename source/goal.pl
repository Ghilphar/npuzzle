%declare itself asa module and export the predicate goal

:- module( goal, [
	goal/2
]).

% define the goal state of the puzzle based on the size and grid (TODO)
goal(Size, Grid):-
	% writeln(Grid),
	% length create the grid with variables who are not assigned. (TODO, length and maplist are equavalents to grid_size fo common)
	length(Grid, Size), maplist( [E]>>length(E, Size), Grid ),
	% writeln(Grid),
	spiral(Grid, Size, 1, 0, 0, left), !.

% When N is equal to Size * Size we put 0 in the center of the grid.
% is/2 is 
spiral(Grid, Size, N, X, Y, _):- N is Size * Size, !,
	put(Grid, X, Y, 0).
spiral(Grid, Size, N, X, Y, Dir):-
	put(Grid, X, Y, N),
	( D = Dir ; true ),
	dir(D, X, Y, X1, Y1),
	N1 is N + 1,
	spiral(Grid, Size, N1, X1, Y1, D).

dir(right, X, Y, X1, Y):- X1 is X + 1.
dir(down, X, Y, X, Y1):- Y1 is Y + 1.
dir(left, X, Y, X1, Y):- X1 is X - 1.
dir(up, X, Y, X, Y1):- Y1 is Y - 1.

% Put put the element N in the Grid.
% Put Grid, 0, 0, 1 => we put the element 1 in the (0, 0). This same predicate could get the value of N.
put(Grid, X, Y, N):- nth0(Y, Grid, Row), nth0(X, Row, P), var(P), P = N.
