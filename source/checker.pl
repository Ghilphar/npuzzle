% declare itself as a module and export the predicate solvable

:- module( checker, [
	solvable/2
]).

% solvable check if the given puzzle is solvable considering the goal state
solvable(Grid, Goal):-
	( nth0(RowZero, Grid, Row), member(0, Row) ) -> true,
	( nth0(GRowZero, Goal, GRow), member(0, GRow) ) -> true,
	append(Grid, List0), select(0, List0, List),
	append(Goal, GList0), select(0, GList0, GList),
	length(Grid, N),
	aggregate_all(count, (
		append([_, [E], Tail], List), append([_, [E], GTail], GList),
		member(Inversion, Tail), \+ member(Inversion, GTail)
	), I),
	( N mod 2 =:= 1 , I mod 2 =:= 0
	; N mod 2 =:= 0 , I mod 2 =:= (RowZero - GRowZero) mod 2
	), !.
