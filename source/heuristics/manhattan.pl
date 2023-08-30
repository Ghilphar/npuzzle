
:- module( manhattan, [ manhattan_ext/3, manhattan_displace/3, manhattan_linear/3, manhattan/3 ] ).

:- use_module('../common.pl').
:- use_module(linear_conflict).
:- use_module(displace).

manhattan_ext(Goal, State, H):-
	manhattan(Goal, State, M), displace(Goal, State, D), linear_conflict(Goal, State, L),
	H is M + max(D, L).

manhattan_displace(Goal, State, H):-
	manhattan(Goal, State, M), displace(Goal, State, D),
	H is M + D.

manhattan_linear(Goal, State, H):-
	manhattan(Goal, State, M), linear_conflict(Goal, State, L),
	H is M + L.

manhattan(Goal, State, H):-
	length( Goal, L ), grid_size( Distance, L ),
	L1 is L - 1,
	findall( Row,
		( between(0, L1, Y), findall( E,
			( between(0, L1, X),
				( nth0( Y, Goal, Row1 ), nth0( X, Row1, N ) ),
				(
					N == 0 -> E = 0
				;
					( nth0( Y1, State, Row2 ), nth0( X1, Row2, N ) -> true ),
					E is abs(Y - Y1) + abs(X - X1)
				)
			),
		Row )
		),
	Distance ),
	append( Distance, Dist1),
	sum_list( Dist1, H ).
