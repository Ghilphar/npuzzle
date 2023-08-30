
:- module( euclidean, [ euclidean/3 ] ).

:- use_module('../common.pl').

euclidean(Goal, State, H):-
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
					E is sqrt((Y - Y1) ** 2 + (X - X1) ** 2)
				)
			),
		Row )
		),
	Distance ),
	append( Distance, Dist1),
	sum_list( Dist1, H ).
