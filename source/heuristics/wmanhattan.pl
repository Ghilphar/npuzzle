
:- module( wmanhattan, [ wmanhattan/3 ] ).

:- use_module('../common.pl').

% Non-admissible
wmanhattan(Goal, State, H):-
	length( Goal, L ), grid_size( Distance, L ),
	( nth0(X0, Goal, Row0), nth0(Y0, Row0, 0) -> true ),
	findall( Row,
		( between(1, L, Y), findall( E,
			( between(1, L, X),
				( nth1( Y, Goal, Row1 ), nth1( X, Row1, N ) ),
				(
					N == 0 -> E = 0
				;
					( nth1( Y1, State, Row2 ), nth1( X1, Row2, N ) -> true ),
					E is ( abs(Y - Y1) + abs(X - X1) ) * ( abs(Y - Y0) + abs(X - X0) )
				)
			),
		Row )
		),
	Distance ),
	append( Distance, Dist1),
	sum_list( Dist1, H ).
