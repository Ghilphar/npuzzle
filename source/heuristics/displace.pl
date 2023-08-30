
:- module( displace, [ displace/3 ] ).

:- use_module('../common.pl').

displace(Goal, State, H):-
	length( Goal, L ), grid_size( Distance, L ),
	L1 is L - 1,
	findall( Row,
		( between(0, L1, Y), findall( D,
			( between(0, L1, X),
				displace1(Goal, State, X, Y, D)
			),
		Row )
		),
	Distance ),
	append( Distance, Dist1),
	sum_list( Dist1, H ).

displace1(Goal, State, X, Y, D):-
	( nth0( Y, Goal, Row1 ), nth0( X, Row1, N ) ),
	(
		( nth0( Y, State, Row2 ), nth0( X, Row2, N ) ) -> D = 0
	;
		N = 0 -> D = 0
	;
		findall( E, ( dir( (X, Y), (X1, Y1) ),
				( nth0( Y1, State, Row3 ), nth0( X1, Row3, E ) ),
				( nth0( Y1, Goal, Row4 ), nth0( X1, Row4, E1 ) ),
				( E == 0 -> true ; E1 \= E )
			), Free ),
		( Free = [_,_|_] ; Free = [N], nth0( Y, State, Row2 ), nth0( X, Row2, 0 ) ) -> D = 0
	;
		D = 1
	).
