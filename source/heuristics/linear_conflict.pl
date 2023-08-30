
:- module( linear_conflict, [ linear_conflict/3 ] ).

:- use_module('../common.pl').

% Used for transpose/2
:- use_module(library(clpfd)).

linear_conflict(Goal, State, H):-
	length(Goal, L),
	L1 is L - 1,
	findall( R,
		(
			nth0(Y, State, Row), nth0(Y, Goal, GoalRow),
			nth0(X, Row, E), E \= 0, nth0(XG, GoalRow, E),
			X1 is X+1, between(X1, L1, X2),
			nth0(X2, Row, E1), E1 \= 0, nth0(X2G, GoalRow, E1), XG > X2G
		-> R = 2
		; R = 0
		),
	RowConflicts ),
	transpose(Goal, CGoal),
	transpose(State, CState),
	findall( C,
		(
			nth0(X, CState, Column), nth0(X, CGoal, GoalColumn),
			nth0(Y, Column, E), E \= 0, nth0(YG, GoalColumn, E),
			Y1 is Y+1, between(Y1, L1, Y2),
			nth0(Y2, Column, E1), E1 \= 0, nth0(Y2G, GoalColumn, E1), YG > Y2G
		-> C = 2
		; C = 0
		),
	ColumnConflicts ),
	append( [RowConflicts, ColumnConflicts], Conflicts),
	sum_list( Conflicts, H ).
