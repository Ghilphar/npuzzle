% declare itself as a module and export the predicate solve
:- module( solver, [
	solve/5
]).

:- use_module(common).
:- use_module(search_strategies).

% det declare that some predicates are deterministic
:- det(search/6).
:- det(integrate_states/5).
:- det(closed_state/3).
:- det(new_elem/6).

:- det(build_solution/3).

% Call the heuristic function to solve the n-puzzle
solve(Heuristic, Initial, Goal, Solution, Stats):-
	% The term call is here used:
	% In this context Heuristic is assumed to be a predicate or a functor that represents a heuristic function.
	% 
	call( Heuristic, Goal, 1, Initial, _, F ), % dinamiclt call the heuristic predicate
	search(Heuristic, [F:1:[]:Initial], [], Goal, Solution, Stats).

% Select Search ? F:G:Pre:Selected findall()
search(Heuristic, Open, Closed, Goal, Solution, Stats):-
	%trace,
	select_search(Open, F:G:Pre:Selected), findall(State, expand(Selected, State), ExStates),
	(
		member(S, ExStates),
		solution(Goal, (S, F:G:Selected, Pre), Open, Closed, Solution, Stats), !
	;
		maplist( [State,F1:State]>>(
			call(Heuristic, Goal, G + 1, State, F, F1)
		), ExStates, NewStates),
		integrate_states(Heuristic, F:G:Pre:Selected, NewStates, (Open, Closed), (NewOpen, NewClosed)),
		search(Heuristic, NewOpen, NewClosed, Goal, Solution, Stats)
	).

solution(Goal, (State, F:G:Selected, Pre), Open, Closed, ['/':G1:State,F:G:Selected|Solution], Stats):-
	Goal == State,
	Stats = (MemComplexity, TimeComplexity),
	length(Open, OLen),
	length(Closed, CLen),
	TimeComplexity is CLen,
	MemComplexity is OLen + CLen,
	G1 is G + 1,
	build_solution( Pre, Solution, Closed ).

build_solution( [], [], _ ):- !.
build_solution( S, [F:G:S|T], Closed ):-
	member( F:G:S1:S, Closed) -> build_solution(S1, T, Closed).

select_search(L, Selected):- min_member(Selected, L).

integrate_states(Heuristic, F:G:Pre:Selected, NewStates, (Open, Closed), (NewOpen, NewClosed)):-
	G1 is G + 1,
	close_state(F:G:Pre:Selected, (Open, Closed), (Open1, Closed1)),
	foldl( new_elem(Heuristic, G1, Selected), NewStates, (Open1, Closed1), (NewOpen, NewClosed) ).

close_state(F:State, (Open, Closed), (NewOpen, [F:State|Closed])):-
	( select( F:State, Open, NewOpen ) -> true ).

new_elem(_, G, Pre, F:State, (Open, Closed), ([F:G:Pre:State|Open], Closed)):-
	\+ member( _:_:_:State, Open ), \+ member( _:_:_:State, Closed ), !.
new_elem(Heuristic, G, Pre, F:State, (Open, Closed), (NewOpen, NewClosed)):-
	member( HR:GR:PR:State, Open ), GR > G, !,
	replace_open( Heuristic, HR:GR:PR:State, F:G:Pre:State, (Open, Closed), (NewOpen, NewClosed) ).
new_elem(Heuristic, G, Pre, F:State, (Open, Closed), (NewOpen, NewClosed)):-
	member( HR:GR:PR:State, Closed ), GR > G, !,
	replace_closed( Heuristic, HR:GR:PR:State, F:G:Pre:State, (Open, Closed), (NewOpen, NewClosed) ).
new_elem(_, _, _, _, OC, OC).

replace_open( Heuristic, State, F:G:NewState, (Open, Closed), ([F:G:NewState|NewOpen], NewClosed) ):-
	( select( State, Open, Open1 ) -> true ),
	update_dependency( Heuristic, G, State, (Open1, Closed), (NewOpen, NewClosed) ).

% This should never be called
replace_closed( Heuristic, State, F:G:NewState, (Open, Closed), (NewOpen, [F:G:NewState|NewClosed]) ):-
	( select( State, Closed, Closed1 ) -> true ),
	update_dependency( Heuristic, G, State, (Open, Closed1), (NewOpen, NewClosed) ).

update_dependency( Heuristic, G, State, (Open, Closed), (NewOpen, NewClosed) ):-
	findall( S-NS, ( S = F1:G1:State:State1, NS = F:G:State:State1, member(S, Open), update(Heuristic, G1, F1, G, F) ), UpdateOpen ),
	pairs_keys_values( UpdateOpen, OldOpenStates, NewOpenStates ),
	foldl( replace_open, OldOpenStates, NewOpenStates, (Open, Closed), (Open1, Closed1) ),
	findall( S-NS, ( S = F:_:State:State1, NS = F:G:State:State1, member(S, Closed), update(Heuristic, G1, F1, G, F) ), UpdateClosed ),
	pairs_keys_values( UpdateClosed, OldClosedStates, NewClosedStates ),
	foldl( replace_closed, OldClosedStates, NewClosedStates, (Open1, Closed1), (NewOpen, NewClosed) ).


expand(State, NewState):-
	length(State, L), grid_size( NewState, L ),
	( nth0(X, State, Row), nth0(Y, Row, 0) -> true ),
	dir((X, Y), (X1, Y1)),
	( nth0(X1, State, Row1), nth0(Y1, Row1, N) -> true ),
	( nth0(X1, NewState, Row2), nth0(Y1, Row2, 0) -> true ),
	( nth0(X, NewState, Row3), nth0(Y, Row3, N) -> true ),
	( maplist( maplist([N1,N2]>>( ignore( N1 = N2 ) )), State, NewState ) -> true ).
