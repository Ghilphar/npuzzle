% module declaration.
% The file declares itself as module export various algorithms and update predicate
% greedy uniform astar/6 astar/7

:- module( search_strategies, [
	uniform/5, greedy/6, astar/6, astar/7,
	update/5
]).

:- use_module(heuristics/manhattan).
:- use_module(heuristics/wmanhattan).
:- use_module(heuristics/displace).
:- use_module(heuristics/linear_conflict).
:- use_module(heuristics/euclidean).

:- det(uniform/5).
:- det(greedy/6).
:- det(astar/6).
:- det(astar/7).

:- det(update/5).
:- det(update_tiebreakers/5).

uniform(_, G, _, _, V):- V is G.
greedy(Heuristic, Goal, _, State, _, H):-
	call(Heuristic, Goal, State, H).
astar(Heuristic, Goal, G, State, _, (F,H)):-
	call(Heuristic, Goal, State, H), F is H + G.

astar(Heuristic, TieBreaker, Goal, G, State, (F1,_), (F2,BreakerValues)):-
	call(Heuristic, Goal, State, H), F is H + G,
	( F @>= F1 -> F2 = F ; F2 = F ),
	tiebreaker(TieBreaker, Goal, G, State, BreakerValues).
tiebreaker([], _, _, _, []).
tiebreaker([f:T|Ts], Goal, G, State, [F|Vs]):- !, call(T, Goal, State, H), F is H + G,
	tiebreaker(Ts, Goal, G, State, Vs).
tiebreaker([h:T|Ts], Goal, G, State, [H|Vs]):- !, call(T, Goal, State, H),
	tiebreaker(Ts, Goal, G, State, Vs).
tiebreaker([g|Ts], Goal, G, State, [V|Vs]):- !, V is G,
	tiebreaker(Ts, Goal, G, State, Vs).

update(uniform, _, _, G, G).
update(greedy(_), _, F, _, F).
update(astar(_), G1, (F1,H), G, (F,H)):- F is F1 - G1 + G.
update(astar_tie(_,TBs), G1, (F1,TBVals1), G, (F,TBVals)):- F is F1 - G1 + G, update_tiebreakers(TBs, G1, TBVals1, G, TBVals).


% define.
% list vide, variable anonyme, liste vide, variable anonyme, liste vide

update_tiebreakers([], _, [], _, []).

%
update_tiebreakers([f:_|TBs], G1, [TBVal1|TBVals1], G, [TBVal|TBVals]):- TBVal is TBVal1 - G1 + G,
	update_tiebreakers(TBs, G1, TBVals1, G, TBVals).
%
update_tiebreakers([h:_|TBs], G1, [TBVal1|TBVals1], G, [TBVal1|TBVals]):-
	update_tiebreakers(TBs, G1, TBVals1, G, TBVals).

% define
update_tiebreakers([g|TBs], G1, [_|TBVals1], G, [G|TBVals]):-
	update_tiebreakers(TBs, G1, TBVals1, G, TBVals).
