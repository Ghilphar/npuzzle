#!/usr/bin/env swipl

% La lib main est appelle implicitement

% use_module Load the files (Need to be modules files)
:- use_module(source/common).
:- use_module(source/parser).
:- use_module(source/goal).
:- use_module(source/checker).
:- use_module(source/solver).
:- use_module(source/file_utils).

% Library for debugging

:- use_module(library(main)).
:- use_module(library(prolog_stack)).

% Cette ligne specifie que l'on doit executer main quand on lance le programme
% initialization(Goal, When)
% this implies we call main when we Goal is main_
:- initialization(main_, main).

% Wrapper permettant de voir les erreurs, notament les endroits ou ca plante.
main_:- catch_with_backtrace(main, Err, error_management(Err) ).

error_management(invalid_file):- writeln('Invalid file').
error_management(Err):- print_message(error, Err).


% declare the number of predicates as deterministic. 
% Le predicat main est deterministic cela signifie qu'il va produire uniquement un seul resultat.
% det(Predicate/Arity) Tells prolog that the given Arity is deterministic.
% A deterministic predicate need to return a solution. 

% det - a deterministic predicate always suceeds exactly once and does not leave a choicepoint
% semidet - A semi-deterministic predicate predicate suceeds at most once. If it succeeds it does not leave a choicpoint
% nondet - A non deterministic predicate is the most general case no claims are made on the number of solutions and
% 			whether or not the predicates leaves an choicepoint on the last solution.
% multi - As nondet but succed at least once.
:- det(main/1).


% Main Predicate with File and Heuristic Argument
% Here main predicate take a list of two elements File and HeuristicArg.
main([File,HeuristicArg]):-
	% We convert the string HeuristicArg to a term called Heuristic
	check_file_exists(File), !,
	term_string(Heuristic, HeuristicArg),

	% TODO We should check if the Heuristic is a valid one. If not we should exit writing the heuristic availables.
	% Also we should check if File exist else we should exit and say the file doesn't exist
	
	% We read the file File into a list of Ascii Codes
	read_file_to_codes(File, Codes, []),
	writeln(Codes),

	% Here we parse theses Codes to get the puzzle Size and the initial State Puzzle
	% phrase come with the standard lib for DCGs We parse the codes we find on the read_file_to_codes
	% We define the parsing rules with npuzzle() predicate 
	( phrase(npuzzle(Size, Puzzle), Codes) -> true ; throw(invalid_file) ),


	% execute goal from goal.pl - Define the state of resolve
	% define if the goal is the pussle is solvable else print
	% The goal predicates return us the goal state based on the puzzle size 

	% So based on the goal size of the npuzzle we can determinate ou goal state.

	% Variable sans valeur pointeur nul et une variable avec valeur c'est un pointeur sur valeur. (L'unification take the same address)
	% writeln(Goal),
	% _80420 = 1,
	% writeln(_80420),
	goal(Size, Goal),
	% writeln(Goal),

	% Goal sa signifie l'appel de fonction 

	% unification, on essaye de faire que les deux cotes du egal ait les memes valeurs
	% Goal = [Line, Line2, Line3 |RestLine],
	% writeln((Line, Line2, Line3, RestLine)),
	% RestLine can be used to test if we are at the end
	% if isn't solvable then (->) write isn't solvable else (;) solve the puzzle, write solution and stats 
	(
		% The negation operator on prolog (\+) We determine if the puzzle is solvable in that case we print we can solve
		\+ solvable(Puzzle, Goal)
		% -> Implies we commit to this part of the code and don't test the other condition.
	->	writeln("Puzzle is not solvable (according to parity check)")
	;
		% print_args(Heuristic, Puzzle, Goal, Solution, Stats),
		% On the other side of the if else condition if the puzzle is solvable we 
		% solve it and write the solution
		solve(Heuristic, Puzzle, Goal, Solution, Stats),
		write_solution( Solution, Stats )
		% print_args(Heuristic, Puzzle, Goal, Solution, Stats)
	).


% This version is used when only HeuristicArg is provided
main([HeuristicArg]):-
	term_string(Heuristic, HeuristicArg), !,
	% set the puzzle size to 3
	Size = 3,
	%random puzzle 
	random_puzzle(Size, Puzzle),
	writeln("Randomly generate_puzzle : "),
	write_grid(Puzzle),
	goal(Size, Goal),
	(
		\+ solvable(Puzzle, Goal)
	->	writeln("Puzzle is not solvable (according to parity check)")
	;
		solve(Heuristic, Puzzle, Goal, Solution, Stats),
		write_solution( Solution, Stats )
	).

main(_):- writeln("Usage : swipl main.pl [file] heuristic.").

random_puzzle(Size, Puzzle):- N is Size * Size - 1, grid_size(Puzzle, Size),
	numlist(0, N, Es), random_permutation(Es, P), append(Puzzle, P).

write_solution( Solution, (MemComplexity, TimeComplexity) ):-
	length(Solution, L),
	write_steps(Solution),
	format("Numbers of steps to reach solution : ~d~n", [L]),
	format("Number of states represented in memory : ~d~n", [MemComplexity]),
	format("Numbers of states analyzed : ~d~n", [TimeComplexity]).

write_steps([]).
write_steps([F:G:S|T]):-
	write_steps(T),
	format("g: ~d, f: ~w~n", [G,F]),
	write_grid(S),
	nl.

write_grid([]).
write_grid([R|Rows]):-
	write_row(R),
	write_grid(Rows).

write_row(R):-
	maplist( [E]>>format("~|~t~d~3+ ", [E]), R ), nl.


print_args(Heuristic, Puzzle, Goal, Solution, Stats) :-
    write("Heuristic: "), write(Heuristic), nl,
    write("Puzzle: "), write(Puzzle), nl,
    write("Goal: "), write(Goal), nl,
    write("Solution: "), write(Solution), nl,
    write("Stats: "), write(Stats), nl.