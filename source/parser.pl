% declare itself as module and export the predicate npuzzle

:- module(parser, [
	npuzzle//2
]).

% import module definite clause grammar (dcg)
:- use_module(library(dcg/basics)).
:- use_module(regdcg).

:- set_prolog_flag(double_quote, codes).

% define the rule npuzzle 
% Try to match the codes into a Puzzle (Grid) and L size od the Grid
% line_end* match a series of line ending 
% White spaces are handled by whites
% The size L representing the size of the puzzle handled by integer(L)
% The Grid is handled by grid(Grid)

% npuzzle parse the puzzle of size L and it's Grid
% grid/1 and line/1 are helper DCG rules that parse the grid and lines 
% line_end and comment deal with line ending and comments 

% name(arguments) --> Rule definition.
% Is the way we Definite Clause Grammar (used for list processing)
% Traditional rules in prolog are defined with :- 
% predicate_name(Arguments) :- rule_body.

% Traditional prolog Rule:
% parent(john, jim).
% grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
% DCG Rule: 
% sentence --> noun_phrase, verb_phrase.
% noun_phrase --> article, noun. 
npuzzle(L, Grid) -->
	line_end*,
	whites, integer(L), line_end,
	% This is a prolog code block within a DCG. It ensures that the Lenght of Grid is L and each row in Grid also has lenght L
	{ length(Grid, L), maplist( [E]>>length(E, L), Grid ) },
	% Match the Grid of the puzzle
	grid(Grid),
	line_end* .

% this rule define that an empty grid is an empty list []
grid([]) --> [].
% This rule define that a grid is a Line followed by more grid
grid([L|Ls]) --> line(L), grid(Ls).

% This rule define that a line can be just a line ending
line([]) --> line_end.
% This rule define that a line starts with whitespaces followed by an integer E, and then the rest of the line
line([E|Es]) --> whites, integer(E), line(Es).

% This rule defines what a line_end looks like. It consist of whitespaces, followed by either
% white spaces, comments or nothing and then a new line char
line_end --> whites, ( comment ; [] ), "\n".
% This rule specified what a comment look like: Start with # and continue until a new_line is encountered
comment --> "#", (\+ "\n", [_])* .
