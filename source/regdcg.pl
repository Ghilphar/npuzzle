% delcares itself as a module and export predicates and various operations
% '*'//1, '*'//2 are used for repetition in Definite Clause Grammars
% 'seq'//3 Used for sequences operations in DCGs
% '*?'//1 and '*?//2' Used for optional repetition in DCGs
% 'char//2' used for character matchin in DCGs

% op(300, xf, *) Defines the operator  '*' with precedence 300 and type xf
% xf means your operant is before your operator.
% fx 

:- module(regdcg, [
	'*'//1, '*'//2,
	'seq'//3,
	'*?'//1, '*?'//2,
	 op(300, xf, *),
	 char//2
]).

char(C, Type) --> [C], { char_ex(C, Type) }.

:- set_prolog_flag(generate_debug_info, false).

char_ex(C, word_start) => char_type(C, alpha) ; C = '_'.
char_ex(C, word) => char_type(C, alnum) ; C = '_'.
char_ex('|', prolog_symbol) => true.
char_ex('%', prolog_symbol) => true.
char_ex(C, Type) => nonvar(Type), char_type(C, Type).

%%% Regex-like syntax

:- meta_predicate *(//, ?, ?). :- meta_predicate *(//, ?, ?, ?).
:- meta_predicate seq(//, //, ?, ?, ?).
:- meta_predicate *?(//, ?, ?). :- meta_predicate *?(//, ?, ?, ?).

arg_list(A-[H|T], A, H, (A-T)).

close_arg_list(R):- maplist( =(_-[]), R).
phrase_arg_list(Phrase, R, RTs):-
	Phrase = phrase(P, H, T),
	maplist( arg_list, R, As, Hs, RTs),
	copy_term((P,As),(Goal,Hs)),
	phrase(Goal, H, T).

*(Phrase) --> *(Phrase,[]).

*(Phrase,R,H,T):- phrase_arg_list(phrase(Phrase,H,T1),R,R1), !, phrase(*(Phrase,R1),T1,T).
*(_,R,H,H):- close_arg_list(R).

*?(Phrase) --> *?(Phrase,[]).

*?(_,R,H,H):- close_arg_list(R).
*?(Phrase,R,H,T):- phrase_arg_list(phrase(Phrase,H,T1),R,R1), phrase(*?(Phrase,R1),T1,T).

seq(Phrase, Del, A, H, T):-
	phrase_arg_list(phrase(Phrase, H, H1), A, A1),
	*?((Del, Phrase), A1, H1, T).
seq(_, _, A, H, H):- close_arg_list(A).
