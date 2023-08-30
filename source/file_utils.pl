% file_utils.pl
:- module(file_utils, [ check_file_exists/1 ]).

check_file_exists(File) :-
    (
        exists_file(File) -> true 
        ;
        format("The file ~w does not exist. Exiting.~n", [File]),
        false
    ).