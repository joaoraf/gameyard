:- module(gameyard_paths,[]).

:- multifile gameyard_option/4.

:- multifile gameyard_option/3.

gameyard_path(P) :-
	prolog_load_context(directory, Dir),
	atomic_list_concat([Dir,'/..'],Dir1_),
	absolute_file_name(Dir1_,Dir1,[]),	
	atomic_list_concat([Dir1,'/gameyard,pl'],TestFile),	
	absolute_file_name(TestFile, _,
                           [file_errors(fail), access(read)]), !,
        P = Dir1.
	

set_gameyard_path :-
        absolute_file_name(gameyard('gameyard.pl'), _,
                           [file_errors(fail), access(read)]), !.
set_gameyard_path :-
	gameyard_path(
        prolog_load_context(directory, Dir),
        asserta(user:file_search_path(gameyard, Dir)).

:- set_gameyard_path.