:- module(gameyard_swish,[]).

inside_swish :- readable(swish('swish.pl')).

:- use_module(config_options).

:- if(inside_swish).

:- else.

set_swish_path :- 
	absolute_file_name(swish('swish.pl'), _,
                           [file_errors(fail), access(read)]), !.
set_swish_path :-
        prolog_load_context(directory, Dir),
        atomic_list_concat([Dir,'/swish'],Dir1),
        asserta(user:file_search_path(swish, Dir1)).

:- set_swish_path.

:- endif.