:- module(config_options,[]).

:- use_module('../misc/misc_io').

:- multifile gameyard_option/4.

:- multifile gameyard_option/3.

:- dynamic gy_config/1.
:- dynamic gy_options/1.

init_all_options :- gy_options(_),!.
init_all_options :-
  \+ gy_options(_),!,
  aggregate(set([Module,Head,Description,Default]),
            gameyard_option(Module,Head,Description,Default),
            Opt4),
  aggregate(set([Module,Head,Description]),
            gameyard_option(Module,Head,Description),
            Opt3),
  append([Opt3,Opt4],Unsorted),
  sort(Unsorted,Opts),
  assertz(gy_options(Opts)),
  compile_predicates(gy_options/1).

all_options(Opts) :- 
	init_all_options,
	gy_options(Opts).

config(Opt) :-
	initialize,
	gy_config(Configs),
	member(Opt,Configs).

initialize :- gy_config(_), !.
initialize :-
	all_options(Opts),
	aggregate(set(T2),
		Head^Default^T1^( member([Module,Head,_,Default],Opts),
		  T1 =.. [Head,Default], T2 = [Module,T1] ), BaseOptions),
	(getenv('GAMEYARD_CONFIG',ConfigFile) -> read_file_options(ConfigFile,BaseOptions,Conf1) ;
	                                         Conf1 = BaseOptions),
	assertz(gy_config(Conf1)),
	compile_predicates(gy_config/1).	

proc_option(not(Opt),InOpts,OutOpts) :-
	delete(InOpts,Opt,OutOpts).
proc_option(Opt,InOpts,OutOpts) :-
	\+ (Opt = not(_)),
	union(InOpts,[Opt],OutOpts).		                                            
read_options(FileSpec,InOpts,OutOpts) :-
	readable(FileSpec),!,
	read_file_to_terms(FileSpec,Terms,[]),
	foldl(proc_option,Terms,InOpts,OutOpts).	
read_options(FileSpec,Opts,Opts) :-
	\+ readable(FileSpec),!,	
	absolute_file_name(FileSpec, FilePath,[]),
	format('Warning: cannot read file ~k.', [FilePath]).
	
		                           	