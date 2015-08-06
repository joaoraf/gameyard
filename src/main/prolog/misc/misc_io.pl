:- module(misc_io,[
	readable/1
   ]).

readable(FileSpec) :-
	absolute_file_name(FileSpec, _,
                           [file_errors(fail), access(read)]).   