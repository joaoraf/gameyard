:- module(state_monad,[
		get//2,
		get//3,
		put//2,
		modify//2,
		modify//3,
		pstate//0,
		exec_state/3
	]).

get(Key,Value,In,Out) :-
	check_assoc(In,Out),
	get_assoc(Key,Out,Value).

get(Key,Default,Value,In,Out) :-
	check_assoc(In,In1),
	\+ get(Key,Value,In1,_),!,
	Value = Default,
	Out = In1.
get(Key,_Default,Value,In,Out) :-
	get(Key,Value,In,Out).		

put(Key,Value,In,Out) :-
	check_assoc(In,Out1),
	put_assoc(Key,Out1,Value,Out).

modify(Key,Monad) -->
	get(Key,InValue),	 	
	call(Monad,InValue,OutValue),
	put(Key,OutValue).

modify(Key,Default,Monad) -->
	get(Key, Default, InValue),	 	
	call(Monad,InValue,OutValue),
	put(Key,OutValue).

pstate -->	
	check_assoc,	
	{ write('State:\n') },
	pstate_entries.

pstate_entries -->
	get(K,V),	
	{ format('\t~k: ~k\n',[K,V]) }.

exec_state(Monad,InState,OutState) :-
	check_assoc(InState,InState1),
	call(Monad,InState1,OutState).

exec_state(Monad,InState,OutState) :-
	check_assoc(InState,InState1),
	call(Monad,InState1,OutState).

check_assoc(In,In) :- is_assoc(In),!.
check_assoc(_In,Out) :- empty_assoc(Out).
	