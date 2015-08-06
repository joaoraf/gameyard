:- module(list_extras,[
	repl/3,	
	split/4,
	take/3,
	drop/3,
	list_map/3,
	set_nth/4,
	count/3,
	find_positions/3,
	list_map_filter/3,
	range/3,
	for_range/4,
	sequence/4,
	head/2,
	fst/2,
	snd/2	
 ]).

filled(_,[]).
filled(V,[V|XS]) :- filled(V,XS).

repl(N,V,L) :- length(L,N), filled(V,L).
    
split_(N,L,AC,AC,L) :- N =< 0, !.
split_(_N,[],AC,AC,[]) :- !.
split_(N,[X | XS],AC,Left, Right) :- 
	N1 is N - 1, 
	split_(N1,XS,[X|AC],Left,Right).

split_reverse_left(N,List,Left,Right) :- split_(N,List,[],Left,Right).
split(N,List,Left,Right) :- 
  split_reverse_left(N,List,RevLeft,Right),
  reverse(RevLeft,Left).

take(N, L, R) :- split(N,L,R,_).   
drop(N, L, R) :- split(N,L,_,R).

list_map_(_F,[], Accum, ListOut) :- !, reverse(Accum,ListOut).
list_map_(F,[X|XS], Accum, ListOut) :-
  call(F,X,Y),
  list_map_(F,XS,[Y|Accum],ListOut). 

:- meta_predicate list_map(2,?,?).

list_map(F, ListIn, ListOut) :- list_map_(F,ListIn,[],ListOut).

is_empty([]) :- !.
is_empty(_L) :- fail.

is_defined([]) :- !, fail.
is_defined(_L).

set_nth(Pos,_List,_Value,_NewList) :- Pos < 0, !, fail.
set_nth(Pos,List,_Value,_NewList) :- length(List,L), L < Pos, !, fail.
set_nth(Pos,List,Value,NewList) :- 
  split(Pos,List,Left,Right),
  (Right = [_|R] -> append(Left,[Value|R],NewList) ; append(Left,[Value],NewList)).     

head([X|_],X).

fst([X|_],X).

snd([_|X],X).

:- meta_predicate count(1,?,?).

count(P,L,N) :- aggregate(count,(member(X,L),call(P,X)),N),!.
count(_P,_L,0).

:- meta_predicate find_positions(1,?,?).

find_positions(Pred,List,PosList) :- aggregate(set(Pos),X^(nth0(Pos,List,X),call(Pred,X)),PosList).

:- meta_predicate list_map_filter(2,?,?).
list_map_filter(F,List,OutList) :-
	aggreate_all(list(X),Y^(member(Y,List),call(F,Y,X)),OutList).

range(From,Until,Range) :-
	Until1 is Until - 1,
	aggregate_all(bag(X),between(From,Until1,X),Range).

:- meta_predicate for_range(?,?,2,?).

for_range(From,Until,F,YS) :-
	range(From,Until,Range),
	list_map_filter(F,Range,YS).	 
	

sequence(_,_,Count,[]) :- Count =< 0, !.
sequence(Start,Step,Count,[Start|XS]) :-
	Next is Start + Step,
	Count1 is Count - 1,
	sequence(Next,Step,Count1,XS).

	 	