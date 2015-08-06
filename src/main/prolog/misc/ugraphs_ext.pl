:- module(ugraphs_ext,[
	shortest_path/4
	]).

:- reexport([library(ugraphs)]).
	
shortest_path(_Graph,V,V,[V]) :- !.
shortest_path(Graph,V1,V2,VL) :-
	V1 \= V2,!,
	shortest_path(Graph,V2,[],[path(V1,[])],VL). 

shortest_path(_Graph,To,_Visited,[path(To,VL1)|Queue],Path) :- !,
	length(VL1,N),
	member(path(To,VL),[path(To,VL1)|Queue]),
	length(VL,N),
	reverse([To|VL],Path).
	
shortest_path(Graph,To,Visited,[path(V,VL)|Queue],Path) :-	
	%format('shortest_path: visited=~k current=~k VL=~k queue=~k\n\n',[Visited,V,VL,Queue]),	
	neighbours(V,Graph,NL1),
	sort(NL1,NL2),
	subtract(NL2,Visited,NL),
	((aggregate(set(path(V1,[V|VL])),member(V1,NL),NewPaths), !) ;
	 (NewPaths = []))  ,
	append(Queue,NewPaths,NewQueue),
	union(Visited,[V],NewVisited),
	shortest_path(Graph,To,NewVisited,NewQueue,Path).	