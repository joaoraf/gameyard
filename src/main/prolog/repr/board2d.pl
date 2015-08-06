:- module(board2d,[
        board2d_new/5, % (+Width,+Height,+InitialValue,+Wt,-Board)
        board2d_walk_type/1, % (+WalkType) WalkType=wt(MoveFreedom,Topology) 
        board2d_valid_position_cons/2,
        board2d_valid_position/2,
        board2d_all_positions/2,
        board2d_direction/2,
        board2d_directions/2,
        board2d_dir_to_delta/3,
        board2d_walk_cons/5,
        board2d_walk/5,
        board2d_neighbour/4,
        board2d_neighbours/3,
        board2d_cell/3,
        board2d_to_graph/3,
        board2d_connected/2,
        board2d_positions/3,
        board2d_change_cell/4,
        board2d_change_cells/4,
        board2d_move_dir/4,
        board2d_min_path/5,
        board2d_distance/5,
        board2d_diameter/3,
        board2d_rows/2	
  ]).
:- use_module('../gameyard_config').
:- use_module(library(clpfd)).
:- use_module(gameyard(misc/ugraphs_ext), [shortest_path/4]).
:- use_module(gameyard(misc/list_extras)).

  
board2d_new(Width,Height,InitialValue,Wt,Board) :-
   Width >= 1,
   Height >= 1,
   board2d_walk_type(Wt),
   repl(Width,InitialValue,Row),
   repl(Height,Row,BaseBoard),
   Board = board2d{
	width: Width,
	height: Height,	
	baseBoard: BaseBoard,
	walkType: Wt
	}.

board2d_walk_type(wt(MoveFreedom,Topology)) :-
	member(MoveFreedom,[4,6]),
	member(Topology,[thorus,rectangle]).	


board2d_valid_position_cons(Board,pos(X,Y)) :-
	X #>= 0,
	X #<  Board.width,
	Y #>= 0,
	Y #<  Board.height.

board2d_valid_position(Board,pos(X,Y)) :-
	board2d_valid_position_cons(Board,pos(X,Y)),
	label([X,Y]).	

board2d_all_positions(Board,Pos) :-
	aggregate(set(pos(X,Y)),board2d_valid_position(Board,pos(X,Y)),Pos). 	


board2d_direction(Board,Dir) :-
	Board.walkType=wt(4,_), 
	member(Dir,[n,s,e,w]).
board2d_direction(Board,Dir) :-
	Board.walkType=wt(8,_), 
	member(Dir,[n,ne,e,se,s,sw,w,ne]).
	
board2d_directions(Board,Dirs) :-
	aggregate(set(Dir),board2d_direction(Board,Dir),Dirs).	

board2d_dir_to_delta(n , 0,-1).
board2d_dir_to_delta(s , 0, 1).
board2d_dir_to_delta(e ,-1, 0).
board2d_dir_to_delta(w , 1, 0).
board2d_dir_to_delta(ne,-1,-1).
board2d_dir_to_delta(se,-1, 1).
board2d_dir_to_delta(nw, 1,-1).
board2d_dir_to_delta(sw, 1, 1).
 
board2d_walk_cons(Board,Dx,Dy,pos(X,Y),pos(X1,Y1)) :-
	Board.walkType = wt(_,thorus),
	board2d_valid_position_cons(Board,pos(X,Y)),
	board2d_valid_position_cons(Board,pos(X1,Y1)),
	(X + Dx + Board.width) mod Board.width #= X1,
	(Y + Dy + Board.height) mod Board.height #= Y1.
board2d_walk_cons(Board,Dx,Dy,pos(X,Y),pos(X1,Y1)) :-
	Board.walkType = wt(_,rectangle),
	board2d_valid_position_cons(Board,pos(X,Y)),
	board2d_valid_position_cons(Board,pos(X1,Y1)),	
	(X + Dx) #= X1,
	(Y + Dy)  #= Y1.
	
board2d_walk(Board,Dx,Dy,pos(X,Y),pos(X1,Y1)) :-	
	board2d_walk_cons(Board,Dx,Dy,pos(X,Y),pos(X1,Y1)),
	label([X,Y,X1,Y1]).


board2d_neighbour(Board,P,P1,Dir) :-
	board2d_direction(Board,Dir),
	board2d_dir_to_delta(Dir,Dx,Dy),
	board2d_valid_position_cons(Board,P),
	board2d_valid_position_cons(Board,P1),
	board2d_walk(Board,Dx,Dy,P,P1).
	
board2d_neighbours(Board,P,PosDir) :-
	aggregate(
		set(neigh(P1,Dir)), 
		board2d_neighbour(Board,P,P1,Dir),
		PosDir).

board2d_cell(Board,pos(X,Y),V) :-
	board2d_valid_position(Board,pos(X,Y)),
	nth0(Y,Board.baseBoard,Row),
	nth0(X,Row,V).


board2d_to_graph_valid_pos(Board,ValidValues,P) :-
	board2d_cell(Board,P,V),
	member(V,ValidValues).
	
board2d_to_graph_valid_move(Board,ValidPos,P,P1) :-
	member(P,ValidPos),
	board2d_neighbours(Board,P,PosDir),
	member(neigh(P1,_),PosDir).
		
board2d_to_graph(Board,Graph,ValidValues) :-
	aggregate(
		set(P),
		board2d_to_graph_valid_pos(Board,ValidValues,P),
		ValidPos),	
	aggregate(
		set(P-P1),
		board2d_to_graph_valid_move(Board,ValidPos,P,P1),
		ValidMoves),
	vertices_edges_to_ugraph(ValidPos,ValidMoves,Graph).

board2d_connected(Board,ValidMoves) :-  
	board2d_to_graph(Board,Graph,ValidMoves),
	vertices(Graph,Vertices),
	Vertices=[V|_],
	reachable(V,Graph,Vertices).

board2d_position_has_value(Board,Values,P) :-
	board2d_cell(Board,P,Value),
	member(Value,Values).
	
board2d_positions(Board,Values,Positions) :-
	aggregate(set(P),board2d_position_has_value(Board,Values,P),Positions).		

board2d_change_cell(Board,Pos,NewValue,NewBoard) :-
	board2d_valid_position(Board,Pos),
	pos(X,Y) = Pos,	
	nth0(Y,Board.baseBoard,Row),
	set_nth(X,Row,NewValue,NewRow),
	set_nth(Y,Board.baseBoard,NewRow,NewBaseBoard),
	put_dict(baseBoard,Board,NewBaseBoard,NewBoard).

board2d_change_cells(Board,[],_NewValue,Board).
board2d_change_cells(Board,[P|PS],NewValue,NewBoard) :-
	board2d_change_cell(Board,P,NewValue,NewBoard1),
	board2d_change_cells(NewBoard1,PS,NewValue,NewBoard).

board2d_move_dir(Board,P1,P2,M) :-
	board2d_valid_position(Board,P1),	
	board2d_dir_to_delta(M,Dx,Dy),
	board2d_direction(Board,M),
	board2d_walk(Board,Dx,Dy,P1,P2).

board2d_pos_list_to_moves(_Board,[],[]).
board2d_pos_list_to_moves(_Board,[_],[]).
board2d_pos_list_to_moves(Board,[P1,P2|PS],[M|ML]) :-
	board2d_move_dir(Board,P1,P2,M),	
	board2d_pos_list_to_moves(Board,[P2|PS],ML).

board2d_min_path(Board,ValidValues,P1,P2,Path) :-
	board2d_to_graph(Board,Graph,ValidValues),
	once(shortest_path(Graph,P1,P2,VertPath)),
	board2d_pos_list_to_moves(Board,VertPath,Path).

board2d_distance(Board,_ValidValues,P,P,0) :- 
	board2d_valid_position(Board,P).
board2d_distance(Board,ValidValues,P1,P2,Len) :-
	board2d_valid_position(Board,P1),
	board2d_valid_position(Board,P2),
	P1 \= P2,
	board2d_position_has_value(Board,ValidValues,P1),	
	board2d_position_has_value(Board,ValidValues,P2),
	once(board2d_min_path(Board,ValidValues,P1,P2,Path)),
	length(Path,Len).	

board2d_distance_1(Board,_ValidValues,P,P,0) :-
	board2d_valid_position(Board,P).
board2d_distance_1(Board,ValidValues,P1,P2,D) :-	
	board2d_valid_position(Board,P1),
	board2d_valid_position(Board,P2),
	P1 @< P2,
	Inf is Board.width * Board.height + 1,
	board2d_distance(Board,ValidValues,P1,P2,D1),
	((Inf = D1, D = inf) ; (Inf \= D1, D = D1)).
	 
board2d_diameter(Board,ValidValues,Diameter) :-
	aggregate(max(Len),P1^P2^board2d_distance_1(Board,ValidValues,P1,P2,Len),Diameter1),
	D is Board.width * Board.height + 1,
	(Diameter1 = [D,P1,P2] -> Diameter = [inf,P1,P2] ; Diameter = Diameter1) .

board2d_rows(Board,Rows) :-
	Rows = Board.baseBoard.	 			