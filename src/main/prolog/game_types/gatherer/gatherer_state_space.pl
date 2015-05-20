:- module(gatherer_state_space,[
	gatherer_game_state_space/1,
	gatherer_state_space_param_create/2,
	gatherer_state_space_initial_state/3,
	gatherer_state_space_players/3,
	gatherer_state_space_current_player/3,	
	gatherer_state_space_transitions/3,
	render_state_space/2
  ]).

:- use_module(library(assoc)).
:- use_module(library(random)).
:- use_module('../../game_state_space').
:- use_module('../../list_extras').
:- use_module('../../board2d').
:- use_module(swish_render_gatherer_state_space,[]).

gatherer_game_state_space(GS) :-
  default_game_state_space(GS1), 
  M=gatherer_state_space,
  GS = GS1.put(
    _{
      state_space_param_check: M:gatherer_state_space_param_check,
      state_space_param_create: M:gatherer_state_space_param_create,
      state_space_initial_state: M:gatherer_state_space_initial_state,
      state_space_players: M:gatherer_state_space_players,
      state_space_current_player: M:gatherer_state_space_current_player,      
      state_space_transitions: M:gatherer_state_space_transitions
    }),
    game_state_space(GS).


gatherer_state_space_param_check(_SS,Params) :-
  is_dict(Params,gatherer_state_space_param),
  number(Params.num_players),
  Params.num_players > 0,
  number(Params.width),
  Params.width >= 4,
  number(Params.height),
  Params.height >= 4,
  number(Params.wall_probability),
  Params.wall_ratio >= 0,
  Params.wall_ratio < 1,
  number(Params.food_count),
  Params.food_count > 0,
  TotalCells is Params.width * Params.height,
  Params.food_count < TotalCells.  
  
gatherer_state_space_param_create(_SS,Params) :-
	Params = gatherer_state_space_param{
	  width : 10,
	  height : 10,
	  wall_ratio : 0.4,
	  food_count : 10,
	  random_seed: rand(0,0,0),
	  num_players : 2
	}. 


select_random(N,L1,L3) :-
	random_permutation(L1,L),
	take(N,L,L2),
	sort(L2,L3).

fix_walls(AllPosL,NumWallCells,Board1,Board) :-
   select_random(NumWallCells,AllPosL,WallPosL),   
  % format('Wall cells: ~k\n',[WallPosL]),
   board2d_change_cells(Board1,WallPosL,w,Board).
   
validate_board(Params,Board,PlayerPosL,FoodPosL) :-
%   format('Validating:\n'),
   board2d_connected(Board,[f]),
%   format('--- connected!'),
   board2d_positions(Board,[f],FreePosL),
   RequiredLength is Params.food_count + Params.num_players,
   length(FreePosL,FreePosLength),   
%   format('--- required length: ~k, actual length: ~k.\n',[RequiredLength,FreePosLength]),
   FreePosLength >= RequiredLength,
%   format('       ok!\n'),
   random_permutation(FreePosL,RandFreePosL),
   split(Params.num_players,RandFreePosL,PlayerPosL1,Rest),
   take(Params.food_count,Rest,FoodPosL1),
   sort(PlayerPosL1,PlayerPosL),
   sort(FoodPosL1,FoodPosL).
%   format('--- all validated!\n')
   

generate_board(0,_Params,_Board,_PlayerPosL,_FoodPosL) :- !, fail.
generate_board(_N,Params,Board,PlayerPosL,FoodPosL) :-
%   format('generate_board: tries left: ~k\n',[N]),   
   board2d_new(Params.width,Params.height,f,wt(4,thorus),Board1),
%   format('Board created!\n'),
   board2d_all_positions(Board1,AllPosL),
%   format('Positions retrieved!\n'),
   NumCells is Params.width * Params.height,
   NumWallCells is floor(Params.wall_ratio * NumCells),
   fix_walls(AllPosL,NumWallCells,Board1,Board),
%   format('Walls fixed!\n'),
   validate_board(Params,Board,PlayerPosL,FoodPosL),!.
generate_board(N,Params,Board,PlayerPosL,FoodPosL) :-
    N1 is N - 1,
    generate_board(N1,Params,Board,PlayerPosL,FoodPosL).   

zip([],_YS,[]).
zip(_XS,[],[]).   
zip([X|XS],[Y|YS],[X-Y|ZS]) :- zip(XS,YS,ZS).

make_opaque_indexes(FreePosL,IdToPos,PosToId) :-   
   length(FreePosL,FreeLen),
   range(0,FreeLen,IdList),
   random_permutation(IdList,RandIdList),
   zip(RandIdList,FreePosL,IdToPosL),
   zip(FreePosL,RandIdList,PosToIdL),
   list_to_assoc(IdToPosL,IdToPos),
   list_to_assoc(PosToIdL,PosToId).
      
gatherer_state_space_initial_state(_SS, Params,InitialState) :-
   setrand(Params.random_seed),
   generate_board(1000,Params,Board,PlayerPosL,FoodPosL),   
   board2d_positions(Board,[f],FreePosL),   
   make_opaque_indexes(FreePosL,IdToPos,PosToId),   
   repl(Params.num_players,0,PlayersFood),
   InitialState = 
   	gatherer_state{
   	  params: Params,
   	  board : Board,
   	  players_pos : PlayerPosL,   	  
   	  opaque_id_to_pos: IdToPos,
   	  pos_to_opaque_id: PosToId,
   	  food_pos : FoodPosL,
   	  players_food: PlayersFood,
   	  round : 0
   	}.

render_players([],_N,Board,Board).
render_players([P|PS],N,Board1,Board2) :-
  format(atom(V),'~k',[N]),
  N1 is N + 1, 
  board2d_change_cell(Board1,P,V,Board),
  render_players(PS,N1,Board,Board2).
  
render_state_space(State,Rendered) :-
  Board = State.board,
  board2d_positions(Board,[f],FreePos),
  board2d_positions(Board,[w],WallPos),
  board2d_change_cells(Board,FreePos,' ',Board1),
  board2d_change_cells(Board1,WallPos,'#',Board2),
  board2d_change_cells(Board2,State.food_pos,'$',Board3),
  render_players(State.players_pos,1,Board3,Board4),
  board2d_rows(Board4,Rows),
  aggregate_all(bag(R),Row^(member(Row,Rows),atomic_list_concat(Row,R1),atomic_list_concat(['|',R1,'|'],R)),RenderedRowList),
  atomic_list_concat(RenderedRowList,'\n',Rendered1),
  W1 is State.params.width + 2,
  repl(W1,'_',TopBorder1),
  atomic_list_concat(TopBorder1,TopBorder),
  repl(W1,'-',BotBorder1),
  atomic_list_concat(BotBorder1,BotBorder),
  atomic_list_concat([TopBorder,Rendered1,BotBorder],'\n',Rendered).

portray_state(State) :- 
  is_dict(State, gatherer_state),
  render_state_space(State,Rendered),
  write(Rendered).

:- assertz(portray(State) :- portray_state(State)). 

gatherer_state_space_players(_SS,State,Players) :-
  Num is State.params.num_players + 1,
  range(1,Num,Players).

gatherer_state_space_current_player(_SS,State,Player) :-
  Player is (State.round mod State.params.num_players) + 1.

gatherer_state_space_is_finished(State) :-
  sum_list(State.players_food,State.params.food_count).

gatherer_state_space_transitions(_SS,State,MovesEndStates1) :-
  ((aggregate( set([Move,State1]),
             enact_move(State,Move,State1), 
             MovesEndStates1),! ) ; (MovesEndStates1 = [])).    

enact_move(State,_Move,_State1) :-
  gatherer_state_space_is_finished(State),!,fail.
enact_move(State,p,State1) :-
  NextRound is State.round + 1,
  State1 = State.put(_{round: NextRound}).  
enact_move(State,Dir,State1) :-
  gatherer_state_space_current_player(_,State,CurPlayer),
  nth1(CurPlayer,State.players_pos,PlayerPos),
  board2d_neighbours(State.board,PlayerPos,Neighbours),  
  member(neigh(P1,Dir),Neighbours),
  board2d_cell(State.board,P1,f),
  CurPlayer1 is CurPlayer - 1,
  NextRound is State.round + 1,
  set_nth(CurPlayer1,State.players_pos,P1,NextPlayersPos),
  check_food(CurPlayer1,P1,State.players_food,State.food_pos,NextPlayersFood,NextFoodPos),
  State1 = State.put(_{   	  
   	  players_pos : NextPlayersPos,   	     	  
   	  food_pos : NextFoodPos,
   	  players_food: NextPlayersFood,
   	  round : NextRound
   	}).
  
check_food(CurPlayer,P,PlayersFood,FoodPos,NextPlayersFood,NextFoodPos) :-
  member(P,FoodPos),!,
  subtract(FoodPos,[P],NextFoodPos),
  nth0(CurPlayer,PlayersFood,CurFood),
  NextFood is CurFood +1,
  set_nth(CurPlayer,PlayersFood,NextFood,NextPlayersFood).
check_food(_CurPlayer,_P,PlayersFood,FoodPos,PlayersFood,FoodPos).  
  
% 
% 
% gatherer_state_space_players(_SS,_State,Players) :-
% 	Num is State.params.num_players - 1,
% 	aggregate(set(X),between(0,Num,X,Players)).
% 
% gatherer_state_space_current_player(_SS,State,Player) :-
%    state_current_player(State,Player).
% 
% gatherer_state_space_transitions(_SS,State,[]) :-
% 	state_is_won(State),!.
% gatherer_state_space_transitions(_SS,State,MoveEndStatePairs) :-
%    aggregate(set([Move,EndState]),gatherer_available_move(State,Move,EndState),MoveEndStatePairs),!.
% gatherer_state_space_transitions(_SS,_State,[]).    
% 
% gatherer_available_move(State,Move,EndState) :-   
%    state_available_moves(State,Moves),
%    member(Move,Moves),
%    state_play(State,Move,EndState).
%    
% %state_size(state(Size,_),Size).
% 
% %state_cells(state(_,Cells),Cells).
% 
% #state_cell(State,X,Y,C) :- 
% #	Pos is Y*State.size+X,
% #	nth0(Pos,State.cells,C).
% #
% #is_circle(circle).
% #
% #is_cross(cross).
% #
% #is_empty(empty).
% #
% #typefor(circle,is_circle).
% #typefor(cross,is_cross).
% #typefor(empty,is_empty).
% #
% #value_to_char(circle,'O').
% #value_to_char(cross,'X').
% #value_to_char(empty,'.').
% #
% #player(circle).
% #player(cross).
% #
% #coord_to_seq(State, pos(X,Y), N) :- N is X + Y * State.size,!.
% #coord_to_seq(_, N, N).
% #
% #seq_to_coord(State, N, pos(X,Y)) :- X is N mod State.size, Y is N div State.size.
% #	 
% #state_cell_counts(State,Cross,Circle) :-
% #	count(is_cross,State.cells,Cross),
% #	count(is_circle,State.cells,Circle).
% #	
% #state_current_player(State,cross) :- state_cell_counts(State,Cross,Cross),!.
% #state_current_player(_,circle).
% #
% #state_cells_change(State, NewCells, State1) :-
% #	State1 = State.put(_{cells: NewCells}).
% #
% #state_cell_change(State,Pos,Value, NewState) :-
% # 	format("state_cell_change: Pos=~k, Value=~k, Cells=~k.\n",[Pos,Value,State.cells]),
% #	set_nth(Pos,State.cells,Value,NewCells),
% #	format("state_cell_change: NewCells=~k.\n",[NewCells]),
% #	state_cells_change(State,NewCells,NewState).
% 
% fresh_state(Params,State) :-
% 	setrand(Params.random_seed),
% 	random_state(Params,State),
% 	is_connectedness(State), !.
% random_state(Params,State) :-	
% 	TotalSize is Params.width * Params.height,
% 	NumWalls is integer(TotalSize * Params.wall_probability),
% 	repl(free,Params.width,Row),
% 	repl(Row,Params.height,Board),
% 	
% 	select_with_prob(Params.wall_probability,AllPos,WallPos),
% 	
% 
% 	repl(empty,S,Cells),
% 	State = gatherer_state{size: Size, cells : Cells}).
% 
% valid_position(W,H,pos(X,Y)) :-
% 	X #>= 0,
% 	X #<  W,
% 	Y #>= 0,
% 	Y #<  H.
% all_positions(W,H,Pos) :-
% 	aggregate(set(pos(X,Y)),(valid_pos
% 	X #>= 0,
% 	X #<  W,
% 	Y #>= 0,
% 	Y #<  H,
% 	
% all_positions(_,0,[]).
% all_positions(W,H,Pos) :- 
% 	range(0,W,R)
% 	all
% 
% select_with_prob(_,[],[]).
% select_with_prob(P,[X|XS],[X|YS]) :-
% 	maybe(P),!,
% 	select_with_prob(P,XS,YS).
% select_with_prob(P,[_|XS],YS) :-
% 	select_with_prob(P,XS,YS).
% 	
% map_positions(_, [], []) :- !.
% map_positions(Size, [P | PS], [pos(Y,X) | XS]) :-
% 	Y is P div Size,
% 	X is P mod Size,
% 	map_positions(Size, PS, XS).
% 	
% find_positions_2d(List, Pred, Size, Pos) :-
% 	find_positions(Pred,List,Pos1), 
% 	map_positions(Size,Pos1,Pos).
% 	
% 
% state_list_pos_x_y(State, Pos, pos(X, Y)) :-
% 	Y is Pos div State.size,
% 	X is Pos mod State.size.
% 
% state_find_positions(State,Value,PosL) :-
% 	typefor(Value,Type),
% 	find_positions(Type,State.cells,PosL).
% 
% state_find_positions(State, CrossPos, CirclePos, EmptyPos) :- 	
% 	find_positions(is_cross,State.cells,CrossPos),
% 	find_positions(is_circle,State.cells,CirclePos), 	
% 	find_positions(is_empty,State.cells,EmptyPos).
% 
% state_find_empty_positions(State, EmptyPos) :-	
% 	find_positions(is_empty,State.cells,EmptyPos).
% 
% column_sequences(Size,Seqs) :- column_sequences_0(Size,0,Seqs).
% 
% column_sequences_0(Size,Size,[]) :- !.
% column_sequences_0(Size,N,[X|XS]) :-
% 	N1 is N + 1,	
% 	sequence(N,Size,Size,X),
% 	column_sequences_0(Size,N1,XS).
% 
% line_sequences(Size,Seqs) :- line_sequences_0(Size,0,Seqs).
% 
% line_sequences_0(Size,Size,[]) :- !.
% line_sequences_0(Size,N,[X|XS]) :- 
% 	N1 is N + 1,
% 	Start is N * Size,	
% 	sequence(Start,1,Size,X),
% 	line_sequences_0(Size,N1,XS).
% 
% diag_sequences(Size,[Forward,Backward]) :-
% 	SizeP1 is Size + 1,
% 	SizeM1 is Size - 1,
% 	sequence(0,SizeP1,Size,Forward),
% 	sequence(SizeM1,SizeM1,Size,Backward).
% 
% complete_sequences(Size,Seqs) :-
% 	column_sequences(Size,CS),
% 	line_sequences(Size,LS),
% 	diag_sequences(Size,DS),
% 	append(CS,LS,XS),
% 	append(XS,DS,Seqs).
% 
% state_is_won(State,Player,CompleteSequence) :-
% 	player(Player),
% 	state_find_positions(State,Player,L),
% 	complete_sequences(State.size,Seqs),
% 	member(CompleteSequence,Seqs),	
%  	subset(CompleteSequence,L).
% 
% state_is_won(State) :- state_is_won(State,_,_).
% 
% state_is_open(State) :- \+ state_is_won(State).
% 
% state_available_moves(State,Moves) :- state_find_positions(State,empty,Moves).
% 
% state_can_win_in_one_step(State,Step) :-
% 	state_is_open(State),
% 	state_available_moves(State,Moves),
% 	state_current_player(State,Player),
% 	state_find_positions(State,Player,Positions),
% 	complete_sequences(State.size,Seqs),
% 	member(CompSeq,Seqs),
% 	intersection(Positions,CompSeq,Seq),
% 	Size1 is State.size - 1,
% 	length(Seq,Size1),
% 	member(Step,CompSeq),
% 	\+ member(Step,Seq),
% 	member(Step,Moves).
% 
% test_state(S) :- fresh_state(3,NS), state_play_seq(NS,[pos(0,0),pos(1,0),pos(0,1),pos(1,1)],S).
% 
% test_state_1(S) :- fresh_state(3,NS), state_play_seq(NS,[pos(0,0),pos(1,0),pos(0,1)],S).
% 
% 	
% state_play(State,PosOrCoord,NewState) :-
%         coord_to_seq(State,PosOrCoord,Pos),
% 	state_current_player(State,Player),
% 	state_cell_change(State,Pos,Player,NewState).
% 	
% state_play_seq(State,[],State) :- !.
% state_play_seq(State,[Pos|XS],NewState) :- 
% 	state_play(State,Pos,NS),
% 	state_play_seq(NS,XS,NewState).	
% 
% state_can_lose_in_two_steps(State,Move,OtherMove) :-
% 	state_is_open(State),
% 	state_available_moves(State,Moves),
% 	member(Move,Moves),
% 	state_play(State,Move,State1),
% 	state_can_win_in_one_step(State1,OtherMove).
%  
% print_state(State) :-
%  	print_state_0(State.size,State.cells,0).
%  
% print_state_0(_,[],_) :- !.
% print_state_0(Size,[X|XS],N) :- 
%  	value_to_char(X,C),
%  	write(C),
%  	check_eol(Size,N),
%  	N1 is N + 1,
%  	print_state_0(Size,XS,N1).
%  
% check_eol(Size,N) :- 0 is ((N+1) mod Size), !, write("\n").
% check_eol(_,_).   
% 
% 	
% %%%%%%%%%%%%%%%%%%%%
% 
% test_play(Size,MaxRounds,Seed) :-
% 	!,
% 	set_random(seed(Seed)),	
% 	%open("/tmp/gatherer-test.log",write,LogStream), 
% 	%   with_output_to(LogStream,test_play_1(Size,MaxRounds,Seed)),
% 	test_play_1(Size,MaxRounds,Seed),	
% 	format("Test ended\n").
% 
% test_play_1(Size,MaxRounds,Seed) :-	 
% 	format("Test start: MaxRounds: ~k, Seed: ~k.\n",[MaxRounds,Seed]),
% 	SS=[],%domino_game_state_space(SS), 
% 	Params = gatherer_state_space_param{size:Size},	
% 	gatherer_state_space_initial_state(SS,Params,State0),
% 	(test_play_2(State0,0,MaxRounds),! ; format("Test failed!\n")).	
% 
% test_play_2(_State,Round,Round) :-
% 	format("maximum number of rounds reached!\n"),!.	
% test_play_2(State,Round,MaxRounds) :-
% 	format("Round: ~a\n",[Round]),
% 	format("\n"),
% 	print_state(State),
% 	format("\n"),
% 	gatherer_state_space_current_player([],State,CurrentPlayer),
% 	format("   Current player: ~k.\n",[CurrentPlayer]),	
% 	format("\n"),	
% 	(gatherer_state_space_transitions([],State,MoveStatePairs),! ;
% 	  (format("Cannot compute transitions from: ~k.\n",[State]),fail)),
% 	test_play_3(Round,MoveStatePairs,MaxRounds).
% 
% test_play_3(_Round,[],_MaxRound) :-
% 	format("Game ended!\n").	
% test_play_3(Round,MoveStatePairs,MaxRounds) :-	
% 	list_map(head,MoveStatePairs,Moves),
% 	format("   Available moves:\n"),
% 	format("      ~k\n",[Moves]),
% 	length(Moves,LM),
% 	M is random(LM),
% 	nth0(M,Moves,Move),
% 	format("   Selected move: ~k\n",[Move]),
% 	nth0(M,MoveStatePairs,[_,NextState]),
% 	format("----------------------------------------\n\n"),
% 	Round1 is Round + 1,
% 	test_play_2(NextState,Round1,MaxRounds).
% 		  
% 

