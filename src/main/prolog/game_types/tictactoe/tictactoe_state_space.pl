:- module(tictactoe_state_space,[
	tictactoe_game_state_space/1
  ]).

:- use_module('../../gameyard_config').
:- use_module(gameyard(game_state_space)).
:- use_module(gameyard(misc/list_extras)).
:- use_module(swish_render_tictactoe).

tictactoe_game_state_space(GS) :-
  default_game_state_space(GS1), 
  M=tictactoe_state_space,
  GS = GS1.put(
    _{
      state_space_param_check: M:tictactoe_state_space_param_check,
      state_space_param_create: M:tictactoe_state_space_param_create,
      state_space_initial_state: M:tictactoe_state_space_initial_state,
      state_space_players: M:tictactoe_state_space_players,
      state_space_current_player: M:tictactoe_state_space_current_player,      
      state_space_transitions: M:tictactoe_state_space_transitions
    }),
    game_state_space(GS).


tictactoe_state_space_param_check(_SS,Params) :-
  is_dict(Params,tictactoe_state_space_param),
  number(Params.size), 
  Params.size >= 3.
  
tictactoe_state_space_param_create(_SS,Params) :-
	Params = tictactoe_state_space_param{size:3}. 

tictactoe_state_space_initial_state(_SS, Params,InitialState) :-
   fresh_state(Params.size,InitialState).

tictactoe_state_space_players(_SS,_State,[circle,cross]).

tictactoe_state_space_current_player(_SS,State,Player) :-
   state_current_player(State,Player).

tictactoe_state_space_transitions(_SS,State,[]) :-
	state_is_won(State),!.
tictactoe_state_space_transitions(_SS,State,MoveEndStatePairs) :-
   aggregate(set([Move,EndState]),tictactoe_available_move(State,Move,EndState),MoveEndStatePairs),!.
tictactoe_state_space_transitions(_SS,_State,[]).    

tictactoe_available_move(State,Move,EndState) :-   
   state_available_moves(State,Moves),
   member(Move,Moves),
   state_play(State,Move,EndState).
   
%state_size(state(Size,_),Size).

%state_cells(state(_,Cells),Cells).

state_cell(State,X,Y,C) :- 
	Pos is Y*State.size+X,
	nth0(Pos,State.cells,C).

is_circle(circle).

is_cross(cross).

is_empty(empty).

typefor(circle,is_circle).
typefor(cross,is_cross).
typefor(empty,is_empty).

value_to_char(circle,'O').
value_to_char(cross,'X').
value_to_char(empty,'.').

player(circle).
player(cross).

coord_to_seq(State, pos(X,Y), N) :- N is X + Y * State.size,!.
coord_to_seq(_, N, N).

seq_to_coord(State, N, pos(X,Y)) :- X is N mod State.size, Y is N div State.size.
	 
state_cell_counts(State,Cross,Circle) :-
	count(is_cross,State.cells,Cross),
	count(is_circle,State.cells,Circle).
	
state_current_player(State,cross) :- state_cell_counts(State,Cross,Cross),!.
state_current_player(_,circle).

state_cells_change(State, NewCells, State1) :-
	State1 = State.put(_{cells: NewCells}).

state_cell_change(State,Pos,Value, NewState) :-
 	format("state_cell_change: Pos=~k, Value=~k, Cells=~k.\n",[Pos,Value,State.cells]),
	set_nth(Pos,State.cells,Value,NewCells),
	format("state_cell_change: NewCells=~k.\n",[NewCells]),
	state_cells_change(State,NewCells,NewState).

fresh_state(Size,tictactoe_state{size: Size, cells : Cells}) :- S is Size*Size, repl(S,empty,Cells).
	
map_positions(_, [], []) :- !.
map_positions(Size, [P | PS], [pos(Y,X) | XS]) :-
	Y is P div Size,
	X is P mod Size,
	map_positions(Size, PS, XS).
	
find_positions_2d(List, Pred, Size, Pos) :-
	find_positions(Pred,List,Pos1), 
	map_positions(Size,Pos1,Pos).
	

state_list_pos_x_y(State, Pos, pos(X, Y)) :-
	Y is Pos div State.size,
	X is Pos mod State.size.

state_find_positions(State,Value,PosL) :-
	typefor(Value,Type),
	find_positions(Type,State.cells,PosL).

state_find_positions(State, CrossPos, CirclePos, EmptyPos) :- 	
	find_positions(is_cross,State.cells,CrossPos),
	find_positions(is_circle,State.cells,CirclePos), 	
	find_positions(is_empty,State.cells,EmptyPos).

state_find_empty_positions(State, EmptyPos) :-	
	find_positions(is_empty,State.cells,EmptyPos).

column_sequences(Size,Seqs) :- column_sequences_0(Size,0,Seqs).

column_sequences_0(Size,Size,[]) :- !.
column_sequences_0(Size,N,[X|XS]) :-
	N1 is N + 1,	
	sequence(N,Size,Size,X),
	column_sequences_0(Size,N1,XS).

line_sequences(Size,Seqs) :- line_sequences_0(Size,0,Seqs).

line_sequences_0(Size,Size,[]) :- !.
line_sequences_0(Size,N,[X|XS]) :- 
	N1 is N + 1,
	Start is N * Size,	
	sequence(Start,1,Size,X),
	line_sequences_0(Size,N1,XS).

diag_sequences(Size,[Forward,Backward]) :-
	SizeP1 is Size + 1,
	SizeM1 is Size - 1,
	sequence(0,SizeP1,Size,Forward),
	sequence(SizeM1,SizeM1,Size,Backward).

complete_sequences(Size,Seqs) :-
	column_sequences(Size,CS),
	line_sequences(Size,LS),
	diag_sequences(Size,DS),
	append(CS,LS,XS),
	append(XS,DS,Seqs).

state_is_won(State,Player,CompleteSequence) :-
	player(Player),
	state_find_positions(State,Player,L),
	complete_sequences(State.size,Seqs),
	member(CompleteSequence,Seqs),	
 	subset(CompleteSequence,L).

state_is_won(State) :- state_is_won(State,_,_).

state_is_open(State) :- \+ state_is_won(State).

state_available_moves(State,Moves) :- state_find_positions(State,empty,Moves).

state_can_win_in_one_step(State,Step) :-
	state_is_open(State),
	state_available_moves(State,Moves),
	state_current_player(State,Player),
	state_find_positions(State,Player,Positions),
	complete_sequences(State.size,Seqs),
	member(CompSeq,Seqs),
	intersection(Positions,CompSeq,Seq),
	Size1 is State.size - 1,
	length(Seq,Size1),
	member(Step,CompSeq),
	\+ member(Step,Seq),
	member(Step,Moves).

test_state(S) :- fresh_state(3,NS), state_play_seq(NS,[pos(0,0),pos(1,0),pos(0,1),pos(1,1)],S).

test_state_1(S) :- fresh_state(3,NS), state_play_seq(NS,[pos(0,0),pos(1,0),pos(0,1)],S).

	
state_play(State,PosOrCoord,NewState) :-
        coord_to_seq(State,PosOrCoord,Pos),
	state_current_player(State,Player),
	state_cell_change(State,Pos,Player,NewState).
	
state_play_seq(State,[],State) :- !.
state_play_seq(State,[Pos|XS],NewState) :- 
	state_play(State,Pos,NS),
	state_play_seq(NS,XS,NewState).	

state_can_lose_in_two_steps(State,Move,OtherMove) :-
	state_is_open(State),
	state_available_moves(State,Moves),
	member(Move,Moves),
	state_play(State,Move,State1),
	state_can_win_in_one_step(State1,OtherMove).
 
print_state(State) :-
 	print_state_0(State.size,State.cells,0).
 
print_state_0(_,[],_) :- !.
print_state_0(Size,[X|XS],N) :- 
 	value_to_char(X,C),
 	write(C),
 	check_eol(Size,N),
 	N1 is N + 1,
 	print_state_0(Size,XS,N1).
 
check_eol(Size,N) :- 0 is ((N+1) mod Size), !, write("\n").
check_eol(_,_).   

	
%%%%%%%%%%%%%%%%%%%%

test_play(Size,MaxRounds,Seed) :-
	!,
	set_random(seed(Seed)),	
	%open("/tmp/tictactoe-test.log",write,LogStream), 
	%   with_output_to(LogStream,test_play_1(Size,MaxRounds,Seed)),
	test_play_1(Size,MaxRounds,Seed),	
	format("Test ended\n").

test_play_1(Size,MaxRounds,Seed) :-	 
	format("Test start: MaxRounds: ~k, Seed: ~k.\n",[MaxRounds,Seed]),
	SS=[],%domino_game_state_space(SS), 
	Params = tictactoe_state_space_param{size:Size},	
	tictactoe_state_space_initial_state(SS,Params,State0),
	(test_play_2(State0,0,MaxRounds),! ; format("Test failed!\n")).	

test_play_2(_State,Round,Round) :-
	format("maximum number of rounds reached!\n"),!.	
test_play_2(State,Round,MaxRounds) :-
	format("Round: ~a\n",[Round]),
	format("\n"),
	print_state(State),
	format("\n"),
	tictactoe_state_space_current_player([],State,CurrentPlayer),
	format("   Current player: ~k.\n",[CurrentPlayer]),	
	format("\n"),	
	(tictactoe_state_space_transitions([],State,MoveStatePairs),! ;
	  (format("Cannot compute transitions from: ~k.\n",[State]),fail)),
	test_play_3(Round,MoveStatePairs,MaxRounds).

test_play_3(_Round,[],_MaxRound) :-
	format("Game ended!\n").	
test_play_3(Round,MoveStatePairs,MaxRounds) :-	
	list_map(head,MoveStatePairs,Moves),
	format("   Available moves:\n"),
	format("      ~k\n",[Moves]),
	length(Moves,LM),
	M is random(LM),
	nth0(M,Moves,Move),
	format("   Selected move: ~k\n",[Move]),
	nth0(M,MoveStatePairs,[_,NextState]),
	format("----------------------------------------\n\n"),
	Round1 is Round + 1,
	test_play_2(NextState,Round1,MaxRounds).
		  