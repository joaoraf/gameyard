:- module(domino_state_space,[
	domino_game_state_space/1
  ]).

:- use_module(library(aggregate)).
:- use_module(library(clpfd)).

:- use_module('../../gameyard_config').
:- use_module(gameyard(game_state_space)).
:- use_module(gameyard(misc/list_extras)).

domino_game_state_space(GS) :-
  default_game_state_space(GS1),
  context_module(M),
  GS = GS1.put(
    _{
      state_space_param_check: M:domino_state_space_param_check,
      state_space_param_create: M:domino_state_space_param_create,
      state_space_initial_state: M:domino_state_space_initial_state,
      state_space_players: M:domino_state_space_players,
      state_space_current_player: M:domino_state_space_current_player,
      state_space_player_view: M:domino_state_space_player_view,
      state_space_view_transitions: M:domino_state_view_transitions,
      state_space_transitions: M:domino_state_space_transitions
    }),
    game_state_space(GS).

domino_state_space_param_check(_SS,Param) :-
  is_dict(Param,domino_state_space_param),
  number(Param.max_tile_num), 
  Param.max_tile_num >= 1,
  number(Param.num_players),
  Param.num_players >= 2,
  number(Param.initial_draw),
  Param.initial_draw >= 0,
  atom(Param.random_seed).
  
domino_state_space_param_create(_SS,
	domino_state_space_param{
		max_tile_num:6,
		num_players: 2,
		initial_draw: 7,
		random_seed: seed(random)}). 

all_tiles(MaxTileNum,AllTiles) :-
  Num1 #=< Num2, 
  [Num1, Num2] ins 0..MaxTileNum, 
  aggregate(set([Num1,Num2]),label([Num1,Num2]),AllTiles).
     

tiles_in(Tiles,Collection) :- tuples_in(Tiles,Collection).
tiles_not_in(Tiles,Collection) :- #\ tuples_in(Tiles,Collection).
tile_in(Tile,Collection) :- tuples_in([Tile],Collection).
tile_not_in(Tile,Collection) :- #\ tuples_in([Tile],Collection).  

player_draws(_DrawSize,0,TL,[],TL) :- !, format("[player_draws] TL=~k\n",[TL]).
player_draws(DrawSize,N,TL,[Hand|PL],RestTL) :-
  N1 is N - 1,
  split(DrawSize,TL,P,TL1),
  sort(P,Hand),
  player_draws(DrawSize,N1,TL1,PL,RestTL).
  
  
domino_tile_list(Params,AllTiles,PermutedTiles) :-
   MaxTileNum = Params.max_tile_num,
   all_tiles(MaxTileNum,AllTiles),
   random_permutation(AllTiles,PermutedTiles).
   
domino_state_space_initial_state(_SS, Params,InitialState) :-   
   set_random(Params.random_seed),
   domino_tile_list(Params,AllTiles, PermTileList),
   player_draws(Params.initial_draw,
                Params.num_players,
                PermTileList,
                PlayersHands,
                DrawPile),
   random(0,Params.num_players,FirstPlayer),
   %format("[domino_state_space_initial_state] InitialState=~k\n",[InitialState])
   InitialState = domino_state{
     params: Params,
     all_tiles: AllTiles,
     current_player: FirstPlayer,          
     hands: PlayersHands,
     draw_pile: DrawPile,
     pass_count: 0,
     exposed : [],
     board: []
   }.

domino_state_space_players(_SS,State,Players) :-
	Num is State.params.num_players - 1,
	aggregate(set(X),between(0,Num,X,Players)).

domino_state_space_current_player(_SS, State,Player) :-
	State.current_player = Player.

domino_state_space_player_view(_SS, State,PlayerNum,PlayerView) :-
	domino_player_view(State,PlayerNum,PlayerView).

domino_player_view(State,PlayerNum,PlayerView) :-
	nth0(PlayerNum,State.hands,PlayerHand),
	append(State.exposed,PlayerHand,ExposedForPlayer1),
	sort(ExposedForPlayer1,ExposedForPlayer),
	length(State.draw_pile,DrawPileSize),
	list_map(length,State.hands,HandSizes),	
	PlayerView = domino_player_view{
	  params : State.params,
	  view_player: PlayerNum,
	  current_player: State.current_player,
	  hand: PlayerHand,
	  board: State.board,
	  exposed: ExposedForPlayer,
	  pass_count: State.pass_count,
	  draw_pile_size: DrawPileSize,
	  hand_sizes: HandSizes,
	  all_tiles: State.all_tiles 	 
	}.	

% View Transition

domain_state_space_view_transitions(_SS,StartView,MoveEndViewPairs) :-
	view_simulate_moves(StartView,MoveEndViewPairs).

view_simulate_moves(StartView,[]) :-
	StartView.pass_count >=4,!.
view_simulate_moves(StartView,[]) :-
	member(0,StartView.hand_sizes),!.		
view_simulate_moves(StartView,MoveEndViewPairs) :-
	aggregate(set([Move1,EndView1]),view_simulate_play_move(StartView,Move1,EndView1),PreMoves) , !,
	view_simulate_moves_1(StartView,PreMoves,MoveEndViewPairs).
view_simulate_moves(StartView,MoveEndViewPairs) :-	
	view_simulate_moves_1(StartView,[],MoveEndViewPairs).

view_simulate_draw(StartView,Unexposed,EndView) :-	
	member(DrawnTile,Unexposed),
	(StartView.current_player = StartView.view_player ->
		sort([DrawnTile|StartView.hand],NewHand),
		sort([DrawnTile|StartView.exposed],NewExposed)
	      ; NewHand = StartView.hand, NewExposed = StartView.exposed ),		
	NewDrawPileSize is StartView.draw_pile_size - 1,
	nth0(StartView.current_player,StartView.hand_sizes,HandSize),
	NewHandSize is HandSize + 1,
	set_nth(StartView.current_player,StartView.hand_sizes,NewHandSize,NewHandSizes),	
	EndView = StartView.put(
	     _{ pass_count: 0,
	        hand : NewHand,
	        hand_sizes: NewHandSizes,
	        draw_pile_size : NewDrawPileSize,
	        exposed: NewExposed }).	

view_simulate_draws(StartView,[]) :-
	StartView.draw_pile_size = 0,!.
view_simulate_draws(StartView,MoveEndViewPairs) :-			 
	subtract(StartView.all_tiles,StartView.exposed,Unexposed),
	aggregate(set([draw,EndView]), view_simulate_draw(StartView,Unexposed,EndView), MoveEndViewPairs).
	
		
view_simulate_moves_1(StartView,[],[[pass,EndView]]) :-		
	view_simulate_pass(StartView,EndView),!.
view_simulate_moves_1(StartView,PreMoves,MoveEndViewPairs) :-	
	PreMoves = [_|_],!,
	((StartView.current_player \= StartView.view_player,
	  view_simulate_pass(StartView,EndView),
	  sort([[pass,EndView]|PreMoves],MoveEndViewPairs1)), ! ; 
	  MoveEndViewPairs1 = PreMoves),
	view_simulate_draws(StartView,MoveEndViewPairs2),
	append(MoveEndViewPairs1,MoveEndViewPairs2,MoveEndViewPairs3),
	sort(MoveEndViewPairs3,MoveEndViewPairs).
view_simulate_moves_1(StartView,[],MoveEndViewPairs) :-
	view_simulate_draws(StartView,MoveEndViewPairs).
		
view_simulate_play_move(StartView,play(Pos,Tile_),EndView) :-
	StartView.current_player = StartView.view_player,
	view_simulate_play_move_1(StartView.hand,StartView,Pos,Tile_,EndView1),
	delete(StartView.hand,Tile_,NewHand),
	nth0(StartView.current_player,StartView.hand_sizes,HandSize),
	HandSize1 is HandSize - 1,
	set_nth(StartView.current_player,StartView.hand_sizes,HandSize1,NewHandSizes),
	EndView = EndView1.put( _{ hand : NewHand, hand_sizes: NewHandSizes }).
view_simulate_play_move(StartView,play(Pos,Tile_),EndView) :-
	StartView.current_player \= StartView.view_player,
	subtract(StartView.all_tiles,StartView.exposed,Unexposed),
	view_simulate_play_move_1(Unexposed,StartView,Pos,Tile_,EndView1),
	nth0(StartView.current_player,StartView.hand_sizes,HandSize),
	HandSize1 is HandSize - 1,
	set_nth(StartView.current_player,StartView.hand_sizes,HandSize1,NewHandSizes),	
	EndView = EndView1.put(_{ hand_sizes: NewHandSizes }).

view_simulate_play_move_1(PossibleTiles,StartView,Pos,Tile_,EndView) :-
	member(Tile_,PossibleTiles),
	Tile_ = [TNum1,TNum2],
        sort([[TNum1,TNum2],[TNum2,TNum1]],Tiles),
        member(Tile,Tiles),
        Tile = [Num1,Num2],            	
	view_simulate_match_tile(StartView.board,Num1,Num2,Pos,NewBoard),
	sort([Tile_ | StartView.exposed],NewExposed),
	next_player(StartView,NextPlayer),		
	EndView = StartView.put(
	     _{ pass_count: 0,	        
	        current_player: NextPlayer,
	        board: NewBoard,
	        exposed: NewExposed }).		  

view_simulate_match_tile([],Num1,Num2,center,[]) :- Num1 > Num2, !, fail. 
view_simulate_match_tile([],Num1,Num2,center,[[Num1],[Num2]]). 	
view_simulate_match_tile(Board,Num1,Num2,Pos,NewBoard) :-
	Trace = [Num1 | _],
	nth0(Pos,Board,Trace),
	NewTrace = [Num2 | Trace],
	set_nth(Pos,Board,NewTrace,NewBoard).
	

view_simulate_pass(StartView,EndView) :-
	StartView.draw_pile_size = 0,
	StartView.pass_count < 4, 
	next_player(StartView,NextPlayer),
	PassCount1 is StartView.pass_count + 1,
	EndView = StartView.put(
	  _{ pass_count: PassCount1,
	     current_player: NextPlayer }).


% End View Transition

% State transitions  	

domain_state_space_transitions(_SS,CurrentState,MoveEndStatePairs) :-
	state_transitions(CurrentState,MoveEndStatePairs).

state_transitions(CurrentState,[]) :-
	CurrentState.pass_count >= 4,!.
state_transitions(CurrentState,[]) :-
	member([],CurrentState.hands),!.	
state_transitions(CurrentState,MoveStatePairs) :-
	(aggregate(set([play(Pos,Tile),NextState]),state_transition_play(CurrentState,Pos,Tile,NextState),PlayTransitions),! ; PlayTransitions=[]),
        state_transitions_1(CurrentState,PlayTransitions,MoveStatePairs).

state_transitions_1(CurrentState,[],[[draw,NextState]]) :-
	CurrentState.draw_pile = [DrawnTile|NewDrawPile],!,
	nth0(CurrentState.current_player,CurrentState.hands,Hand),
	sort([DrawnTile|Hand],NewHand),
	set_nth(CurrentState.current_player,CurrentState.hands,NewHand,NewHands),	
	NextState = CurrentState.put(_{
	        pass_count: 0,
		draw_pile: NewDrawPile,
		hands: NewHands
		}).
state_transitions_1(CurrentState,[],[[pass,NextState]]) :-
	CurrentState.draw_pile = [],!,
	next_player(CurrentState,NextPlayer),	
	PassCount1 is CurrentState.pass_count + 1,			
	NextState = CurrentState.put(_{
	        pass_count: PassCount1,
		current_player: NextPlayer		
		}).				
state_transitions_1(_CurrentState,PlayMoveStatePairs,PlayMoveStatePairs).
		
state_transition_play(CurrentState,Pos,Tile_,EndState) :-
	nth0(CurrentState.current_player,CurrentState.hands,Hand),
	member(Tile_,Hand),
	delete(Hand,Tile_,NewHand),
	set_nth(CurrentState.current_player,CurrentState.hands,NewHand,NewHands),
	Tile_ = [TNum1,TNum2],
        sort([[TNum1,TNum2],[TNum2,TNum1]],Tiles),
        member(Tile,Tiles),
        Tile = [Num1,Num2],            	
	view_simulate_match_tile(CurrentState.board,Num1,Num2,Pos,NewBoard),	
	next_player(CurrentState,NextPlayer),
	sort([Tile_|CurrentState.exposed],NewExposed),	
	EndState = CurrentState.put(
	     _{ pass_count: 0,	        
	        current_player: NextPlayer,
	        board: NewBoard,
	        hands: NewHands,
	        exposed: NewExposed }).		

% End state transition

% Misc routines
    
next_player(ViewOrState,NextPlayer) :-
  NumPlayers is ViewOrState.params.num_players,
  NextPlayer is (ViewOrState.current_player + 1) mod NumPlayers.  	
  

test_play(MaxRounds,Seed) :-
	!,
	open("/tmp/domino-test.log",write,LogStream), with_output_to(LogStream,test_play_1(MaxRounds,Seed)),
	%test_play_1(MaxRounds,Seed),	
	format("Test ended\n").

test_play_1(MaxRounds,Seed) :-	 
	format("Test start: MaxRounds: ~k, Seed: ~k.\n",[MaxRounds,Seed]),
	SS=[],%domino_game_state_space(SS), 
	domino_state_space_param_create(SS,Param0),
	Param = Param0.put(_{random_seed: seed(Seed)}), 
	domino_state_space_initial_state(SS,Param,State0),
	(test_play(State0,0,MaxRounds),! ; format("Test failed!\n")).	

test_play(_State,Round,Round) :-
	format("maximum number of rounds reached!\n"),!.	
test_play(State,Round,MaxRounds) :-
	format("Round: ~a\n",[Round]),
	format("   Board:\n"),
	foreach(member(B,State.board),format("       ~k\n",[B])),
	format("   Hands:\n"),
	foreach(member(B,State.hands),format("       ~k\n",[B])),
	format("   Draw pile:\n"),
	format("        ~k\n",[State.draw_pile]),
	format("   Current player: ~a.\n",[State.current_player]),
	format("   Pass count: ~a.\n",[State.pass_count]),
	format("\n"),
	format("---test start---\n"),
	test_view_consistency_1(State),
	format("---test end---\n\n"),
	(state_transitions(State,MoveStatePairs),! ;
	  (format("Cannot compute transitions from: ~k.\n",[State]),fail)),
	test_play_1(Round,MoveStatePairs,MaxRounds).

test_play_1(_Round,[],_MaxRound) :-
	format("Game ended!\n").	
test_play_1(Round,MoveStatePairs,MaxRounds) :-	
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
	test_play(NextState,Round1,MaxRounds).
		  
test_view_consistency_1(State0) :-
 	format("Testing consistency.\n"),
 	print_state("State0","  ",State0),
 	NumPlayers1 is State0.params.num_players - 1,
 	foreach(between(0,NumPlayers1,Player),
 		test_view_consistency_2(State0,Player)).
test_view_consistency_2(State0,Player) :-
 	format("    Checking player ~k.\n",[Player]),
 	(test_view_consistency_3(State0,Player),! ; 
 	 format("      Inconsistency found for player: ~k.\n",[Player]),fail).
test_view_consistency_3(State0,Player) :-
 	domino_player_view(State0,Player,View0),
 	print_view("View0","    ",View0),
 	state_transitions(State0,StateTrans),
 	print_moves_1("      State0 moves:",StateTrans),
 	(view_simulate_moves(View0,ViewTrans),! ; format("OPS! view_simulate_moves failed!"),fail),
 	print_moves_1("      View0  moves:",ViewTrans),
 	length(StateTrans,NumStateTrans),
 	length(ViewTrans,NumViewTrans),
 	format("      State transitions length: ~k.\n",[NumStateTrans]),
 	format("      View transitions length: ~k.\n",[NumViewTrans]),
 	(NumStateTrans =< NumViewTrans, ! ; format("        ~k > ~k !\n",[NumStateTrans,NumViewTrans]),fail),
 	foreach(member([Move,State1],StateTrans),test_view_consistency_4(View0.view_player,ViewTrans,Move,State1)).
test_view_consistency_4(ViewPlayer,ViewTrans,Move,State1) :-
 	format("      Checking move: ~k,\n",[Move]),
 	format("        to state:\n"),
 	print_state("State1","          ",State1),
 	(test_view_consistency_5(ViewPlayer,ViewTrans,Move,State1),! ; format("          failed!\n"),fail).
 
check_eq(Indent,Name,Value1,Value2) :-
 	Value1 = Value2,!,
 	format("~s~s: ~k = ~k.\n",[Indent,Name,Value1,Value2]).
check_eq(Indent,Name,Value1,Value2) :-
 	format("~s~s: ~k <> ~k!\n",[Indent,Name,Value1,Value2]).
 
test_same_move(Move,[Move,_]).
 
test_view_consistency_5(ViewPlayer,ViewTrans,Move,State1) :-
        domino_player_view(State1,ViewPlayer,View1_),
        print_view("View1_","       ",View1_),
        include(test_same_move(Move),ViewTrans,ViewTrans1),
        (ViewTrans1 = [] -> format("         move not available for view!.\n"), 
                            print_view_trans_set("         ",ViewTrans), fail ; true),
        list_map(snd,ViewTrans1,Views1),
        (member(View1_,Views1),! ; 
         format("         no corresponding view found!.\n"),
         foreach(member(V,Views1),(format("         candidate:\n"), check_view_eq("             ",View1_,V))), fail).

check_view_eq(Indent,View1,View2) :-        
 	check_eq(Indent,"view_player",View1.view_player,View2.view_player),
 	check_eq(Indent,"current_player",View1.current_player,View2.current_player),
 	check_eq(Indent,"hand",View1.hand,View2.hand),
 	check_eq(Indent,"board",View1.board,View2.board),
 	check_eq(Indent,"exposed",View1.exposed,View2.exposed),
 	check_eq(Indent,"pass_count",View1.pass_count,View2.pass_count),
 	check_eq(Indent,"draw_pile_size",View1.draw_pile_size,View2.draw_pile_size),
 	check_eq(Indent,"hand_sizes",View1.hand_sizes,View2.hand_sizes).
 
print_view_trans_set(Indent,ViewTrans) :-
 	member([Move,View],ViewTrans),
 	format("~s candidate[~k]:\n",[Indent,Move]),
 	format(string(Indent2),"~s   ",[Indent]),
 	print_view("V",Indent2,View).
 
print_view(Name,Indent,View) :-
 	format("~s~s:\n",[Indent,Name]),
 	format("~s  view_player: ~k,\n",[Indent,View.view_player]),
 	format("~s  current_player: ~k,\n",[Indent,View.current_player]),
 	format("~s  hand: ~k,\n",[Indent,View.hand]),
 	format("~s  board: ~k,\n",[Indent,View.board]),
 	format("~s  exposed: ~k,\n",[Indent,View.exposed]),
 	format("~s  pass_count: ~k,\n",[Indent,View.pass_count]),
 	format("~s  draw_pile_size: ~k,\n",[Indent,View.draw_pile_size]),
 	format("~s  hand_sizes: ~k,\n",[Indent,View.hand_sizes]).	
print_state(Name,Indent,State) :-
 	format("~s~s:\n",[Indent,Name]),
 	format("~s  current_player: ~k,\n",[Indent,State.current_player]),
 	format("~s  hands: ~k,\n",[Indent,State.hands]),
 	format("~s  board: ~k,\n",[Indent,State.board]),
 	format("~s  pass_count: ~k,\n",[Indent,State.pass_count]),
 	format("~s  draw_pile: ~k,\n",[Indent,State.draw_pile]).
print_moves_1(Prefix,MoveStatePairs) :-
 	list_map(head,MoveStatePairs,Moves),
 	format("~s ~k.\n",[Prefix,Moves]).
 	    