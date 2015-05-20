:- module(swish_render_gatherer_state_space,
	[ term_rendering//3
	]).
	
:- use_module(library(http/html_write)).
:- use_module('/home/joao/workspace_old/pengines/apps/swish/lib/render').
:- use_module(gatherer_state_space).
:- use_module('../../list_extras').
:- use_module('../../board2d').

:- register_renderer(gatherer,"Render gatherer board representations.").  

term_rendering(Term,_Vars,_Options) -->
	{ format('gatherer_term_rendering: starting\n'),
	  is_gatherer_state(Term),
	  TableWidth is 200,
	  TableHeight is floor(TableWidth * Term.params.height / Term.params.width),	  
	  LineHeight is TableHeight/Term.params.height,
	  gatherer_table(Term,Table),
	  gatherer_player_status(Term,PlayersStatus),
	  gatherer_player_status_available_food(Term,AvailableFood)
	},
	html(div([ style('display:inline-block;'
	                 
	                 ),
	           'data-render'('Gatherer Board')
	         ],
	         [ table(
	             [class('gatherer-board'),style('width:40%;height:40%' + 'line-height:'+LineHeight +'px;' +
	                 'font-size:'+LineHeight+'px;')],
	             %style(TableStyle),
	             Table),	           
	           div(PlayersStatus),
	           div(AvailableFood),
	           \gatherer_style
		 ])).

is_gatherer_state(Term) :-
	is_dict(Term,gatherer_state).
	
render_players(_CurPlayerPos,[],_WT,_N,Board,Board).
render_players(CurPlayerPos,[P|PS],WT,N,Board1,Board2) :-  
  (P = CurPlayerPos -> format(atom(Class),'gatherer-current-player gatherer-player-~k',[N]) ; 
                       format(atom(Class),'gatherer-player-~k',[N])),  
  format(atom(Num),'~k',[N]),
  N1 is N + 1, 
  board2d_change_cell(Board1,P,td([class(Class),style(WT)],Num),Board),
  render_players(CurPlayerPos,PS,WT,N1,Board,Board2).
  
gatherer_table(State,TRs) :-  
    TdWidth is 100 / State.params.width,
    format(atom(WidthText),'width:~k%',[TdWidth]),
    Board = State.board,
    board2d_positions(Board,[f],FreePos1),
    board2d_positions(Board,[w],WallPos),
    subtract(FreePos1,State.players_pos,FreePos2),
    subtract(FreePos2,State.food_pos,FreePos3),
    FreeTd = td([class('gatherer-free-cell'),style(WidthText)],&(nbsp)),
    BlockTd = td([class('gatherer-block-cell'),style(WidthText)],&(nbsp)),    
    FoodTd = td([class('gatherer-food-cell'),style(WidthText)],'☕'),
    board2d_change_cells(Board,FreePos3,FreeTd,Board1),
    board2d_change_cells(Board1,WallPos,BlockTd,Board2),
    board2d_change_cells(Board2,State.food_pos,FoodTd,Board3),
    gatherer_state_space_current_player(_,State,CurPlayer),
    nth1(CurPlayer,State.players_pos,CurPlayerPos),
    render_players(CurPlayerPos,State.players_pos,WidthText,1,Board3,Board4),
    board2d_rows(Board4,Rows),
    aggregate_all(bag(tr(class('gatherer-row'),R)),member(R,Rows),TRs).      	 

gatherer_player_status(State,Status) :-
    aggregate_all(bag(X),gatherer_player_status_1(State,X),Status).
    
gatherer_player_status_1(State,PlayerStatus) :- 
    between(1,State.params.num_players,N),
    nth1(N,State.players_food,FoodCount),
    repl(FoodCount,'☕',FoodsL),
    atomic_list_concat(FoodsL,Foods),
    format(atom(PlayerStatusText),'Player ~k: ~s',[N,Foods]),
    format(atom(Class),'gatherer-player-status gatherer-player-~k',[N]),    
    PlayerStatus = p(class(Class),PlayerStatusText).

gatherer_player_status_available_food(State,AvailableFood) :-
    sum_list(State.players_food,Consumed),
    NumAvailable is State.params.food_count - Consumed,
    format(atom(Msg),'Available food: ~k',[NumAvailable]),
    AvailableFood = p(class('gatherer-available-food'),Msg).

gatherer_style -->
	html({|html||
<style>

.gatherer-board {
  border: 1px dashed #aaa;
  border-collapse: collapse;
  table-layout: fixed;
}

.gatherer-board td {
  text-align: center;
  vertical-align: middle;
  border: 1px dotted #bbb;
  background: #eee;
}

td.gatherer-free-cell {
%  background: #fff;
}

td.gatherer-block-cell {
  background: #000;
}

td.gatherer-food-cell {
  color: #060;
}

.gatherer-player-status {
	margin: 0 0 2px;
}

.gatherer-available-food {
	margin: 0 0 2px;
}

td.gatherer-current-player {
	background: #88d;	
}

.gatherer-player-1 {
  color: #4A2ABB;
}

.gatherer-player-2 {
  color: #826EC9;
}

.gatherer-player-3 {
  color: #6349BE;
}

.gatherer-player-4 {
  color: #3310AC;
}

.gatherer-player-5 {
  color: #280B8D;
}

.gatherer-player-6 {
  color: #F01D46;
}

.gatherer-player-7 {
  color: #F3768F;
}

.gatherer-player-8 {
  color: #F14869;
}

.gatherer-player-9 {
  color: #E8002E;
}

.gatherer-player-10 {
  color: #BF0025;
}

.gatherer-player-11 {
  color: #5BE21B;
}

.gatherer-player-12 {
  color: #97E871;
}

.gatherer-player-13 {
  color: #78E344;
}

.gatherer-player-14 {
  color: #46D800;
}

.gatherer-player-15 {
  color: #39B100;
}

.gatherer-player-16 {
  color: #FFD71E;
}

.gatherer-player-17 {
  color: #FFE87C;
}

.gatherer-player-18 {
  color: #FFDF4D;
}

.gatherer-player-19 {
  color: #F9CD00;
}

.gatherer-player-20 {
  color: #CDA800;
}



</style>	
	
	
	|}).
	