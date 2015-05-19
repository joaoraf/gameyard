:- module(player_type,[
            
  ]).

player_type(PT) :-
  is_dict(PT,player_type),
  callable(PT.player_works_with_game_type),
  callable(PT.player_create_params),
  callable(PT.player_create_player).
  
player(P) :-
  is_dict(P),
  callable(P.player_play).
  
default_player_type(PT) :-
  M = player_type,
  PT = player_type {
    player_works_with_game_type: M:works_with_all,
    player_initial_state: M:empty_state
  }.
  
new_player_type(Overrides,PT) :-
  default_player_type(PT1),
  PT = PT1.put(Overrides).

player_works_with_game_type(PT,GT) :-
  call(PT.player_works_with_game_type, PT,GT).

player_create_params(PT,Params) :-
  call(PT.player_create_params,PT,Params).  

player_create_player(PT,Game,Params,Player,InitialState) :-
  call(PT.player_create_player,PT,Game,Params,Player,InitialState).

player_play(P,PlayerState,View,MoveEndViewPairs,Move,NextPlayerState).
  
game_state_space(SS) :-
  is_dict(SS,state_space),
  callable(SS.state_space_param_check),
  callable(SS.state_space_param_create),
  callable(SS.state_space_initial_state),
  callable(SS.state_space_players),
  callable(SS.state_space_current_player),
  callable(SS.state_space_player_view),
  callable(SS.state_space_view_transitions),
  callable(SS.state_space_transitions),  
  callable(SS.state_space_state_render),
  callable(SS.state_space_view_render).

default_game_state_space(SS) :-
  M=game_state_space,
  SS = state_space{
    state_space_param_check: M:default_game_state_param_check,
    state_space_current_player: M:default_game_state_current_player,
    state_space_player_view: M:default_state_space_player_view,
    state_space_view_transitions: M:state_space_transitions,    
    state_space_state_render: M:default_game_state_render,
    state_space_view_render: M:default_game_state_render    
  }.

default_game_state_param_check(_SS,_Param).

default_game_state_current_player(_SS,_State,0).
  
default_state_space_player_view(_SS,State,_PlayerNum,State).

default_game_state_end_state(_SS,_State) :- fail.

default_game_state_render(_SS,State,[RenderedState]) :-
	sformat(RenderedState,"~k",State).  

state_space_param_check(SS,Param) :-
  F = SS.state_space_param_check,
  call(F,SS,Param).

state_space_param_create(SS,PreParams,Params) :-
  F = SS.state_space_param_create,
  call(F,SS,Params1),
  Params = Params1.put(PreParams).

state_space_initial_state(SS,Params,InitialState) :-
  F = SS.state_space_initial_state,
  call(F,SS,Params,InitialState).

state_space_players(SS,State,Players) :-
  F = SS.state_space_players,
  call(F,SS,State,Players).


state_space_current_player(SS,State,Player) :-
  F = SS.state_space_current_player,
  call(F,SS,State,Player).

state_space_player_view(SS,State,PlayerNum,PlayerView) :-
  F = SS.state_space_player_view,
  call(F,SS,State,PlayerNum,PlayerView).

state_space_view_transitions(SS,View,MoveEndViewPairs) :-
  F = SS.state_space_view_transitions,
  call(F,SS,View,MoveEndViewPairs).

state_space_transitions(SS,View,MoveEndStatePairs) :-
  F = SS.state_space_transitions,
  call(F,SS,View,MoveEndStatePairs).
  
state_space_state_render(SS,State,RenderedLines) :-
  F = SS.state_space_state_render,
  call(F,SS,State,RenderedLines).  
  
state_space_view_render(SS,View,RenderedLines) :-
  F = SS.state_space_view_render,
  call(F,SS,View,RenderedLines).  

  