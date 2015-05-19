:- module(game_system,
	[ game_system/1,
	  default_game_system/1,	  
	  player_type/1,
	  default_player_type/1,
	  human_player_type/1,
	  play_type/1,
	  default_play_type/1	
	]).

:- use_module(game_type).

% Game systems

game_system(GS):-  
  is_dict(GS,game_system),  
  is_dict(GS.game_types,game_types),
  forall(GS.game_types._ = GT, game_type(GT)),
  is_dict(GS.player_types,player_types),
  forall(GS.player_types._ = PT, player_type(PT)),
  is_dict(GS.play_types,play_types),
  forall(GS.play_types._ = PT, play_type(PT)).

default_game_system(GS) :- 
  GS = game_system{
          game_types: game_types{},
          player_types: player_types{},
          play_types: play_types{}
    }.


% Player types  
  
player_type(PT) :-
  is_dict(PT,player_type),
  bool(PT.human).

default_player_type(PT) :-
  PT = player_type{human:false}.

human_player_type(PT) :-
  default_player_type(PT),
  nb_set_dict(human,PT,true).

% Play type

play_type(PT) :-
  is_dict(PT,play_type).
  
default_play_type(PT) :-
  PT = play_type{}.  
    
    
% Other stuff

bool(true).
bool(false).

    
% Consistency checks    
    
:- default_game_system(GS), game_system(GS).

:- default_player_type(PT), player_type(PT).

:- human_player_type(PT), player_type(PT).

:- default_play_type(PT), play_type(PT).