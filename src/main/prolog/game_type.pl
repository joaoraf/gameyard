:- module(game_type,[
	game_type/1]).

:- use_module(game_state_space).

% Game types

game_type(GT) :-
  is_dict(GT,game_type),
  game_state_space(GT.state_space).
  