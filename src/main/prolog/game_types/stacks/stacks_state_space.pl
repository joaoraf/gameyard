:- module(stacks_state_space,[
	stacks_game_state_space/1
  ]).

:- use_module(library(random)).
:- use_module('../../game_state_space').
:- use_module('../../list_extras').

stacks_game_state_space(GS) :-
  default_game_state_space(GS1), 
  M=stacks_state_space,
  GS = GS1.put(
    _{
      state_space_param_check: M:stacks_state_space_param_check,
      state_space_param_create: M:stacks_state_space_param_create,
      state_space_initial_state: M:stacks_state_space_initial_state,
      state_space_players: M:stacks_state_space_players,
      state_space_current_player: M:stacks_state_space_current_player,      
      state_space_transitions: M:stacks_state_space_transitions
    }),
    game_state_space(GS).


stacks_state_space_param_check(_SS,Params) :-
  is_dict(Params,stacks_state_space_param),
  number(Params.num_blocks),
  Params.num_blocks >= 4.
  
  
stacks_state_space_param_create(_SS,Params) :-
	Params = stacks_state_space_param{
	  num_blocks : 6,
	  random_seed: rand(0,0,0)
	}. 

stacks_state_space_initial_state(_SS, Params,InitialState) :-
   fresh_state(Params,InitialState).

stacks_state_space_players(_SS,_State,[player]).

stacks_state_space_current_player(_SS,State,player).

stacks_state_space_transitions(_SS,State,[]) :-
	state_is_finished(State),!.
stacks_state_space_transitions(_SS,State,MoveEndStatePairs) :-
   aggregate(set([Move,EndState]),stacks_available_move(State,Move,EndState),MoveEndStatePairs),!.
stacks_state_space_transitions(_SS,_State,[]).    

stacks_available_move(State,Move,EndState) :-   
   state_available_moves(State,Moves),
   member(Move,Moves),
   state_play(State,Move,EndState).
   
stacks(NumBlocks,Stacks) :-
	randseq(NumBlocks,NumBlocks,Available),
	stacks_(Available,Stacks).
	valid_stacks(Stacks).

valid_stacks(Stacks) :- available_movement(Stacks,M), !.

stacks_([],[]) :- Pos >= Max, !.
stacks_(Available,L) :- !,
	length(Available,Len),
	random_between(1,Len,M),
 	split(M,Available,Stack,Available1),
	stacks_(Available1,Stacks1),
	Stacks = [Stack | Stacks1].

fresh_state(Params,State) :-
	setrand(Params.random_seed),
	stacks(params.num_blocks,Stacks),
	State = stacks_state { params : Params, stacks : Stacks }.
	
state_available_moves(State,Moves) :- state_find_positions(State,empty,Moves).

		  
