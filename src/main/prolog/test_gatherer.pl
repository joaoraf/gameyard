:- use_module('/home/joao/workspace_old/gameyard/src/main/prolog/game_types/gatherer/gatherer_state_space').
:- use_module('/home/joao/workspace_old/gameyard/src/main/prolog/board2d').

:- use_rendering(gatherer).

:- dynamic(gatherer_state/1).

create_state(State0) :-    
    getrand(RandState),
	gatherer_state_space_param_create(_,Params1), 
	Params = Params1.put(_{
                   width: 10,
                   height: 10,
                   food_count: 10,
                   wall_ratio:0.2, 
                   random_seed: RandState
                }), 
	gatherer_state_space_initial_state(_,Params,State0).

print_state(State) :-
    write('Board:\n'), 
    render_state_space(State,Rend),
    write(Rend),
    format('\nPlayers positions: ~k.\n',[State.players_pos]),
    format('\nPlayers foods: ~k.\n',[State.players_food]),
    board2d_positions(State.board,[f],FreePos),
    format('\nFree positions: ~k.\n',[FreePos]),
    write('\n').

step_game(State,Move) :-
    gatherer_state(State0),
    gatherer_state_space_transitions(_,State0,MovesStates),
    length(MovesStates,NumMoves),
    ((NumMoves > 1) ->  
    	(subtract(MovesStates,[[p,_]],MovesStates1),
         random_member([Move1,State1],MovesStates1)),
         member([Move1,State1],MovesStates)),
    retractall(gatherer_state(_)),
    assertz(gatherer_state(State1)),!,    
    ((Move = Move1, State = State1) ; (step_game(State,Move))).
step_game(State,Move) :-    
    not(gatherer_state(_)),
    create_state(State1),    
    assertz(gatherer_state(State1)),
    ((Move = start, State = State1) ; (step_game(State,Move))).

reset :- retractall(gatherer_state(_)).

step_game_N(0,S,M,S,M) :- !.
step_game_N(N,_State,_Move,S,M) :- 
    step_game(State,Move),!, 
    N1 is N - 1, 
    step_game_N(N1,State,Move,S,M).
step_game_N(_N,S,M,S,M) :- !.
         

/** <examples>

?- reset, step_game(State,Move).
?- reset, step_game_N(100,_,_,State,Move).

*/