:- dynamic(tictactoe_state/1).
:- dynamic(tictactoe_state_space/1).

:-  tictactoe_game_state_space(SS),
    state_space_param_create(SS,_{size:4},Params),
    state_space_initial_state(SS,Params,State0),
    assertz(tictactoe_state_space(SS)),
    assertz(tictactoe_state(State0)),
    write(State0),
    set_random(seed(1)).

test(Move,State) :-
    tictactoe_state_space(SS),
    tictactoe_state(CurState),
    state_space_transitions(SS,CurState,MoveEndStateTrans),
    length(MoveEndStateTrans,L),    
    L > 0,
    random(0,L,N),
    nth0(N,MoveEndStateTrans,[Move1,State1]),
    retractall(tictactoe_state(_)),
    assertz(tictactoe_state(State1)),
    (Move = Move1, State = State1) ; (test(Move,State)).
