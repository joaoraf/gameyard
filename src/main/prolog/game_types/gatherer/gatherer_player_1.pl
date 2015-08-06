:- module(gatherer_player_1, [
        ]).
      
gatherer_player_1_initial_params(_SS,Params) :-
	Params = gatherer_p_1{
	}.        
        
gatherer_player_1_initial_state(_SS,Params,InitialState) :-
	
	InitialState = gatherer_p1_state{
		graph : Graph,
		edgeMap : EdgeMap,
		vertexMap : VertexMap 
	}.
        