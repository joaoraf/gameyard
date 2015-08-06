:- module(swish_render_tictactoe, 
	[ term_rendering//3
	]).

:- use_module('../../gameyard_config').	
:- use_module(library(http/html_write)).
:- use_module(swish(lib/render)).
:- use_module(tictactoe_state_space).
:- use_module(gameyard(misc/list_extras)).

:- register_renderer(tictactoe,"Render tic-tac-toe board representations.").

term_rendering(Term,_Vars,_Options) -->
	{ is_tictactoe_state(Term),
	  LineHeight is 200/Term.size
	},
	html(div([ style('display:inline-block;'+
	                 'line-height:'+LineHeight +'px;' +
	                 'font-size:'+LineHeight+'px;'
	                 ),
	           'data-render'('Tic-Tac-Toe Board')
	         ],
	         [ table(class('tictactoe-board'),
	           \tictactoe(Term)),
	           \tictactoe_style
		 ])).

is_cell(X) :- tictactoe_state_space:is_empty(X).
is_cell(X) :- tictactoe_state_space:is_cross(X).
is_cell(X) :- tictactoe_state_space:is_circle(X).

is_tictactoe_state(Term) :-
	is_dict(Term,tictactoe_state),
	Term.size > 2,
	length(Term.cells,Len),
	Len is Term.size * Term.size,
	foreach(member(C,Term.cells),is_cell(C)).

tictactoe(State) -->
	{ Size = State.size,
	  Cells = State.cells},
	tictactoe_rows(0,Size,Cells).

tictactoe_rows(Y,Y,_Cells) --> [].
tictactoe_rows(Y,Size,Cells) -->
	{ Y < Size,
	  split(Size,Cells,Row,Rest) },
	html(tr(\tictactoe_row(Row))),
	{ Y1 is Y + 1 },
	tictactoe_rows(Y1,Size,Rest).

tictactoe_row([]) --> [].
tictactoe_row([X|XS]) --> 
	html(td(\tictactoe_cell(X))),
	tictactoe_row(XS).

tictactoe_cell(C) --> 
	{ tictactoe_state_space:is_circle(C) },
	html('O').
tictactoe_cell(C) --> 
	{ tictactoe_state_space:is_cross(C) },
	html('X').
tictactoe_cell(C) --> 
	{ tictactoe_state_space:is_empty(C) },
	html(&('nbsp')).

tictactoe_style -->
	html({|html||
<style>

.tictactoe-board {
  border: 0px width: 200px height: 200px;
  border-collapse: collapse;
}

.tictactoe-board td {
  background: #ff;
  text-align: center;
  vertical-align: middle;
  border: 1px solid #222;
}

.tictactoe tr:first {
  border-top-style: none;
}

.tictactoe tr:last {
  border-bottom-style: none;
}

.tictactoe td:first {
  border-bottom-style: none;
}

.tictactoe td:last {
  border-bottom-style: none;
}

</style>	
	
	
	|}).
