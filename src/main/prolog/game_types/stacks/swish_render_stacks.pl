:- module(swish_render_tictactoe, 
	[ term_rendering//3
	]).
	
:- use_module(library(http/html_write)).
:- use_module('/home/joao/workspace_old/pengines/apps/swish/lib/render').
:- use_module(stacks_state_space).
:- use_module('../../list_extras').

:- register_renderer(tictactoe,"Render stacks board representations.").

term_rendering(Term,_Vars,_Options) -->
	{ is_tictactoe_state(Term),
	  LineHeight is 200/Term.size
	},
	html(div([ style('display:inline-block;'+
	                 'line-height:'+LineHeight +'px;' +
	                 'font-size:'+LineHeight+'px;'
	                 ),
	           'data-render'('Stacks Board')
	         ],
	         [ table(class('stacks-board'),
	           \tictactoe(Term)),
	           \tictactoe_style
		 ])).

is_cell(X) :- stacks_state_space:is_empty(X).
is_cell(X) :- stacks_state_space:is_cross(X).
is_cell(X) :- stacks_state_space:is_circle(X).

is_stacks_state(Term) :-
	is_dict(Term,stacks_state),
	Term.size > 2,
	length(Term.cells,Len),
	Len is Term.size * Term.size,
	foreach(member(C,Term.cells),is_cell(C)).

stacks(State) -->
	{ Size = State.size,
	  Cells = State.cells},
	stacks_rows(0,Size,Cells).

stacks_rows(Y,Y,_Cells) --> [].
stacks_rows(Y,Size,Cells) -->
	{ Y < Size,
	  split(Size,Cells,Row,Rest) },
	html(tr(\stacks_row(Row))),
	{ Y1 is Y + 1 },
	stacks_rows(Y1,Size,Rest).

stacks_row([]) --> [].
stacks_row([X|XS]) --> 
	html(td(\stacks_cell(X))),
	stacks_row(XS).

stacks_cell(C) --> 
	{ stacks_state_space:is_circle(C) },
	html('O').
stacks_cell(C) --> 
	{ stacks_state_space:is_cross(C) },
	html('X').
stacks_cell(C) --> 
	{ stacks_state_space:is_empty(C) },
	html(&('nbsp')).

stacks_style -->
	html({|html||
<style>

.stacks-board {
  border: 0px width: 200px height: 200px;
  border-collapse: collapse;
}

.stacks-board td {
  background: #ff;
  text-align: center;
  vertical-align: middle;
  border: 1px solid #222;
}

.stacks tr:first {
  border-top-style: none;
}

.stacks tr:last {
  border-bottom-style: none;
}

.stacks td:first {
  border-bottom-style: none;
}

.stacks td:last {
  border-bottom-style: none;
}

</style>	
	
	
	|}).
