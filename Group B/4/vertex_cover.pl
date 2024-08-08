:- lib(ic).
:- lib(branch_and_bound).

:- compile(graph).

:- set_flag(print_depth, 1000).

vertexcover(N, D, C) :-
	create_graph(N, D, G),
	length(Sol, N),
	Sol #:: 0 .. 1,
	constrain(G, Sol),
	Cost #= sum(Sol),
	bb_min(search(Sol, 0, input_order, indomain, complete, []), Cost, bb_options{strategy : restart}),
	indices(Sol, 1, C).
	

constrain([], _).
constrain([X1 - X2|Xs], Sol) :-
	getN(X1, Sol, Y1),
	getN(X2, Sol, Y2),
	Y1 + Y2 #> 0,
	constrain(Xs, Sol).


getN(N, L, Item) :-
	getN(N, 0, L, Item).   
getN(N, Count, [H|_], Item) :-
	CountNew is Count + 1,
	CountNew = N,
	Item = H,
	!.
getN(N, Count, [_|T], Item) :-
	CountNew is Count + 1,
	getN(N, CountNew, T, Item),
	!.

indices(List, E, Is) :-
	indices(List, E, Is, 1).
indices([], _, [], _).
indices([E|Xs], E, [I|Is], I) :-
	I1 is I + 1,
	indices(Xs, E, Is, I1).
indices([X|Xs], E, Is, I) :-
	X \= E,
	I1 is I + 1,
	indices(Xs, E, Is, I1).
