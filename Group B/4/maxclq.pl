:- lib(ic).
:- lib(branch_and_bound).

% :- compile(graph).

maxclq(N, D, Clique, Size) :-
	create_graph(N, D, G),
	length(Sol, N),
	Sol #:: 0 .. 1,
	complementary(G, N, C),
	constrain(C, Sol),
	Cost #= -sum(Sol),
	bb_min(search(Sol, 0, input_order, indomain, complete, []), Cost, bb_options{solutions : all}),
	/* έπειτα από more δίνονται όλες οι βέλτιστες λύσεις */
	indices(Sol, 1, Clique),
	length(Clique, Size).

/* Περιορισμοί */
constrain([], _).
constrain([X1 - X2|Xs], Sol) :-
	getN(X1, Sol, Y1),
	getN(X2, Sol, Y2),
	Y1 + Y2 #< 2,
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

/* Εύρεση όλων των θέσεων που περιέχουν το E */
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

/* Εύρεση συμπληρωματικού γράφου θεωρώντας ότι I < J */
	
complementary(T, N, P) :-
	findall(G, pair(T, N, G), P).

pair(T, N, G) :-
	between(1, N, I),
	between(1, N, J),
	I < J,
	\+ member(I - J, T),
	G = I - J.

between(N1,N2,N1) :-
	N1 =< N2.
between(N1,N2,X) :-
	N1 < N2,
	NewN1 is N1 + 1,
	between(NewN1,N2,X).
