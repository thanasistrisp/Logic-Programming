:- lib(ic).
:- set_flag(print_depth, 1000).
:- lib(branch_and_bound).


tents(RowTents, ColumnTents, Trees, Tents) :-
	length(RowTents, N),
	length(ColumnTents, M),
	create_2d_list(N, M, Sol),
	Sol #:: 0 .. 1,
	neighbor_tree(Sol, Trees),
	find_pairs(N, M, Pairs),
	pair_neighbor(Sol, Pairs),
	not_at_tree(Trees, Sol),
	constrain_column(Sol, ColumnTents),
	constrain_row(Sol, RowTents),
	flatten(Sol, Sol1), % flatten to 1d list to be able to use sum
	Cost #= sum(Sol1),
	% Practically most_constrained with strategy:restart were the most effective options, even for the last one
	bb_min(search(Sol, 0, most_constrained, indomain, complete, []), Cost, bb_options{solutions : all, strategy : restart}),
	% for last example I need 253.15 sec in linux
	indices(Sol, 1, N, M, Tents).

% for each tree at least one tent must be placed horizontally or vertically or diagonally
neighbor_tree(_, []).
neighbor_tree(Sol, [X1 - X2|Xs]) :-
	Up is X1 - 1,
	Down is X1 + 1,
	Left is X2 - 1,
	Right is X2 + 1,
	(getE(X1, Left, Sol, E1) -> true ; E1 #= 0),
	(getE(X1, Right, Sol, E2) -> true ; E2 #= 0),
	(getE(Up, X2, Sol, E3) -> true ; E3 #= 0),
	(getE(Down, X2, Sol, E4) -> true ; E4 #= 0),
	(getE(Up, Left, Sol, E5) -> true ; E5 #= 0),
	(getE(Up, Right, Sol, E6) -> true ; E6 #= 0),
	(getE(Down, Left, Sol, E7) -> true ; E7 #= 0),
	(getE(Down, Right, Sol, E8) -> true ; E8 #= 0),
	sum([E1, E2, E3, E4, E5, E6, E7, E8]) #>= 1,
	sum([E1, E2, E3, E4, E5, E6, E7, E8]) #=< 4, % tents are also at most 4 (this reduces time to half)
	neighbor_tree(Sol, Xs).

% a tent cannot be placed in a position that is already occupied by a tree
not_at_tree([], _).
not_at_tree([X1 - X2|Xs], Sol) :-
	getE(X1, X2, Sol, El),
	El #= 0,
	not_at_tree(Xs, Sol).

% create 2d list of trees
create_2d_list(N, M, Sol) :-
	length(Temp, M),
	findall(Temp, between(1, N, _), Sol).


% find all indexes that contain Element in list of lists
indices(L, Element, N, M, IJs) :-
	findall(I-J, indices_1(L, Element, N, M, I-J), IJs).

indices_1(List, Element, N, M, I-J) :-
	between(1, N, I),
	between(1, M, J),
	getN(I, List, L1),
	getN(J, L1, Element).

% find all pairs 1-2,1-3,etc.
find_pairs(N, M, Pairs) :-
	findall(X-Y, find_pairs_1(N, M, X-Y), Pairs).
find_pairs_1(N, M, I-J) :-
	between(1, N, I),
	between(1, M, J).

% two tents cannot be placed "next" to each other
pair_neighbor(_, []).
pair_neighbor(Sol, [X1-X2|Xs]) :-
	Down is X1 + 1,
	Right is X2 + 1,
	getE(X1,X2,Sol,X),
	(getE(X1, Right, Sol, E1) -> true ; E1 #= 0),
	(getE(Down, X2, Sol, E2) -> true ; E2 #= 0),
	(getE(Down, Right, Sol, E3) -> true ; E3 #= 0),
	% exclude right, down, down-right from X and Down-Right
	% Only those are needed as the others will be excluded by next pairs and are redundant
	sum([X, E1]) #=< 1,
	sum([X, E2]) #=< 1,
	sum([E1, E2]) #=< 1,
	sum([X, E3]) #=< 1,
	pair_neighbor(Sol, Xs).

constrain_column(Sol, ColumnTents) :-
	constrain_column_1(Sol, ColumnTents, 1).
constrain_column_1(_, [], _).
constrain_column_1(Sol, [T|Ts], I) :-
	columnN(Sol, I, C),
	(T >= 0 -> sum(C) #=< T ; true),
	I1 is I + 1,
	constrain_column_1(Sol, Ts, I1).

constrain_row(Sol, RowTents) :-
	constrain_row_1(Sol, RowTents, 1).
constrain_row_1(_, [], _).
constrain_row_1(Sol, [T|Ts], I) :-
	rowN(Sol, I, C),
	(T >= 0 -> sum(C) #=< T ; true),
	I1 is I + 1,
	constrain_row_1(Sol, Ts, I1).

% Get whole row I from 2d list
rowN([H|_],1,H):-!.
rowN([_|T],I,X) :-
	I1 is I-1,
	rowN(T,I1,X).

% Get whole column I from 2d list
columnN([],_,[]).
columnN([H|T], I, [R|X]):-
	rowN(H, I, R), 
	columnN(T,I,X).


% Get an element from a 2-dimensional list at (Row,Column)
% using 1-based indexing.
getE(Row, Column, List, Element) :-
	getN(Row, List, SubList),
	getN(Column, SubList, Element).

get_index(Pos, List, Element) :-
	get_index(Pos,List,Element,1).

get_index(Pos, [_|T], Element,N1) :-
	N1 < Pos,
	N2 is N1 + 1,
	get_index(Pos, T, Element, N2).
get_index(_,[H|_],H,_).


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


between(N1,N2,N1) :-
	N1 =< N2.
between(N1,N2,X) :-
	N1 < N2,
	NewN1 is N1 + 1,
	between(NewN1,N2,X).
