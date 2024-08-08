:- lib(ic).
:- set_flag(print_depth, 20).
% :- compile(genrand).

liars(Decl, Sol) :-
	length(Decl, N),
	length(Sol, N),
	Sol #:: 0 .. 1,
	Sum #= sum(Sol),
	constrain(Decl, Sol, Sum),
	labeling(Sol).

constrain([], _, _).
constrain([X1|L1], [X2|L2], Sum) :-
	X2 #= (X1 #> Sum), % ο Χ2 είναι ψεύτης αν δηλώνει περισσότερους ψεύτες από το πλήθος ψευτών
	constrain(L1, L2, Sum).
