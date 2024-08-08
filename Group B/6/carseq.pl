:- lib(ic).
:- lib(ic_global).
:- set_flag(print_depth, 1000).

% :- compile(carseq_data1).

carseq(S) :-
	classes(C),
	N #= sum(C),
	length(S, N),
	length(C, Length),
	S #:: 1 .. Length,
	constrain1(C, S, 1),
	options(O),
	constrain2(O, S, C),
	labeling(S).


/* Περιορισμοί για εμφανίσεις στοιχείων στην λίστα */
constrain1([], _, _).
constrain1([C|Cs], S, I) :-
	occurrences(I, S, C),
	I1 is I + 1,
	constrain1(Cs, S, I1).

/* Περιορισμοί για την χωρητικότητα και το πλήθος των πιθανών συνθέσεων */
constrain2([], _, _).
constrain2([M / K / O|L], S, C) :-
	findall(I, indexOf(O, I), Values),
	sum_index(Values, C, Min),
	Max = Min,
	sequence_total(Min, Max, 0, K, M, S, Values),
	constrain2(L, S, C).

/* Επιστρέφει το άθροισμα επιλεγμένων στοιχείων της λίστας L1 (μέσω της L) */
sum_index(L, L1, N) :-
	sum_index(L, L1, 0, N).

sum_index([], _, N, N).
sum_index([L|T], L1, I, N) :-
	getN(L, L1, Item),
	I1 is I + Item,
	sum_index(T, L1, I1, N).

/* Αν υπάρχει το Nοστό στοιχείο στην λίστα το επιστρέφει, στην αντίστροφη κατεύθυνση επιστρέφει Yes και
μόνο την πρώτη δυνατή θέση στην λίστα */
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

/* Επιστρέφει (με οπισθοδρόμηση) κάθε δυνατή θέση στην οποία υπάρχει 1 */
indexOf([1|_], 1).
indexOf([_|T], I) :-
	indexOf(T, I1),
	I is I1 + 1.
