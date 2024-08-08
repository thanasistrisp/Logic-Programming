/* Ομάδα Α - Άσκηση 2 */

move(X,L,New) :- 
	length(L,N), 1 =< X, X < N, element_at(E,L,X), Temp is X + 1, replace(L,Temp,E,New).

move(X,L,New) :- 
	length(L,N), X = N, element_at(E,L,X), replace(L,1,E,New).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|T1]):- I > 1, I1 is I - 1, replace(T, I1, X, T1).

element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).


swap(I,J,L,N) :-
	I1 is I - 1, J1 is J - 1, swap_r(I1,J1,L,N).

swap_r(I,J,F,L) :- /* zero index */
	same_length(L,F),
	append(BeforeI,[AtI|PastI],L),
	append(BeforeI,[AtJ|PastI],Temp),
	append(BeforeJ,[AtJ|PastJ],Temp),
	append(BeforeJ,[AtI|PastJ],F),
	length(BeforeI,I),
	length(BeforeJ,J).

same_length([],[]).
same_length([_|L1],[_|L2]) :- same_length(L1, L2).


% Iterative deepening dfs (iddfs)
codegen(Init, Final, L) :-
	path(Init,GoalNode,_,Temp),
	replace_var(Final,*,Final1), % '*' means I don't care so I replace it with anonymous variable
	GoalNode = Final1, reverse(Temp, L), !.

path(Node,Node,[Node],_).
path(FirstNode,LastNode,[LastNode|Path],[Moves|Tail]) :-
	path(FirstNode,OneButLast,Path,Tail),
	process(OneButLast,LastNode,Moves),
	\+ member(LastNode,Path).


process(L,F,move(T)) :-
	length(L,Len),
	between(1,Len,T),
	move(T,L,F).

process(L,F,swap(I,J)) :-
	length(L,Len),
	between(1,Len,I),
	I1 is I + 1,
	between(I1,Len,J),
	swap(I,J,L,F).


between(N1,N2,N1) :-
	N1 =< N2.
between(N1,N2,X) :-
	N1 < N2,
	NewN1 is N1 + 1,
	between(NewN1,N2,X).


/* Replace * with different variables in each position */
replace_var([],_,[]).
replace_var([Val|B],Val,[_|Q]) :-
	replace_var(B,Val,Q).
replace_var([A|B],Val,[A|Q]) :-
	A \= Val,
	replace_var(B,Val,Q).
