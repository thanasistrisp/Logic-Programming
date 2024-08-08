% decode_rl/2

decode_rl([],[]).
decode_rl([(X,1)|Tail1],[X|Tail2]) :-
	decode_rl(Tail1,Tail2).
decode_rl([(X,N)|Tail1],[X|Tail2]) :-
	N > 1, N1 is N - 1, decode_rl([(X,N1)|Tail1],Tail2).
decode_rl([X|Tail1],[X|Tail2]) :-
	X \= (_,_), /* Το X πρέπει να είναι απλώς ένα σύμβολο S, χωρίς παρενθέσεις, ώστε να μην επανακληθεί
	ασκόπως στην οπισθοδρόμηση */
	decode_rl(Tail1,Tail2).


% encode_rl/2

encode_rl([],[]).
encode_rl([X|List1],[Y|Tail2]) :-
	total(X,List1,Tail1,Y,1), encode_rl(Tail1,Tail2).

total(X,[],[],X,1).
total(X,[Y|Tail1],[Y|Tail1],X,1) :- X \= Y.

total(X,[],[],(X,N),N) :- N > 1.
total(X,[Y|Tail1],[Y|Tail1],(X,N),N) :- N > 1, X \= Y.

total(X,[X|Y],Tail1,L,N) :-
	N1 is N + 1, total(X,Y,Tail1,L,N1).
