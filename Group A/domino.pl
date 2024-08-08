/* Ομάδα Α - Άσκηση 3 */

dom_pos(H,T) :-
	dominos(D),
	member((H,T),D).

/* Βρίσκουμε τη διάσταση του frame */
length_frame([H|Tail],Columns,Raws) :-
	length(H,Columns),
	length_t(Tail,1,Raws).

length_t([],Y,Y).
length_t([H|Tail],Y1,Y) :-
	Y2 is Y1 + 1,
	length_t(Tail, Y2, Y).

between(N1,N2,N1) :-
	N1 =< N2.
between(N1,N2,X) :-
	N1 < N2,
	NewN1 is N1 + 1,
	between(NewN1,N2,X).


getN( N, L, Item ) :-
	getN( N, 0, L, Item ).   
getN( N, Count, [H|_], Item ) :-
	CountNew is Count + 1,
	CountNew = N,
	Item = H,!.
getN( N, Count, [_|T], Item ) :-
	CountNew is Count + 1,
	getN( N, CountNew, T, Item ),!.

check_dom(I,J,Struct) :-
	dom_pos(X,Y),
	member(L,[up,down,left,right]),
	call(in_frame(I,J,X,Y,Struct,L)).

check_dom(I,J,X,Y,Struct) :-
	member(L,[up,down,left,right]),
	call(in_frame(I,J,X,Y,Struct,L)).

in_frame(Pos,P,Dx,Dy,Struct,Dir) :-
	frame(F), length_frame(F, Ny, Nx),
	All is Ny * Nx,
	flatten(F, NF),
	between(1, All, Pos), getN(Pos,NF,X), X == Dx, (
		in_frame(Pos,P,Dx,Dy,Struct,Nx,Ny,NF,Dir)).
	
in_frame(Pos,P,Dx,Dy,Struct,Nx,Ny,NF,up) :-
	P is Pos - Ny, getN(P,NF,T), T == Dy, Struct = (Dy|Dx), Dx \= Dy.
in_frame(Pos,P,Dx,Dy,Struct,Nx,Ny,NF,down) :-
	P is Pos + Ny, getN(P,NF,T), T == Dy, Struct = (Dx|Dy).
in_frame(Pos,P,Dx,Dy,Struct,Nx,Ny,NF,left) :-
	P is Pos - 1, P1 is P mod Ny, P1 =\= 0, getN(P,NF,T), T == Dy, Struct = (Dy-Dx).
in_frame(Pos,P,Dx,Dy,Struct,Nx,Ny,NF,right) :-
	P is Pos + 1, getN(P,NF,T), T == Dy, Struct = (Dx-Dy), Dx \= Dy.


combine_soldom(Dominos, D, Soldom) :-
	combine_soldom(Dominos, 1, D, Soldom).

combine_soldom([], _, _, []).
combine_soldom([V1-V2/(X,Y)|Dominos], I, D, [V1-V2/X-Y/Domain|SolDom]) :-
	dominos(D), getN(I,D,(X,Y)), findall(X1-Y1,check_dom(X1,Y1,X,Y,_), Domain),
	I1 is I + 1,
	combine_soldom(Dominos, I1, D, SolDom).


put_dominos :-
	dominos(D),
	length(D,N),
	range(1,N,Dominos),
	combine_soldom(Dominos, D, SolDom),
	generate_solution_with_fc_mrv(SolDom),
	format_first_min(Dominos,D1),
	mergeSort(D1,D2),
	frame(F), length_frame(F,Ny, Nx), flatten(F,F1),
	print_steps(D2, F1, Ny).


generate_solution_with_fc_mrv([]).
generate_solution_with_fc_mrv(SolDom1) :-
	mrv_var(SolDom1, V1-V2/I1-I2/Domain, SolDom2),
	member(V1-V2, Domain),
	update_domains(V1-V2/I1-I2, SolDom2, SolDom3),
	generate_solution_with_fc_mrv(SolDom3).

mrv_var([V1-V2/I1-I2/Domain], V1-V2/I1-I2/Domain, []).
mrv_var([V11-V12/I11-I12/Domain1|SolDom1], V01-V02/I01-I02/Domain, SolDom3) :-
	mrv_var(SolDom1, V21-V22/I21-I22/Domain2, SolDom2),
	length(Domain1, N1),
	length(Domain2, N2),
	(N1 < N2 ->
		(V01 = V11, V02 = V12, 
		I01 = I11, I02 = I12,
		Domain = Domain1,
		SolDom3 = SolDom1) ;
		(V01 = V21, V02 = V22,
		I01 = I21, I02 = I22,
		Domain = Domain2,
		SolDom3 = [V11-V12/I11-I12/Domain1|SolDom2])).

generate_solution([]).
generate_solution([V1-V2/I1-I2/Domain|SolDom1]) :-
	member(V1-V2, Domain),
	update_domains(V1-V2/I1-I2, SolDom1, SolDom2),
	generate_solution(SolDom2).

update_domains(_, [], []).
update_domains(X, [V1-V2/V3-V4/Domain1|SolDom1], [V1-V2/V3-V4/Domain2|SolDom2]) :-
	update_domain(X, Domain1, Domain2),
	update_domains(X, SolDom1, SolDom2).

update_domain(X-I1/_-_, Domain1, Domain5) :-
	removeAllF(X-_, Domain1, Domain2),
	removeAllL(_-X, Domain2, Domain3),
	removeAllF(I1-_, Domain3, Domain4),
	removeAllL(_-I1, Domain4, Domain5).

removeAllF(_-_, [], []).
removeAllF(X-_, [X-_|T], L):- 
	removeAllF(X-_, T, L), !.
removeAllF(X-_, [H|T], [H|L]):- 
	removeAllF(X-_, T, L ).

removeAllL(_-_, [], []).
removeAllL(_-X, [_-X|T], L):- 
	removeAllL(_-X, T, L), !.
removeAllL(_-X, [H|T], [H|L]):- 
	removeAllL(_-X, T, L ).

/* Create list of variables of type X-Y in range(i,j) */
range(I,I,[_-_/(_,_)]).
range(I,K,[_-_/(_,_)|L]) :- I < K, I1 is I + 1, range(I1,K,L).

format_first_min([],[]).
format_first_min([Y1-Y2/(X1,X2)|L1],[Y4-Y3/(X4,X3)|L2]) :-
	(Y2 > Y1 -> Y4 = Y1, Y3 = Y2, X4 = X1, X3 = X2 ; Y4 = Y2, Y3 = Y1, X4 = X2, X3 = X1),
	format_first_min(L1,L2).


/* Printing formatting */

print_list1([]).
print_list1([H|T]) :-
	write(H),
	print_list1(T).

print_list2([]).
print_list2([H|T]) :-
	write(H), write(' '),
	print_list2(T).

print_steps(D2, F, Ny) :-
	print_steps(D2, F, Ny, [], [], 1, 1).

print_steps(D2, [F|Tail], Ny, List, Lis, I, Z) :-
	List \= [],
	I1 is I mod Ny, I1 =:= 1,
	writeln(''), print_list1(List), writeln(''),
	append([],[],List1),
	!,
	print_steps(D2, [F|Tail], Ny, List1, Lis, I, Z).

print_steps(D2, [F|Tail], Ny, List, Lis, I, Z) :-
	getN(Z, D2, X-Y/(V1,V2)),
	X = I,
	(X =:= Y - 1 -> 
		write(V1), write('-'), write(V2), write(' '), append(List, ['    '], List1), I1 is I + 2,
	append(Lis, [' ',' '], Lis1)
	; 
	write(V1), write(' '), append(List, ['| '] , List1), I1 is I + 1, append(Lis, [V2], Lis1)
	),
	Z1 is Z + 1, 
	print_steps(D2, Tail, Ny, List1, Lis1, I1, Z1),!.

print_steps(D2, [F|Tail], Ny, List, Lis, I, Z) :-
	getN(Z, D2, X-Y/(V1,V2)),
	X \= I,
	Temp1 is I - Ny, Temp2 is X - Ny - 1,
	findall(Lis, temp_step(Temp1, Temp2, List, Lis, Ny), _), 
	Temp3 is Temp2 - Temp1 + 1,
	build(' ', Temp3, Lis1),
	build('  ', Temp3, List1),
	append(Lis, Lis1, Lis2),
	Temp5 is Temp1 mod Ny, Temp6 is Temp2 mod Ny,
	(between(Temp6, Temp5, 1) -> build('  ', Temp6, Tl), append([], Tl, List2) ;
	between(Temp5, Temp6, 1) -> build('  ', Temp6, Tl), append([], Tl, List2) ;
	append(List, List1, List2)),
	Temp4 is I + Temp3,
	I3 is Temp2 mod Ny,
	(I3 =:= 0 -> writeln(''), print_steps(D2, [F|Tail], Ny, List2, Lis2, Temp4, Z),! ; 
	print_steps(D2, [F|Tail], Ny, List2, Lis2, Temp4, Z),!).
	

print_steps(D2, [F|Tail], Ny, List, Lis, I, Z) :-
	length(Lis,N),
	N1 is N - Ny + 1,
	slice(Lis, N1, N, Lis1),
	(getN(N2, Lis1, ' ') ->
	slice(Lis1, 1, N2, Lis2),
	print_list2(Lis2), writeln('') ; print_list2(Lis1), writeln('')).

temp_step(Temp1, Temp2, List, Lis, Ny) :-
	between(Temp1, Temp2, J),
	(I1 is J mod Ny, I1 =:= 1 ->
	writeln(''), print_list1(List), writeln(''), append([],[],List1), getN(J, Lis, E), write(E), write(' ') ;
	getN(J, Lis, E),
	write(E), write(' ')).

build(X, N, List)  :- 
	findall(X, between(1, N, _), List).


/* Merge sort */

merge1(List, List, []).
merge1(List, [], List).

merge1([Y1-Y2/(X1,X2)|RestMerged], [Y1-Y2/(X1,X2)|RestList1], [Y3-Y4/(X3,X4)|RestList2]) :-
	Y1 =< Y3,
	merge1(RestMerged,RestList1,[Y3-Y4/(X3,X4)|RestList2]).
merge1([Y3-Y4/(X3,X4)|RestMerged], [Y1-Y2/(X1,X2)|RestList1], [Y3-Y4/(X3,X4)|RestList2]) :-
	Y3 =< Y1,
	merge1(RestMerged,[Y1-Y2/(X1,X2)|RestList1],RestList2).

mergeSort([], []).
mergeSort([A|[]],[A]).

mergeSort(List, Sorted) :-
	length(List, N),
	FirstLength is N div 2,
	SecondLength is N - FirstLength,
	length(FirstUnsorted, FirstLength),
	length(SecondUnsorted, SecondLength),
	append(FirstUnsorted, SecondUnsorted, List),
	mergeSort(FirstUnsorted, FirstSorted),
	mergeSort(SecondUnsorted, SecondSorted),
	merge1(Sorted, FirstSorted, SecondSorted),!.

slice([X|_],1,1,[X]).
slice([X|Xs],1,K,[X|Ys]) :- K > 1, 
	K1 is K - 1, slice(Xs,1,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 1, 
	I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).



/* Ορισμός δεδομένων προβλήματος */


dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
			(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
					(2,2),(2,3),(2,4),(2,5),(2,6),
						(3,3),(3,4),(3,5),(3,6),
								(4,4),(4,5),(4,6),
									(5,5),(5,6),
											(6,6)]).

frame([	[3,1,2,6,6,1,2,2],
	[3,4,1,5,3,0,3,6],
	[5,6,6,1,2,4,5,0],
	[5,6,4,1,3,3,0,0],
	[6,1,0,6,3,2,4,0],
	[4,1,5,2,4,3,5,5],
	[4,1,0,2,4,5,2,0]]).


/* dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,a),
(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,a),
	(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,a),
		(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,a),
				(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,a),
					(5,5),(5,6),(5,7),(5,8),(5,9),(5,a),
							(6,6),(6,7),(6,8),(6,9),(6,a),
								(7,7),(7,8),(7,9),(7,a),
										(8,8),(8,9),(8,a),
											(9,9),(9,a),
													(a,a)]).

frame([[6,5,0,5,5,3,3,1,1,4,6],
[1,2,2,a,a,5,7,0,1,0,7],
[5,8,6,0,8,0,9,7,7,4,2],
[4,0,9,0,7,7,9,9,8,8,0],
[1,a,3,8,8,5,a,8,0,0,3],
[9,2,3,5,7,6,9,1,6,3,9],
[2,2,2,5,8,6,0,4,6,a,a],
[9,4,2,1,7,9,5,4,a,4,a],
[9,a,4,9,5,5,6,6,0,a,2],
[1,a,1,2,1,1,8,2,2,7,8],
[7,7,3,3,4,3,6,6,4,3,1],
[5,9,6,3,3,a,7,4,4,8,8]]).  */

/* dominos([(a,b),(b,c),(c,d),(d,e),(e,f),(f,g),(g,h),(h,i),(i,j),(j,k),(k,l),(l,m),
		(a,c),(b,d),(c,e),(d,f),(e,g),(f,h),(g,i),(h,j),(i,k),(j,l),(k,m),(l,n),
		(a,d),(b,e),(c,f),(d,g),(e,h),(f,i),(g,j),(h,k),(i,l),(j,m),(k,n),(l,o),
		(a,e),(b,f),(c,g),(d,h),(e,i),(f,j),(g,k),(h,l),(i,m),(j,n),(k,o),(l,p),
		(a,f),(b,g),(c,h),(d,i),(e,j),(f,k),(g,l),(h,m),(i,n),(j,o),(k,p),(l,q),
		(a,g),(b,h),(c,i),(d,j),(e,k),(f,l),(g,m),(h,n),(i,o),(j,p),(k,q),(l,r),
		(a,h),(b,i),(c,j),(d,k),(e,l),(f,m),(g,n),(h,o),(i,p),(j,q),(k,r),(l,s),
		(a,i),(b,j),(c,k),(d,l),(e,m),(f,n),(g,o),(h,p),(i,q),(j,r),(k,s),(l,t),
		(a,j),(b,k),(c,l),(d,m),(e,n),(f,o),(g,p),(h,q),(i,r),(j,s),(k,t),(l,u),
		(a,k),(b,l),(c,m),(d,n),(e,o),(f,p),(g,q),(h,r),(i,s),(j,t),(k,u),(l,v),
		(a,l),(b,m),(c,n),(d,o),(e,p),(f,q),(g,r),(h,s),(i,t),(j,u),(k,v),(l,w),
		(a,m),(b,n),(c,o),(d,p),(e,q),(f,r),(g,s),(h,t),(i,u),(j,v),(k,w),(l,x)]).

frame([[d,g,i,r,d,f,g,l,n,f,i,s,f,k,w,l],
	[k,e,a,j,k,e,s,k,j,k,b,i,r,c,j,o],
	[l,q,j,p,n,h,k,l,s,j,r,t,f,v,k,k],
	[x,k,a,d,f,m,m,o,c,g,d,h,j,i,c,u],
	[g,q,i,b,m,a,f,e,i,b,l,a,e,i,f,g],
	[n,a,o,i,p,g,r,l,r,h,a,o,g,l,p,i],
	[d,c,d,g,e,f,n,h,b,t,j,e,d,h,c,i],
	[i,m,g,b,q,i,b,f,c,l,l,b,u,i,h,t],
	[j,h,c,g,f,a,s,l,f,l,e,c,d,j,i,j],
	[p,s,n,d,a,p,c,l,b,e,k,j,u,t,h,g],
	[c,f,g,g,b,h,n,e,j,h,m,i,j,f,h,c],
	[f,l,w,h,e,o,h,j,k,j,v,d,b,b,n,k],
	[h,r,g,n,m,d,a,d,h,l,k,b,h,m,a,i],
	[o,j,l,e,k,g,d,m,e,h,k,r,j,j,l,k],
	[e,c,o,h,a,n,f,k,d,q,k,k,a,j,f,p],
	[v,l,i,q,p,p,k,o,m,e,d,l,g,m,k,s],
	[e,n,i,g,e,q,l,m,i,o,g,m,i,c,l,k],
	[q,n,j,q,l,h,f,o,b,j,p,c,l,l,u,t]]).  */

	