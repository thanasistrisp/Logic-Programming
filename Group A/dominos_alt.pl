:- set_flag(print_depth, 10000).

dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
               (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                     (2,2),(2,3),(2,4),(2,5),(2,6),
                           (3,3),(3,4),(3,5),(3,6),
                                  (4,4),(4,5),(4,6),
                                        (5,5),(5,6),
                                              (6,6)]).

frame([[3,1,2,6,6,1,2,2],
       [3,4,1,5,3,0,3,6],
       [5,6,6,1,2,4,5,0],
       [5,6,4,1,3,3,0,0],
       [6,1,0,6,3,2,4,0],
       [4,1,5,2,4,3,5,5],
       [4,1,0,2,4,5,2,0]]).


/* Generating a solution:

The code largely follows N Queens with Forward Checking and MRV implementation. 
TempDom is a sub-part of SolDom: TempDom is a pair of Domino and Variable (that will hold the correct placement once done),
while SolDom combines that with the domain created for each domino.

Make_tile_domains is the predicate for the creation of each domino's domain. It creates a list of lists
of the format [(Value1, Value2), TileDomain], where TileDomain is a list of facts of the format
places(position(X1, Y1), position(X2, Y2)). Value1 and Value2 are strictly in the order the data are given.
So, for something like 2
                       |
                       5, we have:
[(2,5), places(position(1, 3), position(2, 3))], and
[(5,2), places(position(2, 3), position(1, 3))], which
are the vertical head-to-bottom representation of (2,5)
and the vertical bottom-to-head representation of (5,2), respectively.
Since the scanning of the frame to find each tiles domain takes place from top left corner to bottom right corner,
for tiles that hold the same value in each half, we just check if their 'northern' or 'western' neighbor 
holds the same value as them; if so, the tile has been accounted for. 

Generate_solution and update_domains, as well as mrv_var, are largely similar to the N Queens respective predicates.

The printing of the frame is naive. Without looking at the frame template, starting at position (1,1) and scanning left to right
and top to bottom, we find the tile that matches the position, and check for its orientation. If it's horizontal,
we see if the tile is rotated left-right, or right-left, and print a dash in the former case. If it's vertical, 
we check for the orientation again, head-to-bottom or bottom-to-head. If it's head to bottom, we add a pipe symbol to be printed after
the printing of the current line is done for. In any case where there is not a pipe symbol added (horizontal tile, or bottom-to-head vertical tile), 
we add a space to the 'filler' lines printed between actual frame lines, for consistency.
*/

prefix_spec_len(P, N, L) :- append(P, _, L), length(P, N).
last(X, [X]).
last(X, [_|Tail]) :- last(X, Tail).

between(N1, N2, N1):-
    N1 =< N2.
between(N1, N2, X) :-
    N1 < N2,
    NewN1 is N1 + 1,
    between(NewN1, N2, X).

take_element(List, Pos, Element) :- prefix_spec_len(Partial, Pos, List), last(Element, Partial).

put_dominos :-
    frame(L), dominos(D), 
    length(L, Height), length(D, Length),
    L = [ARandomLine|_], length(ARandomLine, FrameLength), length(Template, Length),
    combine_template_domino(D, Template, TempDom),
    make_tile_domains(L, D, Domain),
    combine_soldom(TempDom, Domain, SolDom), !,
    generate_solution(SolDom),
    print_frame(Height, FrameLength, TempDom), nl.

print_frame(Height, FrameLength, TempDom) :-
    between(1, Height, I),
    print_line(I, 1, FrameLength, TempDom, [], NewPendingForNext),  nl,
    print_pending(NewPendingForNext),  nl,
    (I =\= Height -> fail ; !).

print_pending([]).
print_pending([X|Rest]) :-
    write(X), print_pending(Rest).

print_line(_, J, FrameLength, _, PendingForNext, PendingForNext) :-
    J > FrameLength.    
    
print_line(I, J, FrameLength, TempDom, OldPendingForNext, NewPendingForNext) :-
    J =< FrameLength,
    member((Value1, Value2)-places(position(X1, Y1), position(X2, Y2)), TempDom),
    ((X1 =:= I, Y1 =:= J, Match = 1) ; (X2 =:= I, Y2 =:= J, Match = 2)), % We have a match with our current position.
    (X1 =:= X2 -> % Horizontal tile
        ((Match = 1 -> write(Value1) ; write(Value2)), % Print tile value
        (((Y1 < Y2, Match = 2) ; (Y1 > Y2, Match = 1)) -> write(' ') ; write('-'))), % If we just matched the left side of the tile, print the connecting dash. Otherwise, space character for alignment.
        append(OldPendingForNext, ['  '], Temp) % Adding a space, so that if following tiles in this line are head-to-bottom vertical, and pipe symbols are added, they will be aligned properly.
        ;
        (
            (Match = 1 -> write(Value1) ; write(Value2)), % Print tile value
            write(' ') ,
            (
                ((X1 < X2, Match = 1) ; (X1 > X2, Match = 2)) -> append(OldPendingForNext, ['| '], Temp) ; append(OldPendingForNext, ['  '], Temp)
            )
        )
    ), 
    NewJ is J + 1,
    print_line(I, NewJ, FrameLength, TempDom, Temp, NewPendingForNext).

combine_template_domino([], _, []).
combine_template_domino([Domino|RestDominos], [X|RestTemplate], [Domino-X|TempDom]) :-
    combine_template_domino(RestDominos, RestTemplate, TempDom).

combine_soldom([], _, []).
combine_soldom([Domino-X|RestDominos], [[Domino, DominoDomain]|RestDomain], [Domino-X-DominoDomain|SolDom]) :-
    combine_soldom(RestDominos, RestDomain, SolDom).

mrv_var([Domino-Variable-Domain], Domino-Variable-Domain, []).
mrv_var([Domino1-Variable1-Domain1|SolDom1], Domino-Variable-Domain, SolDom3) :-
   mrv_var(SolDom1, Domino2-Variable2-Domain2, SolDom2),
   length(Domain1, N1),
   length(Domain2, N2),
   (N1 < N2 ->
      (Domino = Domino1,
       Variable = Variable1,
       Domain = Domain1,
       SolDom3 = SolDom1) ;
      (Domino = Domino2,
       Variable = Variable2,
       Domain = Domain2,
       SolDom3 = [Domino1-Variable1-Domain1|SolDom2])).

generate_solution([]).
generate_solution(SolDom1) :-
    mrv_var(SolDom1, _-Variable-Domain, SolDom2),
    member(Variable, Domain),
    update_domains(Variable, SolDom2, SolDom3),
    generate_solution(SolDom3).

update_domains(_, [], []).
update_domains(Variable, [Variable2-Domino2-Domain1|SolDom1], [Variable2-Domino2-Domain2|SolDom2]) :-
   update_domain(Variable, Domain1, Domain2),
   update_domains(Variable, SolDom1, SolDom2).

update_domain(_, [], []).
update_domain(places(position(X1, Y1), position(X2, Y2)), [places(position(X3, Y3), position(X4, Y4))|RestDomain], Domain2) :-
   ((X1 = X3, Y1 = Y3) ; (X2 = X3, Y2 = Y3) ;
   (X1 = X4, Y1 = Y4) ; (X2 = X4, Y2 = Y4)), !,
   update_domain(places(position(X1, Y1), position(X2, Y2)), RestDomain, Domain2).
update_domain(places(position(X1, Y1), position(X2, Y2)), [places(position(X3, Y3), position(X4, Y4))|RestDomain1], [places(position(X3, Y3), position(X4, Y4))|RestDomain2]) :-
   update_domain(places(position(X1, Y1), position(X2, Y2)), RestDomain1, RestDomain2).

make_tile_domains(_, [], []).
make_tile_domains(Frame, [(Value1, Value2)|Rest], Domain) :-
    make_tile_domains(Frame, Rest, RestDomain),
    findall(Places, tile_is_a_match(Value1, Value2, Places, Frame), TileDomain),
    append([[(Value1, Value2), TileDomain]], RestDomain, Domain).

illegaldupe(Value1, Value2, Line1, Line2, Column1, Column2) :- 
    Value1 = Value2, % Only when tile is (X, X)
    (Line2 =:= Line1 - 1 ; Column2 =:= Column1 - 1). % If its the northern or western neighbor, then this will fail.

tile_is_a_match(Value1, Value2, places(position(Line1, Column1), position(Line2, Column2)), Frame) :- 
    length(Frame, Height),
    Frame = [ARandomLine|_], length(ARandomLine, Length), 
    between(1, Height, X), take_element(Frame, X, Line),
    between(1, Length, Y), take_element(Line, Y, Element),
    Element = Value1,
    find_neighbors(X, Y, Frame, position(X2, Y2)),
    take_element(Frame, X2, Temp), take_element(Temp, Y2, NewElem),
    NewElem = Value2,
    \+ illegaldupe(Value1, Value2, X, X2, Y, Y2),
    Line1 is X, Column1 is Y, 
    Line2 is X2, Column2 is Y2.

find_a_neighbor(Line, Column, _, _, position(North, Column)) :-
    North is Line - 1, North > 0.
find_a_neighbor(Line, _, _, _, []) :-
    North is Line - 1, North =< 0.

find_a_neighbor(Line, Column, Height, _, position(South, Column)) :-
    South is Line + 1, South =< Height.
find_a_neighbor(Line, _, Height, _, []) :-
    South is Line + 1, South > Height.

find_a_neighbor(Line, Column, _, Length, position(Line, East)) :-
    East is Column + 1, East =< Length.
find_a_neighbor(_, Column, _, Length, []) :-
    East is Column + 1, East > Length.

find_a_neighbor(Line, Column, _, _, position(Line, West)) :-
    West is Column - 1, West > 0.
find_a_neighbor(_, Column, _, _, []) :-
    West is Column - 1, West =< 0.

find_neighbors(Line, Column, Frame, Neighbor) :-
    length(Frame, Height), Line =< Height,
    Frame = [ARandomLine|_], length(ARandomLine, Length), Column =< Length,
    find_a_neighbor(Line, Column, Height, Length, Neighbor).
    
