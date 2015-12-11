:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).

createRow([]).
createRow([Head | Tail]) :- 
	random(1, 10, Random),
	if(Random =< 5, Head = 1, Head = 0),
	createRow(Tail).
		
createBoard([]).
createBoard([Head | Tail]) :-
	createRow(Head),
	createBoard(Tail).

processBlack(Count, 0, Count).
processWhite(Elem, Count, NewCount, H) :- if(Elem == 0, NewCount is Count + 1,
							if(Count  > 0, processBlack(Count, NewCount, H), NewCount is 0)).

countConsecutiveWhiteAux([], Count, [Count|[]]).
countConsecutiveWhiteAux([Elem|Tail], Count, [H | T]) :-
                processWhite(Elem, Count, NewCount, H),
                if((NewCount == 0, Count \== 0),
                countConsecutiveWhiteAux(Tail, NewCount, T),
                countConsecutiveWhiteAux(Tail, NewCount, [H | T])).

countConsecutiveWhite([Elem| Rest], ListCount) :-
	number(Elem),
	countConsecutiveWhiteAux([Elem| Rest], 0, List),
	delete(List, 0, ListCount).

countConsecutiveWhite(_, _).
countWhite([],[]).
countWhite([Row|Rest],[Head|Tail]):-
	countConsecutiveWhite(Row, Head),
	countWhite(Rest,Tail).

getPossibilities(S, Begins, R1):-
                formLines(R1, Lines, Begins, LastVal),
                DomainEnd #= S-LastVal,
                domain(Begins, 0, DomainEnd),
                disjoint1(Lines, [margin(1,1,1)]),
                labeling([], Begins)
                .

formLines([Rfirst], [f(Var,Rfirst,1)], [Var], Rfirst) :- !.
formLines([Rfirst|Rs], [f(Var,Rfirst,1)|Lines], [Var|Begins], LastVal) :- formLines(Rs, Lines, Begins, LastVal).

teste :-
	Vars = [[A1,A2,A3,A4], [B1,B2,B3,B4],[C1,C2,C3,C4], [D1,D2,D3,D4]],
	createBoard(Vars),
	countWhite(Vars,L),
	write(Vars),nl,
	Vars2 = [[E1,E2,E3,E4], [F1,F2,F3,F4],[G1,G2,G3,G4], [H1,H2,H3,H4]],
	domain([F1,F2,F3,F4], 0, 1),
	domain([E1,E2,E3,E4], 0, 1),
	domain([G1,G2,G3,G4], 0, 1),
	domain([H1,H2,H3,H4], 0, 1),

	/*labeling([],[F1,F2,F3,F4]),
	labeling([],[E1,E2,E3,E4]),
	labeling([],[G1,G2,G3,G4]),
	labeling([],[H1,H2,H3,H4]),
	write(Vars2).*/