:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(between)).

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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processWhite2(Count, 0, Count).
processBlack2(Elem, Count, NewCount, H) :- if(Elem == 1, NewCount is Count + 1,
							if(Count  > 0, processWhite2(Count, NewCount, H), NewCount is 0)).

countConsecutiveBlackAux([], Count, [Count|[]]).
countConsecutiveBlackAux([Elem|Tail], Count, [H | T]) :-
                processBlack2(Elem, Count, NewCount, H),
                if((NewCount == 0, Count \== 0),
                countConsecutiveBlackAux(Tail, NewCount, T),
                countConsecutiveBlackAux(Tail, NewCount, [H | T])).

countConsecutiveBlack([Elem| Rest], ListCount) :-
	number(Elem),
	countConsecutiveBlackAux([Elem| Rest], 0, List),
	delete(List, 0, ListCount).

countConsecutiveBlack(_, _).

countBlack([],[]).
countBlack([Row|Rest],[Head|Tail]):-
	countConsecutiveBlack(Row, Head),
	countBlack(Rest,Tail).

getPossibilities(S, Begins, R1):-
                formLines(R1, Lines, Begins, LastVal),
                DomainEnd #= S-LastVal,
                domain(Begins, 0, DomainEnd),
                disjoint1(Lines, [margin(1,1,1)])
                %labeling([], Begins)
                .

merge_begins([], [], []).
merge_begins([B|Bs], [R|Rs], [[B,R]|Merged]):-merge_begins(Bs, Rs, Merged).

formLines([Rfirst], [f(Var,Rfirst,1)], [Var], Rfirst) :- !.
formLines([Rfirst|Rs], [f(Var,Rfirst,1)|Lines], [Var|Begins], LastVal) :- formLines(Rs, Lines, Begins, LastVal).

gen_matrix(_,0,M, M).
gen_matrix(Or, N, M, Acc):- N>0, length(Line, Or), append(Acc, Line, Acc2), N1 is N - 1, gen_matrix(Or, N1, M, Acc2).
gen_matrix(N, M):- gen_matrix(N, N, M, []).

append_board([], Vars, Vars).
append_board([Line|Lines], Vars, Acc):- append(Acc, Line, Acc2), append_board(Lines, Vars, Acc2).
append_board(Board, Vars):- append_board(Board, Vars, []).

swap_color(1,0).
swap_color(0,1).

apply_single_merged(_, _, Length, Length, _, _):-!.
apply_single_merged(List, I, CurDist, Length, Color, Result):-
						CurDist < Length,
						CurPos #= I+CurDist,
						element(CurPos, List, Elem),
						Elem #= Color,
						element(CurPos, Result, Res),
						Res #= 1,
						I1 is I+1,
						apply_single_merged(List, I1, CurDist, Length, Color, Result).

apply_single_merged(List, [I, Length], Color, Result):- apply_single_merged(List, I, 0, Length, Color, Result).

apply_merged(_, [], _, _)
apply_merged(List, [M|Merged], Color, Results):-
						apply_single_merged(List, M, Color, Results),
						apply_merged(List, Merged, Color, Results).

apply_restrictions(S, List, Restrictions, Color):-
						getPossibilities(S, Begins, Restrictions),
						merge_begins(Begins, Restrictions, Merged),
						length(Result, S),
						apply_merged(List, Merged, Color, Result),
						length(Inverted, S),
						invert_list(Result, Inverted),
						swap_color(Color, InvertedColor),
						apply_inverted(List, Inverted, InvertedColor).

apply_inverted([], [], _).
apply_inverted([Elem|Elems], [I|Is], InvertedColor):-
						I #= 0 #=> Elem #= InvertedColor,
						apply_inverted(Elems, Is, InvertedColor).

akkoy(Rcolumns, Rrows):-
						length(Rcolumns, N),
						gen_matrix(N, Board),
						apply_restrictions(N, Board, Rcolumns, 1),
						transpose(Board, Rows),
						apply_restrictions( N, Rows, Rrows, 0),
						append_board(Board, Vars),
						labeling([],Vars).

list_difference([L1H|L1T], L2, L3):- 


restrictWhite(Begins, Length, Length, Row).
restrictWhite(Begins, Length, Counter, Row) :- 
		Counter < Length, 
		nth0(Counter, Begins, Index),
		nth0(Index, Row, Elem),
		Elem #= 0,
		NewCounter is Counter + 1,
		restrictWhite(Begins, Length, NewCounter, Row).

processBeginsWhiteAux(Begins, Row, Length, Length).
processBeginsWhiteAux(Begins, Row, Length, Counter) :- Counter < Length, 
						nth0(Counter, Begins, OneBegin),
						length(OneBegin, BeginLength),
						restrictWhite(OneBegin, BeginLength, 0, Row),
						NewCounter is Counter + 1,
						processBeginsWhiteAux(Begins, Row, Length, NewCounter).

processBeginsWhite(Begins, Row) :- length(Begins, Length), processBeginsWhiteAux(Begins, Row, Length, 0).

restrictBlack(Begins, Length, Length, Row).
restrictBlack(Begins, Length, Counter, Row) :- 
		Counter < Length, 
		nth0(Counter, Begins, Index),
		nth0(Index, Row, Elem),
		Elem #= 1,
		NewCounter is Counter + 1,
		restrictBlack(Begins, Length, NewCounter, Row).

processBeginsBlackAux(Begins, Row, Length, Length).
processBeginsBlackAux(Begins, Row, Length, Counter) :- Counter < Length, nth0(Counter, Begins, OneBegin), length(OneBegin, BeginLength), restrictBlack(OneBegin, BeginLength, 0, Row),
						NewCounter is Counter + 1, processBeginsBlackAux(Begins, Row, Length, NewCounter).  

processBeginsBlack(Begins, Row) :- length(Begins, Length), processBeginsBlackAux(Begins, Row, Length, 0).

findBegins(AllBegins, Rest) :- findall(Begins, getPossibilities(4, Begins, Rest), AllBegins).

teste :-
	Vars = [[A1,A2,A3,A4], [B1,B2,B3,B4],[C1,C2,C3,C4], [D1,D2,D3,D4]],
	createBoard(Vars),
	write(Vars), nl,
	countWhite(Vars,L),
	transpose(Vars, TransposedVars),
	countBlack(TransposedVars, Black),

	nth1(1, L, Rest1),
	nth1(2, L, Rest2),
	nth1(3, L, Rest3),
	nth1(4, L, Rest4),

	findBegins(Begins1, Rest1),
	findBegins(Begins2, Rest2),
	findBegins(Begins3, Rest3),
	findBegins(Begins4, Rest4),

	%%%%%%%%%%%%%%%%%

	nth1(1, Black, Black1),
	nth1(2, Black, Black2),
	nth1(3, Black, Black3),
	nth1(4, Black, Black4),

	findBegins(Bg1, Black1),
	findBegins(Bg2, Black2),
	findBegins(Bg3, Black3),
	findBegins(Bg4, Black4),

	Vars2 = [[E1,E2,E3,E4], [F1,F2,F3,F4], [G1,G2,G3,G4], [H1,H2,H3,H4]],
	domain([F1,F2,F3,F4], 0, 1),
	domain([E1,E2,E3,E4], 0, 1),
	domain([G1,G2,G3,G4], 0, 1),
	domain([H1,H2,H3,H4], 0, 1),

	processBeginsWhite(Begins1, [E1,E2,E3,E4]),

	processBeginsWhite(Begins2, [F1,F2,F3,F4]),

	processBeginsWhite(Begins3, [G1,G2,G3,G4]),

	processBeginsWhite(Begins4, [H1,H2,H3,H4]),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	write('onde esta a merdar?'),nl, nl, 

	processBeginsBlack(Bg1, [E1,F1,G1,H1]),

	processBeginsBlack(Bg2, [E2,F2,G2,H2]),

	processBeginsBlack(Bg3, [E3,F3,G3,H3]),

	processBeginsBlack(Bg4, [E4,F4,G4,H4]),

	write('labeling?'),nl, nl, 
	labeling([],[E1,E2,E3,E4]),
	labeling([],[F1,F2,F3,F4]),
	labeling([],[G1,G2,G3,G4]),
	labeling([],[H1,H2,H3,H4]),

	write(Vars2).