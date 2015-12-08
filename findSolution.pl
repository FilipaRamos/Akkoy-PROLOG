:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(samsort)).

%%%%%%%%%%% PASSAR TABULEIRO LISTA DE LISTAS COM VARIÁVEIS -> 1 - BLACK  2 - WHITE
%%%%%%%%%%% PASSAR RESTRICOES POR COLUNA E POR LINHA

processWhite(Count, 0, Count).

processBlack(Elem, Count, NewCount, H) :- if(Elem == 1, NewCount is Count + 1,
							if(Count  > 0, processWhite(Count, NewCount, H), NewCount is 0)),
  							write(NewCount).

countConsecutiveBlack([], 0, []).
countConsecutiveBlack([], Count, [Count|[]]).
countConsecutiveBlack([Elem|Tail], Count, [H | T]) :-
                processBlack(Elem, Count, NewCount, H),
                nl,
                if((NewCount == 0, Count \== 0),
                countConsecutiveBlack(Tail, NewCount, T),
                countConsecutiveBlack(Tail, NewCount, [H | T])).

count([], 0, []).
count([Elem | Tail], Count, [H|T]) :- NewCount is Count + Elem, if((NewCount == Count, Count \== 0), count(Tail, Count, [H | T]), count(Tail, NewCount, T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processRest(0, Column, List).
processRest(Size, [], List).
processRest(Size, Column, List) :- Size > 0,  nth1(Size, Column, ElemCol), nth1(Size, List, ElemList), write(' Vai dar falso '),
						ElemCol #= ElemList, 
						NewSize is Size - 1, processRest(NewSize, Column, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solutions(B, RWhite, RBlack) :-
		%% BLACK IS 1 // WHITE IS 2
		B = [A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, C1, C2, C3, C4, C5, D1, D2, D3, D4, D5, E1, E2, E3, E4, E5],
		domain(B, 1, 2),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      			RESTRINGIR COLUNAS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([A1, B1, C1, D1, E1], 0, List1),
		write(' Sucessful Count!! '), write(List1),
		length(List1, Size1), write(' Size = '), write(Size1),
		nth1(1, RBlack, Column1), length(Column1, Length1), write(' Length = '), write(Length1),
		Length1 #= Size1,
		write(' true '),
		samsort(Column1, NewColumn1), samsort(List1, NewList1), 
		processRest(Size1, NewColumn1, NewList1),
		write(' process Sucessful! '), 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([A2, B2, C2, D2, E2], 0, List2),
		write(' Sucessful Count!! '), write(List2),
		length(List2, Size2),
		nth1(2, RBlack, Column2), length(Column2, Length2),
		Length2 #= Size2,
		samsort(Column2, NewColumn2), samsort(List2, NewList2), 
		processRest(Size2, NewColumn2, NewList2),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([A3, B3, C3, D3, E3], 0, List3),
		write(' Sucessful Count!! '), write(List3),
		length(List3, Size3),
		nth1(3, RBlack, Column3), length(Column3, Length3),
		Length3 #= Size3,
		samsort(Column3, NewColumn3), samsort(List3, NewList3), 
		processRest(Size3, NewColumn3, NewList3),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([A4, B4, C4, D4, E4], 0, List4),
		write(' Sucessful Count!! '), write(List4),
		length(List4, Size4),
		nth1(4, RBlack, Column4), length(Column4, Length4),
		Length4 #= Size4,
		samsort(Column4, NewColumn4), samsort(List4, NewList4), 
		processRest(Size4, NewColumn4, NewList4),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([A5, B5, C5, D5, E5], 0, List5),
		write(' Sucessful Count!! '), write(List5),
		length(List5, Size5),
		nth1(5, RBlack, Column5), length(Column5, Length5),
		Length5 #= Size5,
		samsort(Column5, NewColumn5), samsort(List5, NewList5), 
		processRest(Size5, NewColumn5, NewList5),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%					RESTRINGIR LINHAS						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([A1, A2, A3, A4, A5], 0, List1),
		length(List1, Size1),
		nth1(1, RWhite, Column1), length(Column1, Length1),
		Length1 #= Size1,
		samsort(Column1, NewColumn1), samsort(List1, NewList1), 
		processRest(Size1, NewColumn1, NewList1),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([B1, B2, B3, B4, B5], 0, List2),
		length(List2, Size2),
		nth1(2, RWhite, Column2), length(Column2, Length2),
		Length2 #= Size2,
		samsort(Column2, NewColumn2), samsort(List2, NewList2), 
		processRest(Size2, NewColumn2, NewList2),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([C1, C2, C3, C4, C5], 0, List3),
		length(List3, Size3),
		nth1(3, RWhite, Column3), length(Column3, Length3),
		Length3 #= Size3,
		samsort(Column3, NewColumn3), samsort(List3, NewList3), 
		processRest(Size3, NewColumn3, NewList3),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([D1, D2, D3, D4, D5], 0, List4),
		length(List4, Size4),
		nth1(4, RWhite, Column4), length(Column4, Length4),
		Length4 #= Size4,
		samsort(Column4, NewColumn4), samsort(List4, NewList4), 
		processRest(Size4, NewColumn4, NewList4),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		countConsecutiveBlack([E1, E2, E3, E4, E5], 0, List5),
		length(List5, Size5),
		nth1(5, RWhite, Column5), length(Column5, Length5),
		Length5 #= Size5,
		samsort(Column5, NewColumn5), samsort(List5, NewList5), 
		processRest(Size5, NewColumn5, NewList5),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		labeling([ffc], B).

test(S, Begins, R1):-
                formLines(R1, Lines, Begins, LastVal),
                DomainEnd #= S-LastVal,
                domain(Begins, 0, DomainEnd),
                disjoint1(Lines, [margin(1,1,1)]),
                labeling([], Begins)
                .

formLines([Rfirst], [f(Var,Rfirst,1)], [Var], Rfirst) :- !.
formLines([Rfirst|Rs], [f(Var,Rfirst,1)|Lines], [Var|Begins], LastVal) :- formLines(Rs, Lines, Begins, LastVal).