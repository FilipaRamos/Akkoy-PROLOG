:- use_module(library(lists)).
:- use_module(library(clpfd)).

:- dynamic count/1.
:- dynamic restList/1.

:- assert(count(0)).
:- assert(restList([])).

readInput :- read(Command), interpret(Command).

interpret(Command) :- Command = 'play', play.
interpret(Command) :- Command = 'Play', play.
interpret(Command) :- Command = 'undo', undo.

replaceP(_, _, [], []).
replaceP(O, R, [O|T], [R|T2]) :- replaceP(O, R, T, T2).
replaceP(O, R, [H|T], [H|T2]) :- dif(H,O), replaceP(O, R, T, T2).


%%%%%%%%%%% PASSAR TABULEIRO LISTA DE LISTAS COM VARIÁVEIS -> 1 - BLACK  2 - WHITE
%%%%%%%%%%% PASSAR RESTRICOES POR COLUNA E POR LINHA

processWhite(Count, [H|T]) :- H = [Count], write(' List-->> '), write(NewList), Count is 0.

processBlack(Elem, Count, List) :- if(Elem = 1, Count is Count + 1, if(Count > 0, processWhite(Count, List), Count is 0)), write(' count -> '), write(Count).

countConsecutiveBlack(Row, 0, Count, Result).
countConsecutiveBlack(Row, NRow, Count, Result) :- NRow > 0, nth1(NRow, Row, Elem), write(' Elem = '), write(Elem), write(' Count1 > '), write(Count), processBlack(Elem, Count, Result), 
								NewN is NRow - 1, retract(count(C)), write(' C = '), write(C), countConsecutiveBlack(Row, NewN, C, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%solutions(B, RC, RL) :-
%%		%% BLACK IS 1 // WHITE IS 2
%%		B = [A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, C1, C2, C3, C4, C5, D1, D2, D3, D4, D5, E1, E2, E3, E4, E5],
%%		RC = [RC1, RC2, RC3, RC4, RC5],
%%		RL = [RL1, RL2, RL3, RL4, RL5],
%% 		retract(count(C)),
%%		countConsecutiveBlack([A1, A2, A3, A4, A5], 5, C),
%%		domain(B, 1, 2).

