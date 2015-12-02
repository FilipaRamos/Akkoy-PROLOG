:- use_module(library(lists)).
:- use_module(library(clpfd)).

:- dynamic count/1.
:- dynamic restList/1.

:- assert(count(0)).

readInput :- read(Command), interpret(Command).

interpret(Command) :- Command = 'play', play.
interpret(Command) :- Command = 'Play', play.
interpret(Command) :- Command = 'undo', undo.

replaceP(_, _, [], []).
replaceP(O, R, [O|T], [R|T2]) :- replaceP(O, R, T, T2).
replaceP(O, R, [H|T], [H|T2]) :- dif(H,O), replaceP(O, R, T, T2).


%%%%%%%%%%% PASSAR TABULEIRO LISTA DE LISTAS COM VARIÁVEIS -> 1 - BLACK  2 - WHITE
%%%%%%%%%%% PASSAR RESTRICOES POR COLUNA E POR LINHA

terminateCount :- assert(count(0)), write(' NOVA CONTAGEM ').

processWhite(Count, [H|T]) :- H = Count, write(' ADICIONADO A LISTA ').

processBlack(Elem, Count, [H|T]) :- NewCount is Count + 1, if(Elem = 1, assert(count(NewCount)), if(Count > 0, processWhite(Count, [H|T]), terminateCount)) .

countConsecutiveBlack(Row, 0, List).
countConsecutiveBlack(Row, NRow, List) :- NRow > 0, retract(count(Count)), nth1(NRow, Row, Elem), processBlack(Elem, Count, List), write(List), NewN is NRow - 1, countConsecutiveBlack(Row, NewN, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%solutions(B, R) :-
%%		%% BLACK IS 1 // WHITE IS 2
%%		B = [A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, C1, C2, C3, C4, C5],
%%		domain(B, 1, 2).


