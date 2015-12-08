:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(samsort)).

test(S, Begins, R1):-
                formLines(R1, Lines, Begins, LastVal),
                DomainEnd #= S-LastVal,
                domain(Begins, 0, DomainEnd),
                disjoint1(Lines, [margin(1,1,1)]),
                labeling([], Begins)
                .

formLines([Rfirst], [f(Var,Rfirst,1)], [Var], Rfirst) :- !.
formLines([Rfirst|Rs], [f(Var,Rfirst,1)|Lines], [Var|Begins], LastVal) :- formLines(Rs, Lines, Begins, LastVal).

processColumn(Size, RestBlack, Size, [H|T]).
processColumn(Size, RestBlack, Counter, [H | T]) :- nth0(Counter, RestBlack, Column), test(Size, H, Column), NewCounter is Counter + 1, processColumn(Size, RestBlack, NewCounter, T).

processLine(Size, RestWhite, Size, [H|T]).
processLine(Size, RestWhite, Counter, [H | T]) :- nth0(Counter, RestWhite, Line), test(Size, H, Line), NewCounter is Counter + 1, processColumn(Size, RestWhite, NewCounter, T).

process(Size, RestBlack, RestWhite, Columns, Lines) :- processColumn(Size, RestBlack, 0, Columns), processLine(Size, RestWhite, 0, Lines).

solution(B, RestBlack, RestWhite, Size) :- 
				B = [L1, L2, L3, L4],
				domain(B, 1, 0),
				process(Size, RestBlack, RestWhite, Columns, Lines),
				labeling([], B).