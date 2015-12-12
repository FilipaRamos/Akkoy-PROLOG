:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(between)).

:- include('cenas.pl'). 

main :- write('\33\[2J'), readSize(Size), randomBoard(W, B, Size), 
		nl, write(B), nl, displayBoard(Size, 0, W, []), readInput(W, B, Size).

readSize(Size) :- nl, write(' 1. Insert Size '), nl, 
					write(' 2. Random Size '), nl,
					read(Decision), Decision > 0, Decision < 3, 
					chooseSize(Decision, Size).

chooseSize(1, Size) :- write('Size? '), read(Size), nl.
chooseSize(2, Size) :- random(2, 10, Size).

readInput(W, B, Size) :- read(Command), interpret(Command, Size, W, B).

interpret('end', _, _, _) :- abort.
interpret('exit', _, _, _) :- abort.
interpret('solutions', Size, W, B) :- solutions(W, B, Solution), nl, write(B), nl, displayBoard(Size, 0, W, Solution), nl, getStats.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% WRITE THE RESTRICTIONS FOR WHITE SQUARES %%
writeWhiteRes(Res, NrLine) :- nth0(NrLine, Res, Result), length(Result, LRes), write(' '),
					writeElement(Result, LRes, 0), nl.

%% DISPLAY A BOARD ELEMENT %%
displayBoardElem(_, _, []) :- write('   ').
displayBoardElem(Index, NrLine, Board) :- nth0(Index, Board, Line), nth0(NrLine, Line, Elem), if(Elem == 0, write(' * '), write('   ')).

%%% WRITE THE NUMBERS %%%
writeElement(_, LengthList, LengthList).
writeElement(List, LengthList, Counter) :- nth0(Counter, List, Elem), write(Elem), write('   '), NewCounter is Counter + 1, writeElement(List, LengthList, NewCounter).

%% DISPLAY HORIZONTAL LINES %%
countLines(Size, Lines) :- Dash is Size - 1, HalfLine is 3*Size, Lines is Dash + HalfLine.

displayLine(Size) :- countLines(Size, Lines), write(' '), writeLine(Lines), nl.

writeLine(0).
writeLine(Lines) :- Lines > 0, write('-'), NewNr is Lines - 1, writeLine(NewNr).

%% DISPLAY VERTICAL LINES %%
writeDashes(Size, _, Size, _).
writeDashes(Size, NrLine, Index, Board) :- Index < Size, write('|'), displayBoardElem(Index, NrLine, Board), 
			NewIndex is Index + 1, writeDashes(Size, NrLine, NewIndex, Board).

createList(0,[]).
createList(Size, [H|T]) :- Size > 0, NewSize is Size - 1, H = '   ', 
					createList(NewSize, T).

createMatrix(0, [], _).
createMatrix(Size, [H|T], MatLength) :- Size > 0, NewSize is Size - 1, createList(MatLength, List),
					H = List, createMatrix(NewSize, T, MatLength).

%% DISPLAY BOARD %%
displayBoard(Size, Size, _, _) :- displayLine(Size).
displayBoard(Size, NrLine, Res, Board) :- displayLine(Size), writeDashes(Size, NrLine, 0, Board), 
							write('|'), writeWhiteRes(Res, NrLine), NewLine is NrLine + 1, displayBoard(Size, NewLine, Res, Board).
