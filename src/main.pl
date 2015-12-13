:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(between)).

:- include('restrictions.pl'). 

main :- write('\33\[2J'), readSize(Size), randomBoard(W, B, Size), 
		nl, drawrestBlack(B, Size), nl, displayBoard(Size, 0, W, []), readInput(W, B, Size).

		%%%%%%%%%%%%%%%%
		%% READ THE SIZE

readSize(Size) :- nl, write(' 1. Insert Size '), nl, 
					write(' 2. Random Size '), nl,
					read(Decision), Decision > 0, Decision < 3, 
					chooseSize(Decision, Size).

chooseSize(1, Size) :- write('Size? '), read(Size), nl.
chooseSize(2, Size) :- random(2, 7, Size).

		%%%%%%%%%%%
		%% COMMANDS

readInput(W, B, Size) :- read(Command), interpret(Command, Size, W, B).

interpret('end', _, _, _) :- abort.
interpret('exit', _, _, _) :- abort.
interpret('solutions', Size, W, B) :- solutions(W, B, Solution), nl, drawrestBlack(B, Size), nl, displayBoard(Size, 0, W, Solution), nl, getStats.
interpret('solution', Size, W, B) :- solutions(W, B, Solution), nl, drawrestBlack(B, Size), nl, displayBoard(Size, 0, W, Solution), nl, getStats.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% WRITE THE RESTRICTIONS FOR BLACK SQUARES %%
doNothing.

displayList(_, Length, Length).
displayList(List, Length, Counter) :- Counter < Length,
		nth0(Counter, List, Elem), 
		if(Counter == 0, doNothing, write(',')), write(Elem), 
		NewCounter is Counter + 1, 
		displayList(List, Length, NewCounter).

restBlackAux(_, Size, Size).
restBlackAux(Rest, Size, Counter) :- Counter < Size,
		nth0(Counter, Rest, List), 
		length(List, Length), 
		if(Counter == 0, doNothing, write(' | ')),
		displayList(List, Length, 0),
		NewCounter is Counter + 1,
		restBlackAux(Rest, Size, NewCounter). 

drawrestBlack(Rest, Size) :-
	nl,
	write(' Black Restrictions: '),
	nl,
	restBlackAux(Rest, Size, 0), nl.

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

