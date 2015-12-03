:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).

%% REPLACE BOARD ELEMENTS %%
%% COUNT STARTS AT INDEX 0 %%

replace([_|T], 0, X, [X|T]).
replace([H|T], Index, X, [H|R]):- Index > -1, NI is Index-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%% CREATE EMPTY MATRIX

createList(0,[]).
createList(Size, [H|T]) :- Size > 0, NewSize is Size - 1, H = 1, 
					createList(NewSize, T).

createMatrix(0, [], _MatLength).
createMatrix(Size, [H|T], _MatLength) :- Size > 0, NewSize is Size - 1, createList(_MatLength, List),
					H = List, createMatrix(NewSize, T, _MatLength).

randomBoard :- random(3, 9, Size), makeBoard(Size).

makeBoard(Size) :- setArea(Size, AreaSize), createArea(AreaSize, 3), createMatrix(Size, Matrix, Size).

setArea(Size, AreaSize) :- random(1, Size, AreaSize).

createArea(Size, AreaSize, NrAreas, Matrix, NewMatrix) :- random(1, Size, LineN), nth1(LineN, Matrix, Line), 
							random(1, Size, Index), write(LineN), write(Index), NewIndex is Index - 1, 
							replace(Line, NewIndex, 2, NewLine), NewL is LineN - 1, replace(Matrix, NewL, NewLine, NewMatrix), findAdjacent(NewMatrix, Size, AreaSize, LineN, Index).

findAdjacent(Matrix, Size, AreaSize, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

randomBoard(B) :- 
		B = [A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1, D2, D3, D4],
		domain(B, 1, 2),
		countConsecutiveWhite([A1, A2, A3, A4], List1),
		length(List1, Size1),
		Size1 #<= 2,
		countConsecutiveWhite([B1, B2, B3, B4], List2),
		length(List2, Size2),
		Size2 #= Size1,
		countConsecutiveWhite([C1, C2, C3, C4], List3),
		length(List3, Size3),
		Size3 #= Size1,
		countConsecutiveWhite([D1, D2, D3, D4], List4),
		length(List4, Size4),
		Size4 #= Size1,
		labeling([ffc], B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getRestrictions(Board, TransposedBoard, WhiteRest, BlackRest, 0).
getRestrictions(Board, TransposedBoard, WhiteRest, BlackRest, Size) :- Size > 0, nth1(Size, Board, Row), length(Row, RowLength), nth1(Size, TransposedBoard, Column), length(Column, ColumnLength),
						countConsecutiveWhite(Row, RowLength, 0, ResultWhite), append(WhiteRest, ResultWhite, WhiteRest), 
						countConsecutiveBlack(Column, ColumnLength, 0, ResultBlack), append(BlackRest, ResultBlack, BlackRest),
						NewSize is Size - 1, getRestrictions(Board, WhiteRest, NewSize). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% DISPLAY A BOARD ELEMENT %%

displayBoardElem(Index, NrLine, Board) :- nth0(NrLine, Board, Line), nth0(Index, Line, Elem), if(Elem = 1, write(' * '), write('   ')).

%% DISPLAY HORIZONTAL LINES %%

countLines(Size, Lines) :- Dash is Size - 1, HalfLine is 3*Size, Lines is Dash + HalfLine.

displayLine(Size) :- countLines(Size, Lines), write(' '), writeLine(Lines), nl.

writeLine(0).
writeLine(Lines) :- Lines > 0, write('-'), NewNr is Lines - 1, writeLine(NewNr).

%% DISPLAY VERTICAL LINES %%

writeDashes(Size, _NrLine, Size, Board).
writeDashes(Size, _NrLine, Index, Board) :- Index < Size, write('|'), displayBoardElem(Index, _NrLine, Board), 
			NewIndex is Index + 1, writeDashes(Size, _NrLine, NewIndex, Board).

createList(0,[]).
createList(Size, [H|T]) :- Size > 0, NewSize is Size - 1, H = '   ', 
					createList(NewSize, T).

createMatrix(0, [], _MatLength).
createMatrix(Size, [H|T], _MatLength) :- Size > 0, NewSize is Size - 1, createList(_MatLength, List),
					H = List, createMatrix(NewSize, T, _MatLength).

%% DISPLAY BOARD %%

displayRandomBoard(Size, Size, _Res, Board) :- displayLine(Size).
displayRandomBoard(Size, NrLine, _Res, Board) :- displayLine(Size), writeDashes(Size, NrLine, 0, Board), 
							write('|'), nl,  NewLine is NrLine + 1, displayRandomBoard(Size, NewLine, _Res, Board).