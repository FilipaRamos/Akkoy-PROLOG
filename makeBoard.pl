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