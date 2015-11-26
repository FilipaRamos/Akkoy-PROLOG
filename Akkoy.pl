:- use_module(library(lists)).
:- use_module(library(clpfd)).

%% SAVE THE RESTRICTIONS LISTS AND THE BOARDS %%
:- dynamic horizontalRestNormal/1.
:- dynamic verticalRestNormal/1.
:- dynamic horizontalRestEasy/1.
:- dynamic verticalRestEasy/1.
:- dynamic board/1.

%% SQUARE REPRESENTA UM QUADRADO DO TABULEIRO E %%
%% PODE SER UM ESPAÇO EM BRANCO OU UM ASTERISCO %% 
%% QUE REPRESENTA OS QUADRADOS PINTADOS %%

square('   ').
square(' * ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      %% BOARD FORMAT %%
%%   -------------------    %%
%%	| * | * |   |   |   |   %%
%% 	 -------------------    %%
%%	|   |   | * |   | * |   %%
%% 	 -------------------    %%
%%	| * | * |   | * |   |   %%
%% 	 -------------------    %%

%% DISPLAY HORIZONTAL LINES %%

countLines(Size, Lines) :- Dash is Size - 1, HalfLine is 3*Size, Lines is Dash + HalfLine.

displayLines(Size) :- countLines(Size, Lines), write(' '), writeLine(Lines), nl.

writeLines(0).
writeLines(Lines) :- Lines > 0, write('-'), NewNr is Lines - 1, writeLines(NewNr).

%% DISPLAY VERTICAL LINES %%

displayDashes(Size, Board, Counter).
displayDashes(Size, Board, Counter) :- PlusSize is Size + 1, writeDashes(PlusSize).

writeDashes(0).
writeDashes(PlusSize) :- write('|'), NewSize is PlusSize - 1, writeDashes(NewSize).

createList(0,[]).
createList(Size, [H|T]) :- Size > 0, NewSize is Size - 1, H = square('   '), 
					createList(NewSize, T).

createMatrix(0, [], _MatLength).
createMatrix(Size, [H|T], _MatLength) :- Size > 0, NewSize is Size - 1, createList(_MatLength, List),
					H = List, createMatrix(NewSize, T, _MatLength).

createBoard(Size) :- createMatrix(Size, Board, Size), assert(boardEasy(Board)).

%%% WRITE THE NUMBERS %%%

writeElement(_List, LengthList, LengthList).
writeElement(_List, LengthList, Counter) :- nth0(Counter, _List, Elem), write(Elem), write(' '), NewCounter is Counter + 1, writeElement(_List, LengthList, NewCounter).

%% WRITE THE RESTRICTIONS FOR BLACK SQUARES %%
writeBlackRes(_Res, 0).
writeBlackRes(_Res, LengthRes) :- LengthRes > 0, nth1(LengthRes, _Res, Result), length(Result, LRes), write(' '), writeElement(Result, LRes, 0), nl, 
							NewLength is LengthRes - 1, writeBlackRes(_Res, NewLength).

%% WRITE THE RESTRICTIONS FOR WHITE SQUARES %%
writeWhiteRes(_Res, LengthRes, LengthRes).
writeWhiteRes(_Res, LengthRes, Counter) :- nth0(Counter, _Res, Result), length(Result, LRes), write(' '),
					writeElement(Result, LRes, 0), nl, NewCounter is Counter + 1, writeWhiteRes(_Res, LengthRes, NewCounter).


%%%%%%%% MENU %%%%%%%%%%

assertsResEasy :- assert(horizontalRestEasy([
								[1,2,1,1,1], 
								[' ', 1, 2, 1, ' ']
								])),
		   assert(verticalRestEasy([
		   						[5,' '],[' ',' '],[2,1], 
		   						[' ',' '], [1,2]
		   						])).

assertsResNormal :- assert(horizontalRestNormal([
								[3,1,' ', 1, ' ',4,1], 
								[' ', 1, ' ', 1, ' ', 1, ' '], 
								[' ', ' ', ' ', 1, ' ', ' ', ' ']
								])),
		   assert(verticalRestNormal([
		   						[1,3,' '],[1,2,1],[2,1,1], 
		   						[1,2,' '], [3,1,' '], [1,1,1], 
		   						[3,3,' ']
		   						])).


logo :- write('        |||        '), nl,
		write('       |||||       '), nl,
		write('      |||||||      '), nl,
		write('     |||||||||     '), nl,
		write('    || AKKOY ||    '), nl,
		write('   || PUZZLES ||   '), nl,
		write('  |||||||||||||||  '), nl,
		write(' ||||||||||||||||| '), nl,
		write('||||| 1. EASY |||||'), nl,
		write(' ||| 2. MEDIUM ||| '), nl,
		write('  ||| 3. HARD |||  '), nl,
		write('   |||||||||||||   '), nl,
		write('    |||||||||||    '), nl,
		write('     |||||||||     '), nl,
		write('      |||||||      '), nl,
		write('       |||||       '), nl,
		write('        |||        '). 

menu :- write('\33\[2J'), logo, write('Choose the difficulty of the puzzle: '), 
		read(A), A < 4, A > 0, choice(A).

%% CHOICE 1 IS EASY %%
%% CHOICE 2 IS NORMAL %%
%% CHOICE 3 IS HARD %%

choice(1) :- assertsResEasy, write('\33\[2J'), nl, retract(horizontalRestEasy(Horizontal)),
			retract(verticalRestEasy(Vertical)), 
			drawRes(Horizontal, Vertical, 2, 5).
choice(2) :- assertsResNormal, write('\33\[2J'), nl, retract(horizontalRestNormal(Horizontal)),
			retract(verticalRestNormal(Vertical)), 
			drawRes(Horizontal, Vertical, 3, 7).
choice(3).

%% DRAW RESTRICTIONS %%
drawRes(ResBlack, ResWhite, SizeBlack, SizeWhite) :- writeBlackRes(ResBlack, SizeBlack), writeWhiteRes(ResWhite, SizeWhite, 0).

