:- use_module(library(lists)).
:- use_module(library(clpfd)).

%% SAVE THE RESTRICTIONS LISTS AND THE BOARDS %%
:- dynamic horizontalRestNormal/1.
:- dynamic verticalRestNormal/1.
:- dynamic horizontalRestEasy/1.
:- dynamic verticalRestEasy/1.
:- dynamic horizontalRestHard/1.
:- dynamic verticalRestHard/1.
:- dynamic board/1.
:- dynamic sum/1.
:- dynamic blackSquares/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      %% BOARD FORMAT %%
%%   -------------------    %%
%%	| * | * |   |   |   |   %%
%% 	 -------------------    %%
%%	|   |   | * |   | * |   %%
%% 	 -------------------    %%
%%	| * | * |   | * |   |   %%
%% 	 -------------------    %%

%% DISPLAY A BOARD ELEMENT %%

displayBoardElem(Index, NrLine) :- retract(board(B)), nth0(NrLine, B, Line), nth0(Index, Line, Elem), write(Elem), assert(board(B)).

%% DISPLAY HORIZONTAL LINES %%

countLines(Size, Lines) :- Dash is Size - 1, HalfLine is 3*Size, Lines is Dash + HalfLine.

displayLine(Size) :- countLines(Size, Lines), write(' '), writeLine(Lines), nl.

writeLine(0).
writeLine(Lines) :- Lines > 0, write('-'), NewNr is Lines - 1, writeLine(NewNr).

%% DISPLAY VERTICAL LINES %%

writeDashes(Size, _NrLine, Size).
writeDashes(Size, _NrLine, Index) :- Index < Size, write('|'), displayBoardElem(Index, _NrLine), 
			NewIndex is Index + 1, writeDashes(Size, _NrLine, NewIndex).

createList(0,[]).
createList(Size, [H|T]) :- Size > 0, NewSize is Size - 1, H = '   ', 
					createList(NewSize, T).

createMatrix(0, [], _MatLength).
createMatrix(Size, [H|T], _MatLength) :- Size > 0, NewSize is Size - 1, createList(_MatLength, List),
					H = List, createMatrix(NewSize, T, _MatLength).

createBoard(Size) :- createMatrix(Size, Board, Size), assert(board(Board)).

%% DISPLAY BOARD %%

displayBoard(Size, Size, _Res) :- displayLine(Size).
displayBoard(Size, NrLine, _Res) :- displayLine(Size), writeDashes(Size, NrLine, 0), 
							write('|'), writeWhiteRes(_Res, NrLine), NewLine is NrLine + 1, displayBoard(Size, NewLine, _Res).

%%% WRITE THE NUMBERS %%%

writeElement(_List, LengthList, LengthList).
writeElement(_List, LengthList, Counter) :- nth0(Counter, _List, Elem), write(Elem), write('   '), NewCounter is Counter + 1, writeElement(_List, LengthList, NewCounter).

%% WRITE THE RESTRICTIONS FOR BLACK SQUARES %%
writeBlackRes(_Res, 0).
writeBlackRes(_Res, LengthRes) :- write('  '), !, LengthRes > 0, nth1(LengthRes, _Res, Result), length(Result, LRes), writeElement(Result, LRes, 0), nl, 
							NewLength is LengthRes - 1, writeBlackRes(_Res, NewLength).

%% WRITE THE RESTRICTIONS FOR WHITE SQUARES %%
writeWhiteRes(_Res, NrLine) :- nth0(NrLine, _Res, Result), length(Result, LRes), write(' '),
					writeElement(Result, LRes, 0), nl.

%% REPLACE BOARD ELEMENTS %%
%% COUNT STARTS AT INDEX 0 %%

replace([_|T], 0, X, [X|T]).
replace([H|T], Index, X, [H|R]):- Index > -1, NI is Index-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%% A PLAY %%

aPlay(X, Y, Elem) :- retract(board(Board)), nth1(X, Board, Line),
				Index is Y - 1, replace(Line, Index, Elem, NewLine), 
				NewX is X - 1, replace(Board, NewX, NewLine, NewBoard), 
				assert(board(NewBoard)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

assertsResHard :- assert(horizontalRestHard([
								[1,1,3,3,3,1,4,1,3],
								[1,1,2,' ',2,1,3,' ',2]
								])),
			assert(verticalRestHard([
								[3,2,1, ' '], [2,1,1,1],
								[2,1,1,1], [3,1,1,1],
								[2,2,1,' '], [3,2,1,' '],
								[1,1,1,1], [2,1,1,1], [4,3]
								])).

assertSum :- assert(sum(0)), assert(blackSquares(0)).


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

choice(1) :- engine('Easy').
choice(2) :- engine('Normal').
choice(3) :- engine('Hard').

%% CALCULATE BOARD SIZE %%

calculateBoardSize(Size, Difficulty) :- Difficulty = 'Easy', retract(verticalRestEasy(Rest)), length(Rest, Size), assert(verticalRestEasy(Rest)).
calculateBoardSize(Size, Difficulty) :- Difficulty = 'Normal', retract(verticalRestNormal(Rest)), length(Rest, Size), assert(verticalRestNormal(Rest)).
calculateBoardSize(Size, Difficulty) :- Difficulty = 'Hard', retract(verticalRestHard(Rest)), length(Rest, Size), assert(verticalRestHard(Rest)).

%% CHECK FINISH %%

displayFinalMessage :- write('Congrats! You just concluded the puzzle with success!!'), abort.

continue(X) :- X < 1, X > 1, write('merdou').

%% GAME LOOP %%

engine(Difficulty) :- Difficulty = 'Easy', 
					write('\33\[2J'), nl, assertsResEasy, %% ASSERT INITIAL VARIABLES
					calculateBoardSize(Size, Difficulty), createBoard(Size), !, assertSum, %% CREATE THE BOARD
					retract(horizontalRestEasy(ResBlack)), length(ResBlack, SizeBlack),  %% WRITE THE RESTRICTIONS
					writeBlackRes(ResBlack, SizeBlack), assert(horizontalRestEasy(ResBlack)), %% WRITE THE RESTRICTIONS
					retract(verticalRestEasy(ResWhite)), displayBoard(Size, 0, ResWhite), assert(verticalRestEasy(ResWhite)), %% WRITE THE RESTRICTIONS, DISPLAY THE BOARD
					readInput,     %% READ INPUT COMMANDS      
					countBoardSquares(Size, Size), retract(blackSquares(NrSquares)), nl, nl, nl,
					finish(NrSquares, Difficulty), displayFinalMessage. %% DISPLAY THE NEW BOARD, CHECK FINAL CONDITION


engine(Difficulty) :- Difficulty = 'Normal', 
					repeat, write('\33\[2J'), nl, assertsResNormal, assertSum, %% ASSERT INITIAL VARIABLES
					calculateBoardSize(Size, Difficulty), createBoard(Size), !,  %% CREATE THE BOARD
					retract(horizontalRestNormal(ResBlack)), length(ResBlack, SizeBlack),  %% WRITE THE RESTRICTIONS
					writeBlackRes(ResBlack, SizeBlack), assert(horizontalRestNormal(ResBlack)), %% WRITE THE RESTRICTIONS
					retract(verticalRestNormal(ResWhite)), displayBoard(Size, 0, ResWhite),  %% WRITE THE RESTRICTIONS, DISPLAY THE BOARD
					readInput, write('\33\[2J'), nl,      %% READ INPUT COMMANDS      
					displayBoard(Size, 0, ResWhite), countBoardSquares(Size, Size), retract(blackSquares(NrSquares)), 
					if(finish(NrSquares, Difficulty), displayFinalMessage, continue(1)). %% DISPLAY THE NEW BOARD, CHECK FINAL CONDITION


engine(Difficulty) :- Difficulty = 'Hard', 
					write('\33\[2J'), nl, assertsResHard, assertSum, %% ASSERT INITIAL VARIABLES
					calculateBoardSize(Size, Difficulty), createBoard(Size), !,  %% CREATE THE BOARD
					retract(horizontalRestHard(ResBlack)), length(ResBlack, SizeBlack),  %% WRITE THE RESTRICTIONS
					writeBlackRes(ResBlack, SizeBlack), assert(horizontalRestHard(ResBlack)), %% WRITE THE RESTRICTIONS
					retract(verticalRestHard(ResWhite)), displayBoard(Size, 0, ResWhite),  %% WRITE THE RESTRICTIONS, DISPLAY THE BOARD
					readInput, write('\33\[2J'), nl,      %% READ INPUT COMMANDS      
					displayBoard(Size, 0, ResWhite), countBoardSquares(Size, Size), retract(blackSquares(NrSquares)), 
					if(finish(NrSquares, Difficulty), displayFinalMessage, continue(1)). %% DISPLAY THE NEW BOARD, CHECK FINAL CONDITION

%% DEFINE CONSOLE COMMANDS %%

readInput :- read(Command), interpret(Command).

interpret(Command) :- Command = 'play', play.

interpret(Command) :- Command = 'undo', undo.

interpret(Command) :- Command = 'instructions', instructions.

interpret(Command) :- Command = 'restart', restart.

interpret(Command) :- Command = 'commands', commands.

interpret(Command) :- Command = 'exit', exit.

interpret(Command) :- Command = 'end', exit.

interpret(Command) :- Command = 'solution', solution.

interpret(Command) :- Command = 'menu', menu.

%% COMMANDS %%

play :- nl, write('X'), read(X), nl, write('Y'), read(Y), aPlay(X, Y, ' * ').

undo :- nl, write('X'), read(X), nl, write('Y'), read(Y), aPlay(X, Y, '   ').

instructions :- nl, nl, write('--- INSTRUCTIONS ---'), nl, write('Paint some cells black so that unpainted cells form at least two areas of the same size. Areas should be formed of edge-to-edge neighbouring cells and they can touch each other only diagonally. Numbers at the top indicate the amount of black cells blocks in the corresponding column. Numbers at the right indicate the amount of white cell blocks in the corresponding row.'), nl, nl.

restart.

commands :- nl, nl, write('--- COMMANDS ---'), nl,
			write('play --> paint a cell'), nl,
			write('undo --> undo a cell'), nl,
			write('restart --> start over'), nl,
			write('solution --> get the solution'), nl,
			write('menu --> menu'), nl,
			write('exit or end --> exit the game'), nl, nl.

exit :- abort.

solution.

%% FINISH THE PUZZLE %%

%% COUNT THE BLACK SQUARES ON THE BOARD %%

noCount.

checkElem(Elem) :- Elem = ' * '.

sumBlackSquares :- retract(blackSquares(NrSquares)), Incr is NrSquares + 1, assert(blackSquares(Incr)).

countSquares(0, Line).
countSquares(LineSize, Line) :- LineSize > 0, nth1(LineSize, Line, Elem), 
				if(checkElem(Elem), sumBlackSquares, noCount),
				NewSize is LineSize - 1, countSquares(NewSize, Line).

countBoardSquares(0, LineSize).
countBoardSquares(BoardSize, LineSize) :- BoardSize > 0, retract(board(Board)), nth1(BoardSize, Board, Line), countSquares(LineSize, Line),
							assert(board(Board)), NewSize is BoardSize - 1, countBoardSquares(NewSize, LineSize).
				

%% SUMS THE BLACK RESTRICTIONS WHICH IS THE FINAL NUMBER OF BLACK SQUARES %%

%% TO RETURN TRUE WHEN IT IS NOT A NUMBER %%
notNumber.

calculateSquares(Elem) :- retract(sum(Sum)), Sum2 is Sum + Elem, assert(sum(Sum2)). 

sumSquares(Line, 0).
sumSquares(Line, LengthRes) :- LengthRes > 0, nth1(LengthRes, Line, Elem), if(number(Elem), calculateSquares(Elem), notNumber), 
								NewLength is LengthRes - 1, sumSquares(Line, NewLength).  

recursiveBlackSquares(Rest, 0).
recursiveBlackSquares(Rest, RestSize) :- RestSize > 0, nth1(RestSize, Rest, Line), length(Line, Length),
										sumSquares(Line, Length), NewSize is RestSize - 1, 
										recursiveBlackSquares(Rest, NewSize).

countBlackSquares(Difficulty) :- Difficulty = 'Easy', retract(horizontalRestEasy(Rest)), length(Rest, RestSize),
								recursiveBlackSquares(Rest, RestSize), assert(horizontalRestEasy(Rest)).
countBlackSquares(Difficulty) :- Difficulty = 'Normal', retract(horizontalRestNormal(Rest)), length(Rest, RestSize),
								recursiveBlackSquares(Rest, RestSize), assert(horizontalRestNormal(Rest)).
countBlackSquares(Difficulty) :- Difficulty = 'Hard', retract(horizontalRestHard(Rest)), length(Rest, RestSize),
								recursiveBlackSquares(Rest, RestSize), assert(horizontalRestHard(Rest)).

finish(NrBlack, Difficulty) :- countBlackSquares(Difficulty), retract(sum(Sum)), NrBlack = Sum. 