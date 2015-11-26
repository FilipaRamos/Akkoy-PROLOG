:- use_module(library(lists)).

%% EASY -- > BLACK [[3,1,'  ', 1, '  ',4,1], ['  ', 1, '  ', 1, '  ', 1, '  '], ['  ', '  ', '  ', 1, '  ', '  ', '  ']] %%
%%%%%%% -- > WHITE [['  ',1,1,'  ','  ',1,'  '],[3,2,1,2,1,1,3],[1,1,2,1,3,1,3]] %%

%%% WRITE THE NUMBERS %%%

writeElement(List, LengthList, LengthList).
writeElement(List, LengthList, Counter) :- nth0(Counter, List, Elem), write(Elem), write(' '), NewCounter is Counter + 1, writeElement(List, LengthList, NewCounter).

%% WRITE THE RESTRICTIONS FOR BLACK SQUARES %%
writeBlackRes(Res, 0).
writeBlackRes(Res, LengthRes) :- LengthRes > 0, nth1(LengthRes, Res, Result), length(Result, LRes), write(' '), writeElement(Result, LRes, 0), nl, 
							NewLength is LengthRes - 1, writeBlackRes(Res, NewLength).

%% WRITE THE RESTRICTIONS FOR WHITE SQUARES %%
writeWhiteRes(Res, LengthRes, LengthRes).
writeWhiteRes(Res, LengthRes, Counter) :- nth0(Counter, Res, Result), length(Result, LRes), write(' '),
					writeElement(Result, LRes, 0), nl, NewCounter is Counter + 1, writeWhiteRes(Res, LengthRes, NewCounter).


%%%%%%%% MENU %%%%%%%%%%

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

choice(1) :- write('\33\[2J'), nl, easy([[3,1,' ', 1, ' ',4,1], [' ', 1, ' ', 1, ' ', 1, ' '], [' ', ' ', ' ', 1, ' ', ' ', ' ']], 
	[[1,3,' '],[1,2,1],[2,1,1], [1,2,' '], [3,1,' '], [1,1,1], [3,3,' ']], 3, 7).
choice(2).
choice(3).


easy(ResBlack, ResWhite, SizeBlack, SizeWhite) :- writeBlackRes(ResBlack, SizeBlack), writeWhiteRes(ResWhite, SizeWhite, 0).
