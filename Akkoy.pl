:- use_module(library(lists)).

%%% GUARDA O TAMANHO MÁXIMO DE NÚMEROS DE TODAS AS COLUNAS EX: 3, [1,1], [3,4,2] numbersSize EQUIVALE AO NR 3 %%%
:- dynamic numbersSize/1.

:- dynamic firstElement/1.

%%%%%%%% RETORNA A LISTA DE TAMANHO MAIOR %%%%%%%%%

getListSizesAux(_, [], 0).
getListSizesAux(L, [HEAD | TAILS], Indice) :- Indice > 0, nth1(Indice, L, X), length(X, Tamanho), HEAD is Tamanho,
								Length2 is Indice - 1, getListSizesAux(L, TAILS, Length2). 

getListSizes(L, R, Llength):- getListSizesAux(L, R1, Llength), reverse(R1,R), 
							select_max(MaxSize, R, _Y), assert(numbersSize(MaxSize)).

getMaxSizeList(L, Llength, Coluna) :- getListSizes(L, R, Llength), select_max(Elem, R, _Y), 
							nth1(Indice, R, Elem), nth1(Indice, L, Coluna), 
							nth1(1, Coluna, FirstElem), assert(firstElement(FirstElem)). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%% ESCREVER OS NÚMEROS %%%%%%%%

%%% L É A LISTA QUE GUARDA AS RESTRIÇÕES DAS COLUNAS %%%

%% ESCREVE ESPAÇOS ATÉ CHEGAR À COLUNA RESPETIVA
writeSpaces(0).
writeSpaces(IndexList) :- write('  '), I2 is IndexList - 1, writeSpaces(I2).

getfirstElem(L, Llength, ListaElim) :- getMaxSizeList(L, Llength, Coluna), 
								retract(firstElement(First)), delete(Coluna, First, ListaElim).

writeNumbers([]).
writeNumbers(L) :- length(L, Llength), getfirstElem(L, Llength, ListaElim), retract(numbersSize(Index)), 
				writeSpaces(Index), retract(firstElement(First)), write(First), writeNumbers(ListaElim).

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

choice(1).
choice(2).
choice(3).
