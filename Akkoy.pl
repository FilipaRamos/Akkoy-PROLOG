:- use_module(library(lists)).

%%%%%%%% RETORNA A LISTA DE TAMANHO MAIOR %%%%%%%%%

getListSizesAux(_, [], 0).
getListSizesAux(L, [HEAD | TAILS], Indice) :- Indice > 0, nth1(Indice, L, X), length(X, Tamanho), HEAD is Tamanho,
								Length2 is Indice - 1, getListSizesAux(L, TAILS, Length2). 

getListSizes(L, R, N):- getListSizesAux(L, R1, N),reverse(R1,R).

getMax(L, N, Coluna) :- getListSizes(L, R, N), write(R), select_max(Elem, R, Y), nth1(Indice, R, Elem), nth1(Indice, L, Coluna). 



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
