:- use_module(library(lists)).

%%%%%%%% RETORNA A LISTA DE TAMANHO MAIOR %%%%%%%%%

getMaxList(L, R, Length, Length).
getMaxList(L, R, C, Length) :- C < Length, nth1(C, L, X), length(X, Tamanho), append(R, Tamanho, L2), write('chegou aqui'),
								C2 is C + 1, getMaxList(L, L2, C2, Length). 

getMax(L, N, Coluna) :- getMaxList(L, R, 0, N), select_max(Elem, R, Y), nth1(Elem, R, Indice), nth1(Indice, L, Coluna). 



%%%%%%%% MENU %%%%%%%%%%

