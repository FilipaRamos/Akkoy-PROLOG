readInput :- read(Command), interpret(Command).

interpret(Command) :- Command = 'play', play.
interpret(Command) :- Command = 'Play', play.
interpret(Command) :- Command = 'undo', undo.

replaceP(_, _, [], []).
replaceP(O, R, [O|T], [R|T2]) :- replaceP(O, R, T, T2).
replaceP(O, R, [H|T], [H|T2]) :- dif(H,O), replaceP(O, R, T, T2).

pls5(V):-
        V = [C1,C2,C3,C4,S1,S2,S3,S4],
        domain(V, 1, 4),
        %Amarelo = 1, Azul= 2; Preto = 3, Verde = 4,
        all_different([C1,C2,C3,C4]),
        all_different([S1,S2,S3,S4]),
        element(PosAzul, [C1,C2,C3,C4], 2),
        PosAntesAzul #= PosAzul - 1,
        element(PosAntesAzul, [S1,S2,S3,S4], SizeAntesAzul),
        PosDepoisAzul #= PosAzul + 1,
        element(PosDepoisAzul, [S1,S2,S3,S4], SizeDepoisAzul),
        SizeAntesAzul #< SizeDepoisAzul,
        element(PosVerde, [C1,C2,C3,C4], 4),
        element(PosVerde, [S1,S2,S3,S4], SizeGreen),
        SizeGreen #= 1,
        PosVerde #> PosAzul, 
        element(PosAmarelo, [C1,C2,C3,C4], 1),
        element(PosPreto, [C1,C2,C3,C4], 3),
        PosAmarelo #> PosPreto,
        labeling([], V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solutions(C) :-
		%% BLACK IS 1 // WHITE IS 2
		C = [C1, C2, C3, C4],
		domain(C, 1, 2).
