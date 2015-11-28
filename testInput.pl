readInput :- read(Command), interpret(Command).

interpret(Command) :- Command = 'play', play.
interpret(Command) :- Command = 'Play', play.
interpret(Command) :- Command = 'undo', undo.

replaceP(_, _, [], []).
replaceP(O, R, [O|T], [R|T2]) :- replaceP(O, R, T, T2).
replaceP(O, R, [H|T], [H|T2]) :- dif(H,O), replaceP(O, R, T, T2).