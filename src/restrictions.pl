:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(between)).

	%%%%%%%%%%%%%%%%
	%%	RANDOM BOARD

createRow([]).
createRow([Head | Tail]) :- 
	random(1, 10, Random),
	if(Random =< 5, Head = 1, Head = 0),
	createRow(Tail).
		
createBoard([]).
createBoard([Head | Tail]) :-
	createRow(Head),
	createBoard(Tail).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	GET WHITE RESTRICTIONS

processBlack(Count, 0, Count).
processWhite(Elem, Count, NewCount, H) :- if(Elem == 0, NewCount is Count + 1,
							if(Count  > 0, processBlack(Count, NewCount, H), NewCount is 0)).

countConsecutiveWhiteAux([], Count, [Count|[]]).
countConsecutiveWhiteAux([Elem|Tail], Count, [H | T]) :-
                processWhite(Elem, Count, NewCount, H),
                if((NewCount == 0, Count \== 0),
                countConsecutiveWhiteAux(Tail, NewCount, T),
                countConsecutiveWhiteAux(Tail, NewCount, [H | T])).

countConsecutiveWhite([Elem| Rest], ListCount) :-
	number(Elem),
	countConsecutiveWhiteAux([Elem| Rest], 0, List),
	delete(List, 0, ListCount).

countConsecutiveWhite(_, _).
countWhite([],[]).
countWhite([Row|Rest],[Head|Tail]):-
	countConsecutiveWhite(Row, Head),
	countWhite(Rest,Tail).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	GET BLACK RESTRICTIONS

processWhite2(Count, 0, Count).
processBlack2(Elem, Count, NewCount, H) :- if(Elem == 1, NewCount is Count + 1,
							if(Count  > 0, processWhite2(Count, NewCount, H), NewCount is 0)).

countConsecutiveBlackAux([], Count, [Count|[]]).
countConsecutiveBlackAux([Elem|Tail], Count, [H | T]) :-
                processBlack2(Elem, Count, NewCount, H),
                if((NewCount == 0, Count \== 0),
                countConsecutiveBlackAux(Tail, NewCount, T),
                countConsecutiveBlackAux(Tail, NewCount, [H | T])).

countConsecutiveBlack([Elem| Rest], ListCount) :-
	number(Elem),
	countConsecutiveBlackAux([Elem| Rest], 0, List),
	delete(List, 0, ListCount).

countConsecutiveBlack(_, _).

countBlack([],[]).
countBlack([Row|Rest],[Head|Tail]):-
	countConsecutiveBlack(Row, Head),
	countBlack(Rest,Tail).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% CREATE A BOARD WITH DOMAIN VARIABLES

newBoard(Size, Board):- newBoardAux(Size, Size, Board).
 
newBoardAux(0,_,[]).
newBoardAux(Pos, Size, [Head | Tails]):-
	Pos > 0,
	length(Head, Size),
	Pos1 is Pos - 1,
	newBoardAux(Pos1, Size, Tails).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% CREATE THE RANDOM BOARD AND GET THE RESTRICTIONS

randomBoard(White, Black, Size) :-
	newBoard(Size, Vars),
	createBoard(Vars),
	countWhite(Vars, White),
	transpose(Vars, TransposedVars),
	countBlack(TransposedVars, Black).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% 	GET THE POSSIBLE POSITIONS FOR EACH ROW

fit([],[],_).
fit([B|Bs], [R|Rs], S):- B + R #=< S+1, fit(Bs, Rs, S).

getPossibilities(S, Begins, R1):-
                formLines(R1, Lines, Begins, _LastVal),
                domain(Begins, 1, S),
                fit(Begins, R1, S),
                disjoint1(Lines, [margin(1,1,1)]).

formLines([Rfirst], [f(Var,Rfirst,1)], [Var], Rfirst) :- !.
formLines([Rfirst|Rs], [f(Var,Rfirst,1)|Lines], [Var|Begins], LastVal) :- formLines(Rs, Lines, Begins, LastVal).

merge_begins([], [], []).
merge_begins([B|Bs], [R|Rs], [[B,R]|Merged]):-merge_begins(Bs, Rs, Merged).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% FORM THE MATRIX WITH THE DOMAIN VARIABLES

gen_matrix(_,0,M, M).
gen_matrix(Or, N, M, Acc):- N>0, length(Line, Or), append(Acc, [Line], Acc2), N1 is N - 1, gen_matrix(Or, N1, M, Acc2).
gen_matrix(N, M):- gen_matrix(N, N, M, []).

append_board([], Vars, Vars).
append_board([Line|Lines], Vars, Acc):- append(Acc, Line, Acc2), append_board(Lines, Vars, Acc2).
append_board(Board, Vars):- append_board(Board, Vars, []).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% SWAP THE COLOR (AUX FUNCTION)

swap_color(1,0).
swap_color(0,1).

	%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% APPLY THE RESTRICTIONS

apply_single_merged(_, _, Length, Length, _, _):-!.
apply_single_merged(List, I, CurDist, Length, Color, Result):-
						CurDist < Length,
						CurPos #= I+CurDist,
						element(CurPos, List, Elem),
						Elem #= Color,
						element(CurPos, Result, Res),
						Res #= 1,
						CurDist1 is CurDist+1,
						apply_single_merged(List, I, CurDist1, Length, Color, Result).

apply_single_merged(List, [I, Length], Color, Result):- apply_single_merged(List, I, 0, Length, Color, Result).

apply_merged(_, [], _, _).
apply_merged(List, [M|Merged], Color, Results):-
						apply_single_merged(List, M, Color, Results),
						apply_merged(List, Merged, Color, Results).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% COUNT THE NUMBER OF PAINTED CELLS

count_painted([], N, N).
count_painted([[_I, V]|Merged], Acc, N):- Acc2 is Acc + V, count_painted(Merged, Acc2, N).
count_painted(Merged, N):- count_painted(Merged, 0, N).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% WHEN THE RESTRICTION LIST IS EMPTY THE LINE IS COLORED WITH THE OPPOSED COLOR 

color_all([],_).
color_all([L|Ls], Color):-
						L #= Color,
						color_all(Ls, Color).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% CALL THE FUNCTIONS THAT RESTRICT EACH LINE

apply_restrictions(_, _, [x], _):- !.
apply_restrictions(_, List, [], Color):- !, swap_color(Color, InvertedColor), color_all(List, InvertedColor).
apply_restrictions(S, List, Restrictions, Color):-
						getPossibilities(S, Begins, Restrictions),
						merge_begins(Begins, Restrictions, Merged),
						length(Result, S),
						domain(Result, 0, 1),
						apply_merged(List, Merged, Color, Result),
						swap_color(Color, InvertedColor),
						count_painted(Merged, NumPainted),
						count(1, Result, #=, NumPainted),
						apply_inverted(List, Result, InvertedColor).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% CALL THE RESTRICTIONS RECURSIVELY

apply_all_restrictions(_, [], [], _).						
apply_all_restrictions(S, [M|Ms], [R|Rs], Color):-
						apply_restrictions(S, M, R, Color),
						apply_all_restrictions(S, Ms, Rs, Color).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% PAINT THE REST OF THE LINE WITH THE OPPOSED COLOR

apply_inverted([], [], _).
apply_inverted([Elem|Elems], [I|Is], InvertedColor):-
						I #= 0 #=> Elem #= InvertedColor,
						apply_inverted(Elems, Is, InvertedColor).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% FIND THE SOLUTION FOR THE PUZZLE

solutions(Rcolumns, Rrows, NewBoard) :- statistics(runtime, _RestrictionsRunTime),
						!,
						length(Rcolumns, N),
						gen_matrix(N, Board),
						!,
						apply_all_restrictions(N, Board, Rcolumns, 1),
						transpose(Board, Rows),
						!,
						apply_all_restrictions(N, Rows, Rrows, 0),
						append_board(Board, Vars),

						transpose(Board, NewBoard),
						domain(Vars, 0,1),
				write(Vars),nl,
						once(labeling([],Vars)).

	%%%%%%%%%%%%%%%%%
	%% GET STATISTICS

getStats :-
	nl,
	statistics(runtime, Stats),
	write('Runtime since the start: '), nth0(0, Stats, SinceStart), write(SinceStart),
	nl,
	write('Runtime of the solution finder: '), nth0(1, Stats, SincePrevious), write(SincePrevious),
	nl.

	%%%%%%%%%%%%%%%%%
	%% IDENTIFY AREAS

mark_areas_aux(ID, S, Matrix, AreaIDs, Color, X, Y):- X1 is X + 1, Y1 is Y + 1, X1 >= 1, X1 =< S, Y1 >= 1, Y1 =< S, 
				element(Y1, Matrix, Row), element(X1, Row, NewColor), element(Y1, AreaIDs, RowID), 
				element(X1, RowID, NewID), Color#=0 #=> (NewColor#=1 #=>NewID #= 0), Color#=0 #=> (NewColor #=0 #=> NewID #= ID).
mark_areas_aux(ID, S, Matrix, AreaIDs, Color, X, Y):- X1 is X + 1, Y1 is Y - 1, X1 >= 1, X1 =< S, Y1 >= 1, Y1 =< S, 
				element(Y1, Matrix, Row), element(X1, Row, NewColor), element(Y1, AreaIDs, RowID), 
				element(X1, RowID, NewID), Color#=0 #=> (NewColor#=1 #=>NewID #= 0), Color#=0 #=> (NewColor #=0 #=> NewID #= ID).
mark_areas_aux(ID, S, Matrix, AreaIDs, Color, X, Y):- X1 is X - 1, Y1 is Y + 1, X1 >= 1, X1 =< S, Y1 >= 1, Y1 =< S, 
				element(Y1, Matrix, Row), element(X1, Row, NewColor), element(Y1, AreaIDs, RowID), 
				element(X1, RowID, NewID), Color#=0 #=> (NewColor#=1 #=>NewID #= 0),  Color#=0 #=> (NewColor #=0 #=> NewID #= ID).
mark_areas_aux(ID, S, Matrix, AreaIDs, Color, X, Y):- X1 is X - 1, Y1 is Y - 1, X1 >= 1, X1 =< S, Y1 >= 1, Y1 =< S,  
				element(Y1, Matrix, Row), element(X1, Row, NewColor), element(Y1, AreaIDs, RowID), 
				element(X1, RowID, NewID), Color#=0 #=> (NewColor#=1 #=>NewID #= 0),  Color#=0 #=> (NewColor #=0 #=> NewID #= ID).

mark_areas(_, _, _, []).
mark_areas(S, Matrix, AreaIDs, [[X,Y]|Cells]):-
											write(Matrix),
                                            element(Y, Matrix, Row),
                                            write(Row), nl, nl,
                                            element(X, Row, Color),
                                            write(Color), nl, nl,
                                            element(Y, AreaIDs, RowID),
                                            write(RowID), nl, nl,
                                            element(X, RowID, ID),
                                            write(ID), nl, nl,
                                            mark_areas_aux(ID, S, Matrix, AreaIDs, Color, X, Y),
                                            mark_areas(S, Matrix, AreaIDs, Cells).

counts(_IDs, [], _ID).
counts(IDs, [C|Counts], ID):- count(ID, IDs, #=, C), counts(IDs, Counts, ID).

same_counts_aux(_C, []).
same_counts_aux(C, [O|OtherCounts]):- C #= 0 #\/ O#=0 #\/ C=O, same_counts_aux(C, OtherCounts).

same_counts([]).
same_counts([C|Counts]):- same_counts_aux(C, Counts), same_counts(Counts).

same_size_areas(N):- 
                     gen_matrix(N, Matrix),
                     append_board(Matrix, Vars),
                     domain(Vars, 0, 1),
                     findall([X,Y], (between(1, N, X), between(1, N, Y)), L),
                     gen_matrix(N, AreaIDs),
                     N2 is N * N,
                     append_board(AreaIDs, IDs),
                     domain(IDs, 0, N2),
                     write(' mark_areas '), nl,
                     mark_areas(N, Matrix, AreaIDs, L),
                     nl, write(' finished '), nl,
                     length(Counts, N2),
                     counts(IDs, Counts, 1),
                     same_counts(Counts),
                     labeling([], Vars), write(Vars).