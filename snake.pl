:-[tests].
:-use_module(library(clpfd)).
:-set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes



% sample usage:
%
% this will print each row in the terminal
% test(p3x3, [_G0, G1, G2, G3|_G4]).
% currently this yields 3 solutions: 1 correct, 2 not connected

test(P, MappedGrid) :-
        puzzle(P,RowClues,ColClues,Grid),
        snake(RowClues,ColClues,Grid,MappedGrid).
        % print_only_grid(MappedGrid).

snake(RowClues, ColClues, Grid, Extended) :-
        copyGrid(Grid,Copied),
        checkRowClues(Copied, RowClues),
        checkColClues(Copied, ColClues),
        extend_grid(Copied, Extended),
        nonTouching(Extended),
        countNeighbors(Extended).

% snake2(_, _, Grid, Copied) :-
%         copyGrid(Grid,Copied).


%% ==============================================================
%% =================== constrain on RowClues ====================
%% ==============================================================

checkRowClues([], []).

checkRowClues([Row|Rows], [Clue|Clues]) :-
        Row ins 0..2,
        Clue #> -1,
        sumRow(Row, Clue),
        checkRowClues(Rows, Clues).

checkRowClues([Row|Rows], [Clue|Clues]) :-
        Row ins 0..2,
        Clue #= -1,
        checkRowClues(Rows, Clues).


sumRow(Row, Sum) :- sumRow(Row, 0, Sum).

sumRow([], Acc, Acc).
sumRow([R|Row], Acc, Sum) :-
        count_cell(R, C),
        NewAcc is Acc + C,
        sumRow(Row, NewAcc, Sum).

%% ==============================================================
%% =================== constrain on ColClues ====================
%% ==============================================================

checkColClues(Grid, ColClues) :-
        transpose(Grid, TransGrid),
        checkRowClues(TransGrid, ColClues).

%% ==============================================================
%% ========== constrain neighbors on N/E/S/W direction ==========
%% ==============================================================

%% idea:
%% 1. expand the grid, surrounding the original grid by padding row & col of 0s
%% 2. check the middle cell, with regards to its 4 neighbors
%% 3. recursively check all the cells in a row, by looking at 3 rows a time
%% 4. recursively check all the rows in a grid, by looking at 3 rows a time

% ----- [0] the copy function as given -----

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[_|S]) :- copyRow(R,S), !.      % a -1 applied with this rule, dont need to try the 3rd rule
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).


% ----- [1] add padding 0s -----

extend_grid(OldGrid,NewGrid) :-
      transpose(OldGrid,TransGrid),
      extend_grid_rows(TransGrid,RowTransGrid),
      transpose(RowTransGrid,RowGrid),
      extend_grid_rows(RowGrid,NewGrid).


extend_grid_rows([], []).
extend_grid_rows([R|Rs], [NewR|NewRs]) :-
    extend_row(R, NewR),
    extend_grid_rows(Rs, NewRs).


extend_row(OldRow,NewRow) :- append([0|OldRow],[0],NewRow).


% ----- [4] check all rows -----

% check_all_rows/1:
% takes a list of list,
% where the outer list is the grid, each inner list is a row
% pass every 3 rows to check_neighbors_rows

% base case: when there is only 2 rows left
countNeighbors([_, _]).
countNeighbors([R1, R2, R3|Rest]) :-
        check_neighbors_rows(R1, R2, R3),
        countNeighbors([R2, R3|Rest]).

% ----- [3] recursively check a row -----

% base case: when each row only has 2 element.
check_neighbors_rows([_, _], [_, _], [_, _]).
check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]):-
        check_neighbors_pattern(M,N,E,S,W),
        check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).


% ----- [2] check the middle cell -----

check_neighbors_pattern(0,_,_,_,_).
check_neighbors_pattern(Piece,N,E,S,W):-
        Piece in 0..2,
        N in 0..2,
        E in 0..2,
        S in 0..2,
        W in 0..2,
        1 #=< Piece,
        count_cell(N,X1),
        count_cell(E,X2),
        count_cell(S,X3),
        count_cell(W,X4),
        Piece #= X1+X2+X3+X4.


count_cell(0, 0).
count_cell(1, 1).
count_cell(2, 1).



% CHECKING FOR DIAGONAL TOUCHING
% simple cases
checkDiagTouch([_],[_]):-!.
%| 2 -
%| - 2
checkDiagTouch([2,X|T1],[Y,2|T2]):-
    (X==2,Y\=2);                               
    (X\=2,Y==2),
    !,
    checkDiagTouch([X|T1],[2,T2]).
%| - 2
%| 2 -
checkDiagTouch([X,2|T1],[2,Y|T2]):-
    (X==2,Y\=2);
    (X\=2,Y==2),
    !,
    checkDiagTouch([2|T1],[Y,T2]).
% other cases
checkDiagTouch([_|T1],[_|T2]):-
    !,checkDiagTouch(T1,T2).

% head
not1or2([]).
not1or2([H|T]):-
    H\=1,
    H\=2,
    !,not1or2(T).
% simple cases
checkHead([_,_],[_,_],[_,_]):-!.
%| [N][2] -
%|  - [1] -
%|  -  -  -
checkHead([_,2,X|R1],[Y,1,Z|R2],[A,B,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    !,checkHead([2,X|R1],[1,Z|R2],[B,C|R3]).
%|  - [2][N]
%|  - [1] -
%|  -  -  -
checkHead([X,2,_|R1],[Y,1,Z|R2],[A,B,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    !,checkHead([2,2|R1],[1,Z|R2],[B,C|R3]).            
%|  -  -  -
%|  - [1][2]
%|  -  - [N]
checkHead([X,Y,Z|R1],[A,1,2|R2],[B,C,_|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    !,checkHead([Y,Z|R1],[1,2|R2],[C,2|R3]).
%|  -  -  -
%| [2][1] -
%| [N] -  -
checkHead([X,Y,Z|R1],[2,1,A|R2],[_,B,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    !,checkHead([Y,Z|R1],[1,A|R2],[B,C|R3]).
%|  -  -  -
%|  - [1] -
%|  - [2][N]
checkHead([X,Y,Z|R1],[A,1,B|R2],[C,2,_|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    !,checkHead([Y,Z|R1],[1,B|R2],[2,2|R3]).
%|  -  -  -
%|  - [1] -
%| [N][2] -
checkHead([X,Y,Z|R1],[A,1,B|R2],[_,2,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    !,checkHead([Y,Z|R1],[1,B|R2],[2,C|R3]).
checkHead([_|R1],[_|R2],[_|R3]):-
    !,checkHead(R1,R2,R3).


nonTouching([Grid1,Grid2]):-
    !,checkDiagTouch(Grid1,Grid2).
nonTouching([GridH,GridF,GridF2|GridT]):-
    % check head and follow for touching parts
    checkDiagTouch(GridH,GridF),
    checkHead(GridH,GridF,GridF2),
    !,nonTouching([GridF,GridF2|GridT]).

%% head and tail control
%  ---------------------
%| [2][2] -  |  - [2][2]
%|  - [1] -  |  - [1] -
%|  -  -  -  |  -  -  -
%|
%|  -  -  -  |  -  -  -
%|  - [1][2] | [2][1] -
%|  -  - [2] | [2] -  -
%|
%|  -  -  -  |  -  -  -
%|  - [1] -  |  - [1] -
%|  - [2][2] | [2][2] -




%% ========== some comments ==========
%% for `checkDiagTouch`, the pattern 
%% %| 2 1
%% %| - 2
%% would pass, though it is not correct.
%% 
%% (But I see you are checking the 1 cells in `chechHead`.)
%% 
%% line 187/193/205, you cannot be certain that N is 2.
%% for example, in this case N should be 0: 
%% %|  -  -  -
%% %|  - [1] -
%% %|  - [2] [N] 
%% %|  - [2] - 
%% %|  - [2] -
%% 
%% ========== improvement idea ==========
%% 1. for `checkDiagTouch`, can build on what we have now, 
%%    pass the block of 4 to a helper predicate like: 
%%    helper([A11, A12], [A21, A22]) :- (A11 #= nonZero,  A12 #= zero, 
%%                                       A21 #= zero,     A22 #\= nonZero), !, fail.
%%    helper([A11, A12], [A21, A22]) :- (A11 #= zero,     A12 #= nonZero, 
%%                                       A21 #= nonZero,  A22 #\= zero), !, fail.
%%    helper([_,_], [_,_]) :- succeed.  
%%      
%%    in the bracket is the pattern we don't want, anything else succeds.
%%
%% 2. for `checkHead` you are doing the backtracking (trying for diff possibilities) manually, 
%%    can make prolog do that, the structure will be very similiar to countNeighbors, 
%%    I'm thinking maybe we can move `checkHead` to countNeighbors, 
%%    just need to add a case in `check_neighbors_pattern` when Piece #= 1.
%%








