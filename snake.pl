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
        countNeighbors(Extended),
        nonTouching(Extended).

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

% Checking for diagonal touching
touchingLeftDiag([2,0],[0,2]).
touchingRightDiag([0,2],[2,0]).

checkDiagTouch([_],[_]):-!.
checkDiagTouch([X1,Y1|T1],[X2,Y2|T2]):-
    \+ touchingLeftDiag([X1,Y1],[X2,Y2]),
    \+ touchingRightDiag([X1,Y1],[X2,Y2]),
    checkDiagTouch([Y1|T1],[Y2|T2]).

% Checking for body-head touching
case1(M,0):- M \= 0.
case2(0,N):- N \= 0.
case3(0,0).

%| [M][2][N]
%|  - [1] -
%|  -  -  -
touchingN([M,2,N],[0,1,0],[0,0,0]):-
    case1(M,N),
    case2(M,N),
    case3(M,N).
%|  -  - [M]
%|  - [1][2]
%|  -  - [N]
touchingE([0,0,M],[0,1,2],[0,0,N]):-
    case1(M,N),
    case2(M,N),
    case3(M,N).
%| [M] -  -
%| [2][1] -
%| [N] -  -
touchingW([M,0,0],[2,1,0],[N,0,0]):-
    case1(M,N),
    case2(M,N),
    case3(M,N).
%|  -  -  -
%|  - [1] -
%| [M][2][N]
touchingS([0,0,0],[0,1,0],[M,2,N]):-
    case1(M,N),
    case2(M,N),
    case3(M,N).

checkHead([_,_],[_,_],[_,_]):-!.
checkHead([X,Y,Z|R1],[A,B,C|R2],[D,E,F|R3]):-
    \+ touchingS([X,Y,Z],[A,B,C],[D,E,F]),
    \+ touchingN([X,Y,Z],[A,B,C],[D,E,F]),
    \+ touchingE([X,Y,Z],[A,B,C],[D,E,F]),
    \+ touchingW([X,Y,Z],[A,B,C],[D,E,F]),
    checkHead([Y,Z|R1],[B,C|R2],[E,F|R3]).

nonTouching([Grid1,Grid2]):-
    !,checkDiagTouch(Grid1,Grid2).
nonTouching([GridH,GridF,GridF2|GridT]):-
    checkDiagTouch(GridH,GridF),
    checkHead(GridH,GridF,GridF2),
    !,nonTouching([GridF,GridF2|GridT]).

%[0,0,2,1,0]
%[0,2,0,2,0]
%[0,1,0,0,0]
