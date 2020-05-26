:-[tests].
:-use_module(library(clpfd)).
:-set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]):-
    copyRow(Row,RowS),
    copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[_|S]):-
    copyRow(R,S).
copyRow([Clue|R],[Clue|S]):-
    copyRow(R,S).

snake(RowClues,ColClues,Grid,Solution):-
    copyGrid(Grid,Solution),
   % checkRowClues(Solution,RowClues),
   % checkColClues(Solution,ColClues),
    nonTouching(Solution)
   % countNeighbors(Solution),
   % snakeConnected(Solution)
   .

count_cell(0,X):-X is 0.
count_cell(_,X):-X is 1.

check_neighbors_pattern(0,_,_,_,_).
check_neighbors_pattern(Piece,N,E,S,W):-
    1 #=< Piece,
    count_cell(N,X1),
    count_cell(E,X2),
    count_cell(S,X3),
    count_cell(W,X4),
    Piece #= X1+X2+X3+X4.

check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]):-
    check_neighbors_pattern(M,N,E,S,W),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).


% CHECKING FOR DIAGONAL TOUCHING
% simple cases
checkDiagTouch([_],_):-!.
checkDiagTouch(_,[_]):-!.
% -----
%| 2 -
%| - 2
checkDiagTouch([2,X|T1],[Y,2|T2]):-
    (X==2,Y\=2);
    (X\=2,Y==2),
    !,
    checkDiagTouch([X|T1],[2,T2]).
% -----
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
checkHead([_,_],_,_):-!.
checkHead(_,[_,_],_):-!.
checkHead(_,_,[_,_]):-!.
%| [N][2] -
%|  - [1] -
%|  -  -  -
checkHead([N,2,X|R1],[Y,1,Z|R2],[A,B,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    N\=1,
    !,checkHead([2,X|R1],[1,Z|R2],[B,C|R3]).
%|  - [2][N]
%|  - [1] -
%|  -  -  -
checkHead([X,2,N|R1],[Y,1,Z|R2],[A,B,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    N\=1,
    !,checkHead([2,2|R1],[1,Z|R2],[B,C|R3]).
%|  -  -  -
%|  - [1][2]
%|  -  - [N]
checkHead([X,Y,Z|R1],[A,1,2|R2],[B,C,N|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    N\=1,
    !,checkHead([Y,Z|R1],[1,2|R2],[C,2|R3]).
%|  -  -  -
%| [2][1] -
%| [N] -  -
checkHead([X,Y,Z|R1],[2,1,A|R2],[N,B,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    N\=1,
    !,checkHead([Y,Z|R1],[1,A|R2],[B,C|R3]).
%|  -  -  -
%|  - [1] -
%|  - [2][N]
checkHead([X,Y,Z|R1],[A,1,B|R2],[C,2,N|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    N\=1,
    !,checkHead([Y,Z|R1],[1,B|R2],[2,2|R3]).
%|  -  -  -
%|  - [1] -
%| [N][2] -
checkHead([X,Y,Z|R1],[A,1,B|R2],[N,2,C|R3]):-
    not1or2([X,Y,Z,A,B,C]),
    N\=1,
    !,checkHead([Y,Z|R1],[1,B|R2],[2,C|R3]).
nonTouching([Grid1,Grid2]):-
    !,checkDiagTouch(Grid1,Grid2).
nonTouching([GridH,GridF,GridF2|GridT]):-
    % check head and follow for touching parts
    %checkDiagTouch(GridH,GridF),
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
